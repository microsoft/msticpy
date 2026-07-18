# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
Session bootstrap for the MSTICPy MCP server.

Initializes MSTICPy at server startup: loads configuration, initializes the
Pivot subsystem, loads and (non-interactively) connects any configured query
providers, and holds mutable session state (the query timespan).
"""

from __future__ import annotations

import importlib
import logging
from datetime import datetime, timedelta, timezone
from typing import Any

from .._version import VERSION
from .catalog import PivotCatalog
from .config import ComponentConfig, McpServerConfig, QueryProviderConfig
from .results import ResultStore

__version__ = VERSION
__author__ = "Ian Hellen"

logger = logging.getLogger(__name__)


class ProviderState:
    """Tracks the state of a loaded query provider or component."""

    def __init__(self, name: str, environment: str) -> None:
        """Initialize provider state record."""
        self.name = name
        self.environment = environment
        self.connected = False
        self.error: str | None = None

    def as_dict(self) -> dict[str, Any]:
        """Return a JSON-serializable status dict (no secrets)."""
        return {
            "name": self.name,
            "environment": self.environment,
            "connected": self.connected,
            "error": self.error,
        }


class MpMcpSession:
    """Holds all runtime state for the MSTICPy MCP server."""

    def __init__(self, config: McpServerConfig | None = None) -> None:
        """
        Initialize (but do not start) the session.

        Parameters
        ----------
        config : McpServerConfig | None, optional
            Server configuration. If None, it is loaded from MSTICPy settings
            during :meth:`start`.

        """
        self.config = config
        self.pivot: Any = None
        self.catalog = PivotCatalog()
        self.results: ResultStore = ResultStore()
        self.providers: dict[str, ProviderState] = {}
        self._config_path: str | None = None
        self._started = False

    @property
    def started(self) -> bool:
        """Return True if the session has been started."""
        return self._started

    def start(self) -> MpMcpSession:
        """
        Start the session: init MSTICPy, load pivots, connect providers.

        Returns
        -------
        MpMcpSession
            This session (for chaining).

        """
        if self._started:
            return self
        # Lazy imports so importing this module does not pull in the full stack.
        from msticpy.common import (  # noqa: PLC0415  # pylint: disable=import-outside-toplevel
            pkg_config,
        )
        from msticpy.init.pivot import (  # noqa: PLC0415  # pylint: disable=import-outside-toplevel
            Pivot,
        )

        settings = pkg_config.get_config()
        try:
            self._config_path = pkg_config.current_config_path()
        except Exception:  # pylint: disable=broad-except
            self._config_path = None
        if self.config is None:
            self.config = McpServerConfig.from_msticpy_settings(settings)

        self.pivot = Pivot()
        self.pivot.reload_pivots()

        self.results = ResultStore(
            max_results=self.config.result_store.max_results,
            max_rows_per_result=self.config.result_store.max_rows_per_result,
        )
        # Set a sensible default timespan for query pivots.
        self.set_timespan(days=self.config.defaults.timespan_days)

        self._load_query_providers()
        self._load_components()

        # Build the catalog after providers are attached (so query pivots appear).
        self.catalog.build()
        self._started = True
        logger.info(
            "MSTICPy MCP session started: %d pivots, %d providers",
            len(self.catalog),
            len(self.providers),
        )
        return self

    def _load_query_providers(self) -> None:
        """Instantiate and connect configured query providers."""
        from msticpy.data.core.data_providers import (  # noqa: PLC0415  # pylint: disable=import-outside-toplevel
            QueryProvider,
        )

        assert self.config is not None  # nosec - set in start()
        for name, prov_cfg in self.config.query_providers.items():
            state = ProviderState(name, prov_cfg.data_environment)
            self.providers[name] = state
            try:
                provider = QueryProvider(prov_cfg.data_environment, **prov_cfg.init_args)
            except Exception as err:  # pylint: disable=broad-except
                state.error = f"init failed: {err}"
                logger.warning("Query provider %s init failed: %s", name, err)
                continue
            if prov_cfg.connect:
                self._connect_provider(provider, prov_cfg, state)
            try:
                self.pivot.add_query_provider(provider)
            except Exception as err:  # pylint: disable=broad-except
                logger.warning("Failed to attach provider %s to pivot: %s", name, err)

    def _connect_provider(
        self,
        provider: Any,
        prov_cfg: QueryProviderConfig,
        state: ProviderState,
    ) -> None:
        """Connect a query provider (non-interactive), recording any error."""
        try:
            provider.connect(**prov_cfg.connect_args)
            state.connected = True
            logger.info("Connected query provider %s", state.name)
        except Exception as err:  # pylint: disable=broad-except
            state.error = f"connect failed: {err}"
            logger.warning("Query provider %s connect failed: %s", state.name, err)

    def _load_components(self) -> None:
        """Instantiate and connect configured non-query components."""
        assert self.config is not None  # nosec - set in start()
        for name, comp_cfg in self.config.components.items():
            state = ProviderState(name, f"{comp_cfg.module}.{comp_cfg.class_name}")
            self.providers[name] = state
            comp = self._create_component(name, comp_cfg, state)
            if comp is None:
                continue
            if comp_cfg.connect and hasattr(comp, "connect"):
                try:
                    comp.connect(**comp_cfg.connect_args)
                    state.connected = True
                except Exception as err:  # pylint: disable=broad-except
                    state.error = f"connect failed: {err}"
                    logger.warning("Component %s connect failed: %s", name, err)

    def _create_component(
        self, name: str, comp_cfg: ComponentConfig, state: ProviderState
    ) -> Any | None:
        """Import and instantiate a component, recording any error."""
        try:
            module = importlib.import_module(comp_cfg.module)
            comp_class = getattr(module, comp_cfg.class_name)
            return comp_class(**comp_cfg.init_args)
        except Exception as err:  # pylint: disable=broad-except
            state.error = f"init failed: {err}"
            logger.warning("Component %s init failed: %s", name, err)
            return None

    def set_timespan(
        self,
        start: str | None = None,
        end: str | None = None,
        days: float | None = None,
    ) -> dict[str, str]:
        """
        Set the pivot query timespan.

        Parameters
        ----------
        start : str | None, optional
            ISO 8601 start datetime.
        end : str | None, optional
            ISO 8601 end datetime (defaults to now if only ``days`` given).
        days : float | None, optional
            Relative look-back window in days from now (ignored if ``start`` set).

        Returns
        -------
        dict[str, str]
            The effective ``start`` and ``end`` as ISO 8601 strings.

        """
        from msticpy.common.timespan import (  # noqa: PLC0415  # pylint: disable=import-outside-toplevel
            TimeSpan,
        )

        now = datetime.now(timezone.utc)
        if start is None and days is not None:
            end_dt = _parse_dt(end) or now
            start_dt = end_dt - timedelta(days=days)
        else:
            start_dt = _parse_dt(start) or (now - timedelta(days=1))
            end_dt = _parse_dt(end) or now
        if start_dt >= end_dt:
            raise ValueError(
                f"Invalid timespan: start ({start_dt.isoformat()}) must be before "
                f"end ({end_dt.isoformat()})."
            )
        timespan = TimeSpan(start=start_dt, end=end_dt)
        if self.pivot is not None:
            self.pivot.set_timespan(timespan)
        return {"start": start_dt.isoformat(), "end": end_dt.isoformat()}

    def get_timespan(self) -> dict[str, str | None]:
        """Return the current pivot timespan as ISO 8601 strings."""
        if self.pivot is None:
            return {"start": None, "end": None}
        start = getattr(self.pivot, "start", None)
        end = getattr(self.pivot, "end", None)
        return {
            "start": start.isoformat() if start is not None else None,
            "end": end.isoformat() if end is not None else None,
        }

    def status(self) -> dict[str, Any]:
        """Return a diagnostic status dictionary (no secrets)."""
        ti_providers = self._loaded_ti_providers()
        return {
            "config_path": self._config_path,
            "timespan": self.get_timespan(),
            "providers": [p.as_dict() for p in self.providers.values()],
            "ti_providers": ti_providers,
            "entity_count": len(self.catalog.entities()),
            "pivot_count": len(self.catalog),
            "results_cached": len(self.results),
        }

    @staticmethod
    def _loaded_ti_providers() -> list[str]:
        """Return the names of loaded TI providers (best-effort, no secrets)."""
        try:
            from msticpy.context.tilookup import (  # noqa: PLC0415  # pylint: disable=import-outside-toplevel
                TILookup,
            )

            return sorted(TILookup().loaded_providers.keys())
        except Exception:  # pylint: disable=broad-except
            return []


def _parse_dt(value: str | None) -> datetime | None:
    """Parse an ISO 8601 datetime string, returning a tz-aware datetime or None."""
    if not value:
        return None
    text = value.strip()
    if text.endswith("Z"):
        text = f"{text[:-1]}+00:00"
    parsed = datetime.fromisoformat(text)
    if parsed.tzinfo is None:
        parsed = parsed.replace(tzinfo=timezone.utc)
    return parsed
