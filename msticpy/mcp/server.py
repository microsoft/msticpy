# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
FastMCP server exposing MSTICPy pivot functionality.

Defines a compact, generic set of MCP tools that let an AI agent enumerate
and execute MSTICPy pivot functions across all entity types.
"""

from __future__ import annotations

import logging
import threading
from typing import Any, cast

import anyio

from .._version import VERSION
from .catalog import PivotEntry
from .dispatch import PivotDispatchError, invoke_pivot
from .session import MpMcpSession

__version__ = VERSION
__author__ = "Ian Hellen"

logger = logging.getLogger(__name__)

try:
    from mcp.server.fastmcp import FastMCP
except ImportError as imp_err:  # pragma: no cover - handled at runtime
    raise ImportError(
        "The MCP SDK is required to run the MSTICPy MCP server. "
        "Install it with: pip install 'msticpy[mcp]'"
    ) from imp_err


_SESSION: MpMcpSession | None = None
_SESSION_LOCK = threading.Lock()


def get_session() -> MpMcpSession:
    """Return the started global session, creating/starting it on first use."""
    global _SESSION  # noqa: PLW0603  # pylint: disable=global-statement
    if _SESSION is not None and _SESSION.started:
        return _SESSION
    with _SESSION_LOCK:
        if _SESSION is None:
            _SESSION = MpMcpSession()
        if not _SESSION.started:
            _SESSION.start()
    return _SESSION


def set_session(session: MpMcpSession) -> None:
    """Set the global session (primarily for testing)."""
    global _SESSION  # noqa: PLW0603  # pylint: disable=global-statement
    _SESSION = session


def _error(error: str, detail: str) -> dict[str, Any]:
    """Build a structured error response."""
    return {"ok": False, "error": error, "detail": detail}


def _entry_summary(entry: PivotEntry) -> dict[str, Any]:
    """Build a compact listing summary for a pivot entry."""
    summary: dict[str, Any] = {
        "entity": entry.entity,
        "pivot_path": entry.pivot_path,
        "description": entry.description,
        "kind": entry.kind,
    }
    if entry.provider:
        summary["provider"] = entry.provider
    return summary


def _match_provider(session: MpMcpSession, provider: str) -> Any:
    """Match a catalog provider name against loaded providers (case-insensitive).

    Matches by configured provider name or by data environment, since the pivot
    container name may differ from the config key.
    """
    provider_cf = provider.casefold()
    for state in session.providers.values():
        if provider_cf in (state.name.casefold(), state.environment.casefold()):
            return state
    return None


def create_server() -> FastMCP:
    """
    Create and configure the FastMCP server with all pivot tools.

    Returns
    -------
    FastMCP
        The configured server instance.

    """
    mcp = FastMCP(
        "msticpy-pivot",
        instructions=(
            "Exposes MSTICPy pivot functions for enriching and querying security "
            "entities (IP addresses, hosts, accounts, files, URLs, domains). "
            "Typical flow: list_entities -> list_pivots -> describe_pivot -> "
            "run_pivot -> get_result. Use set_timespan before query pivots and "
            "server_status to check provider connectivity."
        ),
    )

    @mcp.tool()
    def list_entities() -> list[dict[str, Any]]:
        """List entity types that have pivot functions available."""
        session = get_session()
        catalog = session.catalog
        result = []
        for entity in catalog.entities():
            entries = catalog.list_entries(entity=entity)
            result.append(
                {
                    "entity": entity,
                    "aliases": catalog.aliases_for(entity),
                    "pivot_count": len(entries),
                    "query_pivot_count": sum(1 for e in entries if e.kind == "query"),
                    "enrichment_pivot_count": sum(
                        1 for e in entries if e.kind == "enrichment"
                    ),
                }
            )
        return result

    @mcp.tool()
    def list_pivots(
        entity: str | None = None,
        search: str | None = None,
        kind: str = "all",
    ) -> list[dict[str, Any]]:
        """
        List available pivot functions, optionally filtered.

        Parameters
        ----------
        entity : str | None
            Restrict to a single entity type (name or alias, e.g. "IpAddress" or "ip").
        search : str | None
            Case-insensitive substring match against the pivot path or description.
        kind : str
            One of "query", "enrichment" or "all" (default).

        """
        catalog = get_session().catalog
        if entity and catalog.resolve_entity(entity) is None:
            return [_error("unknown_entity", f"No entity '{entity}'.")]
        entries = catalog.list_entries(entity=entity, search=search, kind=kind)
        return [_entry_summary(entry) for entry in entries]

    @mcp.tool()
    def describe_pivot(entity: str, pivot_path: str) -> dict[str, Any]:
        """
        Return full detail for a single pivot: parameters, kind and provider.

        Parameters
        ----------
        entity : str
            Entity type (name or alias).
        pivot_path : str
            The pivot path as returned by list_pivots (e.g. "util.whois").

        """
        entry = get_session().catalog.get(entity, pivot_path)
        if entry is None:
            return _error("unknown_pivot", f"No pivot '{pivot_path}' for entity '{entity}'.")
        return {
            "ok": True,
            "entity": entry.entity,
            "pivot_path": entry.pivot_path,
            "description": entry.description,
            "kind": entry.kind,
            "primary_param": entry.primary_param,
            "provider": entry.provider,
            "uses_timespan": entry.uses_timespan,
            "params": [
                {
                    "name": p.name,
                    "type": p.type_name,
                    "description": p.description,
                    "required": p.required,
                    "default": p.default,
                }
                for p in entry.params
            ],
        }

    @mcp.tool()
    async def run_pivot(
        entity: str,
        pivot_path: str,
        value: str | list[str],
        params: dict[str, Any] | None = None,
        max_rows: int = 25,
    ) -> dict[str, Any]:
        """
        Execute a pivot function and return a summary plus a capped row sample.

        The full result is stored and can be paged through with get_result using
        the returned result_id.

        Parameters
        ----------
        entity : str
            Entity type (name or alias).
        pivot_path : str
            The pivot path (e.g. "util.whois").
        value : str | list[str]
            The entity value(s) to look up or query (IP, hash, domain, etc.).
        params : dict | None
            Additional keyword arguments for the pivot (see describe_pivot).
        max_rows : int
            Number of sample rows to include inline (default 25).

        """
        session = get_session()
        if max_rows < 0:
            return _error("invalid_argument", "max_rows must be >= 0.")
        entry = session.catalog.get(entity, pivot_path)
        if entry is None:
            return _error("unknown_pivot", f"No pivot '{pivot_path}' for entity '{entity}'.")
        if entry.kind == "query" and entry.provider:
            prov_state = _match_provider(session, entry.provider)
            if prov_state is not None and not prov_state.connected:
                return _error(
                    "provider_not_connected",
                    f"Query provider '{entry.provider}' is not connected: "
                    f"{prov_state.error or 'unknown reason'}.",
                )
        try:
            frame = await anyio.to_thread.run_sync(lambda: invoke_pivot(entry, value, params))
        except PivotDispatchError as err:
            return _error("pivot_failed", str(err))
        result_id, truncated = session.results.add(frame)
        summary = session.results.summarize(frame, result_id, max_rows, truncated)
        summary["ok"] = True
        summary["kind"] = entry.kind
        return summary

    @mcp.tool()
    def get_result(
        result_id: str,
        offset: int = 0,
        limit: int = 100,
        columns: list[str] | None = None,
        format: str = "records",  # pylint: disable=redefined-builtin
    ) -> dict[str, Any]:
        """
        Page through or reshape a stored pivot result.

        Parameters
        ----------
        result_id : str
            The result id returned by run_pivot.
        offset : int
            Starting row offset (default 0).
        limit : int
            Maximum rows to return (default 100).
        columns : list[str] | None
            Optional subset of columns to return.
        format : str
            "records" (default) or "markdown".

        """
        if offset < 0 or limit < 0:
            return _error("invalid_argument", "offset and limit must be >= 0.")
        return get_session().results.page(
            result_id, offset=offset, limit=limit, columns=columns, fmt=format
        )

    @mcp.tool()
    def set_timespan(
        start: str | None = None,
        end: str | None = None,
        days: float | None = None,
    ) -> dict[str, Any]:
        """
        Set the query timespan used by query pivots.

        Parameters
        ----------
        start : str | None
            ISO 8601 start datetime.
        end : str | None
            ISO 8601 end datetime (defaults to now).
        days : float | None
            Relative look-back window in days from now (used if start not given).

        """
        if days is not None and days <= 0:
            return _error("invalid_argument", "days must be > 0.")
        try:
            timespan = get_session().set_timespan(start=start, end=end, days=days)
        except ValueError as err:
            return _error("invalid_timespan", str(err))
        return {"ok": True, **timespan}

    @mcp.tool()
    def server_status() -> dict[str, Any]:
        """Return server diagnostics: providers, timespan, pivot counts, config."""
        return get_session().status()

    return mcp


def run(transport: str = "stdio") -> None:
    """
    Start the MSTICPy MCP session and run the server.

    Parameters
    ----------
    transport : str, optional
        One of "stdio" (default), "sse" or "streamable-http".

    """
    logger.info("Starting MSTICPy MCP session...")
    get_session()
    server = create_server()
    logger.info("Running MSTICPy MCP server (transport=%s)", transport)
    server.run(transport=cast(Any, transport))
