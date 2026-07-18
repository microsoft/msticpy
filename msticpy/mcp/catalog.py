# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
Pivot catalog - introspection of MSTICPy pivot functions.

Builds a queryable in-memory catalog of all pivot functions attached to
MSTICPy entities, classifying each as a "query" pivot (backed by a
QueryProvider) or an "enrichment" pivot (TI lookup, whois, geoip, IoC
extraction, etc.), and extracting parameter metadata from the numpydoc
docstrings.
"""

from __future__ import annotations

import logging
import re
from collections.abc import Callable
from dataclasses import dataclass, field
from typing import Any

from .._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"

logger = logging.getLogger(__name__)

#: Pivot names to exclude from the catalog (utility helpers that are not
#: useful as agent tools or require DataFrame inputs).
_EXCLUDED_PIVOTS: set[str] = {
    "util.b64decode",
    "util.extract_iocs",
    "b64decode",
    "extract_iocs",
}

#: Human-friendly description overrides for pivots with poor docstrings.
_DESCRIPTION_OVERRIDES: dict[str, str] = {
    "geoloc": "Get the geolocation information for the entity value.",
    "whois": "Get the WHOIS information for the IP address.",
    "whois_asn": "Get the ASN (autonomous system) information for the IP address.",
}

#: Heuristic mapping of entity name -> candidate primary query parameter names.
_ENTITY_PRIMARY_PARAMS: dict[str, tuple[str, ...]] = {
    "IpAddress": ("ip_address", "ip_address_list", "ip", "source_ip", "host_ip"),
    "Host": ("host_name", "device_name", "device_id", "computer"),
    "Account": ("account_name", "user", "user_name"),
    "Process": ("process_name", "process_id", "commandline"),
    "File": ("file_hash", "file_name", "file_path", "sha256", "md5", "sha1"),
    "Dns": ("dns", "domain", "domain_name"),
    "Url": ("url", "uri"),
}

#: Parameters that are never the "primary" input for a query pivot.
_NON_PRIMARY_PARAMS: set[str] = {
    "start",
    "end",
    "add_query_items",
    "subscription_filter",
    "table",
    "data",
    "column",
}


@dataclass
class PivotParam:
    """A single parameter of a pivot function."""

    name: str
    type_name: str | None = None
    description: str = ""
    required: bool = False
    default: str | None = None


@dataclass
class PivotEntry:
    """A single catalog entry describing one pivot function."""

    entity: str
    pivot_path: str
    description: str
    kind: str  # "query" | "enrichment"
    func: Callable[..., Any]
    params: list[PivotParam] = field(default_factory=list)
    primary_param: str | None = None
    provider: str | None = None
    uses_timespan: bool = False

    @property
    def key(self) -> tuple[str, str]:
        """Return the unique (entity, pivot_path) key for this entry."""
        return (self.entity, self.pivot_path)


def is_query_pivot(pivot_func: Any) -> bool:
    """Return True if the pivot function is backed by a QueryProvider."""
    props = getattr(pivot_func, "pivot_properties", None)
    if not props:
        return False
    return props.get("src_class") == "QueryProvider"


def get_doc_description(func: Any) -> str:
    """Return the short (summary) portion of a function docstring."""
    if not func.__doc__:
        return ""
    desc_lines: list[str] = []
    for raw_line in func.__doc__.split("\n"):
        line = raw_line.strip()
        if not line:
            if desc_lines:
                break
            continue
        if line.startswith(("Parameters", "Returns", ":param", "----")):
            break
        desc_lines.append(line)
    return " ".join(desc_lines)


_RE_DEFAULT = re.compile(
    r"(?:default(?:\s+value)?\s*(?:is|:|=)?\s*)(?P<val>[^.)\n]+)",
    re.IGNORECASE,
)


def parse_numpy_params(docstring: str | None) -> list[PivotParam]:
    """
    Parse the Parameters section of a numpydoc docstring.

    Parameters
    ----------
    docstring : str | None
        The function docstring.

    Returns
    -------
    list[PivotParam]
        The parsed parameters (empty list if none found).

    """
    if not docstring:
        return []
    lines = docstring.split("\n")
    # locate the Parameters section
    start_idx: int | None = None
    for idx, line in enumerate(lines):
        if (
            line.strip() == "Parameters"
            and idx + 1 < len(lines)
            and set(lines[idx + 1].strip()) == {"-"}
        ):
            start_idx = idx + 2
            break
    if start_idx is None:
        return []
    params: list[PivotParam] = []
    current: PivotParam | None = None
    desc_lines: list[str] = []
    for line in lines[start_idx:]:
        stripped = line.strip()
        # A section terminator (Returns, Raises, etc.)
        if stripped in ("Returns", "Raises", "Notes", "Examples", "See Also") and (
            current is not None or params
        ):
            break
        # A parameter header line has no leading indentation and contains a colon
        # e.g. "ip_column : str" or "all_columns:" or "value : str, optional"
        is_header = bool(line) and not line[0].isspace() and ":" in line
        if is_header:
            if current is not None:
                current.description = " ".join(desc_lines).strip()
                _finalize_param(current)
                params.append(current)
            name_part, _, type_part = stripped.partition(":")
            type_part = type_part.strip()
            optional = "optional" in type_part.lower()
            type_name = type_part.split(",")[0].strip() or None
            current = PivotParam(
                name=name_part.strip(),
                type_name=type_name,
                required=not optional,
            )
            desc_lines = []
        elif current is not None and stripped:
            desc_lines.append(stripped)
    if current is not None:
        current.description = " ".join(desc_lines).strip()
        _finalize_param(current)
        params.append(current)
    return params


def _finalize_param(param: PivotParam) -> None:
    """Extract default value from the parameter description, if present."""
    if not param.required or param.default is not None:
        if match := _RE_DEFAULT.search(param.description):
            param.default = match.group("val").strip().strip("\"'`")


def _select_primary_param(entity: str, params: list[PivotParam]) -> str | None:
    """Select the most likely primary input parameter for a query pivot."""
    candidate_names = _ENTITY_PRIMARY_PARAMS.get(entity, ())
    param_names = {p.name for p in params}
    for cand in candidate_names:
        if cand in param_names:
            return cand
    # fall back to the first required parameter that is not a control param
    for param in params:
        if param.required and param.name not in _NON_PRIMARY_PARAMS:
            return param.name
    return None


def get_unique_pivots(
    entity_cls: type,
    pivot_type: str = "all",
) -> dict[str, Any]:
    """
    Return the unique pivot functions for an entity.

    De-duplicates pivots that appear under multiple paths (e.g. a shortcut and
    a container path pointing at the same underlying function), preferring the
    shortest path.

    Parameters
    ----------
    entity_cls : type
        The entity class to enumerate.
    pivot_type : str, optional
        One of "query", "enrichment" or "all", by default "all".

    Returns
    -------
    dict[str, Any]
        Mapping of pivot_path -> bound pivot function.

    """
    seen_funcs: dict[Any, str] = {}
    unique: dict[str, Any] = {}
    for pivot_path in entity_cls.get_pivot_list():  # type: ignore[attr-defined]
        pivot_func: Any = entity_cls
        try:
            for elem in pivot_path.split("."):
                pivot_func = getattr(pivot_func, elem)
        except AttributeError:
            continue
        if not hasattr(pivot_func, "pivot_properties"):
            continue
        is_query = is_query_pivot(pivot_func)
        if pivot_type == "query" and not is_query:
            continue
        if pivot_type == "enrichment" and is_query:
            continue
        wrapped = getattr(pivot_func, "__wrapped__", pivot_func)
        if wrapped not in seen_funcs:
            seen_funcs[wrapped] = pivot_path
            unique[pivot_path] = pivot_func
        elif len(pivot_path) < len(seen_funcs[wrapped]):
            # prefer the shorter path
            unique.pop(seen_funcs[wrapped], None)
            seen_funcs[wrapped] = pivot_path
            unique[pivot_path] = pivot_func
    return unique


def _derive_provider(pivot_path: str) -> str | None:
    """
    Best-effort extraction of the provider/container name from a query pivot path.

    Query pivots are attached either under a container (e.g.
    ``MSSentinel.list_host_logons``) or as a flat, provider-prefixed name (e.g.
    ``mssentinel_list_host_logons``). Return the leading segment in either case.
    """
    if "." in pivot_path:
        return pivot_path.split(".", maxsplit=1)[0]
    if "_" in pivot_path:
        return pivot_path.split("_", maxsplit=1)[0]
    return None


def _build_entry(entity: str, pivot_path: str, func: Any) -> PivotEntry:
    """Build a single PivotEntry from a bound pivot function."""
    kind = "query" if is_query_pivot(func) else "enrichment"
    description = _DESCRIPTION_OVERRIDES.get(
        pivot_path.rsplit(".", maxsplit=1)[-1], get_doc_description(func)
    )
    params = parse_numpy_params(func.__doc__)
    props = getattr(func, "pivot_properties", {}) or {}
    provider: str | None = None
    primary_param: str | None = None
    uses_timespan = False
    if kind == "query":
        provider = _derive_provider(pivot_path)
        primary_param = _select_primary_param(entity, params)
        param_names = {p.name for p in params}
        uses_timespan = "start" in param_names or "end" in param_names
    else:
        # enrichment pivots take the entity value as their primary input
        primary_param = props.get("func_input_value_arg") or "value"
    return PivotEntry(
        entity=entity,
        pivot_path=pivot_path,
        description=description,
        kind=kind,
        func=func,
        params=params,
        primary_param=primary_param,
        provider=provider,
        uses_timespan=uses_timespan,
    )


def _iter_entity_classes() -> dict[str, type]:
    """Return unique entity classes that may carry pivots, keyed by class name."""
    # Imported lazily so that importing this module does not force entity import.
    from msticpy.datamodel.entities import (  # noqa: PLC0415  # pylint: disable=import-outside-toplevel
        Entity,
    )

    classes: dict[str, type] = {}
    for entity_cls in Entity.ENTITY_NAME_MAP.values():
        if not isinstance(entity_cls, type):
            continue
        if entity_cls is Entity:
            continue
        classes.setdefault(entity_cls.__name__, entity_cls)
    return classes


class PivotCatalog:
    """A queryable catalog of MSTICPy pivot functions."""

    def __init__(self) -> None:
        """Initialize an empty catalog."""
        self._entries: dict[tuple[str, str], PivotEntry] = {}
        self._entity_aliases: dict[str, str] = {}

    def build(self) -> PivotCatalog:
        """
        Populate the catalog from currently-registered entity pivots.

        Assumes ``Pivot()`` has been initialized and ``reload_pivots()`` called.

        Returns
        -------
        PivotCatalog
            This catalog instance (for chaining).

        """
        self._entries.clear()
        self._entity_aliases.clear()
        for entity_name, entity_cls in _iter_entity_classes().items():
            pivots = get_unique_pivots(entity_cls, pivot_type="all")
            for pivot_path, func in pivots.items():
                if pivot_path in _EXCLUDED_PIVOTS:
                    continue
                entry = _build_entry(entity_name, pivot_path, func)
                self._entries[entry.key] = entry
        self._build_aliases()
        logger.info(
            "Pivot catalog built: %d pivots across %d entities",
            len(self._entries),
            len(self.entities()),
        )
        return self

    def _build_aliases(self) -> None:
        """Build a case-insensitive entity-name alias map."""
        from msticpy.datamodel.entities import (  # noqa: PLC0415  # pylint: disable=import-outside-toplevel
            Entity,
        )

        canonical = {e for e, _ in self._entries}
        for alias, entity_cls in Entity.ENTITY_NAME_MAP.items():
            if isinstance(entity_cls, type) and entity_cls.__name__ in canonical:
                self._entity_aliases[alias.casefold()] = entity_cls.__name__

    def resolve_entity(self, entity: str) -> str | None:
        """Resolve an entity name or alias to its canonical class name."""
        known = {ent for ent, _ in self._entries}
        if entity in known:
            return entity
        return self._entity_aliases.get(entity.casefold())

    def entities(self) -> list[str]:
        """Return the sorted list of entity names with pivots."""
        return sorted({entity for entity, _ in self._entries})

    def aliases_for(self, entity: str) -> list[str]:
        """Return known aliases for an entity name."""
        return sorted(
            alias
            for alias, canonical in self._entity_aliases.items()
            if canonical == entity and alias.casefold() != entity.casefold()
        )

    def get(self, entity: str, pivot_path: str) -> PivotEntry | None:
        """Return the catalog entry for an (entity, pivot_path) pair, if present."""
        canonical = self.resolve_entity(entity)
        if canonical is None:
            return None
        return self._entries.get((canonical, pivot_path))

    def list_entries(
        self,
        entity: str | None = None,
        search: str | None = None,
        kind: str = "all",
    ) -> list[PivotEntry]:
        """
        Return catalog entries matching the given filters.

        Parameters
        ----------
        entity : str | None, optional
            Restrict to a single entity (name or alias), by default None.
        search : str | None, optional
            Case-insensitive substring match against path or description.
        kind : str, optional
            One of "query", "enrichment" or "all", by default "all".

        Returns
        -------
        list[PivotEntry]
            The matching entries, sorted by (entity, pivot_path).

        """
        canonical = self.resolve_entity(entity) if entity else None
        search_cf = search.casefold() if search else None
        results = []
        for entry in self._entries.values():
            if canonical is not None and entry.entity != canonical:
                continue
            if kind not in ("all", entry.kind):
                continue
            if search_cf is not None and (
                search_cf not in entry.pivot_path.casefold()
                and search_cf not in entry.description.casefold()
            ):
                continue
            results.append(entry)
        return sorted(results, key=lambda e: (e.entity, e.pivot_path))

    def __len__(self) -> int:
        """Return the number of catalog entries."""
        return len(self._entries)
