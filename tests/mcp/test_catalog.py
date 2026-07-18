# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Tests for msticpy.mcp.catalog."""
from __future__ import annotations

from msticpy.mcp.catalog import (
    get_doc_description,
    parse_numpy_params,
)

__author__ = "Ian Hellen"

_SAMPLE_DOC = """
Retrieve Whois ASN information for an IP Address.

Parameters
----------
ip_address : str
    IP Address to look up.
all_columns:
    Expand all whois data to columns.
asn_col : str, optional
    Name of the output column for ASN description,
    by default "ASNDescription".
show_progress : bool, optional
    Show progress for each query, by default False

Returns
-------
pd.DataFrame
    Output DataFrame with results in added columns.

"""


def test_get_doc_description():
    """The short description is the first paragraph of the docstring."""
    assert get_doc_description.__doc__  # sanity
    desc = get_doc_description(type("F", (), {"__doc__": _SAMPLE_DOC}))
    assert desc.startswith("Retrieve Whois ASN information")
    assert "Parameters" not in desc


def test_parse_numpy_params():
    """The numpydoc parameter parser extracts names, types, defaults, optional."""
    params = parse_numpy_params(_SAMPLE_DOC)
    by_name = {p.name: p for p in params}
    assert set(by_name) == {"ip_address", "all_columns", "asn_col", "show_progress"}
    assert by_name["ip_address"].type_name == "str"
    assert by_name["ip_address"].required is True
    assert by_name["asn_col"].required is False
    assert by_name["asn_col"].default == "ASNDescription"
    assert by_name["show_progress"].default in ("False", "false")


def test_parse_numpy_params_no_section():
    """A docstring without a Parameters section yields no params."""
    assert parse_numpy_params("Just a summary, no params.") == []
    assert parse_numpy_params(None) == []


def test_catalog_build(mcp_catalog):
    """The catalog builds with the expected local (no-provider) entities."""
    entities = mcp_catalog.entities()
    # These entities have enrichment pivots that require no data provider.
    assert "IpAddress" in entities
    assert "Dns" in entities
    assert "Url" in entities
    assert len(mcp_catalog) > 0


def test_catalog_alias_resolution(mcp_catalog):
    """Entity aliases resolve to canonical names."""
    assert mcp_catalog.resolve_entity("ip") == "IpAddress"
    assert mcp_catalog.resolve_entity("IpAddress") == "IpAddress"
    assert mcp_catalog.resolve_entity("does-not-exist") is None


def test_catalog_filtering(mcp_catalog):
    """Listing supports entity, search and kind filters."""
    ip_entries = mcp_catalog.list_entries(entity="ip")
    assert ip_entries
    assert all(e.entity == "IpAddress" for e in ip_entries)

    enrichment = mcp_catalog.list_entries(entity="ip", kind="enrichment")
    assert all(e.kind == "enrichment" for e in enrichment)

    whois = mcp_catalog.list_entries(search="whois")
    assert whois
    assert all("whois" in e.pivot_path or "whois" in e.description.lower() for e in whois)


def test_catalog_get(mcp_catalog):
    """Specific pivots can be fetched and expose metadata."""
    entry = mcp_catalog.get("ip", "ip_type")
    assert entry is not None
    assert entry.entity == "IpAddress"
    assert entry.kind == "enrichment"
    assert mcp_catalog.get("ip", "no_such_pivot") is None
