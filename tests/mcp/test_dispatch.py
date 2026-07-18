# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Tests for msticpy.mcp.dispatch."""
from __future__ import annotations

import pandas as pd
import pytest

from msticpy.mcp.catalog import PivotEntry
from msticpy.mcp.dispatch import PivotDispatchError, _to_dataframe, invoke_pivot

__author__ = "Ian Hellen"


@pytest.mark.parametrize(
    "value, expected_cols",
    [
        (pd.DataFrame({"a": [1]}), ["a"]),
        ({"x": 1, "y": 2}, ["x", "y"]),
        ([{"a": 1}, {"a": 2}], ["a"]),
        (["one", "two"], ["result"]),
        ("scalar", ["result"]),
        ([], []),
    ],
)
def test_to_dataframe_normalization(value, expected_cols):
    """Various result shapes normalize to a DataFrame."""
    frame = _to_dataframe(value)
    assert isinstance(frame, pd.DataFrame)
    assert list(frame.columns) == expected_cols


def _make_entry(func, kind="enrichment", primary_param=None):
    """Build a minimal PivotEntry wrapping a callable."""
    return PivotEntry(
        entity="IpAddress",
        pivot_path="test.func",
        description="test",
        kind=kind,
        func=func,
        primary_param=primary_param,
    )


def test_invoke_enrichment_positional():
    """Enrichment pivots receive the value as the first positional arg."""
    captured = {}

    def fake_pivot(value, **kwargs):
        captured["value"] = value
        captured["kwargs"] = kwargs
        return pd.DataFrame({"in": [value]})

    entry = _make_entry(fake_pivot)
    frame = invoke_pivot(entry, "8.8.8.8", {"extra": 1})
    assert captured["value"] == "8.8.8.8"
    assert captured["kwargs"] == {"extra": 1}
    assert frame.iloc[0]["in"] == "8.8.8.8"


def test_invoke_query_keyword():
    """Query pivots receive the value via the named primary parameter."""
    captured = {}

    def fake_query(**kwargs):
        captured.update(kwargs)
        return pd.DataFrame({"host": [kwargs.get("host_name")]})

    entry = _make_entry(fake_query, kind="query", primary_param="host_name")
    invoke_pivot(entry, "host1")
    assert captured["host_name"] == "host1"


def test_invoke_query_value_is_authoritative():
    """The MCP value overrides a same-named entry in params for query pivots."""
    captured = {}

    def fake_query(**kwargs):
        captured.update(kwargs)
        return pd.DataFrame()

    entry = _make_entry(fake_query, kind="query", primary_param="ip_address")
    invoke_pivot(entry, "8.8.8.8", {"ip_address": "1.1.1.1", "extra": 2})
    assert captured["ip_address"] == "8.8.8.8"
    assert captured["extra"] == 2


def test_invoke_query_no_primary_no_params_errors():
    """A query pivot with no inferable primary param and no params errors clearly."""

    def fake_query(**kwargs):
        return pd.DataFrame()

    entry = _make_entry(fake_query, kind="query", primary_param=None)
    with pytest.raises(PivotDispatchError, match="primary parameter"):
        invoke_pivot(entry, "x")


def test_invoke_query_no_primary_with_params_passes_through():
    """A query pivot with explicit params (no primary) passes them through."""
    captured = {}

    def fake_query(**kwargs):
        captured.update(kwargs)
        return pd.DataFrame()

    entry = _make_entry(fake_query, kind="query", primary_param=None)
    invoke_pivot(entry, "ignored", {"custom_param": "v"})
    assert captured == {"custom_param": "v"}


def test_invoke_wraps_typeerror():
    """Bad arguments raise a PivotDispatchError with guidance."""

    def strict(value):
        return value

    entry = _make_entry(strict)
    with pytest.raises(PivotDispatchError, match="Invalid arguments"):
        invoke_pivot(entry, "x", {"unexpected": 1})


def test_invoke_wraps_runtime_error():
    """Runtime failures raise a PivotDispatchError."""

    def boom(value):
        raise ValueError("kaboom")

    entry = _make_entry(boom)
    with pytest.raises(PivotDispatchError, match="failed"):
        invoke_pivot(entry, "x")
