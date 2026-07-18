# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Tests for msticpy.mcp.results."""
from __future__ import annotations

import numpy as np
import pandas as pd
import pytest

from msticpy.mcp.results import ResultStore, summarize_columns

__author__ = "Ian Hellen"


@pytest.fixture
def frame():
    """Return a small sample DataFrame."""
    return pd.DataFrame(
        {
            "ip": ["1.1.1.1", "2.2.2.2", "3.3.3.3"],
            "score": [10, 20, np.nan],
        }
    )


def test_add_and_get(frame):
    """Adding a frame returns an id that retrieves the same frame."""
    store = ResultStore()
    result_id, truncated = store.add(frame)
    assert not truncated
    assert result_id
    assert store.get(result_id) is frame
    assert len(store) == 1


def test_row_truncation():
    """Frames longer than max_rows_per_result are truncated on store."""
    store = ResultStore(max_rows_per_result=2)
    big = pd.DataFrame({"n": range(10)})
    result_id, truncated = store.add(big)
    assert truncated
    assert len(store.get(result_id)) == 2


def test_lru_eviction(frame):
    """Exceeding max_results evicts the least-recently-used entry."""
    store = ResultStore(max_results=2)
    id1, _ = store.add(frame)
    id2, _ = store.add(frame)
    # touch id1 so id2 becomes LRU
    store.get(id1)
    id3, _ = store.add(frame)
    assert store.get(id2) is None
    assert store.get(id1) is not None
    assert store.get(id3) is not None


def test_summarize(frame):
    """Summary includes row count, columns and a capped, JSON-safe sample."""
    store = ResultStore()
    result_id, truncated = store.add(frame)
    summary = store.summarize(frame, result_id, sample_rows=2, truncated=truncated)
    assert summary["row_count"] == 3
    assert summary["truncated"] is True  # 3 rows > 2 sample rows
    assert len(summary["sample"]) == 2
    assert {c["name"] for c in summary["columns"]} == {"ip", "score"}
    # NaN must be JSON-safe (None)
    all_rows = store.page(result_id, limit=10)["rows"]
    assert all_rows[2]["score"] is None


def test_page_records_and_markdown(frame):
    """Paging supports offset/limit, column subset and markdown format."""
    store = ResultStore()
    result_id, _ = store.add(frame)
    page = store.page(result_id, offset=1, limit=1)
    assert page["ok"] is True
    assert len(page["rows"]) == 1
    assert page["rows"][0]["ip"] == "2.2.2.2"

    subset = store.page(result_id, columns=["ip"])
    assert subset["columns"] == ["ip"]

    md = store.page(result_id, fmt="markdown")
    assert isinstance(md["rows"], str)
    assert "ip" in md["rows"]


def test_page_errors(frame):
    """Paging returns structured errors for bad ids or columns."""
    store = ResultStore()
    result_id, _ = store.add(frame)
    assert store.page("missing")["error"] == "result_not_found"
    assert store.page(result_id, columns=["nope"])["error"] == "unknown_columns"


def test_summarize_columns(frame):
    """summarize_columns returns name/dtype descriptors."""
    cols = summarize_columns(frame)
    names = {c["name"] for c in cols}
    assert names == {"ip", "score"}
