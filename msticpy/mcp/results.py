# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""In-memory result store for pivot query results."""

from __future__ import annotations

import logging
import uuid
from collections import OrderedDict
from typing import Any

import pandas as pd

from .._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"

logger = logging.getLogger(__name__)


def _json_safe(value: Any) -> Any:
    """Convert a pandas/NumPy value to a JSON-serializable scalar."""
    if value is None or (isinstance(value, float) and pd.isna(value)):
        return None
    if isinstance(value, (str, int, float, bool)):
        return value
    try:
        if pd.isna(value):
            return None
    except (ValueError, TypeError):
        pass
    return str(value)


def _records(frame: pd.DataFrame) -> list[dict[str, Any]]:
    """Return DataFrame rows as JSON-safe records."""
    return [{col: _json_safe(val) for col, val in row.items()} for _, row in frame.iterrows()]


def summarize_columns(frame: pd.DataFrame) -> list[dict[str, str]]:
    """Return a list of {name, dtype} column descriptors for a DataFrame."""
    return [{"name": str(col), "dtype": str(dtype)} for col, dtype in frame.dtypes.items()]


def _to_markdown(frame: pd.DataFrame) -> str:
    """Render a DataFrame as a GitHub-flavored markdown table (no extra deps)."""
    headers = [str(col) for col in frame.columns]
    lines = [
        "| " + " | ".join(headers) + " |",
        "| " + " | ".join("---" for _ in headers) + " |",
    ]
    for _, row in frame.iterrows():
        cells = ["" if _json_safe(val) is None else str(_json_safe(val)) for val in row]
        lines.append("| " + " | ".join(cells) + " |")
    return "\n".join(lines)


class ResultStore:
    """LRU-bounded, row-capped in-memory store of pivot result DataFrames."""

    def __init__(self, max_results: int = 50, max_rows_per_result: int = 100_000):
        """
        Initialize the result store.

        Parameters
        ----------
        max_results : int, optional
            Maximum number of results retained (LRU eviction), by default 50.
        max_rows_per_result : int, optional
            Maximum rows retained per stored result, by default 100000.

        """
        self.max_results = max_results
        self.max_rows_per_result = max_rows_per_result
        self._store: OrderedDict[str, pd.DataFrame] = OrderedDict()

    def add(self, frame: pd.DataFrame) -> tuple[str, bool]:
        """
        Store a DataFrame and return its result id.

        Parameters
        ----------
        frame : pd.DataFrame
            The result DataFrame to store.

        Returns
        -------
        tuple[str, bool]
            The generated ``result_id`` and a flag indicating whether the stored
            frame was truncated to ``max_rows_per_result``.

        """
        truncated = False
        if len(frame) > self.max_rows_per_result:
            frame = frame.head(self.max_rows_per_result)
            truncated = True
        result_id = uuid.uuid4().hex[:12]
        self._store[result_id] = frame
        self._store.move_to_end(result_id)
        while len(self._store) > self.max_results:
            evicted, _ = self._store.popitem(last=False)
            logger.debug("Evicted result %s (store full)", evicted)
        return result_id, truncated

    def get(self, result_id: str) -> pd.DataFrame | None:
        """Return the stored DataFrame for ``result_id`` (or None)."""
        frame = self._store.get(result_id)
        if frame is not None:
            self._store.move_to_end(result_id)
        return frame

    def summarize(
        self,
        frame: pd.DataFrame,
        result_id: str,
        sample_rows: int,
        truncated: bool,
    ) -> dict[str, Any]:
        """
        Build the compact inline summary of a result for ``run_pivot``.

        Parameters
        ----------
        frame : pd.DataFrame
            The result DataFrame.
        result_id : str
            The stored result id.
        sample_rows : int
            Number of sample rows to include inline.
        truncated : bool
            Whether the stored frame was row-truncated.

        Returns
        -------
        dict[str, Any]
            The summary payload.

        """
        sample = frame.head(sample_rows) if sample_rows else frame.head(0)
        return {
            "result_id": result_id,
            "row_count": int(len(frame)),
            "columns": summarize_columns(frame),
            "sample": _records(sample),
            "truncated": truncated or len(frame) > sample_rows,
        }

    def page(
        self,
        result_id: str,
        offset: int = 0,
        limit: int = 100,
        columns: list[str] | None = None,
        fmt: str = "records",
    ) -> dict[str, Any]:
        """
        Return a page of a stored result.

        Parameters
        ----------
        result_id : str
            The stored result id.
        offset : int, optional
            Row offset, by default 0.
        limit : int, optional
            Maximum rows to return, by default 100.
        columns : list[str] | None, optional
            Subset of columns to return, by default all.
        fmt : str, optional
            "records" (list of dicts) or "markdown" (table string), default "records".

        Returns
        -------
        dict[str, Any]
            The page payload or an error dict.

        """
        frame = self.get(result_id)
        if frame is None:
            return {
                "ok": False,
                "error": "result_not_found",
                "detail": f"No stored result with id '{result_id}'.",
            }
        if columns:
            missing = [col for col in columns if col not in frame.columns]
            if missing:
                return {
                    "ok": False,
                    "error": "unknown_columns",
                    "detail": f"Columns not in result: {missing}",
                }
            frame = frame[columns]
        page = frame.iloc[offset : offset + limit]
        payload: dict[str, Any] = {
            "ok": True,
            "result_id": result_id,
            "offset": offset,
            "limit": limit,
            "row_count": int(len(frame)),
            "columns": [str(col) for col in frame.columns],
        }
        if fmt == "markdown":
            payload["rows"] = _to_markdown(page)
        else:
            payload["rows"] = _records(page)
        return payload

    def __len__(self) -> int:
        """Return the number of stored results."""
        return len(self._store)
