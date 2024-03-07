"""Testing generic cache methods."""
from __future__ import annotations

from typing import TYPE_CHECKING, Any
from unittest.mock import patch

import pytest
import pytest_check as check

from msticpy.common import cache
from msticpy.common.cache import cell, file, read_cache, write_cache

if TYPE_CHECKING:
    from pathlib import Path

    import pandas as pd

    from msticpy.datamodel.result import QueryResult


def test_write_cache_cell(tmp_path: Path, simple_dataframe: pd.DataFrame) -> None:
    """Test method to write cache from a cell."""
    params: dict[str, Any] = {"key": "digest"}

    with patch.object(
        cell,
        "write_cache",
        return_value=None,
    ) as mocked_cache, patch.object(
        cache,
        "is_ipython",
        return_value=True,
    ) as mocked_ipython:
        # "Real" tests are managed in the cell.write_cache test"
        write_cache(
            simple_dataframe,
            params,
            str(tmp_path),
            "name",
            display=True,
        )

        check.is_true(mocked_ipython.called)
        check.equal(mocked_ipython.call_count, 1)

        check.is_true(mocked_cache.called)
        check.equal(mocked_cache.call_count, 1)


def test_write_cache_file(tmp_path: Path, simple_dataframe: pd.DataFrame) -> None:
    """Test method to read cache from a file."""
    params: dict[str, Any] = {"key": "digest"}

    with patch.object(cell, "write_cache", return_value=None):
        # "Real" tests are managed in the file.write_cache test"
        write_cache(
            data=simple_dataframe,
            search_params=params,
            query="query",
            cache_path=str(tmp_path),
            name="name",
        )


def test_read_cache_no_path() -> None:
    """Test method to read cache without an export path."""
    params: dict[str, Any] = {"key": "digest"}

    with pytest.raises(ValueError, match="Cache not provided."):
        read_cache(params, "", "name")


def test_read_cache_cell(
    tmp_path: Path,
    simple_dataframeresult: QueryResult,
) -> None:
    """Test method to read cache from a call."""
    params: dict[str, Any] = {"key": "digest"}

    with patch.object(
        cell,
        "read_cache",
        return_value=simple_dataframeresult,
    ) as mocked_read_cache, patch.object(
        cache,
        "is_ipython",
        return_value=True,
    ) as mocked_ipython:
        # "Real" tests are managed in the cell.read_cache test"
        read_cache(search_params=params, cache_path=str(tmp_path), name="name")

        check.is_true(mocked_ipython.called)
        check.equal(mocked_ipython.call_count, 1)

        check.is_true(mocked_read_cache.called)
        check.equal(mocked_read_cache.call_count, 1)


def test_read_cache_cell_cache_not_found(
    tmp_path: Path,
    simple_dataframeresult: QueryResult,
) -> None:
    """Test method to read cache from a cell."""
    params: dict[str, Any] = {"key": "digest"}

    with patch.object(
        cell,
        "read_cache",
        side_effect=ValueError,
    ) as mocked_read_cache_cell, patch.object(
        file,
        "read_cache",
        return_value=simple_dataframeresult,
    ) as mocked_read_cache_file, patch.object(
        cache,
        "is_ipython",
        return_value=True,
    ) as mocked_ipython, patch.object(
        cell,
        "write_cache",
    ) as mocked_write_cache_cell:
        # "Real" tests are managed in the cell.read_cache test"
        read_cache(search_params=params, cache_path=str(tmp_path), name="name")

        check.is_true(mocked_ipython.called)
        check.equal(mocked_ipython.call_count, 2)

        check.is_true(mocked_read_cache_cell.called)
        check.equal(mocked_read_cache_cell.call_count, 1)

        # When reading from a cell, if the content is not found a failover to a file is attempted
        check.is_true(mocked_read_cache_file.called)
        check.equal(mocked_read_cache_file.call_count, 1)

        # Additionally, the cell cache must be re-written
        check.is_true(mocked_write_cache_cell.called)
        check.equal(mocked_write_cache_cell.call_count, 1)


def test_read_cache_file(
    tmp_path: Path,
    simple_dataframeresult: QueryResult,
) -> None:
    """Test method to read cache from a file."""
    params: dict[str, Any] = {"key": "digest"}

    with patch.object(file, "read_cache", return_value=simple_dataframeresult):
        # "Real" tests are managed in the file.read_cache test"
        read_cache(search_params=params, cache_path=str(tmp_path), name="name")


def test_read_cache_file_not_exist(tmp_path: Path) -> None:
    """Test method to read cache from a non existing file."""
    params: dict[str, Any] = {"key": "digest"}

    with patch.object(file, "read_cache", side_effect=FileNotFoundError), patch.object(
        cache,
        "is_ipython",
        return_value=False,
    ), pytest.raises(ValueError, match="Could not read from cache."):
        read_cache(params, str(tmp_path), "name")
