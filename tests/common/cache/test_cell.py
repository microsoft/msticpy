"""Testing cell notebook cache."""
from __future__ import annotations

from typing import TYPE_CHECKING, Any
from unittest.mock import patch

import nbformat
import pandas as pd
import pytest
import pytest_check as check
from typing_extensions import Self

from msticpy.common.cache import cell
from msticpy.datamodel.result import QueryResult

if TYPE_CHECKING:
    from pathlib import Path


def test_pickle_encode_decode(simple_dataframeresult: QueryResult) -> None:
    """Test method encode_as_base64_pickle and decode_base64_as_pickle."""
    encoded: str = cell.encode_as_base64_pickle(simple_dataframeresult)
    check.is_not_none(encoded)
    check.is_instance(encoded, str)
    check.greater(len(encoded), 100)

    decoded: QueryResult = cell.decode_base64_as_pickle(encoded)
    check.is_not_none(decoded)
    check.is_instance(decoded, QueryResult)
    check.is_false(decoded.raw_results.empty)
    check.is_true(simple_dataframeresult.raw_results.equals(decoded.raw_results))


def test_write_cache(simple_dataframeresult: QueryResult) -> None:
    """Test method write_cache."""
    digest: str = "digest"
    name: str = "name"

    with patch.object(cell, "display") as patched_display:
        cell.write_cache(
            simple_dataframeresult,
            name,
            digest,
        )
        check.is_true(patched_display.called)
        check.equal(patched_display.call_count, 1)

        check.equal(len(patched_display.call_args.args), 1)

        data: pd.DataFrame = patched_display.call_args.args[0]
        check.is_instance(data, pd.DataFrame)

        kwargs: dict[str, Any] = patched_display.call_args.kwargs
        check.is_not_none(kwargs)
        check.is_instance(kwargs, dict)
        check.equal(len(kwargs), 2)
        check.is_in("metadata", kwargs)
        check.is_in("exclude", kwargs)

        metadata: dict[str, str] = kwargs["metadata"]
        check.is_instance(metadata, dict)
        check.equal(len(metadata), 6)
        check.is_in("data", metadata)
        check.is_in("hash", metadata)
        check.is_in("name", metadata)
        check.is_in("query", metadata)
        check.is_in("arguments", metadata)
        check.is_in("timestamp", metadata)

        meta_data: str = metadata["data"]
        check.equal(meta_data, cell.encode_as_base64_pickle(simple_dataframeresult))
        meta_hash: str = metadata["hash"]
        check.equal(meta_hash, digest)
        meta_name: str = metadata["name"]
        check.equal(meta_name, name)
        meta_query: str = metadata["query"]
        check.equal(meta_query, simple_dataframeresult.query)
        meta_args: str = metadata["arguments"]
        check.equal(meta_args, simple_dataframeresult.arguments)
        meta_timestamp: str = metadata["timestamp"]
        check.equal(meta_timestamp, simple_dataframeresult.timestamp)


class MyNotebook:  # pylint:disable=too-few-public-methods
    """Dummy notebook class."""

    def __init__(self: Self, metadata: dict[str, Any] | None = None) -> None:
        """Init dummy object."""
        self.cells: list[dict[str, list[dict[str, dict[str, Any]]]]] = [
            {"outputs": [{"metadata": metadata or {}}]},
        ]


def test_get_cache_item(tmp_path: Path) -> None:
    """Test method get_cache_item."""
    digest: str = "digest"
    name: str = "name"

    # Create file with digest content
    (tmp_path / "random.ipynb").write_text(digest, encoding="utf-8")

    with patch.object(
        nbformat,
        "reads",
        return_value=MyNotebook({"hash": digest, "name": name}),
    ):
        res: dict[str, Any] = cell.get_cache_item(
            tmp_path / "random.ipynb",
            name=name,
            digest=digest,
        )
        check.is_instance(res, dict)
        check.is_in("hash", res)
        check.is_in("name", name)
        check.equal(res["hash"], digest)
        check.equal(res["name"], name)


def test_get_cache_item_wrong_path(tmp_path: Path) -> None:
    """Test method get_cache_item with invalid notebook path."""
    with pytest.raises(FileNotFoundError, match="Notebook not found"):
        cell.get_cache_item(tmp_path / "random.ipynb", "name", "digest")


def test_get_cache_item_wrong_digest(tmp_path: Path) -> None:
    """Test method get_cache_item with invalid digest."""
    # Create file with some content
    (tmp_path / "random.ipynb").write_text("", encoding="utf-8")
    name: str = "name"
    digest: str = "digest"

    with patch.object(
        nbformat,
        "reads",
        return_value=MyNotebook({"hash": digest, "name": name}),
    ):
        res: dict[str, Any] = cell.get_cache_item(tmp_path / "random.ipynb", name, name)
        check.is_instance(res, dict)
        check.equal(len(res), 0)


def test_get_cache_item_wrong_name(tmp_path: Path) -> None:
    """Test method get_cache_item with invalid name."""
    # Create file with some content
    (tmp_path / "random.ipynb").write_text("", encoding="utf-8")
    name: str = "name"
    digest: str = "digest"

    with patch.object(
        nbformat,
        "reads",
        return_value=MyNotebook({"hash": digest, "name": name}),
    ):
        res: dict[str, Any] = cell.get_cache_item(
            tmp_path / "random.ipynb",
            digest,
            digest,
        )
        check.is_instance(res, dict)
        check.equal(len(res), 0)


def test_read_cache(tmp_path: Path, simple_dataframeresult: QueryResult) -> None:
    """Test method read_cache."""
    nb_path: Path = tmp_path / "test.ipynb"
    digest: str = "digest"
    encoded: str = cell.encode_as_base64_pickle(simple_dataframeresult)
    name: str = "name"
    # Create file with no content
    nb_path.write_text(digest, encoding="utf-8")

    with patch.object(
        nbformat,
        "reads",
        return_value=MyNotebook({"hash": digest, "name": name, "data": encoded}),
    ):
        data: QueryResult = cell.read_cache(
            name=name,
            digest=digest,
            nb_path=str(nb_path),
        )
        check.is_instance(data, QueryResult)
        check.is_true(simple_dataframeresult.raw_results.equals(data.raw_results))


def test_read_cache_wrong_digest(tmp_path: Path) -> None:
    """Test method read_cache with an incorrect digest."""
    nb_path: Path = tmp_path / "test.ipynb"
    digest: str = "random"
    name: str = "name"
    # Create file with no content
    nb_path.write_text(digest, encoding="utf-8")

    with patch.object(
        nbformat,
        "reads",
        return_value=MyNotebook({"hash": digest, "name": name}),
    ), pytest.raises(ValueError, match=f"Cache {name} not found"):
        cell.read_cache(name, name, str(nb_path))


def test_read_cache_wrong_name(tmp_path: Path) -> None:
    """Test method read_cache with an incorrect name."""
    nb_path: Path = tmp_path / "test.ipynb"
    digest: str = "random"
    name: str = "name"
    # Create file with no content
    nb_path.write_text(digest, encoding="utf-8")

    with patch.object(
        nbformat,
        "reads",
        return_value=MyNotebook({"hash": "hash", "name": name}),
    ), pytest.raises(ValueError, match=f"Cache {digest} not found"):
        cell.read_cache(digest, digest, str(nb_path))


def test_read_cache_wrong_nb(tmp_path: Path) -> None:
    """Test method read_cache with incorrect noteboob path."""
    nb_path: Path = tmp_path / "test.ipynb"
    with pytest.raises(FileNotFoundError, match="Notebook not found"):
        cell.read_cache("name", "digest", str(nb_path))


def test_read_cache_no_nb() -> None:
    """Test method read_cache with incorrect noteboob path."""
    with pytest.raises(ValueError, match="Argument nb_path must be defined."):
        cell.read_cache("name", "digest", "")
