"""Common methods to handle caching."""

from __future__ import annotations

import logging
from typing import TYPE_CHECKING, Any

from ...datamodel.result import QueryResult
from ..utility.ipython import is_ipython
from . import cell
from . import file as cache_file
from .codec import compute_digest

if TYPE_CHECKING:
    import pandas as pd

LOGGER: logging.Logger = logging.getLogger(__name__)


def write_cache(  # noqa: PLR0913
    data: pd.DataFrame,
    search_params: dict[str, Any],
    query: str,
    name: str,
    cache_path: str | None = None,
    *,
    display: bool = False,
) -> None:
    """Cache query result in a cell or a parquet file."""
    cache_digest: str = compute_digest(search_params)
    cache: QueryResult = QueryResult(
        name=name,
        query=query,
        raw_results=data,
        arguments=search_params,
    )
    if is_ipython() and display:
        cell.write_cache(
            cache,
            name,
            cache_digest,
        )
    if cache_path:
        LOGGER.info("Writing cache to %s", cache_path)
        cache_file.write_cache(
            data=cache,
            file_name=f"{name}_{cache_digest}",
            export_folder=cache_path,
        )


def read_cache(
    search_params: dict[str, Any],
    cache_path: str | None,
    name: str | None = None,
) -> QueryResult:
    """Retrieve result from cache in a cell or a archive file."""
    if not cache_path:
        error_msg: str = "Cache not provided."
        raise ValueError(error_msg)
    cache_digest: str = compute_digest(search_params)
    if is_ipython():
        try:
            return cell.read_cache(
                name or cache_digest,
                cache_digest,
                cache_path,
            )
        except ValueError:
            pass
    try:
        cache: QueryResult = cache_file.read_cache(
            f"{name}_{cache_digest}",
            cache_path,
        )
    except FileNotFoundError as exc:
        error_msg = "Could not read from cache."
        raise ValueError(error_msg) from exc
    if is_ipython():
        # Writing cache to cell since it has not been found.
        cell.write_cache(
            cache,
            name or cache_digest,
            cache_digest,
        )
    return cache
