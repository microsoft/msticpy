"""Handle caching in Notebook cell."""
from __future__ import annotations

import logging
from pathlib import Path
from typing import Any

import nbformat
from IPython.display import display

from ...datamodel.result import QueryResult
from .codec import decode_base64_as_pickle, encode_as_base64_pickle

LOGGER: logging.Logger = logging.getLogger(__name__)


def write_cache(
    data: QueryResult,
    name: str,
    digest: str,
) -> None:
    """Cache content in cell."""
    cache: str = encode_as_base64_pickle(data)
    metadata: dict[str, Any] = {
        "data": cache,
        "hash": digest,
    }
    if isinstance(data, QueryResult):
        metadata.update(
            {
                "name": name,
                "query": data.query,
                "arguments": data.arguments,
                "timestamp": data.timestamp,
            },
        )
    LOGGER.debug("Data %s written to Notebook cache", name)
    display(
        data.raw_results,
        metadata=metadata,
        exclude=["text/plain"],
    )


def get_cache_item(path: Path, name: str, digest: str) -> dict[str, Any]:
    """
    Get named object from cache.

    Parameters
    ----------
    path : Path
        Path to notebook
    name : str
        name of the cached object to search
    digest : str
        Hash of the cached object to search

    Returns
    -------
    dict[str, Any]
        Cached object.
    """
    if not path.exists():
        error_msg: str = "Notebook not found"
        raise FileNotFoundError(error_msg)

    notebook: nbformat.NotebookNode = nbformat.reads(
        path.read_text(encoding="utf-8"),
        as_version=nbformat.current_nbformat,
    )

    try:
        cache: dict[str, Any] = next(
            iter(
                [
                    (output.get("metadata", {}) or {})
                    for cell in (notebook.cells or [])
                    for output in (cell.get("outputs", []) or [])
                    if output.get("metadata", {}).get("hash") == digest
                    and output.get("metadata", {}).get("name") == name
                ],
            ),
        )
    except StopIteration:
        LOGGER.debug("%s not found in %s cache...", digest, path)
        cache = {}

    return cache


def read_cache(name: str, digest: str, nb_path: str) -> QueryResult:
    """Read cache content from file."""
    if not nb_path:
        error_msg: str = "Argument nb_path must be defined."
        raise ValueError(error_msg)

    notebook_fp: Path = Path(nb_path).absolute()

    if not notebook_fp.exists():
        error_msg = "Notebook not found"
        raise FileNotFoundError(error_msg)

    cache: dict[str, Any] = get_cache_item(path=notebook_fp, name=name, digest=digest)
    if cache and (data := cache.get("data")):
        LOGGER.debug("Digest %s found in cache...", digest)
        return decode_base64_as_pickle(data)
    error_msg = f"Cache {digest} not found"
    raise ValueError(error_msg)
