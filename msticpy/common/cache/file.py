"""Handle caching in files."""
from __future__ import annotations

import logging
from pathlib import Path
from typing import TYPE_CHECKING

from .codec import decode_base64_as_pickle, encode_as_base64_pickle

if TYPE_CHECKING:
    from ...datamodel.result import QueryResult


LOGGER: logging.Logger = logging.getLogger(__name__)
CACHE_FOLDER_NAME = "artifacts"


def write_cache(
    data: QueryResult,
    file_name: str,
    export_folder: str = CACHE_FOLDER_NAME,
) -> None:
    """Cache content in file."""
    export_path: Path = Path(export_folder)
    if export_path.is_file():
        export_path = export_path.parent / CACHE_FOLDER_NAME
    if not export_path.exists():
        export_path.mkdir(exist_ok=True, parents=True)
    export_file: Path = export_path / file_name
    encoded_text: str = encode_as_base64_pickle(data)
    export_file.write_text(encoded_text)
    LOGGER.debug("Data written to file %s", export_folder)


def read_cache(
    file_name: str,
    export_folder: str = CACHE_FOLDER_NAME,
) -> QueryResult:
    """Read cache content from file."""
    export_path: Path = Path(export_folder)
    if export_path.is_file():
        export_path = export_path.parent / CACHE_FOLDER_NAME
    export_file: Path = export_path / file_name
    if export_file.exists():
        LOGGER.debug("Found data in cache %s", export_file)
        encoded_text: str = export_file.read_text()
        return decode_base64_as_pickle(encoded_text)
    raise FileNotFoundError
