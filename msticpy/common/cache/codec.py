"""Functions to encode/decode cached objects."""

import base64
import json
import logging
from collections.abc import MutableMapping
from hashlib import sha256
from io import BytesIO

import compress_pickle  # type: ignore[import-untyped]

from ...datamodel.result import QueryResult

from ..._version import VERSION

__version__ = VERSION
__author__ = "Florian Bracq"

LOGGER: logging.Logger = logging.getLogger(__name__)


def encode_as_base64_pickle(data: QueryResult) -> str:
    """Encode data as Base64 pickle to be written to cache."""
    with BytesIO() as bytes_io:
        compress_pickle.dump(data, bytes_io, compression="lzma")
        return base64.b64encode(bytes_io.getvalue()).decode()


def decode_base64_as_pickle(b64_string: str) -> QueryResult:
    """Decode Base64 pickle from cache to Results."""
    return compress_pickle.loads(base64.b64decode(b64_string), compression="lzma")


def compute_digest(obj: MutableMapping) -> str:
    """Compute the digest from the parameters."""
    str_params: str = json.dumps(obj, sort_keys=True, default=str)
    LOGGER.debug("Received: %s", str_params)
    digest: str = sha256(bytes(str_params, "utf-8")).hexdigest()
    LOGGER.debug("Generated digest: %s", digest)
    return digest
