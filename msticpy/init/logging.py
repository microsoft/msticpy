# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Logging global config."""
import logging
import os
from typing import NamedTuple, Optional

from .._version import VERSION
from ..common.pkg_config import get_config

__version__ = VERSION
__author__ = "Ian Hellen"

_MP_LOG_FILE_ENV = "MSTICPYLOGFILE"
_MP_LOG_LEVEL_ENV = "MSTICPYLOGLEVEL"


class LoggingConfig(NamedTuple):
    """Logging configuration tuple."""

    log_file: Optional[str] = None
    log_level: int = logging.WARNING


def setup_logging():
    """Initiate logging."""
    logging_config = _get_logging_config()
    logging.basicConfig(
        filename=logging_config.log_file,
        encoding="utf-8",
        level=logging_config.log_level,
        format="%(asctime)s: %(levelname)s - %(message)s (%(module)s#%(lineno)d)",
    )


def _get_logging_config():
    """Obtain logging configuration from environment or config."""
    log_file = os.environ.get(_MP_LOG_FILE_ENV)
    log_level = os.environ.get(_MP_LOG_LEVEL_ENV)

    log_file = log_file or get_config("Logging.FileName", None)
    log_level = log_level or get_config("Logging.LogLevel", None)
    log_level_int = logging.getLevelName(log_level or "WARNING")
    if not isinstance(log_level_int, int):
        log_level_int = logging.WARNING
    return LoggingConfig(
        log_file=log_file,
        log_level=log_level_int,
    )
