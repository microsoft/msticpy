# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Logging global config."""
import logging
import os
import sys
from typing import NamedTuple, Optional, Union

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


def set_logging_level(log_level: Union[int, str]):
    """
    Set global logging level.

    Parameters
    ----------
    log_level : Union[int, str]
        Either an integer - one of logging.INFO, logging.WARNING,
        etc. or the name of a valid logging level.

    Raises
    ------
    ValueError
        If an invalid logging level is passed.

    See Also
    --------
    logging

    """
    logger = logging.getLogger("msticpy")
    log_level_int = logging.getLevelName(log_level or "WARNING")
    if not isinstance(log_level_int, int):
        raise ValueError(f"Invalid log level specified: {log_level}")
    logger.setLevel(log_level_int)


def setup_logging():
    """Initiate logging."""
    logging_config = _get_logging_config()
    # encoding param only supported in 3.9+
    params_39 = {"encoding": "utf-8"} if sys.version_info >= (3, 9) else {}
    logging.basicConfig(
        filename=logging_config.log_file,
        level=logging_config.log_level,
        format="%(asctime)s: %(levelname)s - %(message)s (%(module)s#%(lineno)d)",
        **params_39,
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
