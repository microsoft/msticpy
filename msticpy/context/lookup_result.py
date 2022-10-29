# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Lookup Status class."""
from collections import namedtuple
from enum import Enum

from .._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"


SanitizedObservable = namedtuple("SanitizedObservable", ["observable", "status"])


class LookupStatus(Enum):
    """Threat intelligence lookup status."""

    OK = 0
    NOT_SUPPORTED = 1
    BAD_FORMAT = 2
    QUERY_FAILED = 3
    NO_DATA = 4
    OTHER = 10
