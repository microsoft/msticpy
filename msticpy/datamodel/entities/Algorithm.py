# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Algorithm Entity class."""
import pprint
from abc import ABC, abstractmethod
from enum import Enum
from ipaddress import IPv4Address, IPv6Address, ip_address
from typing import Any, Dict, Mapping, Type, Union, Optional

from ..._version import VERSION
from ...common.utility import export
from .entity import Entity

__version__ = VERSION
__author__ = "Ian Hellen"


_ENTITY_ENUMS: Dict[str, Type] = {}


@export
class Algorithm(Enum):
    """FileHash Algorithm Enumeration."""

    Unknown = 0
    MD5 = 1
    SHA1 = 2
    SHA256 = 3
    SHA256AC = 4


_ENTITY_ENUMS[Algorithm.__name__] = Algorithm
