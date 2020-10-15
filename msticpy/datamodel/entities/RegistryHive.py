# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""RegistryHive Entity class."""
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
class RegistryHive(Enum):
    """RegistryHive enumeration."""

    # <summary>HKEY_LOCAL_MACHINE</summary>
    HKEY_LOCAL_MACHINE = 0
    # <summary>HKEY_CLASSES_ROOT</summary>
    HKEY_CLASSES_ROOT = 1
    # <summary>HKEY_CURRENT_CONFIG</summary>
    HKEY_CURRENT_CONFIG = 2
    # <summary>HKEY_USERS</summary>
    HKEY_USERS = 3
    # <summary>HKEY_CURRENT_USER_LOCAL_SETTINGS</summary>
    HKEY_CURRENT_USER_LOCAL_SETTINGS = 4
    # <summary>HKEY_PERFORMANCE_DATA</summary>
    HKEY_PERFORMANCE_DATA = 5
    # <summary>HKEY_PERFORMANCE_NLSTEXT</summary>
    HKEY_PERFORMANCE_NLSTEXT = 6
    # <summary>HKEY_PERFORMANCE_TEXT</summary>
    HKEY_PERFORMANCE_TEXT = 7
    # <summary>HKEY_A</summary>
    HKEY_A = 8
    # <summary>HKEY_CURRENT_USER</summary>
    HKEY_CURRENT_USER = 9


_ENTITY_ENUMS[RegistryHive.__name__] = RegistryHive
