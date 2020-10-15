# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""RegistryValue Entity class."""
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
class RegistryKey(Entity):
    """
    RegistryKey Entity class.

    Attributes
    ----------
    Hive : RegistryHive
        RegistryKey Hive
    Key : str
        RegistryKey Key

    """

    def __init__(self, src_entity: Mapping[str, Any] = None, **kwargs):
        """
        Create a new instance of the entity type.

        Parameters
        ----------
        src_entity : Mapping[str, Any], optional
            Create entity from existing entity or
            other mapping object that implements entity properties.
            (the default is None)

        Other Parameters
        ----------------
        kwargs : Dict[str, Any]
            Supply the entity properties as a set of
            kw arguments.

        """
        super().__init__(src_entity=src_entity, **kwargs)

    @property
    def description_str(self) -> str:
        """Return Entity Description."""
        return f"{self.Hive}\\{self.Key}"

    _entity_schema = {
        # Hive (type System.Nullable`1
        # [Microsoft.Azure.Security.Detection.AlertContracts.V3.Entities.RegistryHive])
        "Hive": "RegistryHive",
        # Key (type System.String)
        "Key": None,
    }


class RegistryValue(Entity):
    """
    RegistryValue Entity class.

    Attributes
    ----------
    Key : str
        RegistryValue Key
    Name : str
        RegistryValue Name
    Value : str
        RegistryValue Value
    ValueType : str
        RegistryValue ValueType

    """

    def __init__(self, src_entity: Mapping[str, Any] = None, **kwargs):
        """
        Create a new instance of the entity type.

        Parameters
        ----------
        src_entity : Mapping[str, Any], optional
            Create entity from existing entity or
            other mapping object that implements entity properties.
            (the default is None)

        Other Parameters
        ----------------
        kwargs : Dict[str, Any]
            Supply the entity properties as a set of
            kw arguments.

        """
        super().__init__(src_entity=src_entity, **kwargs)

    @property
    def description_str(self) -> str:
        """Return Entity Description."""
        return f"{self.Name}[{self.ValueType}]:{repr(self.Value)}"

    _entity_schema = {
        # Key (type Microsoft.Azure.Security.Detection
        # .AlertContracts.V3.Entities.RegistryKey)
        "Key": None,
        # Name (type System.String)
        "Name": None,
        # Value (type System.String)
        "Value": None,
        # ValueType (type System.Nullable`1[Microsoft.Win32.RegistryValueKind])
        "ValueType": None,
    }
