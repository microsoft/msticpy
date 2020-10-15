# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""SecurityGroup Entity class."""
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
class SecurityGroup(Entity):
    """
    SecurityGroup Entity class.

    Attributes
    ----------
    DistinguishedName : str
        SecurityGroup DistinguishedName
    SID : str
        SecurityGroup SID
    ObjectGuid : str
        SecurityGroup ObjectGuid

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
    def description_str(self):
        """Return Entity Description."""
        return self.DistinguishedName

    _entity_schema = {
        # DistinguishedName (type System.String)
        "DistinguishedName": None,
        # SID (type System.String)
        "SID": None,
        # ObjectGuid (type System.String)
        "ObjectGuid": None,
    }
