# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""IpAddress Entity class."""
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
class IpAddress(Entity):
    """
    IPAddress Entity class.

    Attributes
    ----------
    Address : str
        IpAddress Address
    Location : GeoLocation
        IpAddress Location
    ThreatIntelligence : List[ThreatIntelligence]
        IpAddress ThreatIntelligence

    """

    def __init__(
        self,
        src_entity: Mapping[str, Any] = None,
        src_event: Mapping[str, Any] = None,
        **kwargs,
    ):
        """
        Create a new instance of the entity type.

        Parameters
        ----------
        src_entity : Mapping[str, Any], optional
            Create entity from existing entity or
            other mapping object that implements entity properties.
            (the default is None)
        src_event : Mapping[str, Any], optional
            Create entity from event properties
            (the default is None)

        Other Parameters
        ----------------
        kwargs : Dict[str, Any]
            Supply the entity properties as a set of
            kw arguments.

        """
        super().__init__(src_entity=src_entity, **kwargs)

        if src_event is not None and "IpAddress" in src_event:
            self.Address = src_event["IpAddress"]

    @property
    def ip_address(self) -> Union[IPv4Address, IPv6Address]:
        """Return a python ipaddress object from the entity property."""
        return ip_address(self["Address"])

    @property
    def description_str(self) -> str:
        """Return Entity Description."""
        return self.Address

    _entity_schema = {
        # Address (type System.String)
        "Address": None,
        # Location (type Microsoft.Azure.Security.Detection.AlertContracts
        # .V3.ContextObjects.GeoLocation)
        "Location": "GeoLocation",
        # ThreatIntelligence (type System.Collections.Generic.List`1
        # [Microsoft.Azure.Security.Detection.AlertContracts.V3
        # .ContextObjects.ThreatIntelligence])
        "ThreatIntelligence": (list, "Threatintelligence"),
    }
