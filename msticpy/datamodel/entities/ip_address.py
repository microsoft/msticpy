# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""IpAddress Entity class."""
from __future__ import annotations
from ipaddress import IPv4Address, IPv6Address, ip_address
from typing import Any, Mapping

from ..._version import VERSION
from ...common.utility import export
from .entity import Entity
from .geo_location import GeoLocation
from .threat_intelligence import Threatintelligence

__version__ = VERSION
__author__ = "Ian Hellen"


# pylint: disable=invalid-name, too-many-instance-attributes


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
    ThreatIntelligence : List[Threatintelligence]
        IpAddress ThreatIntelligence

    """

    ID_PROPERTIES: list[str] = ["Address"]

    def __init__(
        self: IpAddress,
        src_entity: Mapping[str, Any] | None = None,
        src_event: Mapping[str, Any] | None = None,
        **kwargs,
    ) -> None:
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
        self.Address: str = ""
        self.Location: GeoLocation | None = None
        self.ThreatIntelligence: list[Threatintelligence] = []
        self.hostname: str | None = None
        self.SourceComputerId: str | None = None
        self.OSType: str | None = None
        self.OSName: str | None = None
        self.OSVMajorVersion: str | None = None
        self.OSVMinorVersion: str | None = None
        self.ComputerEnvironment: str | None = None
        self.OmsSolutions: list[str] | None = None
        self.VMUUID: str | None = None
        self.SubscriptionId: str | None = None
        super().__init__(src_entity=src_entity, **kwargs)

        if src_event is not None and "Location" in src_event:
            self.Location = GeoLocation(src_event["Location"])
        if src_event is not None:
            if "IpAddress" in src_event:
                self.Address = src_event["IpAddress"]
            elif "Address" in src_event:
                self.Address = src_event["Address"]

    @property
    def ip_address(self) -> IPv4Address | IPv6Address | None:
        """Return a python IP address object from the entity property."""
        try:
            return ip_address(self.Address)
        except ValueError:
            return None

    @property
    def description_str(self) -> str:
        """Return Entity Description."""
        return (
            f"{self.Address} - {self.Location.CountryCode}"
            if self.Location
            else self.Address
        )

    @property
    def name_str(self) -> str:
        """Return Entity Name."""
        return self.Address or self.__class__.__name__

    _entity_schema: dict[str, Any] = {
        # Address (type System.String)
        "Address": None,
        # Location (type Microsoft.Azure.Security.Detection.AlertContracts
        # .V3.ContextObjects.GeoLocation)
        "Location": "GeoLocation",
        # ThreatIntelligence (type System.Collections.Generic.List`1
        # [Microsoft.Azure.Security.Detection.AlertContracts.V3
        # .ContextObjects.ThreatIntelligence])
        "ThreatIntelligence": (list, "Threatintelligence"),
        "TimeGenerated": None,
        "StartTime": None,
        "EndTime": None,
    }


# Alias for IpAddress
Ip: type[IpAddress] = IpAddress
