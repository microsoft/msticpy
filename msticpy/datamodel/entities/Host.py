# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Host Entity class."""
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
class Host(Entity):
    """
    Host Entity class.

    Attributes
    ----------
    DnsDomain : str
        Host DnsDomain
    NTDomain : str
        Host NTDomain
    HostName : str
        Host HostName
    NetBiosName : str
        Host NetBiosName
    AzureID : str
        Host AzureID
    OMSAgentID : str
        Host OMSAgentID
    OSFamily : str
        Host OSFamily
    IsDomainJoined : bool
        Host IsDomainJoined

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
        self._computer = None
        if src_event is not None and "Computer" in src_event:
            self._computer = src_event["Computer"]
            if "." in src_event["Computer"]:
                self.HostName = src_event["Computer"].split(".", 1)[0]
                self.DnsDomain = src_event["Computer"].split(".", 1)[1]
            else:
                self.HostName = src_event["Computer"]
            self.NetBiosName = self.HostName

    @property
    def computer(self) -> str:
        """Return computer from source event."""
        return self._computer if self._computer is not None else self.fqdn

    @property
    def fqdn(self) -> str:
        """Construct FQDN from host + dns."""
        if self.DnsDomain:
            return f"{self.HostName}.{self.DnsDomain}"
        return self.HostName

    @property
    def description_str(self) -> str:
        """Return Entity Description."""
        return f"{self.fqdn} ({self.OSFamily})"

    _entity_schema = {
        # DnsDomain (type System.String)
        "DnsDomain": None,
        # NTDomain (type System.String)
        "NTDomain": None,
        # HostName (type System.String)
        "HostName": None,
        # NetBiosName (type System.String)
        "NetBiosName": None,
        # AzureID (type System.String)
        "AzureID": None,
        # OMSAgentID (type System.String)
        "OMSAgentID": None,
        # OSFamily (type System.Nullable`1
        # [Microsoft.Azure.Security.Detection.AlertContracts.V3.Entities.OSFamily])
        "OSFamily": None,
        # IsDomainJoined (type System.Nullable`1[System.Boolean])
        "IsDomainJoined": None,
    }
