# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Account Entity class."""
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
class Account(Entity):
    """
    Account Entity class.

    Attributes
    ----------
    Name : str
        Account Name
    NTDomain : str
        Account NTDomain
    UPNSuffix : str
        Account UPNSuffix
    Host : Host
        Account Host
    LogonId : str
        Account LogonId (deprecated)
    Sid : str
        Account Sid
    AadTenantId : str
        Account AadTenantId
    AadUserId : str
        Account AadUserId
    PUID : str
        Account PUID
    IsDomainJoined : bool
        Account IsDomainJoined
    DisplayName : str
        Account DisplayName

    """

    def __init__(
        self,
        src_entity: Mapping[str, Any] = None,
        src_event: Mapping[str, Any] = None,
        role: str = "subject",
        **kwargs,
    ):
        """
        Create a new instance of the entity type.

        Parameters
        ----------
        src_entity : Mapping[str, Any], optional
            Create entity from existing Account entity or
            other mapping object that implements entity properties.
            (the default is None)
        src_event : Mapping[str, Any], optional
            Create entity from event properties
            (the default is None)
        role : str, optional
            'subject' or 'target' - only relevant if the entity
            is being constructed from an event.
            (the default is 'subject')

        Other Parameters
        ----------------
        kwargs : Dict[str, Any]
            Supply the entity properties as a set of
            kw arguments.

        """
        # pylint: disable=locally-disabled, line-too-long
        super().__init__(src_entity=src_entity, **kwargs)
        if src_event is not None:
            if role == "subject" and "SubjectUserName" in src_event:
                self.Name = src_event["SubjectUserName"]
                self.NTDomain = (
                    src_event["SubjectUserDomain"]
                    if "SubjectUserDomain" in src_event
                    else None
                )
                self.Sid = (
                    src_event["SubjectUserSid"]
                    if "SubjectUserSid" in src_event
                    else None
                )
                self.LogonId = (
                    src_event["SubjectLogonId"]
                    if "SubjectLogonId" in src_event
                    else None
                )
            if role == "target" and "TargetUserName" in src_event:
                self.Name = src_event["TargetUserName"]
                self.NTDomain = (
                    src_event["TargetUserDomain"]
                    if "TargetUserDomain" in src_event
                    else None
                )
                self.Sid = (
                    src_event["TargetUserSid"] if "TargetUserSid" in src_event else None
                )
                self.LogonId = (
                    src_event["TargetLogonId"] if "TargetLogonId" in src_event else None
                )

            self.AadTenantId = (
                src_event["AadTenantId"] if "AadTenantId" in src_event else None
            )
            self.AadUserId = (
                src_event["AadUserId"] if "AadUserId" in src_event else None
            )
            self.PUID = src_event["PUID"] if "PUID" in src_event else None
            self.DisplayName = (
                src_event["DisplayName"] if "DisplayName" in src_event else None
            )
            self.UPNSuffix = (
                src_event["UPNSuffix"] if "UPNSuffix" in src_event else None
            )

    # pylint: enable=locally-disabled, line-too-long

    @property
    def description_str(self) -> str:
        """Return Entity Description."""
        return self.qualified_name

    @property
    def qualified_name(self) -> str:
        """Windows qualified account name."""
        if "Name" not in self:
            return ""
        name = self["Name"]
        if "NTDomain" in self and self.NTDomain:
            return "{}\\{}".format(self.NTDomain, name)
        if "UPNSuffix" in self and self.UPNSuffix:
            return "{}@{}".format(name, self.UPNSuffix)
        if "Host" in self and self.Host:
            return "{}\\{}".format(self.Host.HostName, name)
        return name

    _entity_schema = {
        # Name (type System.String)
        "Name": None,
        # NTDomain (type System.String)
        "NTDomain": None,
        # UPNSuffix (type System.String)
        "UPNSuffix": None,
        # Host (type Microsoft.Azure.Security.Detection
        # .AlertContracts.V3.Entities.Host)
        "Host": "Host",
        # LogonId (type System.String)
        "LogonId": None,
        # Sid (type System.String)
        "Sid": None,
        # AadTenantId (type System.Nullable`1[System.Guid])
        "AadTenantId": None,
        # AadUserId (type System.Nullable`1[System.Guid])
        "AadUserId": None,
        # PUID (type System.Nullable`1[System.Guid])
        "PUID": None,
        # IsDomainJoined (type System.Nullable`1[System.Boolean])
        "IsDomainJoined": None,
        # DisplayName (type System.String)
        "DisplayName": None,
    }
