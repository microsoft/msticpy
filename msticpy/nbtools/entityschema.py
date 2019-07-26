# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
entityschema module.

Module for V3 Entities class
"""
from ipaddress import ip_address, IPv4Address, IPv6Address
import json
import pprint
from abc import ABC, abstractmethod
from enum import Enum

# pylint: disable=locally-disabled, unused-import
from typing import Mapping, Any, Union, Dict, Type  # noqa: F401

from .utility import export
from .._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"


_ENTITY_ENUMS: Dict[str, type] = {}


# pylint: disable=too-many-lines, invalid-name
# pylint: disable=too-many-instance-attributes
@export
class Entity(ABC):
    """
    Entity abstract base class.

    Implements common methods for Entity classes
    """

    ENTITY_NAME_MAP: Dict[str, Type] = {}
    _entity_schema: Dict[str, Any] = {}

    def __init__(self, src_entity: Mapping[str, Any] = None, **kwargs):
        """
        Create a new instance of an entity.

        Parameters
        ----------
        src_entity : Mapping[str, Any], optional
            If src_entity is supplied it attempts to extract common
            properties from the source entity and assign them to
            the new instance. (the default is None)

        Other Parameters
        ----------------
        kwargs : Dict[str, Any]
            Supply the entity properties as a set of
            kw arguments.

        """
        # if we didn't populate AdditionalData, add an empty dict in case it's
        # needed
        if "AdditionalData" not in self:
            self["AdditionalData"] = {}

        if src_entity is not None:
            self._extract_src_entity(src_entity)
            # add AdditionalData dictionary if it's populated
            if "AdditionalData" in src_entity:
                self["AdditionalData"] = src_entity["AdditionalData"]

        if kwargs:
            self.__dict__.update(kwargs)

        self.Type = type(self).__name__.lower()
        self._entity_schema["Type"] = None

    def _extract_src_entity(self, src_entity: Mapping[str, Any]):
        """
        Extract source entity properties.

        Parameters
        ----------
        src_entity : Mapping[str, Any]
            The source mappable object from which to
            extract entity properties.

        """
        for k, v in self._entity_schema.items():
            if k not in src_entity:
                continue
            self[k] = src_entity[k]

            if v is not None:
                try:
                    # If the property is an enum
                    if v in _ENTITY_ENUMS:
                        self[k] = _ENTITY_ENUMS[v][src_entity[k]]
                        continue
                except KeyError:
                    # Catch key errors from invalid enum values
                    self[k] = None

                if isinstance(v, tuple):
                    # if the property is a collection
                    entity_list = []
                    for col_entity in src_entity[k]:
                        entity_list.append(Entity.instantiate_entity(col_entity))
                    self[k] = entity_list
                else:
                    # else try to instantiate an entity
                    self[k] = Entity.instantiate_entity(src_entity[k])

    def __getitem__(self, key: str):
        """Allow property get using dictionary key syntax."""
        if key in self.__dict__:
            return self.__dict__[key]
        if key in self._entity_schema:
            return None
        raise KeyError

    def __setitem__(self, key: str, value: Any):
        """Allow property set using dictionary key syntax."""
        self.__dict__[key] = value

    def __contains__(self, key: str):
        """Allow property in test."""
        # In operator overload
        return key in self.__dict__

    def __getattr__(self, name: str):
        """Return the value of the named property 'name'."""
        if name in self._entity_schema:
            return None
        raise AttributeError(f"{name} is not a valid attribute.")

    def __iter__(self):
        """Iterate over entity_properties."""
        return iter(self.properties)

    def __len__(self) -> int:
        """Return length/number of entity_properties."""
        return len(self.properties)

    def __str__(self) -> str:
        """Return string representation of entity."""
        return pprint.pformat(self._to_dict(self), indent=2, width=100)

    def __repr__(self) -> str:
        """Return repr of entity."""
        return json.dumps(self._to_dict(self), default=self._jdump_default)

    def _to_dict(self, entity) -> dict:
        """Return as simple nested dictionary."""
        ent_dict = {}
        for prop, val in entity.properties.items():
            if val:
                if isinstance(val, Entity):
                    ent_dict[prop] = self._to_dict(val)
                else:
                    ent_dict[prop] = val
        return ent_dict

    @staticmethod
    def _jdump_default(o):
        """
        json.dumps default method.

        Allows it to work (at least not fail) on non-serializable types.
        """
        return o.__dict__

    @property
    def properties(self) -> dict:
        """
        Return dictionary properties of entity.

        Returns
        -------
        dict
            Entity properties.

        """
        return {
            name: value
            for name, value in self.__dict__.items()
            if not name.startswith("_")
        }

    @property
    @abstractmethod
    def description_str(self) -> str:
        """
        Return Entity Description.

        Returns
        -------
        str
            Entity description (optional). If not overridden
            by the Entity instance type, it will return the
            Type string.

        """
        return self.Type

    # pylint: disable=bad-continuation, too-many-branches
    @classmethod
    def instantiate_entity(  # noqa: C901
        cls, raw_entity: Mapping[str, Any]
    ) -> Union["Entity", Mapping[str, Any]]:
        """
        Class factory to return entity from raw dictionary representation.

        Parameters
        ----------
        raw_entity : Mapping[str, Any]
            A mapping object (e.g. dictionary or pandas Series)
            that contains the properties of the entity.

        Returns
        -------
        Entity
            The instantiated entity

        """
        if "Type" not in raw_entity:
            return raw_entity

        entity_type = raw_entity["Type"]

        # We get an undefined-variable warning here. _ENTITY_NAME_MAP
        # is not defined/populated until end of module since it needs
        # entity
        if entity_type in cls.ENTITY_NAME_MAP:
            return cls.ENTITY_NAME_MAP[entity_type](raw_entity)

        raise TypeError("Could not find a suitable type for {}".format(entity_type))


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
        if "Name" in self:
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


@export
class HostLogonSession(Entity):
    """
    HostLogonSession Entity class.

    Attributes
    ----------
    Account : Account
        HostLogonSession Account
    StartTimeUtc : datetime
        HostLogonSession StartTimeUtc
    EndTimeUtc : datetime
        HostLogonSession EndTimeUtc
    Host : Host
        HostLogonSession Host
    SessionId : str
        HostLogonSession SessionId

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

        if src_event is not None:
            if "TimeCreatedUtc" in src_event:
                self.StartTimeUtc = src_event["TimeCreatedUtc"]
            elif "TimeGenerated" in src_event:
                self.StartTimeUtc = src_event["TimeGenerated"]
            self.EndTimeUtc = self.StartTimeUtc
            self.SessionId = (
                src_event["TargetLogonId"] if "TargetLogonId" in src_event else None
            )

    @property
    def description_str(self) -> str:
        """Return Entity Description."""
        return f"{self.Host.HostName}: session: {self.SessionId}"

    _entity_schema = {
        # Account
        "Account": "Account",
        # StartTimeUtc (type System.Nullable`1[System.DateTime])
        "StartTimeUtc": None,
        # EndTimeUtc (type System.Nullable`1[System.DateTime])
        "EndTimeUtc": None,
        # Host
        "Host": "Host",
        # SessionId (type System.String)
        "SessionId": None,
    }


@export
class CloudApplication(Entity):
    """
    CloudApplication Entity class.

    Attributes
    ----------
    Name : str
        CloudApplication Name

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
        return self.Name

    _entity_schema = {
        # Name (type System.String)
        "Name": None
    }


@export
class DnsResolve(Entity):
    """
    DNS Resolve Entity class.

    Attributes
    ----------
    DomainName : str
        DnsResolve DomainName
    IpAdresses : List[str]
        DnsResolve IpAdresses
    DnsServerIp : IPAddress
        DnsResolve DnsServerIp
    HostIpAddress : IPAddress
        DnsResolve HostIpAddress

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
        return f"{self.DomainName}: IPs: {repr(self.IpAdresses)}"

    _entity_schema = {
        # DomainName (type System.String)
        "DomainName": None,
        # IpAdresses (type System.Collections.Generic.List`1
        # [Microsoft.Azure.Security.Detection.AlertContracts.V3.Entities.IP])
        "IpAdresses": None,
        # DnsServerIp (type Microsoft.Azure.Security.Detection
        # .AlertContracts.V3.Entities.IP)
        "DnsServerIp": "IPAddress",
        # HostIpAddress (type Microsoft.Azure.Security.Detection
        # .AlertContracts.V3.Entities.IP)
        "HostIpAddress": "IPAddress",
    }


@export
class File(Entity):
    """
    File Entity class.

    Attributes
    ----------
    FullPath : str
        File FullPath
    Directory : str
        File Directory
    Name : str
        File Name
    Md5 : str
        File Md5
    Host : str
        File Host
    Sha1 : str
        File Sha1
    Sha256 : str
        File Sha256
    Sha256Ac : str
        File Sha256Ac
    FileHashes : List[FileHash]
        File FileHashes

    """

    def __init__(
        self,
        src_entity: Mapping[str, Any] = None,
        src_event: Mapping[str, Any] = None,
        role: str = "new",
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
        role : str, optional
            'new' or 'parent' - only relevant if the entity
            is being constructed from an event.
            (the default is 'new')

        Other Parameters
        ----------------
        kwargs : Dict[str, Any]
            Supply the entity properties as a set of
            kw arguments.

        """
        super().__init__(src_entity=src_entity, **kwargs)

        if src_event is not None:
            if role == "new" and "NewProcessName" in src_event:
                self._add_paths(src_event["NewProcessName"])
            elif role == "parent" and "ParentProcessName" in src_event:
                self._add_paths(src_event["ParentProcessName"])

        if "FullPath" not in self:
            file = self["Name"]
            directory = self["Directory"]
            sep = self.path_separator if directory else None
            self["FullPath"] = f"{directory}{sep}{file}"

    @property
    def path_separator(self):
        """Return the path separator used by the file."""
        directory = self["Directory"]
        if directory and "/" in directory:
            return "/"
        return "\\"

    @property
    def description_str(self) -> str:
        """Return Entity Description."""
        return self.FullPath

    _entity_schema = {
        # FullPath (type System.String)
        "FullPath": None,
        # Directory (type System.String)
        "Directory": None,
        # Name (type System.String)
        "Name": None,
        # Md5 (type System.String)
        "Md5": None,
        # Host (type Microsoft.Azure.Security.Detection
        # .AlertContracts.V3.Entities.Host)
        "Host": None,
        # Sha1 (type System.String)
        "Sha1": None,
        # Sha256 (type System.String)
        "Sha256": None,
        # Sha256Ac (type System.String)
        "Sha256Ac": None,
        "FileHashes": (list, "FileHash"),
    }

    def _add_paths(self, full_path):
        if "/" in full_path:
            self.PathSeparator = "/"
            self.OSFamily = OSFamily.Linux
        else:
            self.PathSeparator = "\\"
            self.OSFamily = OSFamily.Windows

        self.FullPath = full_path
        self.Name = full_path.split(self.PathSeparator)[-1]
        self.Directory = full_path.split(self.PathSeparator)[:-1]


@export
class FileHash(Entity):
    """
    File Hash class.

    Attributes
    ----------
    Algorithm : Algorithm
        FileHash Algorithm
    Value : str
        FileHash Value


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
        return f"{self.Algorithm}: {self.Value}"

    _entity_schema = {
        # The hash algorithm (type System.String)
        "Algorithm": "Algorithm",
        # Value (type System.String)
        "Value": None,
    }


@export
class Algorithm(Enum):
    """FileHash Algorithm Enumeration."""

    Unknown = 0
    MD5 = 1
    SHA1 = 2
    SHA256 = 3
    SHA256AC = 4


_ENTITY_ENUMS[Algorithm.__name__] = Algorithm


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
        if src_event is not None:
            if "Computer" in src_event:
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

        if src_event is not None:
            if "IpAddress" in src_event:
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


@export
class GeoLocation(Entity):
    """
    GeoLocation class.

    Attributes
    ----------
    CountryCode : str
        GeoLocation CountryCode
    CountryName : str
        GeoLocation CountryName
    State : str
        GeoLocation State
    City : str
        GeoLocation City
    Longitude : float
        GeoLocation Longitude
    Latitude : float
        GeoLocation Latitude
    Asn : str
        GeoLocation Asn

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
        return f"{self.CountryCode}; {self.State}; {self.City}"

    _entity_schema = {
        # str
        "CountryCode": None,
        # str
        "CountryName": None,
        # str
        "State": None,
        # str
        "City": None,
        # double?
        "Longitude": None,
        # double?
        "Latitude": None,
        # int
        "Asn": None,
    }


@export
class Malware(Entity):
    """
    Malware Entity class.

    Attributes
    ----------
    Name : str
        Malware Name
    Category : str
        Malware Category
    File : File
        Malware File
    Files : List[File]
        Malware Files
    Processes : List[Process]
        Malware Processes

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
        return f"{self.Name}: {self.Category}"

    _entity_schema = {
        # Name (type System.String)
        "Name": None,
        # Category (type System.String)
        "Category": None,
        # File (type Microsoft.Azure.Security.Detection.AlertContracts.V3.Entities.File)
        "File": "File",
        "Files": (list, "File"),
        "Processes": (list, "Process"),
    }


@export
class NetworkConnection(Entity):
    """
    NetworkConnection Entity class.

    Attributes
    ----------
    SourceAddress : IPAddress
        NetworkConnection SourceAddress
    SourcePort : int
        NetworkConnection SourcePort
    DestinationAddress : IPAddress
        NetworkConnection DestinationAddress
    DestinationPort : int
        NetworkConnection DestinationPort
    Protocol : str
        NetworkConnection Protocol


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
        desc = "{}:{} [{}]-> {}:{}".format(
            self.SourceAddress,
            self.SourcePort,
            self.Protocol,
            self.DestinationAddress,
            self.DestinationPort,
        )
        return desc

    _entity_schema = {
        # SourceAddress (type Microsoft.Azure.Security.Detection
        # .AlertContracts.V3.Entities.IP)
        "SourceAddress": "IPAddress",
        # SourcePort (type System.Nullable`1[System.Int32])
        "SourcePort": None,
        # DestinationAddress (type Microsoft.Azure.Security.Detection
        # .AlertContracts.V3.Entities.IP)
        "DestinationAddress": "IPAddress",
        # DestinationPort (type System.Nullable`1[System.Int32])
        "DestinationPort": None,
        # Protocol (type System.Nullable`1[System.Net.Sockets.ProtocolType])
        "Protocol": None,
    }


@export
class Process(Entity):
    """
    Process Entity class.

    Attributes
    ----------
    ProcessId : str
        Process ProcessId
    CommandLine : str
        Process CommandLine
    ElevationToken : str
        Process ElevationToken
    CreationTimeUtc : datetime
        Process CreationTimeUtc
    ImageFile : File
        Process ImageFile
    Account : Account
        Process Account
    ParentProcess : Process
        Process ParentProcess
    Host : Host
        Process Host
    LogonSession : HostLogonSession
        Process LogonSession

    """

    def __init__(
        self,
        src_entity: Mapping[str, Any] = None,
        src_event: Mapping[str, Any] = None,
        role="new",
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
        role : str, optional
            'new' or 'parent' - only relevant if the entity
            is being constructed from an event.
            (the default is 'new')

        Other Parameters
        ----------------
        kwargs : Dict[str, Any]
            Supply the entity properties as a set of
            kw arguments.

        """
        super().__init__(src_entity=src_entity, **kwargs)
        # pylint: disable=locally-disabled, line-too-long
        if src_event is not None:
            if role == "new":
                self.ProcessId = (
                    src_event["NewProcessId"] if "NewProcessId" in src_event else None
                )
                self.CommandLine = (
                    src_event["CommandLine"] if "CommandLine" in src_event else None
                )
                if "TimeCreatedUtc" in src_event:
                    self.CreationTimeUtc = src_event["TimeCreatedUtc"]
                elif "TimeGenerated" in src_event:
                    self.CreationTimeUtc = src_event["TimeGenerated"]
                self.ProcessId = (
                    src_event["NewProcessId"] if "NewProcessId" in src_event else None
                )
                self.ImageFile = File(src_event=src_event, role="new")
                self.Account = Account(src_event=src_event, role="subject")

                if "ParentProcessName" in src_event or "ProcessName" in src_event:
                    parent = Process(src_event=src_event, role="parent")
                    self.ParentProcess = parent

                # Linux properties
                self.success = src_event["success"] if "success" in src_event else None
                self.audit_user = (
                    src_event["audit_user"] if "audit_user" in src_event else None
                )
                self.auid = src_event["auid"] if "auid" in src_event else None
                self.group = src_event["group"] if "group" in src_event else None
                self.gid = src_event["gid"] if "gid" in src_event else None
                self.effective_user = (
                    src_event["effective_user"]
                    if "effective_user" in src_event
                    else None
                )
                self.euid = src_event["euid"] if "euid" in src_event else None
                self.effective_group = (
                    src_event["effective_group"]
                    if "effective_group" in src_event
                    else None
                )
                self.egid = (
                    src_event["effective_group"]
                    if "effective_group" in src_event
                    else None
                )
                self.cwd = src_event["cwd"] if "cwd" in src_event else None
                self.name = src_event["cwd"] if "cwd" in src_event else None
            else:
                self.ProcessId = (
                    src_event["ProcessId"] if "ProcessId" in src_event else None
                )
                self.ImageFile = File(src_event=src_event, role="parent")

    # pylint: enable=locally-disabled, line-too-long

    @property
    def ProcessName(self) -> str:  # noqa: N802
        """Return the name of the process file."""
        file = self["ImageFile"]
        return file.Name if file else None

    @property
    def ProcessFilePath(self) -> str:  # noqa: N802
        """Return the name of the process file path."""
        file = self["ImageFile"]
        return file.FullPath if file else None

    @property
    def description_str(self) -> str:
        """Return Entity Description."""
        return f"{self.ProcessFilePath}: {self.CommandLine}"

    _entity_schema = {
        # ProcessId (type System.String)
        "ProcessId": None,
        # CommandLine (type System.String)
        "CommandLine": None,
        # ElevationToken (type System.Nullable`1
        # [Microsoft.Azure.Security.Detection
        # .AlertContracts.V3.Entities.ElevationToken])
        "ElevationToken": None,
        # CreationTimeUtc (type System.Nullable`1[System.DateTime])
        "CreationTimeUtc": None,
        # ImageFile (type Microsoft.Azure.Security.Detection
        # .AlertContracts.V3.Entities.File)
        "ImageFile": "File",
        # Account (type Microsoft.Azure.Security.Detection
        # .AlertContracts.V3.Entities.Account)
        "Account": "Account",
        # ParentProcess (type Microsoft.Azure.Security.Detection.AlertContracts
        # .V3.Entities.Process)
        "ParentProcess": "Process",
        # Host (type Microsoft.Azure.Security.Detection
        # .AlertContracts.V3.Entities.Host)
        "Host": "Host",
        # Host (type Microsoft.Azure.Security.Detection
        # .AlertContracts.V3.Entities.HostLogonSession)
        "LogonSession": "HostLogonSession",
    }


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


@export
class OSFamily(Enum):
    """OSFamily enumeration."""

    Linux = 0
    Windows = 1


_ENTITY_ENUMS[OSFamily.__name__] = OSFamily


@export
class ElevationToken(Enum):
    """ElevationToken enumeration."""

    Default = 0
    Full = 1
    Limited = 2


_ENTITY_ENUMS[ElevationToken.__name__] = ElevationToken


@export
class AzureResource(Entity):
    """
    AzureResource Entity class.

    Attributes
    ----------
    ResourceId : str
        AzureResource ResourceId
    SubscriptionId : str
        AzureResource SubscriptionId
    ResourceIdParts : Dict[str, str]
        AzureResource ResourceIdParts


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
        return self.ResourceId

    _entity_schema = {
        # ResourceId (type System.String)
        "ResourceId": None,
        # SubscriptionId (type System.String)
        "SubscriptionId": None,
        # ResourceIdParts (type System.Collections.Generic.IReadOnlyDictionary`2
        # [System.String,System.String])
        "ResourceIdParts": None,
    }


@export
class Alert(Entity):
    """
    Alert Entity class.

    Attributes
    ----------
    DisplayName : str
        Alert DisplayName
    CompromisedEntity : str
        Alert CompromisedEntity
    Count : int
        Alert Count
    StartTimeUtc : datetime
        Alert StartTimeUtc
    EndTimeUtc : datetime
        Alert EndTimeUtc
    Severity : str
        Alert Severity
    SystemAlertIds : List[str]
        Alert SystemAlertIds
    AlertType : str
        Alert AlertType
    VendorName : str
        Alert VendorName
    ProviderName : str
        Alert ProviderName

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
        return f"{self.DisplayName} ({self.StartTimeUtc}) {self.CompromisedEntity}"

    _entity_schema = {
        # DisplayName (type System.String)
        "DisplayName": None,
        # CompromisedEntity (type System.String)
        "CompromisedEntity": None,
        # Count (type System.Nullable`1[System.Int32])
        "Count": None,
        # StartTimeUtc (type System.Nullable`1[System.DateTime])
        "StartTimeUtc": None,
        # EndTimeUtc (type System.Nullable`1[System.DateTime])
        "EndTimeUtc": None,
        # Severity (type System.Nullable`1
        # [Microsoft.Azure.Security.Detection.AlertContracts.V3.Severity])
        "Severity": None,
        # SystemAlertIds (type System.Collections.Generic.List`1[System.String])
        "SystemAlertIds": None,
        # AlertType (type System.String)
        "AlertType": None,
        # VendorName (type System.String)
        "VendorName": None,
        # ProviderName (type System.String)
        "ProviderName": None,
    }


@export
class Threatintelligence(Entity):
    """
    Threatintelligence Entity class.

    Attributes
    ----------
    ProviderName : str
        Threatintelligence ProviderName
    ThreatType : str
        Threatintelligence ThreatType
    ThreatName : str
        Threatintelligence ThreatName
    Confidence : str
        Threatintelligence Confidence
    ReportLink : str
        Threatintelligence ReportLink
    ThreatDescription : str
        Threatintelligence ThreatDescription

    """

    def __init__(self, src_entity: Mapping[str, Any] = None, **kwargs):
        """
        Create a new instance of the entity type.

            :param src_entity: instantiate entity using properties of src entity
            :param kwargs: key-value pair representation of entity
        """
        super().__init__(src_entity=src_entity, **kwargs)

    @property
    def description_str(self) -> str:
        """Return Entity Description."""
        return f"{self.DisplayName} ({self.StartTimeUtc}) {self.CompromisedEntity}"

    _entity_schema = {
        # String Name of the provider from whom this
        # Threat Intelligence information was received
        "ProviderName": None,
        "ThreatType": None,
        "ThreatName": None,
        "Confidence": None,
        "ReportLink": None,
        "ThreatDescription": None,
    }


@export
class UnknownEntity(Entity):
    """Generic Entity class."""

    def __init__(self, src_entity: Mapping[str, Any] = None, **kwargs):
        """
        Create a new instance of the entity type.

            :param src_entity: instantiate entity using properties of src entity
            :param kwargs: key-value pair representation of entity
        """
        super().__init__(src_entity=src_entity, **kwargs)

    @property
    def description_str(self) -> str:
        """Return Entity Description."""
        return "OtherEntity"

    _entity_schema = {}  # type: Dict[str, Any]


# Dictionary to map text names of types to the class.
Entity.ENTITY_NAME_MAP.update(
    {
        "account": Account,
        "host": Host,
        "process": Process,
        "file": File,
        "cloudapplication": CloudApplication,
        "dnsresolve": DnsResolve,
        "ipaddress": IpAddress,
        "ip": IpAddress,
        "networkconnection": NetworkConnection,
        "malware": Malware,
        "registry-key": RegistryKey,
        "registrykey": RegistryKey,
        "registry-value": RegistryValue,
        "registryvalue": RegistryValue,
        "host-logon-session": HostLogonSession,
        "hostlogonsession": HostLogonSession,
        "filehash": FileHash,
        "security-group": SecurityGroup,
        "securitygroup": SecurityGroup,
        "alerts": Alert,
        "alert": Alert,
    }
)
