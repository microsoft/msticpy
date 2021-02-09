# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Entity sub-package."""
from .account import Account
from .alert import Alert
from .azure_resource import AzureResource
from .cloud_application import CloudApplication
from .dns import Dns
from .entity import Entity
from .entity_enums import (  # noqa: F401
    Algorithm,
    ElevationToken,
    OSFamily,
    RegistryHive,
)
from .file import File
from .file_hash import FileHash
from .geo_location import GeoLocation
from .host import Host
from .host_logon_session import HostLogonSession
from .ip_address import IpAddress
from .malware import Malware
from .network_connection import NetworkConnection
from .process import Process
from .registry_key import RegistryKey
from .registry_value import RegistryValue
from .security_group import SecurityGroup
from .threat_intelligence import Threatintelligence
from .unknown_entity import UnknownEntity
from .url import Url

# Dictionary to map text names of types to the class.
Entity.ENTITY_NAME_MAP.update(
    {
        "account": Account,
        "azureresource": AzureResource,
        "host": Host,
        "process": Process,
        "file": File,
        "cloudapplication": CloudApplication,
        "dnsresolve": Dns,
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
        "threatintelligence": Threatintelligence,
        "url": Url,
        "unknown": UnknownEntity,
        "geolocation": GeoLocation,
    }
)
