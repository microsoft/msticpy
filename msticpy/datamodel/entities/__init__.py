# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Entity sub-package."""
import difflib
from typing import List

from .account import Account
from .alert import Alert
from .azure_resource import AzureResource
from .cloud_application import CloudApplication
from .cloud_logon_session import CloudLogonSession
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
from .iot_device import IoTDevice
from .ip_address import IpAddress
from .mail_cluster import MailCluster
from .mail_message import MailMessage
from .mailbox import Mailbox
from .mailbox_configuration import MailboxConfiguration
from .malware import Malware
from .network_connection import NetworkConnection
from .oauth_application import OAuthApplication
from .process import Process
from .registry_key import RegistryKey
from .registry_value import RegistryValue
from .security_group import SecurityGroup
from .service_principal import ServicePrincipal
from .submission_mail import SubmissionMail
from .threat_intelligence import Threatintelligence
from .unknown_entity import UnknownEntity
from .url import Url

from ..soc.incident import Incident  # isort: skip


# Defender class equivalents
class User(Account):
    """Alias for Account."""


class Ip(IpAddress):
    """Alias for IpAddress."""


class Machine(Host):
    """Alias for Host."""


# Dictionary to map text names of types to the class.
Entity.ENTITY_NAME_MAP.update(
    {
        "account": Account,
        "alert": Alert,
        "alerts": Alert,
        "azure-resource": AzureResource,
        "azureresource": AzureResource,
        "cloud-application": CloudApplication,
        "cloud-logon-session": CloudLogonSession,
        "cloudapplication": CloudApplication,
        "cloudlogonsession": CloudLogonSession,
        "dns": Dns,
        "dnsresolve": Dns,
        "file": File,
        "filehash": FileHash,
        "geolocation": GeoLocation,
        "host-logon-session": HostLogonSession,
        "host": Host,
        "hostlogonsession": HostLogonSession,
        "incident": Incident,
        "iotdevice": IoTDevice,
        "ip": IpAddress,
        "ipaddress": IpAddress,
        "location": GeoLocation,
        "machine": Machine,
        "mail-cluster": MailCluster,
        "mail-message": MailMessage,
        "mailbox": Mailbox,
        "mailcluster": MailCluster,
        "mailboxconfiguration": MailboxConfiguration,
        "mailmessage": MailMessage,
        "malware": Malware,
        "network-connection": NetworkConnection,
        "networkconnection": NetworkConnection,
        "oauthapplication": OAuthApplication,
        "process": Process,
        "registry-key": RegistryKey,
        "registry-value": RegistryValue,
        "registrykey": RegistryKey,
        "registryvalue": RegistryValue,
        "security-group": SecurityGroup,
        "securitygroup": SecurityGroup,
        "ServicePrincipal": ServicePrincipal,
        "SubmissionMail": SubmissionMail,
        "threatintelligence": Threatintelligence,
        "unknown": UnknownEntity,
        "url": Url,
        "user": User,
    }
)


def find_entity(entity):
    """Find entity name."""
    entity_cf = entity.casefold()
    entity_cls_dict = {
        cls.__name__.casefold(): cls for cls in Entity.ENTITY_NAME_MAP.values()
    }
    if entity_cf in Entity.ENTITY_NAME_MAP:
        print(f"Match found '{Entity.ENTITY_NAME_MAP[entity].__name__}'")
        return Entity.ENTITY_NAME_MAP[entity]
    if entity_cf in entity_cls_dict:
        print(f"Match found '{entity_cls_dict[entity_cf].__name__}'")
        return entity_cls_dict[entity_cf]
    # Try to find the closest matches
    closest = difflib.get_close_matches(entity, entity_cls_dict.keys(), cutoff=0.4)
    mssg = [f"No exact match found for '{entity}'. "]
    if len(closest) == 1:
        mssg.append(f"Closest match is '{entity_cls_dict[closest[0]].__name__}'")
    elif closest:
        match_list = [f"'{entity_cls_dict[match].__name__}'" for match in closest]
        mssg.append(f"Closest matches are {', '.join(match_list)}")
    else:
        mssg.extend(
            [
                "No close match found. Entities available:",
                *(cls.__name__ for cls in entity_cls_dict.values()),
            ]
        )
    print("\n".join(mssg))
    return None


def list_entities() -> List[str]:
    """List entities."""
    return sorted([cls.__name__ for cls in set(Entity.ENTITY_NAME_MAP.values())])


def entity_classes() -> List[type]:
    """Return a list of all entity classes."""
    return list(set(Entity.ENTITY_NAME_MAP.values()))
