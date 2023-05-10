# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""msticpyconfig.yaml data model."""

from enum import Enum
from typing import Dict, List, Optional, Tuple, Union
from uuid import UUID

from pydantic import AnyUrl, BaseModel, Extra

from .._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"


# pylint: disable=too-few-public-methods


class BaseNoExtraModel(BaseModel):
    """Base model with extra fields forbidden."""

    class Config:
        """Pydantic config overrides."""

        extra = Extra.forbid


class ProtectedValueEnv(BaseModel):
    """Protected environment variable definition."""

    EnvironmentVar: str


class ProtectedValueKv(BaseModel):
    """Protected KeyVault secret definition."""

    KeyVault: Optional[str]


class ProtectedValue(BaseModel):
    """Protected value definition."""

    __root__: Union[str, ProtectedValueEnv, ProtectedValueKv]


class Args(BaseModel):
    """Provider Args definition."""

    class Config:
        """Pydantic config overrides."""

        extra = Extra.allow

    AuthKey: Optional[ProtectedValue] = None
    AuthId: Optional[ProtectedValue] = None
    ClientId: Optional[ProtectedValue] = None
    TenantId: Optional[ProtectedValue] = None
    ClientSecret: Optional[ProtectedValue] = None
    Cluster: Optional[AnyUrl] = None
    IntegratedAuth: Optional[bool] = True
    Database: Optional[str] = None


class SentinelWorkspace(BaseNoExtraModel):
    """Azure Sentinel Workspace configuration."""

    TenantId: UUID
    WorkspaceId: UUID
    SubscriptionId: Optional[UUID] = None
    ResourceGroup: Optional[str] = None
    WorkspaceName: Optional[str] = None


class SentinelWorkspacesModel(BaseNoExtraModel):
    """Azure Sentinel Workspaces definition."""

    Workspaces: Optional[Dict[str, SentinelWorkspace]] = None


class KustoClusterModel(BaseModel):
    """Kusto cluster definition."""

    ClusterGroups: Optional[List[str]] = None
    Args: Args


class TIProviderModel(BaseModel):
    """Threat Intelligence provider definition."""

    Args: Optional[Args]
    Primary: bool
    Provider: str


class DataProviderModel(BaseModel):
    """Data provider definition."""

    Args: Optional[Args]
    Provider: Optional[str]


class AzureCloud(Enum):
    """Azure cloud enumeration."""

    GLOBAL = "global"
    CN = "cn"
    DE = "de"
    USGOV = "usgov"
    PPE = "ppe"
    US = "us"


# pylint: disable=invalid-name
class AuthMethod(Enum):
    """Authentication methods definition."""

    env = "env"
    cli = "cli"
    msi = "msi"
    vscode = "vscode"
    powershell = "powershell"
    interactive = "interactive"
    interactive_browser = "interactive_browser"
    devicecode = "devicecode"
    device_code = "device_code"
    device = "device"
    environment = "environment"
    managedidentity = "managedidentity"
    managed_identity = "managed_identity"
    clientsecret = "clientsecret"
    client_secret = "client_secret"
    certificate = "certificate"
    cert = "cert"


class AzureModel(BaseModel):
    """Azure global configuration."""

    cloud: Optional[AzureCloud] = AzureCloud.GLOBAL
    auth_methods: List[AuthMethod]


class KeyVaultModel(BaseNoExtraModel):
    """Azure Key Vault settings."""

    VaultName: str
    ResourceGroup: Optional[str] = None
    SubscriptionId: Optional[UUID] = None
    TenantId: UUID
    AzureRegion: Optional[str] = None
    UseKeyring: Optional[bool] = False
    Authority: Optional[AzureCloud] = AzureCloud.GLOBAL
    auth_methods: Optional[List[AuthMethod]] = None


class QueryDefinitionsModel(BaseModel):
    """List of locations to look for query files."""

    Custom: Optional[List[str]] = None


class ProxyDefinition(BaseNoExtraModel):
    """Proxy definition."""

    Url: str
    UserName: Optional[ProtectedValue] = None
    Password: Optional[ProtectedValue] = None


class ProxiesModel(BaseModel):
    """Proxies settings."""

    Https: ProxyDefinition
    Http: Optional[ProxyDefinition] = None


class LoggingModel(BaseNoExtraModel):
    """Logging settings."""

    LoggingLevel: Optional[str] = "Info"
    FileName: Optional[str] = None


class MsticpyGlobalModel(BaseNoExtraModel):
    """Msticpy global settings."""

    FriendlyExceptions: Optional[bool] = True
    Proxies: Optional[ProxiesModel] = None
    Logging: Optional[LoggingModel] = None
    HttpTimeout: Union[None, float, Tuple[float, ...], Dict[str, float]] = 30


class MsticpyConfigSchema(BaseModel):
    """Msticpy configuration definition."""

    msticpy: Optional[MsticpyGlobalModel] = None
    AzureSentinel: Optional[SentinelWorkspacesModel] = None
    Azure: Optional[AzureModel] = None
    KustoClusters: Optional[Dict[str, KustoClusterModel]] = None
    TIProviders: Optional[Dict[str, TIProviderModel]] = None
    DataProviders: Optional[Dict[str, DataProviderModel]] = None
    OtherProviders: Optional[Dict[str, DataProviderModel]] = None
    ContextProviders: Optional[Dict[str, DataProviderModel]] = None
    QueryDefinitions: Optional[QueryDefinitionsModel] = None
    KeyVault: Optional[KeyVaultModel] = None
