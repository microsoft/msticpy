# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Azure authentication handling."""
from __future__ import annotations

import os

from azure.identity import DeviceCodeCredential
from azure.mgmt.subscription import SubscriptionClient

from msticpy.common.provider_settings import ProviderSettings

from .._version import VERSION
from ..common.exceptions import MsticpyAzureConnectionError

# pylint: enable=unused-import
from ..common.provider_settings import get_provider_settings

# importing only_interactive_cred for client use.
from .azure_auth_core import (  # noqa: F401
    AzCredentials,
    AzureCloudConfig,
    AzureCredEnvNames,
    ChainedTokenCredential,
    az_connect_core,
)
from .cred_wrapper import CredentialWrapper

__version__ = VERSION
__author__ = "Pete Bryan"


def az_connect(
    auth_methods: list[str] | None = None,
    tenant_id: str | None = None,
    *,
    silent: bool = False,
    cloud: str | None = None,
    **kwargs,
) -> AzCredentials:
    """
    Connect to Azure SDK/API.

    Parameters
    ----------
    auth_methods : List[str], optional
        List of authentication methods to try
        For a list of possible authentication methods use the `list_auth_methods`
        function.
        Default is ["env", "msi", "vscode", "cli", "powershell", "devicecode"]
    tenant_id : str, optional
        The tenant to authenticate against. If not supplied, the default
        tenant for the identity will be used.
    silent : bool, optional
        Set True to hide all output during connection, by default False
    credential : AzureCredential
        If an Azure credential is passed, it will be used directly.
    cloud : str, optional
        What Azure cloud to connect to.
        By default it will attempt to use the cloud setting from config file.
        If this is not set it will default to Azure Public Cloud

    Returns
    -------
    AzCredentials
        Named tuple of:
        - legacy (ADAL) credentials
        - modern (MSAL) credentials


    Raises
    ------
    MsticpyAzureConnectionError
        If chained token credential creation fails.

    See Also
    --------
    list_auth_methods

    """
    az_cloud_config = AzureCloudConfig(cloud=cloud)
    # Use auth_methods param or configuration defaults
    data_provs: dict[str, ProviderSettings] = get_provider_settings(
        config_section="DataProviders"
    )
    auth_methods = auth_methods or az_cloud_config.auth_methods
    tenant_id = tenant_id or az_cloud_config.tenant_id

    # Ignore AzCLI settings except for authentication creds for EnvCred
    az_cli_config: ProviderSettings | None = data_provs.get("AzureCLI")
    if (
        az_cli_config
        and az_cli_config.args
        and isinstance(auth_methods, list)
        and "env" in auth_methods
    ):
        os.environ[AzureCredEnvNames.AZURE_CLIENT_ID] = (
            az_cli_config.args.get("clientId") or ""
        )
        os.environ[AzureCredEnvNames.AZURE_TENANT_ID] = (
            az_cli_config.args.get("tenantId") or ""
        )
        os.environ[AzureCredEnvNames.AZURE_CLIENT_SECRET] = (
            az_cli_config.args.get("clientSecret") or ""
        )
    credentials: AzCredentials = az_connect_core(
        auth_methods=auth_methods,
        tenant_id=tenant_id,
        silent=silent,
        cloud=cloud,
        **kwargs,
    )
    sub_client = SubscriptionClient(
        credential=credentials.modern,
        base_url=az_cloud_config.resource_manager,  # type: ignore
        credential_scopes=[az_cloud_config.token_uri],
    )
    if not sub_client:
        err_msg: str = "Could not create an Azure Subscription client with credentials."
        raise MsticpyAzureConnectionError(
            err_msg,
            title="Azure authentication error",
        )

    return credentials


def az_user_connect(
    tenant_id: str | None = None,
    *,
    silent: bool = False,
) -> AzCredentials:
    """
    Authenticate to the SDK using user based authentication methods, Azure CLI or interactive logon.

    Parameters
    ----------
    tenant_id : str, optional
        The tenant to authenticate against. If not supplied, the default tenant for the
        identity will be used.
    silent : bool, optional
        Whether you want the auth process to display any output, by default False

    Returns
    -------
    AzCredentials - Dataclass combining two types of Azure credentials:
    - legacy (ADAL) credentials
    - modern (MSAL) credentials

    """
    return az_connect_core(
        auth_methods=["cli", "interactive"],
        tenant_id=tenant_id,
        silent=silent,
    )


def fallback_devicecode_creds(
    cloud: str | None = None,
    tenant_id: str | None = None,
    region: str | None = None,
) -> AzCredentials:
    """
    Authenticate using device code as a fallback method.

    Parameters
    ----------
    cloud : str, optional
        What Azure cloud to connect to.
        By default it will attempt to use the cloud setting from config file.
        If this is not set it will default to Azure Public Cloud
    tenant_id : str, optional
        The tenant to authenticate against. If not supplied,
        the tenant ID is read from configuration, or the default tenant for the identity.

    Returns
    -------
    AzCredentials - Dataclass combining two types of Azure credentials:
    - legacy (ADAL) credentials
    - modern (MSAL) credentials

    Raises
    ------
    MsticpyAzureConnectionError
        If chained token credential creation fails.

    """
    cloud = cloud or region or AzureCloudConfig().cloud
    az_config: AzureCloudConfig = AzureCloudConfig(cloud)
    aad_uri: str = az_config.authority_uri
    tenant_id = tenant_id or az_config.tenant_id
    creds = DeviceCodeCredential(authority=aad_uri, tenant_id=tenant_id)
    legacy_creds = CredentialWrapper(creds, resource_id=az_config.token_uri)
    if not creds:
        err_msg: str = (
            f"Could not obtain credentials for tenant {tenant_id}"
            "Please check your Azure configuration and try again."
        )
        raise MsticpyAzureConnectionError(
            err_msg,
            title="Azure authentication error",
        )

    return AzCredentials(legacy_creds, ChainedTokenCredential(creds))  # type: ignore[arg-type]


def get_default_resource_name(resource_uri: str) -> str:
    """Get a default resource name for a resource URI."""
    separator: str = "" if resource_uri.strip().endswith("/") else "/"
    return f"{resource_uri}{separator}.default"
