# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Azure authentication handling."""
import os
from typing import List, Optional

from azure.common.exceptions import CloudError
from azure.identity import DeviceCodeCredential
from azure.mgmt.subscription import SubscriptionClient

from .._version import VERSION

# pylint: enable=unused-import
from ..common.provider_settings import get_provider_settings

# importing only_interactive_cred for client use.
# pylint: disable=unused-import
from .azure_auth_core import (  # noqa: F401
    AzCredentials,
    AzureCloudConfig,
    AzureCredEnvNames,
    az_connect_core,
    list_auth_methods,
    only_interactive_cred,
)
from .cred_wrapper import CredentialWrapper

__version__ = VERSION
__author__ = "Pete Bryan"


def az_connect(
    auth_methods: Optional[List[str]] = None,
    tenant_id: Optional[str] = None,
    silent: bool = False,
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

    Returns
    -------
    AzCredentials
        Named tuple of:
        - legacy (ADAL) credentials
        - modern (MSAL) credentials


    Raises
    ------
    CloudError
        If chained token credential creation fails.

    See Also
    --------
    list_auth_methods

    """
    az_cloud_config = AzureCloudConfig()
    # Use auth_methods param or configuration defaults
    data_provs = get_provider_settings(config_section="DataProviders")
    auth_methods = auth_methods or az_cloud_config.auth_methods

    # Ignore AzCLI settings except for authentication creds for EnvCred
    az_cli_config = data_provs.get("AzureCLI")
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
    credentials = az_connect_core(
        auth_methods=auth_methods, tenant_id=tenant_id, silent=silent, **kwargs
    )
    sub_client = SubscriptionClient(
        credential=credentials.modern,
        base_url=az_cloud_config.endpoints.resource_manager,  # type: ignore
        credential_scopes=[az_cloud_config.token_uri],
    )
    if not sub_client:
        raise CloudError("Could not create a Subscription client.")

    return credentials


def az_user_connect(
    tenant_id: Optional[str] = None, silent: bool = False
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
    AzCredentials

    """
    return az_connect_core(
        auth_methods=["cli", "interactive"], tenant_id=tenant_id, silent=silent
    )


def fallback_devicecode_creds(
    cloud: Optional[str] = None, tenant_id: Optional[str] = None, **kwargs
):
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
    AzCredentials
                Named tuple of:
        - legacy (ADAL) credentials
        - modern (MSAL) credentials

    Raises
    ------
    CloudError
        If chained token credential creation fails.

    """
    cloud = cloud or kwargs.pop("region", AzureCloudConfig().cloud)
    az_config = AzureCloudConfig(cloud)
    aad_uri = az_config.endpoints.active_directory
    tenant_id = tenant_id or AzureCloudConfig().tenant_id
    creds = DeviceCodeCredential(authority=aad_uri, tenant_id=tenant_id)
    legacy_creds = CredentialWrapper(
        creds, resource_id=AzureCloudConfig(cloud).token_uri
    )
    if not creds:
        raise CloudError("Could not obtain credentials.")

    return AzCredentials(legacy_creds, creds)
