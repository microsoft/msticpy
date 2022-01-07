# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Azure authentication handling."""
import os
from typing import List

from azure.common.exceptions import CloudError
from azure.mgmt.subscription import SubscriptionClient

from .._version import VERSION

# importing only_interactive_cred for client use.
# pylint: disable=unused-import
from .azure_auth_core import (  # noqa: F401
    AzCredentials,
    az_connect_core,
    AzureCloudConfig,
    only_interactive_cred,
)

# pylint: enable=unused-import
from .provider_settings import get_provider_settings

__version__ = VERSION
__author__ = "Pete Bryan"


def az_connect(
    auth_methods: List[str] = None,
    tenant_id: str = None,
    silent: bool = False,
) -> AzCredentials:
    """
    Connect to Azure SDK/API.

    Parameters
    ----------
    auth_methods : List[str], optional
        List of authentication methods to try
        Possible options are:
        - "env" - to get authentication details from environment variables
        - "cli" - to use Azure CLI authentication details
        - "msi" - to user Managed Service Identity details
        - "interactive" - to prompt for interactive login
        Default is ["env", "cli", "msi", "interactive"]
    tenant_id : str, optional
        The tenant to authenticate against. If not supplied, the default tenant for the identity will be used.
    silent : bool, optional
        Set True to hide all output during connection, by default False

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
    az_cloud_config = AzureCloudConfig()
    # If using env options try to load from msticpy
    data_provs = get_provider_settings(config_section="DataProviders")
    az_cli_config = data_provs.get("AzureCLI")
    auth_methods = auth_methods or az_cloud_config.auth_methods
    if az_cli_config and az_cli_config.args:
        if "auth_methods" in az_cli_config.args:
            auth_methods = az_cli_config.args.get("auth_methods")
        if isinstance(auth_methods, list) and "env" in auth_methods:
            os.environ["AZURE_CLIENT_ID"] = az_cli_config.args.get("clientId") or ""
            os.environ["AZURE_TENANT_ID"] = az_cli_config.args.get("tenantId") or ""
            os.environ["AZURE_CLIENT_SECRET"] = (
                az_cli_config.args.get("clientSecret") or ""
            )
    credentials = az_connect_core(
        auth_methods=auth_methods, tenant_id=tenant_id, silent=silent
    )
    sub_client = SubscriptionClient(
        credential=credentials.modern,
        base_url=az_cloud_config.endpoints.resource_manager,
        credential_scopes=[az_cloud_config.token_uri],
    )
    if not sub_client:
        raise CloudError("Could not create a Subscription client.")

    return credentials


def az_user_connect(tenant_id: str = None, silent: bool = False) -> AzCredentials:
    """
    Authenticate to the SDK using user based authentication methods, Azure CLI or interactive logon.

    Parameters
    ----------
    silent : bool, optional
        Whether you want the auth process to display any output, by default False

    Returns
    -------
    AzCredentials

    """
    return az_connect_core(
        auth_methods=["cli", "interactive"], tenant_id=tenant_id, silent=silent
    )
