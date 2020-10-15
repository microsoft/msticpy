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
from .azure_auth_core import AzCredentials, az_connect_core
from .provider_settings import get_provider_settings

__version__ = VERSION
__author__ = "Pete Bryan"


def az_connect(
    auth_methods: List = None,
    silent: bool = False,
) -> AzCredentials:
    """
    Connect to Azure SDK/API.

    Parameters
    ----------
    auth_methods : List, optional
        List of authentication methods to try
        Possible options are:
        - "env" - to get authentication details from environment varibales
        - "cli" - to use Azure CLI authentication details
        - "msi" - to user Managed Service Indenity details
        - "interactive" - to prompt for interactive login
        Default is ["env", "cli", "msi", "interactive"]
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
    # If using env options try to load from msticpy
    if auth_methods is None or "env" in auth_methods:
        try:
            data_provs = get_provider_settings(config_section="DataProviders")
            az_cli_config = data_provs.get("AzureCLI")
            config_items = az_cli_config.args  # type: ignore
            os.environ["AZURE_CLIENT_ID"] = config_items["clientId"]
            os.environ["AZURE_TENANT_ID"] = config_items["tenantId"]
            os.environ["AZURE_CLIENT_SECRET"] = config_items["clientSecret"]
        except KeyError:
            pass
    credentials = az_connect_core(auth_methods=auth_methods, silent=silent)
    sub_client = SubscriptionClient(credentials.modern)  # type: ignore
    if not sub_client:
        raise CloudError("Could not create a Subscription client.")

    return credentials


def az_user_connect(silent: bool = False) -> AzCredentials:
    """
    Authenticate to the SDK using user based authenticaiton methods, Azure CLI or interactive logon.

    Parameters
    ----------
    silent : bool, optional
        Whether you want the auth process to display any output, by default False

    Returns
    -------
    AzCredentials

    """
    credentials = az_connect_core(auth_methods=["cli", "interactive"], silent=silent)
    return credentials
