# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Azure authentication handling."""
import os

from azure.common.exceptions import CloudError
from azure.mgmt.subscription import SubscriptionClient

from .._version import VERSION
from .azure_auth_core import AzCredentials, az_connect_core
from .provider_settings import get_provider_settings

__version__ = VERSION
__author__ = "Pete Bryan"


def az_connect(
    client_id: str = None,
    tenant_id: str = None,
    secret: str = None,
    silent: bool = False,
) -> AzCredentials:
    """Authenticate with the SDK."""
    # Use details of msticpyyaml if not provided
    if client_id is None and tenant_id is None and secret is None:
        try:
            data_provs = get_provider_settings(config_section="DataProviders")
            az_cli_config = data_provs.get("AzureCLI")
            config_items = az_cli_config.args
            os.environ["AZURE_CLIENT_ID"] = config_items["clientId"]
            os.environ["AZURE_TENANT_ID"] = config_items["tenantId"]
            os.environ["AZURE_CLIENT_SECRET"] = config_items["clientSecret"]
        except KeyError:
            pass

    credentials = az_connect_core(silent=silent)
    sub_client = SubscriptionClient(credentials.modern)
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
