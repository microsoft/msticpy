# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Azure authentication handling."""
import os
from collections import namedtuple

from azure.mgmt.subscription import SubscriptionClient
from azure.common.exceptions import CloudError
from azure.identity import (
    InteractiveBrowserCredential,
    ChainedTokenCredential,
    EnvironmentCredential,
    AzureCliCredential,
    ManagedIdentityCredential,
)

# uncomment once circular dependencies fixed
# from .provider_settings import get_provider_settings
from .cred_wrapper import CredentialWrapper
from .keyvault_pre_auth import az_connect as pre_auth
from .keyvault_pre_auth import AzCredentials
from .pkg_config import get_config

from .exceptions import MsticpyAzureConfigError


from .._version import VERSION

__version__ = VERSION
__author__ = "Pete Bryan"


def az_connect(
    client_id: str = None, tenant_id: str = None, secret: str = None
) -> AzCredentials:
    """
    Authenticate with Azure using multiple auth sources.

    Parameters
    ----------
    client_id : str, optional
        Client ID for Azure authentication, by default None
    tenant_id : str, optional
        TenantID to authenticate with, by default None
    secret : str, optional
        Client secret, by default None

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
    MsticpyAzureConfigError
        If AzureCli configuration not found in
        msticpyconfig.yaml

    Notes
    -----
    The function tries to obtain credentials from the following
    sources:
    - Azure Auth Environment variables
    - Azure CLI (if an active session is logged on)
    - Managed Service Identity
    - Interactive browser logon

    If the authentication is successful both ADAL (legacy) and
    MSAL (modern) credential types are returned.

    """
    # Use details of msticpyyaml if not provided
    if client_id is None and tenant_id is None and secret is None:
        try:
            az_cli_config = get_config("DataProviders.AzureCLI")
        except AttributeError:
            az_cli_config = None

        # @Pete - do we want to raise an exception here?
        # What if I want to do an interactive auth
        if not az_cli_config:
            raise MsticpyAzureConfigError(
                "No AzureCLI section found in configuration settings.",
                title="no AzureCLI settings available.",
            )
        try:
            config_items = get_config("DataProviders.AzureCLI.args")
            # TODO - can get rid of this check if the 2-stage auth works
            for azcli_setting in ("clientId", "tenantId", "clientSecret"):
                if "KeyVault" in azcli_setting:
                    raise MsticpyAzureConfigError(
                        "Cannot use a KeyVault-protected value to do",
                        "initial authentication",
                        title="",
                    )
            os.environ["AZURE_CLIENT_ID"] = config_items["clientId"]
            os.environ["AZURE_TENANT_ID"] = config_items["tenantId"]
            os.environ["AZURE_CLIENT_SECRET"] = config_items["clientSecret"]
        # TODO delete
        # except:
        #    pass
        except KeyError as key_err:
            key_name = key_err.args[0]
            raise MsticpyAzureConfigError(
                f"{key_name} is missing from AzureCLI section in your",
                "configuration.",
                title="missing f{key_name} settings for AzureCLI.",
            ) from key_err

    # @Pete - might some of these thow exceptions?
    # Create credentials and connect to the subscription client to validate

    # @Pete - when we call this (taken from code below), we might
    # already have run this code if it was called from the kv_pre_auth
    # module. When this happens does each piece silently succeed (if
    # they succeeded previously). In particular, we want to avoid a
    # a second browser auth flow.
    credentials = pre_auth()

    # env = EnvironmentCredential()
    # cli = AzureCliCredential()
    # interactive = InteractiveBrowserCredential()
    #
    # # mi = ManagedIdentityCredential  # nothing done with this
    # creds = ChainedTokenCredential(env, cli, interactive)
    # legacy_creds = CredentialWrapper(creds)
    # if not creds:
    #     raise CloudError("Could not obtain credentials.")
    # Is this a test to see if it's working?
    sub_client = SubscriptionClient(credentials.modern)
    if not sub_client:
        raise CloudError("Could not create a Subscription client.")

    # credentials = namedtuple("credentials", ["legacy", "modern"])
    return credentials
