# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Azure KeyVault pre-authentication."""
import logging
import sys
from collections import namedtuple
from typing import List

from azure.common.exceptions import CloudError
from azure.identity import (
    AzureCliCredential,
    ChainedTokenCredential,
    EnvironmentCredential,
    InteractiveBrowserCredential,
    ManagedIdentityCredential,
)

from .._version import VERSION
from .cred_wrapper import CredentialWrapper
from .exceptions import MsticpyAzureConnectionError

__version__ = VERSION
__author__ = "Pete Bryan"


AzCredentials = namedtuple("AzCredentials", ["legacy", "modern"])

_AUTH_OPTIONS = {
    "env": EnvironmentCredential(),
    "cli": AzureCliCredential(),
    "msi": ManagedIdentityCredential(),
    "interactive": InteractiveBrowserCredential(),
}


def az_connect_core(
    auth_methods: List[str] = None, silent: bool = False
) -> AzCredentials:
    """
    Authenticate using multiple authentication sources.

    Parameters
    ----------
    auth_methods
        List of authentication methods to try
        Possible options are:
        - "env" - to get authentication details from environment varibales
        - "cli" - to use Azure CLI authentication details
        - "msi" - to user Managed Service Indenity details
        - "interactive" - to prompt for interactive login
        Default is ["env", "cli", "msi", "interactive"]

    silent
        Whether to display any output during auth process. Default is False.

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
    MsticpyAzureConnectionError
        If invalid auth options are presented.

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
    if not auth_methods:
        auth_methods = default_auth_methods()
    try:
        auths = [_AUTH_OPTIONS[meth] for meth in auth_methods]
    except KeyError as err:
        raise MsticpyAzureConnectionError(
            "Unknown authentication option, valid options are; env, cli, msi, interactive"
        ) from err

    # Filter and replace error message when credentials not found
    handler = logging.StreamHandler(sys.stdout)
    if silent:
        handler.addFilter(_filter_all_warnings)
    else:
        handler.addFilter(_filter_credential_warning)
    logging.basicConfig(level=logging.WARNING, handlers=[handler])

    # Create credentials and connect to the subscription client to validate
    creds = ChainedTokenCredential(*auths)  # type: ignore
    legacy_creds = CredentialWrapper(creds)
    if not creds:
        raise CloudError("Could not obtain credentials.")

    return AzCredentials(legacy_creds, creds)


def _filter_credential_warning(record) -> bool:
    """Rewrite out credential not found message."""
    if (
        not record.name.startswith("azure.identity")
        or record.levelno != logging.WARNING
    ):
        return True
    message = record.getMessage()
    if ".get_token" in message:
        if message.startswith("EnvironmentCredential"):
            print("Attempting to sign-in with environment variable credentials...")
        if message.startswith("AzureCliCredential"):
            print("Attempting to sign-in with Azure CLI credentials...")
        if message.startswith("ManagedIdentityCredential"):
            print("Attempting to sign-in with Managed Instance credentials...")
            print("Falling back to interactive logon.")
    return not message


def _filter_all_warnings(record) -> bool:
    """Filter out credential error messages."""
    if record.name.startswith("azure.identity") and record.levelno == logging.WARNING:
        message = record.getMessage()
        if ".get_token" in message:
            return not message
    return True


def default_auth_methods() -> List[str]:
    """Get the default (all) authentication options."""
    return list(_AUTH_OPTIONS)
