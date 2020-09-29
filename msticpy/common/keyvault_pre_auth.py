# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Azure KeyVault pre-authentication."""
from collections import namedtuple

from azure.common.exceptions import CloudError
from azure.identity import (
    InteractiveBrowserCredential,
    ChainedTokenCredential,
    EnvironmentCredential,
    AzureCliCredential,
    ManagedIdentityCredential,
)

from .cred_wrapper import CredentialWrapper
from .._version import VERSION

__version__ = VERSION
__author__ = "Pete Bryan"


AzCredentials = namedtuple("AzCredentials", ["legacy", "modern"])


def az_connect() -> AzCredentials:
    """
    Authenticate using multiple authentication sources.

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
    # Create credentials and connect to the subscription client to validate
    env = EnvironmentCredential()
    cli = AzureCliCredential()

    # Can we test here if we have sufficient creds and skip
    # interactive browser?
    interactive = InteractiveBrowserCredential()
    # TODO
    # mi = ManagedIdentityCredential  # nothing done with this
    creds = ChainedTokenCredential(env, cli, interactive)
    legacy_creds = CredentialWrapper(creds)
    if not creds:
        raise CloudError("Could not obtain credentials.")

    return AzCredentials(legacy_creds, creds)
