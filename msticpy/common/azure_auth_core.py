# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Azure KeyVault pre-authentication."""
import logging
import sys
from collections import namedtuple
from datetime import datetime
from enum import Enum
from typing import List, Optional, Tuple

from azure.common.exceptions import CloudError
from azure.common.credentials import get_cli_profile
from azure.identity import (
    AzureCliCredential,
    ChainedTokenCredential,
    EnvironmentCredential,
    InteractiveBrowserCredential,
    ManagedIdentityCredential,
)
from dateutil import parser
from msrestazure import azure_cloud

from .._version import VERSION
from .cred_wrapper import CredentialWrapper
from .exceptions import MsticpyAzureConnectionError
from ..common import pkg_config as config
from .cloud_mappings import get_all_endpoints, get_all_suffixes

__version__ = VERSION
__author__ = "Pete Bryan"


AzCredentials = namedtuple("AzCredentials", ["legacy", "modern"])


def default_auth_methods() -> List[str]:
    """Get the default (all) authentication options."""
    try:
        az_settings = config.get_config("Azure")
        if az_settings and "auth_methods" in az_settings:
            return az_settings["auth_methods"]
    except KeyError:
        pass  # no Azure section in config
    return ["env", "cli", "msi", "interactive"]


class AzureCloudConfig:
    """Azure Cloud configuration."""

    def __init__(self, cloud: str = None):
        """
        Initialize AzureCloudConfig from `cloud` or configuration.

        Parameters
        ----------
        cloud : str, optional
            The cloud to retrieve configuration for. If not supplied,
            the cloud ID is read from configuration. If this is not available,
            it defaults to 'global'.

        """
        if cloud:
            self.cloud = cloud
        else:
            self.cloud = "global"
            try:
                az_settings = config.get_config("Azure")
                if az_settings and "cloud" in az_settings:
                    self.cloud = az_settings["cloud"]
            except KeyError:
                pass  # no Azure section in config
        self.auth_methods = default_auth_methods()
        try:
            self.auth_methods = config.get_config("Azure").get(
                "auth_methods", default_auth_methods()
            )
        except KeyError:
            pass  # no Azure section in config

    @property
    def endpoints(self) -> azure_cloud.CloudEndpoints:
        """
        Get a list of all the endpoints for an Azure cloud.

        Returns
        -------
        dict
            A dictionary of endpoints for the cloud.

        Raises
        ------
        MsticpyAzureConfigError
            If the cloud name is not valid.

        """
        return get_all_endpoints(self.cloud)

    @property
    def suffixes(self) -> azure_cloud.CloudSuffixes:
        """
        Get a list of all the suffixes for an Azure cloud.

        Returns
        -------
        dict
            A dictionary of suffixes for the cloud.

        Raises
        ------
        MsticpyAzureConfigError
            If the cloud name is not valid.

        """
        return get_all_suffixes(self.cloud)

    @property
    def token_uri(self) -> str:
        """Return the resource manager token URI."""
        return f"{self.endpoints.resource_manager}.default"


def _az_connect_core(
    auth_methods: List[str] = None, cloud: str = None, silent: bool = False, **kwargs
) -> AzCredentials:
    """
    Authenticate using multiple authentication sources.

    Parameters
    ----------
    auth_methods : List[str], optional
        List of authentication methods to try
        Possible options are:
        - "env" - to get authentication details from environment variables
        - "cli" - to use Azure CLI authentication details
        - "msi" - to user Managed Service Identity details
        - "interactive" - to prompt for interactive login
        If not set, it will use the value defined in msticpyconfig.yaml.
        If this is not set, the default is ["env", "cli", "msi", "interactive"]
    cloud : str, optional
        What Azure cloud to connect to.
        By default it will attempt to use the cloud setting from config file.
        If this is not set it will default to Azure Public Cloud
    silent : bool, optional
        Whether to display any output during auth process. Default is False.

    Returns
    -------
    AzCredentials
                Named tuple of:
        - legacy (ADAL) credentials
        - modern (MSAL) credentials

    Raises
    ------
    MsticpyAzureConnectionError
        If invalid auth options are presented.
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
    # Create the auth methods with the specified cloud region
    cloud = cloud or kwargs.pop("region", AzureCloudConfig().cloud)
    auth_options = _create_auth_options(cloud)
    if not auth_methods:
        auth_methods = default_auth_methods()
    try:
        auths = [auth_options[meth] for meth in auth_methods]
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
    legacy_creds = CredentialWrapper(
        creds, resource_id=AzureCloudConfig(cloud).token_uri
    )
    if not creds:
        raise CloudError("Could not obtain credentials.")

    return AzCredentials(legacy_creds, creds)


class _AzCachedConnect:
    """Singleton class caching Azure credentials."""

    _instance = None

    def __new__(cls):
        """Override new to check and return existing instance."""
        if cls._instance is None:
            cls._instance = super(_AzCachedConnect, cls).__new__(cls)
            cls.connect.__doc__ = _az_connect_core.__doc__
        return cls._instance

    def __init__(self):
        """Initialize the class."""
        self.az_credentials: Optional[AzCredentials] = None
        self.cred_cloud: str = self.current_cloud

    @property
    def current_cloud(self) -> str:
        """Return current cloud."""
        return AzureCloudConfig().cloud

    def connect(self, *args, **kwargs):
        """Call az_connect_core if token is not present or expired."""
        if self.az_credentials is None:
            self.az_credentials = _az_connect_core(*args, **kwargs)
            return self.az_credentials
        # Check expiry
        if (
            datetime.utcfromtimestamp(
                self.az_credentials.modern.get_token(
                    AzureCloudConfig().token_uri
                ).expires_on
            )
            <= datetime.utcnow()
        ):
            self.az_credentials = _az_connect_core(*args, **kwargs)
        # Check changed cloud
        if self.cred_cloud != kwargs.get(
            "cloud", kwargs.get("region", self.current_cloud)
        ):
            self.az_credentials = _az_connect_core(*args, **kwargs)
        return self.az_credentials


# externally callable function using the class above
_AZ_CACHED_CONNECT = _AzCachedConnect()
az_connect_core = _AZ_CACHED_CONNECT.connect


def only_interactive_cred(chained_cred: ChainedTokenCredential):
    """Return True if only interactivebrowser credentials available."""
    return len(chained_cred.credentials) == 1 and isinstance(
        chained_cred.credentials[0], InteractiveBrowserCredential
    )


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


def _create_auth_options(cloud: str = None) -> dict:
    """Create auth options dict with correct cloud set."""
    az_config = AzureCloudConfig(cloud)

    aad_uri = az_config.endpoints.active_directory  # type: ignore

    return {
        "env": EnvironmentCredential(),
        "cli": AzureCliCredential(),
        "msi": ManagedIdentityCredential(),
        "interactive": InteractiveBrowserCredential(authority=aad_uri),
    }


class AzureCliStatus(Enum):
    """Enumeration for _check_cli_credentials return values."""

    CLI_OK = 0
    CLI_NOT_INSTALLED = 1
    CLI_NEEDS_SIGN_IN = 2
    CLI_TOKEN_EXPIRED = 3
    CLI_UNKNOWN_ERROR = 4


def check_cli_credentials() -> Tuple[AzureCliStatus, Optional[str]]:
    """Check to see if there is a CLI session with a valid AAD token."""
    try:
        cli_profile = get_cli_profile()
        raw_token = cli_profile.get_raw_token()
        bearer_token = None
        if (
            isinstance(raw_token, tuple)
            and len(raw_token) == 3
            and len(raw_token[0]) == 3
        ):
            bearer_token = raw_token[0][2]
            if (
                parser.parse(bearer_token.get("expiresOn", datetime.min))
                < datetime.now()
            ):
                raise ValueError("AADSTS70043: The refresh token has expired")

        return AzureCliStatus.CLI_OK, "Azure CLI credentials available."
    except ImportError:
        # Azure CLI not installed
        return AzureCliStatus.CLI_NOT_INSTALLED, None
    except Exception as ex:  # pylint: disable=broad-except
        if "AADSTS70043: The refresh token has expired" in str(ex):
            message = (
                "Azure CLI was detected but the token has expired. "
                "For Azure CLI single sign-on, please sign in using '!az login'."
            )
            return AzureCliStatus.CLI_TOKEN_EXPIRED, message
        if "Please run 'az login' to setup account" in str(ex):
            message = (
                "Azure CLI was detected but no token is available. "
                "For Azure CLI single sign-on, please sign in using '!az login'."
            )
            return AzureCliStatus.CLI_NEEDS_SIGN_IN, message
        return AzureCliStatus.CLI_UNKNOWN_ERROR, None
