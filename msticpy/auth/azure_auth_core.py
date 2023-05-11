# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Azure KeyVault pre-authentication."""

import logging
import os
import sys
from collections import namedtuple
from datetime import datetime
from enum import Enum
from typing import List, Optional, Tuple, Union

from azure.common.credentials import get_cli_profile
from azure.identity import (
    AzureCliCredential,
    AzurePowerShellCredential,
    CertificateCredential,
    ChainedTokenCredential,
    ClientSecretCredential,
    DeviceCodeCredential,
    EnvironmentCredential,
    InteractiveBrowserCredential,
    ManagedIdentityCredential,
    VisualStudioCodeCredential,
)
from dateutil import parser

from .._version import VERSION
from ..common.exceptions import MsticpyAzureConfigError

# pylint: disable=unused-import
from .cloud_mappings import AzureCloudConfig, default_auth_methods  # noqa: F401
from .cred_wrapper import CredentialWrapper

__version__ = VERSION
__author__ = "Pete Bryan"

logger = logging.getLogger(__name__)

AzCredentials = namedtuple("AzCredentials", ["legacy", "modern"])

_HELP_URI = (
    "https://msticpy.readthedocs.io/en/latest/"
    "getting_started/AzureAuthentication.html"
)


# pylint: disable=too-few-public-methods
class AzureCredEnvNames:
    """Enumeration of Azure environment credential names."""

    AZURE_CLIENT_ID = "AZURE_CLIENT_ID"  # The app ID for the service principal
    AZURE_TENANT_ID = "AZURE_TENANT_ID"  # The service principal's Azure AD tenant ID
    # pylint: disable=line-too-long
    # [SuppressMessage("Microsoft.Security", "CS002:SecretInNextLine", Justification="This is an enum of env variable names")]
    AZURE_CLIENT_SECRET = "AZURE_CLIENT_SECRET"  # nosec  # noqa


def _build_env_client(
    aad_uri: Optional[str] = None, **kwargs
) -> Optional[EnvironmentCredential]:
    """Build a credential from environment variables."""
    del kwargs
    if (
        AzureCredEnvNames.AZURE_CLIENT_ID not in os.environ
        and AzureCredEnvNames.AZURE_CLIENT_SECRET not in os.environ
    ):
        # avoid creating env credential if require envs not set.
        logger.info("'env' credential requested but required env vars not set")
        return None
    return EnvironmentCredential(authority=aad_uri)  # type: ignore


def _build_cli_client(**kwargs) -> AzureCliCredential:
    """Build a credential from Azure CLI."""
    del kwargs
    return AzureCliCredential()


def _build_msi_client(
    tenant_id: Optional[str] = None, aad_uri: Optional[str] = None, **kwargs
) -> ManagedIdentityCredential:
    """Build a credential from Managed Identity."""
    msi_kwargs = kwargs.copy()
    if AzureCredEnvNames.AZURE_CLIENT_ID in os.environ:
        msi_kwargs["client_id"] = os.environ[AzureCredEnvNames.AZURE_CLIENT_ID]

    return ManagedIdentityCredential(
        tenant_id=tenant_id, authority=aad_uri, **msi_kwargs
    )


def _build_vscode_client(
    tenant_id: Optional[str] = None, aad_uri: Optional[str] = None, **kwargs
) -> VisualStudioCodeCredential:
    """Build a credential from Visual Studio Code."""
    del kwargs
    return VisualStudioCodeCredential(authority=aad_uri, tenant_id=tenant_id)


def _build_interactive_client(
    tenant_id: Optional[str] = None, aad_uri: Optional[str] = None, **kwargs
) -> InteractiveBrowserCredential:
    """Build a credential from Interactive Browser logon."""
    return InteractiveBrowserCredential(
        authority=aad_uri, tenant_id=tenant_id, **kwargs
    )


def _build_device_code_client(
    tenant_id: Optional[str] = None, aad_uri: Optional[str] = None, **kwargs
) -> DeviceCodeCredential:
    """Build a credential from Device Code."""
    return DeviceCodeCredential(authority=aad_uri, tenant_id=tenant_id, **kwargs)


def _build_client_secret_client(
    tenant_id: Optional[str] = None, aad_uri: Optional[str] = None, **kwargs
) -> Optional[ClientSecretCredential]:
    """Build a credential from Client Secret."""
    client_id = kwargs.pop("client_id", None)
    client_secret = kwargs.pop("client_secret", None)
    if not client_secret or not client_id:
        logger.info("'clientsecret' credential requested but no params supplied")
        return None
    return ClientSecretCredential(
        authority=aad_uri,
        tenant_id=tenant_id,  # type: ignore
        client_id=client_id,
        client_secret=client_secret,
        **kwargs,
    )


def _build_certificate_client(
    tenant_id: Optional[str] = None, aad_uri: Optional[str] = None, **kwargs
) -> Optional[CertificateCredential]:
    """Build a credential from Certificate."""
    client_id = kwargs.pop("client_id", None)
    if not client_id:
        logger.info(
            "'certificate' credential requested but client_id param not supplied"
        )
        return None
    return CertificateCredential(
        authority=aad_uri, tenant_id=tenant_id, client_id=client_id, **kwargs  # type: ignore
    )


def _build_powershell_client(**kwargs) -> AzurePowerShellCredential:
    """Build a credential from PowerShell logon session."""
    del kwargs
    return AzurePowerShellCredential()


_CLIENTS = dict(
    {
        "env": _build_env_client,
        "cli": _build_cli_client,
        "msi": _build_msi_client,
        "vscode": _build_vscode_client,
        "powershell": _build_powershell_client,
        "interactive": _build_interactive_client,
        "interactive_browser": _build_interactive_client,
        "devicecode": _build_device_code_client,
        "device_code": _build_device_code_client,
        "device": _build_device_code_client,
        "environment": _build_env_client,
        "managedidentity": _build_msi_client,
        "managed_identity": _build_msi_client,
        "clientsecret": _build_client_secret_client,
        "client_secret": _build_client_secret_client,
        "certificate": _build_certificate_client,
        "cert": _build_certificate_client,
    }
)


def list_auth_methods() -> List[str]:
    """Return list of accepted authentication methods."""
    return sorted(_CLIENTS.keys())


def _az_connect_core(
    auth_methods: Optional[List[str]] = None,
    cloud: Optional[str] = None,
    tenant_id: Optional[str] = None,
    silent: bool = False,
    **kwargs,
) -> AzCredentials:
    """
    Authenticate using multiple authentication sources.

    Parameters
    ----------
    auth_methods : List[str], optional
        List of authentication methods to try
        For a list of possible authentication methods use the `list_auth_methods`
        function.
        If not set, it will use the value defined in msticpyconfig.yaml.
        If this is not set, the default is ["env", "cli", "msi", "interactive"]
    cloud : str, optional
        What Azure cloud to connect to.
        By default it will attempt to use the cloud setting from config file.
        If this is not set it will default to Azure Public Cloud
    tenant_id : str, optional
        The tenant to authenticate against. If not supplied,
        the tenant ID is read from configuration, or the default tenant for the identity.
    silent : bool, optional
        Whether to display any output during auth process. Default is False.
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
    MsticpyAzureConfigError
        If chained token credential creation fails.

    See Also
    --------
    list_auth_methods

    Notes
    -----
    The function tries to obtain credentials from the following
    sources:
    - Azure Auth Environment variables
    - Azure CLI (if an active session is logged on)
    - Managed Service Identity
    - Interactive browser logon
    - and others - see list_auth_methods for full list.
    If the authentication is successful both ADAL (legacy) and
    MSAL (modern) credential types are returned.

    """
    # Create the auth methods with the specified cloud region
    cloud = cloud or kwargs.pop("region", AzureCloudConfig().cloud)
    az_config = AzureCloudConfig(cloud)
    aad_uri = az_config.endpoints.active_directory
    logger.info("az_connect_core - using %s cloud and endpoint: %s", cloud, aad_uri)

    tenant_id = tenant_id or az_config.tenant_id
    auth_methods = auth_methods or az_config.auth_methods
    logger.info(
        "TenantId:  %s, requested auth methods: %s",
        tenant_id,
        ", ".join(auth_methods or ["none"]),
    )
    creds = kwargs.pop("credential", None)
    if not creds:
        creds = _build_chained_creds(
            aad_uri=aad_uri,
            requested_clients=auth_methods,
            tenant_id=tenant_id,
            **kwargs,
        )

    # Filter and replace error message when credentials not found
    azure_identity_logger = logging.getLogger("azure.identity")
    handler = logging.StreamHandler(sys.stdout)
    if silent:
        handler.addFilter(_filter_all_warnings)
    else:
        handler.addFilter(_filter_credential_warning)
    azure_identity_logger.setLevel(logging.WARNING)
    azure_identity_logger.handlers = [handler]

    # Connect to the subscription client to validate
    legacy_creds = CredentialWrapper(
        creds, resource_id=AzureCloudConfig(cloud).token_uri
    )
    if not creds:
        raise MsticpyAzureConfigError(
            "Cannot authenticate with specified credential types.",
            "At least one valid authentication method required.",
            help_uri=_HELP_URI,
            title="Authentication failure",
        )

    return AzCredentials(legacy_creds, creds)


az_connect_core = _az_connect_core


def _build_chained_creds(
    aad_uri,
    requested_clients: Union[List[str], None] = None,
    tenant_id: Optional[str] = None,
    **kwargs,
) -> ChainedTokenCredential:
    """
    Build a chained token credential.

    Parameters
    ----------
    requested_clients : List[str]
        List of clients to chain.
    aad_uri : str
        The URI of the Azure AD cloud to connect to
    tenant_id : str
        The tenant ID to connect to

    Returns
    -------
    ChainedTokenCredential
        A chained token credential.

    Raises
    ------
    MsticpyAzureConfigError
        If the chained credential creation fails.

    """
    # Create the chained credential
    if not requested_clients:
        requested_clients = ["env", "cli", "msi", "interactive"]
        logger.info("No auth methods requested defaulting to: %s", requested_clients)
    cred_list = []
    invalid_cred_types: List[str] = []
    unusable_cred_type: List[str] = []
    for cred_type in requested_clients:  # type: ignore[union-attr]
        if cred_type not in _CLIENTS:
            invalid_cred_types.append(cred_type)
            logger.info("Unknown authentication type requested: %s", cred_type)
            continue
        cred_client = _CLIENTS[cred_type](  # type: ignore[operator]
            tenant_id=tenant_id, aad_uri=aad_uri, **kwargs
        )
        if cred_client is not None:
            cred_list.append(cred_client)
        else:
            unusable_cred_type.append(cred_type)
    logger.info(
        "Cred types added to chained credential: %s",
        ", ".join(cred.__class__.__name__ for cred in cred_list if cred is not None),
    )
    if not cred_list:
        exception_args = [
            "Cannot authenticate - no valid credential types.",
            "At least one valid authentication method required.",
            f"Configured auth_types: {','.join(requested_clients)}",
        ]
        if invalid_cred_types:
            exception_args.append(
                f"Unrecognized auth_types: {','.join(invalid_cred_types)}",
            )
        if unusable_cred_type:
            exception_args.extend(
                [
                    "The following auth types could not be used due to",
                    "missing parameters or environment variables:",
                    ",".join(invalid_cred_types),
                ]
            )
        raise MsticpyAzureConfigError(
            *exception_args,
            help_uri=_HELP_URI,
            title="Authentication failure",
        )
    return ChainedTokenCredential(*cred_list)


def only_interactive_cred(chained_cred: ChainedTokenCredential):
    """Return True if only interactive browser credentials available."""
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
            print("Unable to sign-in with environment variable credentials.")
        if message.startswith("AzureCliCredential"):
            print("Unable to sign-in with Azure CLI credentials.")
        if message.startswith("ManagedIdentityCredential"):
            print("Unable to sign-in with Managed Instance credentials.")
    return not message


def _filter_all_warnings(record) -> bool:
    """Filter out credential error messages."""
    if record.name.startswith("azure.identity") and record.levelno == logging.WARNING:
        message = record.getMessage()
        if ".get_token" in message:
            return not message
    return True


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
