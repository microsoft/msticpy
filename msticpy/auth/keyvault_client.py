# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Keyvault client - adapted from Bluehound code."""
from __future__ import annotations

import logging
from typing import ClassVar

from azure.core.exceptions import ClientAuthenticationError, ResourceNotFoundError
from azure.keyvault.secrets import KeyVaultSecret, SecretClient
from azure.mgmt.keyvault import KeyVaultManagementClient
from azure.mgmt.keyvault.models import Vault
from IPython.core.display import HTML
from IPython.display import display
from typing_extensions import Self

from .._version import VERSION
from ..common.exceptions import (
    MsticpyKeyVaultConfigError,
    MsticpyKeyVaultMissingSecretError,
    MsticpyUserConfigError,
)
from ..common.utility import export, is_ipython
from .azure_auth_core import AzCredentials, az_connect_core
from .keyvault_settings import KeyVaultSettings

__version__ = VERSION
__author__ = "Matt Richard, Ian Hellen"

LOGGER: logging.Logger = logging.getLogger(__name__)

_KV_CLIENT_AUTH_ERROR: list[str] = [
    "Retry authentication with msticpy.settings.auth_secrets_client",
    "using a different credential type.",
    "Alteratively use Azure CLI authentication:",
    (
        "1. Add 'cli' to the list of authentication methods in Azure/auth_methods"
        " in msticpyconfig.yaml "
    ),
    "2. run 'az login` at the start of your session.",
    "3. Re-run the MSTICPy function that failed with this error.",
    "",
]


@export
class BHKeyVaultClient:
    """Core KeyVault client."""

    _KEYRING_NAME: ClassVar[str] = "keyvault"

    def __init__(  # noqa: PLR0913
        self: BHKeyVaultClient,
        tenant_id: str | None = None,
        vault_uri: str | None = None,
        vault_name: str | None = None,
        settings: KeyVaultSettings | None = None,
        *,
        debug: bool = False,
        authn_type: str | None = None,
        auth_methods: list[str] | None = None,
        authority: str | None = None,
        authority_uri: str | None = None,
        credential: AzCredentials | None = None,
        **kwargs: str,
    ) -> None:
        """
        Initialize the BHKeyVault client.

        Parameters
        ----------
        tenant_id : str
            The tenant ID of the service
        vault_uri : str, optional
            The full URI of the keyvault, by default None
        vault_name : str, optional
            The name of the keyvault in the public cloud, by default None
        settings : KeyVaultSettings
            An instance of KeyVaultSettings containing KV parameters.
        auth_methods : list[str]
            The authentication methods to use for Key Vault auth
            Possible values are:
            - "env" - to get authentication details from environment variables
            - "cli" - to use Azure CLI authentication details
            - "msi" - to user Managed Service Identity details
            - "interactive" - to prompt for interactive login
            - "vscode" - to use VSCode credentials
            - "powershell" - to use PowerShell credentials
            - "interactive" - to prompt for interactive login
            - "cache" - to use shared token cache credentials
            - "devicecode" - to use device code with web login
            - "clientsecret" - to use client id/secret login.
        authn_type : str, optional
            [deprecated - use auth_methods]
            Authentication mode, by default 'interactive'
            Supported options are:
            - 'device' for device code authentication
            - 'interactive' for interactive browser authentication
        authority : str, optional
            The AAD authority - one of 'global', 'usgov', 'de' or 'chi'
        authority_uri : str, optional
            The AAD authority URI - overrides `authority`
        credential : Optional[AzureCredential]
            Azure credential
        client_id : Optional[str]
            Required if auth_methods is ["clientsecret"]
        client_secret : Optional[str]
            Required if auth_methods is ["clientsecret"]
        debug : bool, optional
            [description], by default False
        kwargs: str
            Other parameters

        Raises
        ------
        KeyVaultMissingVaultException
            No Vault name or URI supplied.

        Notes
        -----
        The parameter values can also be obtained from the
        KeyVault section of msticpyconfig.yaml.


        """
        self.debug: bool = debug
        self.settings: KeyVaultSettings = settings or KeyVaultSettings()

        tenant_id = tenant_id or self.settings.get("tenantid")
        if not tenant_id:
            err_msg: str = (
                "Could not get TenantId from function parameters or configuration."
                " Please add this to the KeyVault section of msticpyconfig.yaml"
            )
            raise MsticpyKeyVaultConfigError(
                err_msg,
                title="missing tenant ID value.",
                azcli_uri="https://docs.microsoft.com/cli/azure/authenticate-azure-cli",
            )
        self.tenant_id: str = tenant_id
        self.authn_type: str = authn_type or self.settings.get(
            "authntype",
            "interactive",
        )
        self.auth_methods: list[str] = auth_methods or self.settings.get(
            "auth_methods",
            ["interactive"],
        )

        # for authority and authority_uri, any parameters take priority
        # and fall back on settings if not specified.
        if authority:
            self.settings["authority"] = authority
        self.authority_uri: str = self.settings.get_tenant_authority_host(
            authority_uri=authority_uri,
            tenant=self.tenant_id,
        )

        self._vault_name, self.vault_uri = self._get_vault_name_and_uri(
            vault_name,
            vault_uri,
        )
        self.kv_client: SecretClient = self._try_credential_types(
            credential=credential,
            **kwargs,
        )

    def _try_credential_types(
        self: Self,
        *,
        credential: AzCredentials | None = None,
        **kwargs: str,
    ) -> SecretClient:
        """Try to access Key Vault to establish usable authentication method."""
        if credential:
            kv_client: SecretClient = SecretClient(
                self.vault_uri,
                credential=credential.modern,
            )
            try:
                self._get_working_kv_client(kv_client)
            except ClientAuthenticationError as client_err:
                _print_status(
                    f"Could not obtain access token using {credential.__class__.__name__}.",
                )
                self._raise_auth_failed_error(client_err)

        for idx, auth_method in enumerate(self.auth_methods):
            _print_status(
                f"Attempting connection to Key Vault using {auth_method} credentials...",
                newline=False,
            )
            credential = az_connect_core(
                auth_methods=[auth_method],
                **kwargs,
            )
            kv_client = SecretClient(self.vault_uri, credential.modern)
            try:
                return self._get_working_kv_client(kv_client)
            except ClientAuthenticationError as client_err:
                _print_status(f"Could not obtain access token using {auth_method}.")
                if idx + 1 < len(self.auth_methods):
                    continue
                self._raise_auth_failed_error(client_err)
        err_msg: str = "Failed to obtain token from all provided methods: " + ", ".join(
            self.auth_methods,
        )
        raise ClientAuthenticationError(err_msg)

    def _get_working_kv_client(self: Self, kv_client: SecretClient) -> SecretClient:
        """Try to list secrets - will throw ClientAuthentication error on failure."""
        # need to list to force iterator to run
        list(kv_client.list_properties_of_secrets())
        _print_status("done")
        return kv_client

    def _raise_auth_failed_error(self: Self, client_err: Exception) -> None:
        err_msg: str = (
            "No configured authentication methods found with credentials "
            f"with access the Key Vault '{self._vault_name}'"
        )
        raise MsticpyUserConfigError(
            err_msg,
            *_KV_CLIENT_AUTH_ERROR,
            title="Key Vault authentication configuration failed.",
        ) from client_err

    def _get_vault_name_and_uri(
        self: Self,
        vault_name: str | None,
        vault_uri: str | None,
    ) -> tuple[str | None, str]:
        """Validate and return vault name and URI."""
        if not vault_uri and not vault_name:
            if "vaultname" in self.settings:
                vault_name = self.settings["vaultname"]
            else:
                err_msg: str = (
                    "Check that you have specified the right value for VaultName"
                    " in your configuration"
                )
                raise MsticpyKeyVaultConfigError(
                    err_msg,
                    title="Key Vault vault name not found.",
                )
        if not vault_uri:
            vault_uri = self.settings.keyvault_uri
            if vault_uri:
                vault_uri = vault_uri.format(vault=vault_name)
            else:
                cloud = self.settings.cloud
                err_msg = (
                    f"Could not determine keyvault URI for national cloud {cloud}."
                    " Please verify that you have the correct national cloud"
                    " specified in the KeyVault section of msticpyconfig.yaml"
                )
                raise MsticpyKeyVaultConfigError(
                    err_msg,
                    title="no Key Vault URI for national cloud",
                )
        if self.debug:
            # Mask the vault name in the URI before logging
            masked_vault_uri = vault_uri
            if vault_uri and isinstance(vault_name, str) and vault_name:
                masked_vault_uri = vault_uri.replace(vault_name, "[REDACTED]")
            LOGGER.debug("Using Vault URI %s", masked_vault_uri)
        return vault_name, vault_uri

    @property
    def vault_name(self: Self) -> str:
        """Return the Key Vault name."""
        return (
            self._vault_name
            or self.vault_uri.replace("https://", "").split(".", maxsplit=1)[0]
        )

    @property
    def secrets(self: Self) -> list[str]:
        """Return the list of secret names from the vault."""
        return [x.id for x in self.kv_client.list_properties_of_secrets() if x.id]

    def get_secret(self: Self, secret_name: str) -> str:
        """
        Retrieve a secret from the Vault.

        Parameters
        ----------
        secret_name : str
            Name of the secret

        Returns
        -------
        str
            The secret value

        Raises
        ------
        KeyVaultMissingSecretException
            Secret not found in the Vault.

        """
        if "/" in secret_name:
            # If we're passed the full URL to the secret - extract just the
            # name
            secret_name = secret_name.rsplit("/", maxsplit=1)[-1]
        try:
            secret_bundle: KeyVaultSecret = self.kv_client.get_secret(name=secret_name)
        except ResourceNotFoundError as err:
            if self.debug:
                LOGGER.debug(
                    "Secret: is missing from vault: %s",
                    self.vault_uri,
                )
            err_msg: str = (
                f"Secret name {secret_name} could not be found in {self.vault_uri}"
                f" Provider returned: {err}"
            )
            raise MsticpyKeyVaultMissingSecretError(
                err_msg,
                title=f"secret {secret_name} not found.",
            ) from err
        if secret_bundle.value is None or not secret_bundle.value:
            if self.debug:
                LOGGER.debug(
                    "Secret was empty in vault %s",
                    self.vault_uri,
                )
            err_msg = (
                f"Secret name {secret_name} in {self.vault_uri}"
                "has blank or null value."
            )
            raise MsticpyKeyVaultMissingSecretError(
                err_msg,
                title=f"secret {secret_name} empty.",
            )
        return secret_bundle.value

    def set_secret(self: Self, secret_name: str, value: str) -> KeyVaultSecret:
        """
        Set a secret in the Vault.

        Parameters
        ----------
        secret_name : str
            Name of the secret
        value: str
            Secret value

        Returns
        -------
        KeyVaultSecret
            The secrets bundle for the secret

        """
        if self.debug:
            LOGGER.debug("Storing secret in %s", self.vault_uri)
        return self.kv_client.set_secret(name=secret_name, value=value)


@export
class BHKeyVaultMgmtClient:
    """Core KeyVault Management client."""

    def __init__(  # noqa:PLR0913
        self: BHKeyVaultMgmtClient,
        tenant_id: str | None = None,
        subscription_id: str | None = None,
        resource_group: str | None = None,
        azure_region: str | None = None,
        settings: KeyVaultSettings | None = None,
        *,
        debug: bool = False,
        mgmt_uri: str | None = None,
    ) -> None:
        """
        Initialize BH KeyVault Management Client.

        Parameters
        ----------
        tenant_id : str, Optional
            Tenant ID
        subscription_id : str, Optional
            Subscription ID
        resource_group : str, Optional
            Resource Group name
        azure_region : str, Optional
            Azure region - needed to create a new vault.
            By default, None
        settings : KeyVaultSettings
            An instance of KeyVaultSettings containing KV parameters.
        mgmt_uri : str, Optional
            The URI for Azure management endpoints.
        debug: bool, Optional
            Display additional debug details. Defaults to False

        Notes
        -----
        The parameter values can also be obtained from the
        KeyVault section of msticpyconfig.yaml.

        """
        self.debug: bool = debug
        self.settings: KeyVaultSettings = settings or KeyVaultSettings()
        self.tenant_id: str = tenant_id or self.settings.get("tenantid")
        if not self.tenant_id:
            err_msg: str = (
                "Could not get TenantId from function parameters or configuration."
                "Please add this to the KeyVault section of msticpyconfig.yaml"
            )
            raise MsticpyKeyVaultConfigError(
                err_msg,
                title="missing tenant ID value.",
            )
        subscription_id = subscription_id or self.settings.get(
            "subscriptionid",
        )
        if not subscription_id:
            err_msg = (
                "Could not get SubscriptionId from function parameters or configuration."
                " Please add this to the KeyVault section of msticpyconfig.yaml"
            )
            raise MsticpyKeyVaultConfigError(
                err_msg,
                title="missing SubscriptionId value.",
            )
        self.subscription_id: str = subscription_id
        mgmt_uri = mgmt_uri or self.settings.mgmt_uri
        if not mgmt_uri:
            cloud: str = self.settings.cloud
            err_msg = (
                f"Could not obtain an azure management URI for national cloud {cloud}."
                " Please verify that you have the correct national cloud"
                " specified in the KeyVault section of msticpyconfig.yaml"
            )
            raise MsticpyKeyVaultConfigError(
                err_msg,
                title="no Azure Management URI for national cloud",
            )
        self._client_uri: str = mgmt_uri

        self.auth_client: AzCredentials = az_connect_core()
        self.resource_group: str = resource_group or self.settings.get("resourcegroup")
        self.azure_region: str = azure_region or self.settings.get("azureregion")

    def list_vaults(self: Self) -> list[str]:
        """
        Return a list of vaults for the subscription.

        Returns
        -------
        list[str]
            Vault names

        """
        mgmt: KeyVaultManagementClient = KeyVaultManagementClient(
            self.auth_client.modern,
            self.subscription_id,
        )
        # vaults.list does not require api_version or filter parameters
        # pylint: disable=no-value-for-parameter
        return [v.name for v in mgmt.vaults.list() or [] if v.name]

    def get_vault_uri(self: Self, vault_name: str) -> str:
        """
        Return the URI for a vault name.

        Parameters
        ----------
        vault_name : str
            The Vault name.

        Returns
        -------
        str
            Vault URI.

        """
        mgmt: KeyVaultManagementClient = KeyVaultManagementClient(
            self.auth_client.modern,
            self.subscription_id,
        )
        try:
            vault = mgmt.vaults.get(self.resource_group, vault_name)
        except ResourceNotFoundError as cloud_err:
            err_msg: str = (
                "Check that you have specified the right value for VaultName"
                " in your configuration"
                f" Error returned from provider was {cloud_err}"
            )
            raise MsticpyKeyVaultConfigError(
                err_msg,
                title=f"Key Vault vault '{vault_name}' not found.",
            ) from cloud_err
        if not vault.properties.vault_uri:
            err_msg = (
                f"Vault '{vault_name}' does not have a URI."
                " Please check the vault configuration."
            )
            raise MsticpyKeyVaultConfigError(
                err_msg,
                title="missing Vault URI",
            )
        return vault.properties.vault_uri

    def create_vault(self: Self, vault_name: str) -> Vault:
        """
        Create new or update existing vault.

        Parameters
        ----------
        vault_name : str
            Name of the Vault

        Returns
        -------
        Vault
            The Vault object.

        """
        raise NotImplementedError(
            "create_vault is no longer supported in msticpy",
            "Please create the vault manually.",
        )


def _print_status(message: str, *, newline: bool = True) -> None:
    if is_ipython():
        line_break = "<br>" if newline else ""
        display(HTML(f"{message}{line_break}"))
    else:
        line_break = "\n" if newline else ""
        message += line_break
        LOGGER.info(message)
