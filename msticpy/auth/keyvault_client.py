# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Keyvault client - adapted from Bluehound code."""
import base64
import json
from typing import Any, List

from azure.core.exceptions import ClientAuthenticationError, ResourceNotFoundError
from azure.keyvault.secrets import KeyVaultSecret, SecretClient
from azure.mgmt.keyvault import KeyVaultManagementClient
from azure.mgmt.keyvault.models import (
    AccessPolicyEntry,
    CertificatePermissions,
    KeyPermissions,
    Permissions,
    SecretPermissions,
    Sku,
    Vault,
    VaultCreateOrUpdateParameters,
    VaultProperties,
)
from IPython.display import HTML, display
from msrestazure.azure_exceptions import CloudError

from .._version import VERSION
from ..common.exceptions import (
    MsticpyKeyVaultConfigError,
    MsticpyKeyVaultMissingSecretError,
    MsticpyUserConfigError,
)
from ..common.utility import export, is_ipython
from .azure_auth_core import az_connect_core
from .keyvault_settings import KeyVaultSettings

__version__ = VERSION
__author__ = "Matt Richard, Ian Hellen"

_KV_CLIENT_AUTH_ERROR = [
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

    _KEYRING_NAME = "keyvault"

    def __init__(
        self,
        tenant_id: str = None,
        vault_uri: str = None,
        vault_name: str = None,
        settings: KeyVaultSettings = None,
        **kwargs,
    ):
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

        Other Parameters
        ----------------
        auth_methods : List[str]
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

        Raises
        ------
        KeyVaultMissingVaultException
            No Vault name or URI supplied.

        Notes
        -----
        The parameter values can also be obtained from the
        KeyVault section of msticpyconfig.yaml.


        """
        self.debug = kwargs.pop("debug", False)
        self.settings: KeyVaultSettings = settings or KeyVaultSettings()

        self.tenant_id = tenant_id or self.settings.get("tenantid")
        if not self.tenant_id:
            raise MsticpyKeyVaultConfigError(
                "Could not get TenantId from function parameters or configuration.",
                "Please add this to the KeyVault section of msticpyconfig.yaml",
                title="missing tenant ID value.",
                azcli_uri="https://docs.microsoft.com/cli/azure/authenticate-azure-cli",
            )
        self.authn_type = kwargs.pop(
            "authn_type", self.settings.get("authntype", "interactive")
        )
        self.auth_methods = kwargs.pop(
            "auth_methods", self.settings.get("auth_methods", ["interactive"])
        )

        # for authority and authority_uri, any parameters take priority
        # and fall back on settings if not specified.
        if "authority" in kwargs:
            self.settings["authority"] = kwargs.pop("authority")
        self.authority_uri = self.settings.get_tenant_authority_host(
            authority_uri=kwargs.get("authority_uri"), tenant=self.tenant_id
        )

        self._vault_name, self.vault_uri = self._get_vault_name_and_uri(
            vault_name, vault_uri
        )
        self.kv_client = self._try_credential_types(**kwargs)

    def _try_credential_types(self, **kwargs):
        """Try to access Key Vault to establish usable authentication method."""
        credential = kwargs.pop("credential", None)
        if credential:
            kv_client = SecretClient(self.vault_uri, credential=credential)
            try:
                self._get_working_kv_client(kv_client)
            except ClientAuthenticationError as client_err:
                _print_status(
                    f"Could not obtain access token using {credential.__class__.__name__}."
                )
                self._raise_auth_failed_error(client_err)

        for idx, auth_method in enumerate(self.auth_methods):
            _print_status(
                f"Attempting connection to Key Vault using {auth_method} credentials...",
                newline=False,
            )
            credential = az_connect_core(auth_methods=[auth_method], **kwargs)
            kv_client = SecretClient(self.vault_uri, credential.modern)
            try:
                return self._get_working_kv_client(kv_client)
            except ClientAuthenticationError as client_err:
                _print_status(f"Could not obtain access token using {auth_method}.")
                if idx + 1 < len(self.auth_methods):
                    continue
                self._raise_auth_failed_error(client_err)
        return None

    def _get_working_kv_client(self, kv_client):
        """Try to list secrets - will throw ClientAuthentication error on failure."""
        # need to list to force iterator to run
        list(kv_client.list_properties_of_secrets())
        _print_status("done")
        return kv_client

    def _raise_auth_failed_error(self, client_err):
        raise MsticpyUserConfigError(
            "No configured authentication methods found with credentials "
            f"with access the Key Vault '{self._vault_name}'",
            *_KV_CLIENT_AUTH_ERROR,
            title="Key Vault authentication configuration failed.",
        ) from client_err

    def _get_vault_name_and_uri(self, vault_name, vault_uri):
        """Validate and return vault name and URI."""
        if not vault_uri and not vault_name:
            if "vaultname" in self.settings:
                vault_name = self.settings["vaultname"]
            else:
                raise MsticpyKeyVaultConfigError(
                    "Check that you have specified the right value for VaultName"
                    + " in your configuration",
                    title="Key Vault vault name not found.",
                )
        if not vault_uri:
            vault_uri = self.settings.keyvault_uri
            if vault_uri:
                vault_uri = vault_uri.format(vault=vault_name)
            else:
                cloud = self.settings.cloud
                raise MsticpyKeyVaultConfigError(
                    f"Could not determine keyvault URI for national cloud {cloud}.",
                    "Please verify that you have the correct national cloud"
                    + "specified in the KeyVault section of msticpyconfig.yaml",
                    title="no Key Vault URI for national cloud",
                )
        if self.debug:
            print(f"Using Vault URI {vault_uri}")
        return vault_name, vault_uri

    @property
    def vault_name(self) -> str:
        """Return the Key Vault name."""
        return (
            self._vault_name
            or self.vault_uri.replace("https://", "").split(".", maxsplit=1)[0]
        )

    @property
    def secrets(self):
        """Return the list of secret names from the vault."""
        return [x.id for x in self.kv_client.list_properties_of_secrets()]

    def get_secret(self, secret_name: str) -> Any:
        """
        Retrieve a secret from the Vault.

        Parameters
        ----------
        secret_name : str
            Name of the secret

        Returns
        -------
        Any
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
            secret_bundle = self.kv_client.get_secret(name=secret_name)
        except ResourceNotFoundError as err:
            if self.debug:
                print(f"Secret: '{secret_name}' missing from vault: {self.vault_uri}")
            raise MsticpyKeyVaultMissingSecretError(
                f"Secret name {secret_name} could not be found in {self.vault_uri}",
                f"Provider returned: {err}",
                title=f"secret {secret_name} not found.",
            ) from err
        if secret_bundle.value is None or not secret_bundle.value:
            if self.debug:
                print(f"Secret: '{secret_name}' was empty in vault {self.vault_uri}")
            raise MsticpyKeyVaultMissingSecretError(
                f"Secret name {secret_name} in {self.vault_uri}",
                "has blank or null value.",
                title=f"secret {secret_name} empty.",
            )
        return secret_bundle.value

    def set_secret(self, secret_name: str, value: Any) -> KeyVaultSecret:
        """
        Set a secret in the Vault.

        Parameters
        ----------
        secret_name : str
            Name of the secret
        value: Any
            Secret value

        Returns
        -------
        KeyVaultSecret
            The secrets bundle for the secret

        """
        if self.debug:
            print(f"Storing {secret_name} in {self.vault_uri}")
        return self.kv_client.set_secret(name=secret_name, value=value)


# pylint: disable=too-many-instance-attributes
@export
class BHKeyVaultMgmtClient:
    """Core KeyVault Management client."""

    # pylint: disable=too-many-arguments
    def __init__(
        self,
        tenant_id: str = None,
        subscription_id: str = None,
        resource_group: str = None,
        azure_region: str = None,
        settings: KeyVaultSettings = None,
        **kwargs,
    ):
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

        Notes
        -----
        The parameter values can also be obtained from the
        KeyVault section of msticpyconfig.yaml.

        """
        self.debug = kwargs.pop("debug", False)
        self.settings: KeyVaultSettings = settings or KeyVaultSettings()
        self.tenant_id = tenant_id or self.settings.get("tenantid")
        if not self.tenant_id:
            raise MsticpyKeyVaultConfigError(
                "Could not get TenantId from function parameters or configuration.",
                "Please add this to the KeyVault section of msticpyconfig.yaml",
                title="missing tenant ID value.",
            )
        self.subscription_id = subscription_id or self.settings.get("subscriptionid")
        if not self.subscription_id:
            raise MsticpyKeyVaultConfigError(
                "Could not get SubscriptionId from function parameters or configuration.",
                "Please add this to the KeyVault section of msticpyconfig.yaml",
                title="missing SubscriptionId value.",
            )
        self._client_uri = kwargs.pop("mgmt_uri", None) or self.settings.mgmt_uri
        if not self._client_uri:
            cloud = self.settings.cloud
            raise MsticpyKeyVaultConfigError(
                f"Could not obtain an azure management URI for national cloud {cloud}.",
                "Please verify that you have the correct national cloud"
                + "specified in the KeyVault section of msticpyconfig.yaml",
                title="no Azure Management URI for national cloud",
            )

        self.auth_client = az_connect_core()
        self.resource_group = resource_group or self.settings.get("resourcegroup")
        self.azure_region = azure_region or self.settings.get("azureregion")

    # pylint: enable=too-many-arguments

    def list_vaults(self) -> List[str]:
        """
        Return a list of vaults for the subscription.

        Returns
        -------
        List[str]
            Vault names

        """
        mgmt = KeyVaultManagementClient(self.auth_client.legacy, self.subscription_id)
        return [v.name for v in mgmt.vaults.list()]

    def get_vault_uri(self, vault_name: str) -> str:
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
        mgmt = KeyVaultManagementClient(self.auth_client.legacy, self.subscription_id)
        try:
            vault = mgmt.vaults.get(self.resource_group, vault_name)
        except (CloudError, ResourceNotFoundError) as cloud_err:
            raise MsticpyKeyVaultConfigError(
                "Check that you have specified the right value for VaultName"
                + " in your configuration",
                f"Error returned from provider was {cloud_err}",
                title=f"Key Vault vault '{vault_name}' not found.",
            ) from cloud_err
        return vault.properties.vault_uri

    def create_vault(self, vault_name: str) -> Vault:
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
        if not self.azure_region:
            raise MsticpyKeyVaultConfigError(
                "Could not get Azure region in which to create the vault.",
                "Please add AzureRegion to the KeyVault section of msticpyconfig.yaml",
                title="missing AzureRegion value.",
            )
        parameters = self._get_params()
        if not self.resource_group:
            raise MsticpyKeyVaultConfigError(
                "Could not get Azure resource group in which to create the vault.",
                "Please add ResourceGroup to the KeyVault section of msticpyconfig.yaml",
                title="missing ResourceGroup value.",
            )
        mgmt = KeyVaultManagementClient(self.auth_client.legacy, self.subscription_id)
        return mgmt.vaults.create_or_update(
            self.resource_group, vault_name, parameters
        ).result()

    def _get_params(self):
        """Build the vault parameters block."""
        oid = _user_oid(self.auth_client.legacy.token)
        sec_perms_all = [perm.value for perm in SecretPermissions]
        key_perms_all = [perm.value for perm in KeyPermissions]
        cert_perms_all = [perm.value for perm in CertificatePermissions]
        permissions = Permissions()
        permissions.keys = key_perms_all
        permissions.secrets = sec_perms_all
        permissions.certificates = cert_perms_all

        policy = AccessPolicyEntry(
            tenant_id=self.tenant_id, object_id=oid, permissions=permissions
        )

        properties = VaultProperties(
            tenant_id=self.tenant_id,
            sku=Sku(name="standard", family="A"),
            access_policies=[policy],
        )
        parameters = VaultCreateOrUpdateParameters(
            location=self.azure_region, properties=properties
        )
        parameters.properties.enabled_for_deployment = True
        parameters.properties.enabled_for_disk_encryption = True
        parameters.properties.enabled_for_template_deployment = True
        return parameters


# pylint: enable=too-many-instance-attributes


def _user_oid(token) -> str:
    """
    Return the user Object ID.

    Returns
    -------
    str
        User OID.

    """
    data = _get_parsed_token_data(token)
    return data.get("oid")


def _get_parsed_token_data(token) -> Any:
    tok_data = token
    tok_data = tok_data.split(".")[1]
    tok_data += "=" * ((4 - len(tok_data) % 4) % 4)
    return json.loads(base64.b64decode(tok_data))


def _print_status(message, newline=True):
    if is_ipython():
        line_break = "<br>" if newline else ""
        display(HTML(f"{message}{line_break}"))
    else:
        line_break = "\n" if newline else ""
        print(message, end=line_break)
