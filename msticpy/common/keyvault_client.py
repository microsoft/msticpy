# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Keyvault client - adapted from Bluehound code."""
import base64
from datetime import datetime
import json
from typing import List, Any, Optional
import warnings

from adal import AuthenticationContext, AdalError
from azure.core.exceptions import ResourceNotFoundError
from azure.keyvault.secrets import SecretClient, KeyVaultSecret
from azure.identity import DeviceCodeCredential, InteractiveBrowserCredential
from azure.mgmt.keyvault import KeyVaultManagementClient
from azure.mgmt.keyvault.models import (
    AccessPolicyEntry,
    VaultProperties,
    Sku,
    KeyPermissions,
    SecretPermissions,
    CertificatePermissions,
    Permissions,
    VaultCreateOrUpdateParameters,
    Vault,
)
from IPython.display import display, HTML
import keyring
from keyring.errors import KeyringError
from msrest.authentication import BasicTokenAuthentication
from msrestazure.azure_exceptions import CloudError
import pandas.io.clipboard as pyperclip

from .exceptions import MsticpyKeyVaultConfigError, MsticpyKeyVaultMissingSecretError
from . import pkg_config as config
from .utility import export, is_ipython
from .._version import VERSION

__version__ = VERSION
__author__ = "Matt Richard, Ian Hellen"


# pylint: disable=too-many-lines
@export
class KeyVaultSettings:
    """
    KeyVaultSettings class - reads settings from msticpyconfig.

    Notes
    -----
    The KeyVault section in msticpyconfig.yaml can contain
    the following::

        KeyVault:
            TenantId: {tenantid-to-use-for-authentication}
            SubscriptionId: {subscriptionid-containing-vault}
            ResourceGroup: {resource-group-containing-vault}
            AzureRegion: {region-for-vault}
            VaultName: {vault-name}
            UseKeyring: True
            Authority: global

    `SubscriptionId`, `ResourceGroup` and `AzureRegion` are only
    used when creating new vaults.
    `UseKeyring` instructs the `SecretsClient` to cache Keyvault
    secrets locally using Python keyring.
    `Authority` is one of 'global', 'usgov', 'de', 'chi'
    Alternatively, you can specify `AuthorityURI` with the value
    pointing to the URI for logon requests.

    """

    AAD_AUTHORITIES = {
        "global": "https://login.microsoftonline.com/",
        "usgov": "https://login.microsoftonline.us",
        "de": "https://login.microsoftonline.de",
        "chi": "https://login.chinacloudapi.cn",
    }

    KV_URIS = {
        "global": "https://{vault}.vault.azure.net/",
        "usgov": "https://{vault}.vault.usgovcloudapi.net/",
        "de": None,
        "chi": None,
    }

    MGMT_URIS = {
        "global": "https://management.azure.com/",
        "usgov": "https://management.usgovcloudapi.net/",
        "de": None,
        "chi": None,
    }

    # Azure CLI Client ID
    CLIENT_ID = "04b07795-8ddb-461a-bbee-02f9e1bf7b46"  # xplat

    def __init__(self):
        """Initialize new instance of KeyVault Settings."""
        try:
            kv_config = config.get_config("KeyVault")
        except KeyError:
            raise MsticpyKeyVaultConfigError(
                "No KeyVault section found in msticpyconfig.yaml",
                title="missing Key Vault configuration",
            )
        norm_settings = {key.casefold(): val for key, val in kv_config.items()}
        self.__dict__.update(norm_settings)
        if "authority_uri" in self:
            rev_lookup = {uri.casefold(): code for code, uri in self.AAD_AUTHORITIES}
            self.__dict__["authority"] = rev_lookup.get(
                self["authorityuri"].casefold(), "global"
            ).casefold()

    def __getitem__(self, key: str):
        """Allow property get using dictionary key syntax."""
        if key.casefold() in self.__dict__:
            return self.__dict__[key.casefold()]
        raise KeyError

    def __setitem__(self, key: str, value: Any):
        """Allow property set using dictionary key syntax."""
        self.__dict__[key.casefold()] = value

    def __contains__(self, key: str):
        """Return true if key is a valid attribute."""
        return key.casefold() in self.__dict__

    def get(self, key: str, default: Any = None) -> Any:
        """Return dict value or default."""
        return self.__dict__.get(key.casefold(), default)

    @property
    def cloud(self) -> str:
        """Return the cloud for the settings."""
        return self.get("authority", "global").casefold()

    @property
    def authority_uri(self) -> str:
        """
        Return authority URI for cloud.

        Returns
        -------
        str
            Authority URI

        """
        if "authorityuri" in self:
            return self["authorityuri"]
        if self.cloud in self.AAD_AUTHORITIES:
            return self.AAD_AUTHORITIES[self.cloud]
        return self.AAD_AUTHORITIES["global"]

    @property
    def keyvault_uri(self) -> Optional[str]:
        """Return KeyVault URI template for current cloud."""
        kv_uri = self.KV_URIS.get(self.cloud)
        if not kv_uri:
            mssg = f"Could not find a valid KeyVault endpoint for {self.cloud}"
            warnings.warn(mssg)
        return kv_uri

    @property
    def mgmt_uri(self) -> Optional[str]:
        """Return Azure management URI template for current cloud."""
        mgmt_uri = self.MGMT_URIS.get(self.cloud)
        if not mgmt_uri:
            mssg = f"Could not find a valid KeyVault endpoint for {self.cloud}"
            warnings.warn(mssg)
        return mgmt_uri

    def get_tenant_authority_uri(
        self, authority_uri: str = None, tenant: str = None
    ) -> str:
        """
        Return authority URI for tenant.

        Parameters
        ----------
        authority_uri : str, optional
            The authority URI - otherwise try to retrieve
            from settings
        tenant : str, optional
            TenantID or name, by default None.
            If not passed as a parameter try to get tenant from
            KeyVault configuration in msticpyconfig.yaml

        Returns
        -------
        str
            Tenant Authority

        Raises
        ------
        KeyVaultConfigException
            If tenant is not defined.

        """
        auth = authority_uri or self.authority_uri.strip()
        if not tenant:
            tenant = self.get("tenantid")
        if not tenant:
            raise MsticpyKeyVaultConfigError(
                "Could not get TenantId from function parameters or configuration.",
                "Please add this to the KeyVault section of msticpyconfig.yaml",
                title="missing tenant ID value.",
            )
        if auth.endswith("/"):
            return auth + tenant.strip()
        return auth + "/" + tenant.strip()

    def get_tenant_authority_host(
        self, authority_uri: str = None, tenant: str = None
    ) -> str:
        """
        Return tenant authority URI with no leading scheme.

        Parameters
        ----------
        authority_uri : str, optional
            The authority URI - otherwise try to retrieve
            from settings
        tenant : str, optional
            TenantID or name, by default None.
            If not passed as a parameter try to get tenant from
            KeyVault configuration in msticpyconfig.yaml

        Returns
        -------
        str
            Tenant Authority

        Raises
        ------
        KeyVaultConfigException
            If tenant is not defined.

        """
        if not tenant:
            tenant = self.get("tenantid")
        if not tenant:
            raise MsticpyKeyVaultConfigError(
                "Could not get TenantId from function parameters or configuration.",
                "Please add this to the KeyVault section of msticpyconfig.yaml",
                title="missing tenant ID value.",
            )
        return (
            self.get_tenant_authority_uri(authority_uri, tenant)
            .lower()
            .replace("https://", "")
        )


# pylint: disable=too-many-instance-attributes
@export
class AuthClient:
    """Authentication class base."""

    def __init__(
        self,
        tenant_id: str,
        client_id: str,
        client_uri: str,
        name: str = None,
        **kwargs,
    ):
        """
        Initialize base authentication client for credential caching.

        Parameters
        ----------
        tenant_id : str
            Tenant ID of Azure User
        client_id : str
            Client ID of application client
        client_uri : str
            [description]
        name : str, optional
            Name of the secret store, by default None
        authority : str, optional
            The AAD authority - one of 'global', 'usgov', 'de' or 'chi'
        authority_uri : str, optional
            The AAD authority URI - overrides `authority`
        debug : bool, optional
            Output debug information if True, by default False

        Notes
        -----
        The parameter values can also be obtained from the
        KeyVault section of msticpyconfig.yaml.

        """
        self.name = name
        self.debug = kwargs.pop("debug", False)
        self.settings: KeyVaultSettings = kwargs.pop(
            "settings", None
        ) or KeyVaultSettings()
        self.tenant_id = tenant_id or self.settings.get("tenantid")
        if not self.tenant_id:
            raise MsticpyKeyVaultConfigError(
                "Could not get TenantId from function parameters or configuration.",
                "Please add this to the KeyVault section of msticpyconfig.yaml",
                title="missing tenant ID value.",
            )
        self.authority = kwargs.pop(
            "authority", self.settings.get_tenant_authority_host(tenant_id)
        )
        self.client_id = client_id or self.settings.CLIENT_ID
        self.client_uri = client_uri
        self.authority_uri = self.settings.get_tenant_authority_uri(
            authority_uri=kwargs.get("authority_uri"), tenant=self.tenant_id
        )

        if self.debug:
            print("AuthClient for %s - %s" % (client_id, client_uri))
        self._get_creds()
        if self._expired_creds:
            if self.debug:
                print("expired creds")
            try:
                self._refresh_creds()
                return
            except AdalError:
                if self.debug:
                    print("Token was no longer valid, forcing a new one.")
            self._get_token()

    def _get_token(self):
        context = AuthenticationContext(self.authority_uri)
        code = context.acquire_user_code(self.client_uri, self.client_id)
        _prompt_for_code(code)
        self.config_data = context.acquire_token_with_device_code(
            self.client_uri, code, self.client_id
        )
        self._cache_creds()

    def _get_creds(self):
        self._get_token()

    def _cache_creds(self):
        pass

    def _is_valid_config_data(self):
        keys = ["accessToken", "refreshToken", "expiresOn"]
        if all([key in self.config_data for key in keys]):
            if all([self.config_data.get(key) for key in keys]):
                if all([len(self.config_data.get(key)) > 0 for key in keys]):
                    return True
        return False

    @property
    def auth_id(self) -> str:
        """Return name or ID of client."""
        return self.name if self.name is not None else self.client_id

    @property
    def user_oid(self) -> str:
        """
        Return the user Object ID.

        Returns
        -------
        str
            User OID.

        """
        data = self._get_parsed_token_data()
        return data.get("oid")

    def _get_parsed_token_data(self) -> Any:
        tok_data = self.token
        tok_data = tok_data.split(".")[1]
        tok_data += "=" * ((4 - len(tok_data) % 4) % 4)
        return json.loads(base64.b64decode(tok_data))

    def _refresh_creds(self):
        context = AuthenticationContext(self.authority_uri)
        self.config_data = context.acquire_token_with_refresh_token(
            self.config_data["refreshToken"], self.client_id, self.client_uri
        )
        if self.debug:
            print(f"got new token expiring {self.config_data['expiresOn']}")
            self._cache_creds()

    @property
    def _expired_creds(self) -> bool:
        return self._expires_on < datetime.now()

    @property
    def _expires_on(self) -> datetime:
        """Return token expiry date as string."""
        return datetime.strptime(self.config_data["expiresOn"], "%Y-%m-%d %H:%M:%S.%f")

    @property
    def token(self) -> str:
        """
        Return the access token.

        Returns
        -------
        str
            Access Token

        """
        if self._expired_creds:
            try:
                self._refresh_creds()
            except AdalError:
                self._get_token()
        return self.config_data["accessToken"]

    def _adal_callback(self, server: str, resource: str, scope: str, scheme: str):
        """
        ADAL Callback for authentication.

        Parameters
        ----------
        server : str
            Not used
        resource : str
            Not used
        scope : str
            Not used
        scheme : str
            Not used

        Returns
        -------
        Tuple(str, str)
            Bearer, Token

        Notes
        -----
        None of the parameters are used in this function. However,
        they are required because of the expected callback signature.

        """
        del (server, resource, scope, scheme)
        return "Bearer", self.token


# pylint: enable=too-many-instance-attributes


@export
class KeyringAuthClient(AuthClient):
    """
    Key Authentication Client.

    Handles management of authentication and refresh tokens
    via keyring
    """

    # pylint: disable=too-many-arguments
    def __init__(
        self,
        tenant_id: str,
        client_id: str,
        client_url: str,
        name: str = None,
        debug: bool = False,
    ):
        """
        Initialize KeyringAuthClient.

        Parameters
        ----------
        tenant_id : str
            Tenant ID of Azure User
        client_id : str
            Client ID of application client
        client_url : str
            [description]
        name : str, optional
            Name of the secret store, by default None
        debug : bool, optional
            Output debug information if True, by default False

        """
        self.name = name
        self.keyring = self.auth_id
        super().__init__(tenant_id, client_id, client_url, name=name, debug=debug)

    # pylint: enable=too-many-arguments

    def _get_creds(self):
        if self.debug:
            print("Fetching creds from keyring")
        try:
            access_token = (
                keyring.get_password(self.keyring, "adal_context_1")
                + keyring.get_password(self.keyring, "adal_context_2")
                + keyring.get_password(self.keyring, "adal_context_3")
                + keyring.get_password(self.keyring, "adal_context_4")
            )
            refresh_token = (
                keyring.get_password(self.keyring, "adal_context_5")
                + keyring.get_password(self.keyring, "adal_context_6")
                + keyring.get_password(self.keyring, "adal_context_7")
            )
            expires_on = keyring.get_password(self.keyring, "adal_context_8")
            self.config_data = {
                "accessToken": access_token,
                "refreshToken": refresh_token,
                "expiresOn": expires_on,
            }
        except (TypeError, KeyringError):
            if self.debug:
                print("No valid credentials in keyring %s" % self.keyring)
            self._get_token()
        if not self._is_valid_config_data():
            if self.debug:
                print("No valid authtoken config found in keyring")
            self._get_token()

    def _cache_creds(self):
        if self.debug:
            print("Saving config data to keyring %s" % self.keyring)
        keyring.set_password(
            self.keyring, "adal_context_1", self.config_data["accessToken"][:400]
        )
        keyring.set_password(
            self.keyring, "adal_context_2", self.config_data["accessToken"][400:800]
        )
        keyring.set_password(
            self.keyring, "adal_context_3", self.config_data["accessToken"][800:1200]
        )
        keyring.set_password(
            self.keyring, "adal_context_4", self.config_data["accessToken"][1200:]
        )
        keyring.set_password(
            self.keyring, "adal_context_5", self.config_data["refreshToken"][:400]
        )
        keyring.set_password(
            self.keyring, "adal_context_6", self.config_data["refreshToken"][400:800]
        )
        keyring.set_password(
            self.keyring, "adal_context_7", self.config_data["refreshToken"][800:]
        )
        keyring.set_password(
            self.keyring, "adal_context_8", self.config_data["expiresOn"]
        )


# class KeyVaultAuthClient(AuthClient):
#     """
#     Keyvault Auth client.

#     Handles management of authentication tokens in keyvault.
#     """

#     def __init__(
#         self,
#         tenant_id: str,
#         client_id: str,
#         client_url: str,
#         secret_name: str,
#         name: str = None,
#         debug: bool = False,
#     ):
#         """
#         Initialize KeyvaultAuthClient.

#         Parameters
#         ----------
#         tenant_id : str
#             Tenant ID of Azure User
#         client_id : str
#             Client ID of application client
#         client_url : str
#             [description]
#         name : str, optional
#             Name of the secret store, by default None
#         debug : bool, optional
#             Output debug information if True, by default False

#         """
#         self.secret_name = secret_name
#         self._get_creds = self._get_keyvault_creds
#         self._cache_creds = self._cache_creds_keyvault
#         self.keyvault_client = BHKeyVaultClient(
#             tenant_id=tenant_id, vault_uri=client_url
#         )
#         self.config_data: Any = None
#         super().__init__(tenant_id, client_id, client_url, name=name, debug=debug)

#     def _get_keyvault_creds(self):
#         if self.debug:
#             print("getting tokens from keyvault")
#         try:
#             self.config_data = json.loads(
#                 self.keyvault_client.get_secret(self.secret_name)
#             )
#         except KeyVaultMissingSecretException:
#             if self.debug:
#                 print("missing secret from keyvault, fetching manually")
#             self._get_token()
#         except KeyVaultErrorException as err:
#             if self.debug:
#                 print("bad creds in keyvault, you gotta getem")
#                 print("here is what went wrong: %s" % str(err))
#             self._get_token()

#     def _cache_creds_keyvault(self):
#         self.keyvault_client.set_secret(self.secret_name, json.dumps(self.config_data))


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
        authn_type : str, optional
            Authentication mode, by default 'interactive'
            Supported options are:
            - 'device' for device code authentication
            - 'interactive' for interactive browser authentication
        authority : str, optional
            The AAD authority - one of 'global', 'usgov', 'de' or 'chi'
        authority_uri : str, optional
            The AAD authority URI - overrides `authority`
        settings : KeyVaultSettings
            An instance of KeyVaultSettings containing KV parameters.
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
            )
        self.authn_type = kwargs.pop(
            "authn_type", self.settings.get("authntype", "interactive")
        )

        # for authority and authority_uri, any parameters take priority
        # and fall back on settings if not specified.
        if "authority" in kwargs:
            self.settings["authority"] = kwargs.pop("authority")
        self.authority_uri = self.settings.get_tenant_authority_host(
            authority_uri=kwargs.get("authority_uri"), tenant=self.tenant_id
        )

        if not vault_uri and not vault_name:
            if "vaultname" in self.settings:
                vault_name = self.settings["vaultname"]
            else:
                raise MsticpyKeyVaultConfigError(
                    "Check that you have specified the right value for VaultName"
                    + " in your configuration",
                    title="Key Vault vault name not found.",
                )
        if vault_uri:
            self.vault_uri = vault_uri
        else:
            vault_uri = self.settings.keyvault_uri
            if vault_uri:
                self.vault_uri = vault_uri.format(vault=vault_name)
            else:
                cloud = self.settings.cloud
                raise MsticpyKeyVaultConfigError(
                    f"Could not determine keyvault URI for national cloud {cloud}.",
                    "Please verify that you have the correct national cloud"
                    + "specified in the KeyVault section of msticpyconfig.yaml",
                    title="no Key Vault URI for national cloud",
                )
        if self.debug:
            print(f"Using Vault URI {self.vault_uri}")

        # self.auth_client = KeyringAuthClient(
        #     tenant_id,
        #     self._CLIENT_ID,
        #     self._CLIENT_URI,
        #     self._KEYRING_NAME,
        #     debug=self.debug,
        # )
        self.kv_client = self._get_secret_client()

    def _get_secret_client(self):
        if self.authn_type == "device":
            authority = self.authority_uri.replace("https://", "")
            credentials = DeviceCodeCredential(
                client_id=self.settings.CLIENT_ID,
                authority=authority,
                prompt_callback=_device_code_callback,
            )
        else:
            credentials = InteractiveBrowserCredential()

        # Create a secret client
        secret_client = SecretClient(self.vault_uri, credentials)
        return secret_client

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
        try:
            secret_bundle = self.kv_client.get_secret(name=secret_name)
        except ResourceNotFoundError as err:
            if self.debug:
                print(
                    "Secret: '%s' missing from vault: %s"
                    % (secret_name, self.vault_uri)
                )
            raise MsticpyKeyVaultMissingSecretError(
                f"Secret name {secret_name} could not be found in {self.vault_uri}",
                f"Provider returned: {err}",
                title=f"secret {secret_name} not found.",
            )
        if secret_bundle.value is None or not secret_bundle.value:
            if self.debug:
                print(
                    "Secret: '%s' was empty in vault %s" % (secret_name, self.vault_uri)
                )
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
            print("Storing %s in %s" % (secret_name, self.vault_uri))
        secret_bundle = self.kv_client.set_secret(name=secret_name, value=value)

        return secret_bundle


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
        self.auth_client = AuthClient(
            tenant_id=self.tenant_id,
            client_id=self.settings.CLIENT_ID,
            client_uri=self._client_uri,
            name="mgmt",
        )
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
        cred = BasicTokenAuthentication({"access_token": self.auth_client.token})
        mgmt = KeyVaultManagementClient(cred, self.subscription_id)
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
        cred = BasicTokenAuthentication({"access_token": self.auth_client.token})
        mgmt = KeyVaultManagementClient(cred, self.subscription_id)
        try:
            vault = mgmt.vaults.get(self.resource_group, vault_name)
        except (CloudError, ResourceNotFoundError) as cloud_err:
            raise MsticpyKeyVaultConfigError(
                "Check that you have specified the right value for VaultName"
                + " in your configuration",
                f"Error returned from provider was {cloud_err}",
                title="Key Vault vault '{vault_name}' not found.",
            )
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
        cred = BasicTokenAuthentication({"access_token": self.auth_client.token})
        if not self.resource_group:
            raise MsticpyKeyVaultConfigError(
                "Could not get Azure resource group in which to create the vault.",
                "Please add ResourceGroup to the KeyVault section of msticpyconfig.yaml",
                title="missing ResourceGroup value.",
            )
        mgmt = KeyVaultManagementClient(cred, self.subscription_id)
        vault = mgmt.vaults.create_or_update(
            self.resource_group, vault_name, parameters
        ).result()
        return vault

    def _get_params(self):
        """Build the vault parameters block."""
        oid = self.auth_client.user_oid
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
            tenant_id=self.tenant_id, sku=Sku(name="standard"), access_policies=[policy]
        )
        parameters = VaultCreateOrUpdateParameters(
            location=self.azure_region, properties=properties
        )
        parameters.properties.enabled_for_deployment = True
        parameters.properties.enabled_for_disk_encryption = True
        parameters.properties.enabled_for_template_deployment = True
        return parameters


# pylint: enable=too-many-instance-attributes


def _device_code_callback(verification_uri, user_code, expires_on):
    device_code = {
        "verification_uri": verification_uri,
        "user_code": user_code,
        "expires_on": expires_on,
    }
    _prompt_for_code(device_code)


def _prompt_for_code(device_code):
    # copy code to clipboard
    pyperclip.copy(device_code["user_code"])
    verif_uri = device_code.get("verification_url", device_code.get("verification_uri"))
    title = "Authentication needed for KeyVault access."
    logon_mssg = "User code {} copied to clipboard.".format(device_code["user_code"])

    if is_ipython():
        display(HTML(f"<h3>{title}</h3>"))
        logon_mssg += "<br><a href='{}' target='_blank'>".format(verif_uri)
        logon_mssg += "Click to open logon page</a><br>"
    else:
        print(title)
        print("-" * len(title))
        logon_mssg += "\nOpen the URL '{}' in a browser\n".format(verif_uri)
    logon_mssg += "then press Ctrl/Cmd-V to paste your code to authenticate. "
    if "expires_in" in device_code:
        logon_mssg += "(Code valid for {} min)".format(
            int(int(device_code["expires_in"]) / 60)
        )
    elif "expires_on" in device_code:
        logon_mssg += "(Code valid until {})".format(device_code["expires_on"])
    if is_ipython():
        display(HTML(logon_mssg))
    else:
        print(logon_mssg)
