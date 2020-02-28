# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Keyvault client - adapted from Bluehound code."""
import json
from typing import List, Any

from adal import AuthenticationContext
from azure.keyvault import KeyVaultClient, KeyVaultAuthentication, KeyVaultId, Vault
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
)
import base64
import keyring
import datetime

from msrest.authentication import BasicTokenAuthentication
import requests

from ..nbtools.utility import export
from ..nbtools import pkg_config as config
from .._version import VERSION

__version__ = VERSION
__author__ = "Matt Richard"


class KeyVaultMissingSecretException(Exception):
    """Missing secret exception."""

    pass


class KeyVaultMissingVaultException(Exception):
    """Missing vault exception."""

    pass


class AuthClient(object):
    """Authentication class base."""

    _AUTHORITY_URL = "https://login.microsoftonline.com/{tenant_id}"

    def __init__(
        self,
        tenant_id: str,
        client_id: str,
        client_url: str,
        name: str = None,
        debug: bool = False,
    ):
        self.name = name
        self.client_id = client_id
        self.client_url = client_url
        self.authority_url = self._AUTHORITY_URL.format(tenant_id=tenant_id)
        self.debug = debug
        if self.debug:
            print("AuthClient for %s - %s" % (client_id, client_url))
        self._get_creds()
        if self._expired_creds():
            if self.debug:
                print("expired creds")
            try:
                self._refresh_creds()
                return
            except:
                if self.debug:
                    print("Token was no longer valid, forcing a new one.")
            self._get_token()

    def _get_token(self):
        context = AuthenticationContext(self.authority_url)
        code = context.acquire_user_code(self.client_url, self.client_id)
        print(code["message"])
        self.config_data = context.acquire_token_with_device_code(
            self.client_url, code, self.client_id
        )
        self._cache_creds()

    def _get_creds(self):
        self._get_token()

    def _cache_creds(self):
        pass

    def _is_valid_config_data(self):
        keys = ["accessToken", "refreshToken", "expiresOn"]
        if all([key in self.config_data for key in keys]) == True:
            if all([self.config_data.get(key) for key in keys]):
                if all([len(self.config_data.get(key)) > 0 for key in keys]):
                    return True
        return False

    def __repr__(self):
        return self.name if self.name is not None else self.client_id

    def get_parsed_token_data(self) -> Any:
        tok_data = self.token
        tok_data = tok_data.split(".")[1]
        tok_data += "=" * ((4 - len(tok_data) % 4) % 4)
        return json.loads(base64.b64decode(tok_data))

    def _refresh_creds(self):
        context = AuthenticationContext(self.authority_url)
        self.config_data = context.acquire_token_with_refresh_token(
            self.config_data["refreshToken"], self.client_id, self.client_url
        )
        if self.debug:
            print(f"got new token expiring {self.config_data['expiresOn']}")
            self._cache_creds()

    def _expired_creds(self):
        return (
            datetime.datetime.strptime(
                self.config_data["expiresOn"], "%Y-%m-%d %H:%M:%S.%f"
            )
            < datetime.datetime.now()
        )

    @property
    def expires_on(self) -> str:
        return self.config_data["expiresOn"]

    @property
    def token(self) -> str:
        if self._expired_creds():
            try:
                self._refresh_creds()
            except:
                self._get_token()
        return self.config_data["accessToken"]

    def adal_callback(self, server, resource, scope, scheme):
        del (server, resource, scope, scheme)
        return "Bearer", self.token


class KeyringAuthClient(AuthClient):
    """
    Key Authentication Client.

    Handles management of authentication and refresh tokens
    via keyring
    """

    def __init__(
        self, client_id: str, client_url: str, name: str = None, debug: bool = False
    ):
        self.name = name
        self.keyring = repr(self)
        super().__init__(client_id, client_url, name=name, debug=debug)

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
        except:
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


class KeyvaultAuthClient(AuthClient):
    """
    Keyvault Auth client.

    Handles management of authentication tokens in keyvault.
    """

    def __init__(
        self,
        client_id: str,
        client_url: str,
        secret_name: str,
        name: str = None,
        debug: bool = False,
    ):
        self.secret_name = secret_name
        self._get_creds = self._get_keyvault_creds
        self._cache_creds = self._cache_creds_keyvault
        self.keyvault_client = BHKeyVaultClient(vault_uri=client_url)
        super().__init__(client_id, client_url, name=name, debug=debug)

    def _get_keyvault_creds(self):
        if self.debug:
            print("getting tokens from keyvault")
        try:
            self.config_data = json.loads(
                self.keyvault_client.get_secret(self.secret_name)
            )
        except KeyVaultMissingSecretException:
            if self.debug:
                print("missing secret from keyvault, fetching manually")
            self._get_token()
        except Exception as e:
            if self.debug:
                print("bad creds in keyvault, you gotta getem")
                print("here is what went wrong: %s" % str(e))
            self._get_token()

    def _cache_creds_keyvault(self):
        self.keyvault_client.put_secret(self.secret_name, json.dumps(self.config_data))


class BHKeyVaultClient(object):
    """Core KeyVault client."""

    # client id is the app id used in the oauth grant
    # this client id is the xplat command line interface available
    # on all tenants and subscriptions
    _CLIENT_ID = "04b07795-8ddb-461a-bbee-02f9e1bf7b46"
    _CLIENT_URI = "https://vault.azure.net"
    _VAULT_URI_TMPLT = "https://{vault}.vault.azure.net/"
    _KEYRING_NAME = "keyvault"

    def __init__(
        self, vault_uri: str = None, vault_name: str = None, debug: bool = False
    ):
        self.debug = debug

        if not vault_uri and not vault_name:
            raise KeyVaultMissingVaultException("No vault name or URI was supplied.")
        if vault_uri:
            self.vault_uri = vault_uri
        else:
            self.vault_uri = self._VAULT_URI_TMPLT.format(vault=vault_name)
        if debug:
            print("Using Vault URI {self.vault_uri}")
        self.auth_client = KeyringAuthClient(
            self._CLIENT_ID, self._CLIENT_URI, self._KEYRING_NAME, debug=self.debug
        )
        kv_auth = KeyVaultAuthentication(
            authorization_callback=self.auth_client.adal_callback
        )
        self.kv_client = KeyVaultClient(kv_auth)
        self.secrets: List[str] = []
        self._get_secrets()

    def _get_secrets(self):
        try:
            self.secrets = [x.id for x in self.kv_client.get_secrets(self.vault_uri)]
        except:
            if self.debug:
                print(
                    "You appear to be missing a proper Vault config: %s"
                    % self.vault_uri
                )
            raise KeyVaultMissingVaultException(self.vault_uri)

    def get_secret(self, secret_name: str) -> Any:
        try:
            secret_bundle = self.kv_client.get_secret(
                self.vault_uri, secret_name, secret_version=KeyVaultId.version_none
            )
        except:
            if self.debug:
                print(
                    "Secret: '%s' missing from vault: %s"
                    % (secret_name, self.vault_uri)
                )
            raise KeyVaultMissingSecretException(secret_name, self.vault_uri)
        if secret_bundle.value is None or not secret_bundle.value:
            if self.debug:
                print(
                    "Secret: '%s' was empty in vault %s" % (secret_name, self.vault_uri)
                )
            raise KeyVaultMissingSecretException(secret_name, self.vault_uri)
        return secret_bundle.value

    def set_secret(self, secret_name: str, value: Any):
        if self.debug:
            print("Storing %s in %s" % (secret_name, self.vault_uri))
        secret_bundle = self.kv_client.set_secret(self.vault_uri, secret_name, value)

        return secret_bundle


class BHKeyVaultMgmtClient(object):
    """Core KeyVault Management client."""

    CLIENT_URI = "https://management.azure.com"
    # client id is the app id used in the oauth grant
    # this client id is the xplat command line interface available
    # on all tenants and subscriptions
    CLIENT_ID = "04b07795-8ddb-461a-bbee-02f9e1bf7b46"  # xplat

    def __init__(
        self,
        tenant_id: str,
        subscription_id: str,
        resource_group: str,
        azure_region: str,
    ):
        self.tenant_id = tenant_id
        self.auth_client = AuthClient(self.CLIENT_ID, self.CLIENT_URI, "mgmt")
        self.subscription_id = subscription_id
        self.resource_group = resource_group
        self.azure_region = azure_region

    def _get_user_oid(self):
        data = self.auth_client.get_parsed_token_data()
        return data.get("oid")

    def list_vaults(self) -> List[str]:
        cred = BasicTokenAuthentication({"access_token": self.auth_client.token})
        mgmt = KeyVaultManagementClient(cred, self.subscription_id)
        return [v.name for v in mgmt.vaults.list()]

    def get_vault_uri(self, vault_name: str) -> str:
        cred = BasicTokenAuthentication({"access_token": self.auth_client.token})
        mgmt = KeyVaultManagementClient(cred, self.subscription_id)
        vault = mgmt.vaults.get(self.resource_group, vault_name)
        return vault.properties.vault_uri

    def create_vault(self, vault_name: str) -> Vault:
        parameters = self._get_params()
        cred = BasicTokenAuthentication({"access_token": self.auth_client.token})
        mgmt = KeyVaultManagementClient(cred, self.subscription_id)
        vault = mgmt.vaults.create_or_update(
            self.resource_group, vault_name, parameters
        ).result()
        return vault

    def _get_params(self):
        oid = self._get_user_oid()
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
