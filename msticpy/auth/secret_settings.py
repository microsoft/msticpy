# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Settings provider for secrets."""
import re
from functools import partial
from typing import Any, Callable, Dict, List, Optional, Tuple

from .._version import VERSION
from ..common import pkg_config as config
from ..common.exceptions import MsticpyKeyVaultConfigError

try:
    from .keyring_client import KeyringClient

    _KEYRING_INSTALLED = True
except ImportError:
    _KEYRING_INSTALLED = False
from ..common.utility import export
from .keyvault_client import BHKeyVaultClient
from .keyvault_settings import KeyVaultSettings

__version__ = VERSION
__author__ = "Ian Hellen"


@export
class SecretsClient:
    """Secrets client - manages Key Vault and keyring secrets."""

    def __init__(
        self,
        tenant_id: str = None,
        use_keyring: bool = False,
        auth_methods: Optional[List[str]] = None,
        credential: Any = None,
        **kwargs,
    ):
        """
        Initialize SecretsClient instance.

        Parameters
        ----------
        tenant_id : str, optional
            TenantID, by default None
        use_keyring : bool, optional
            If True use keyring to cache secrets, by default False
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
        credential : Optional[AzureCredential]
            Azure credential

        Other Parameters
        ----------------
        client_id : Optional[str]
            Required if auth_methods is ["clientsecret"]
        client_secret : Optional[str]
            Required if auth_methods is ["clientsecret"]

        Raises
        ------
        MsticpyKeyVaultConfigError
            Missing or invalid configuration settings or failure to
            authenticate to Key Vault with the chosen credential type.

        Notes
        -----
        Requires KeyVault settings to be defined in msticpyconfig.yaml

        """
        self._kv_settings = KeyVaultSettings()

        self.tenant_id = tenant_id or self._kv_settings.get("tenantid")
        self._auth_dict = {
            "tenant_id": self.tenant_id,
            "auth_methods": auth_methods,
            "credential": credential,
            **kwargs,
        }
        if not self.tenant_id:
            raise MsticpyKeyVaultConfigError(
                "Could not get TenantId from function parameters or configuration.",
                "Please add this to the KeyVault section of msticpyconfig.yaml",
                title="missing tenant ID value.",
            )
        self.kv_secret_vault: Dict[str, str] = {}
        self.kv_vaults: Dict[str, BHKeyVaultClient] = {}
        self._use_keyring = (
            _KEYRING_INSTALLED
            and KeyringClient.is_keyring_available()
            and (use_keyring or self._kv_settings.get("UseKeyring", False))
        )
        if self._use_keyring:
            self._keyring_client = KeyringClient("Providers")

    def get_secret_accessor(self, setting_path: str) -> Callable[[], Any]:
        """
        Return accessor function for a secret.

        Parameters
        ----------
        setting_path : str
            The msticpy configuration path (dot-separated)

        Returns
        -------
        Callable[[None], Any]
            Accessor function for the secret value.

        """
        vault_name, secret_name = self._get_kv_vault_and_name(setting_path)
        if vault_name is None or secret_name is None:
            return lambda: secret_name or ""
        return self._get_secret_func(secret_name, vault_name)

    def _add_key_vault(self, vault_name: str, secret_name: str):
        """Add the KeyVault instance responsible for storing `secret_name`."""
        vault = self.kv_vaults.get(vault_name)
        if not vault:
            vault = BHKeyVaultClient(self.tenant_id, vault_name=vault_name)
            self.kv_vaults[vault_name] = vault
        self.kv_secret_vault[secret_name] = vault_name

    @staticmethod
    def format_kv_name(setting_path):
        """Return normalized name for use as a KeyVault secret name."""
        return re.sub("[^0-9a-zA-Z-]", "-", setting_path)

    def _get_kv_vault_and_name(
        self, setting_path: str
    ) -> Tuple[Optional[str], Optional[str]]:
        """Return the vault and secret name for a config path."""
        setting_item = config.get_config(setting_path)

        if not isinstance(setting_item, dict):
            return None, str(setting_item)
        if "KeyVault" in setting_item:
            kv_val = setting_item.get("KeyVault")
            def_vault_name = self._kv_settings.get("VaultName")
            if not kv_val or kv_val.casefold() == "default":
                # If no value, get the default VaultName from settings
                # and use the setting path as the secret name
                if not def_vault_name:
                    raise ValueError("No VaultName defined in KeyVault settings.")
                secret_name = self.format_kv_name(setting_path)
                return def_vault_name, secret_name
            if "/" in kv_val:
                # '/' delimited string means VaultName/Secret
                vault_name, secret_name = kv_val.split("/")
                return vault_name, self.format_kv_name(secret_name)
            if not def_vault_name:
                raise MsticpyKeyVaultConfigError(
                    "Check that you have specified the right value for VaultName"
                    + " in your configuration",
                    f"No VaultName defined in KeyVault settings for {setting_path}.",
                    title="Key Vault vault name not found.",
                )
            # If there is a single string - take that as the secret name
            return def_vault_name, self.format_kv_name(kv_val)
        return None, None

    def _get_secret_func(self, secret_name: str, vault_name: str) -> Callable[[], Any]:
        """Return a func to access a secret."""
        if self._use_keyring and self._keyring_client.get_secret(secret_name):
            return self._create_secret_func(self._keyring_client, secret_name)

        # If the secret is not in keyring, get the vault holding this secret
        if not self.kv_secret_vault.get(secret_name):
            self._add_key_vault(secret_name=secret_name, vault_name=vault_name)

        vault = self.kv_vaults[vault_name]
        if self._use_keyring:
            # store the secret in keyring and return an accessor
            # to the keyring value.
            self._keyring_client.set_secret(secret_name, vault.get_secret(secret_name))
            return self._create_secret_func(self._keyring_client, secret_name)
        # if not using Keyring - return a KeyVault accessor
        return self._create_secret_func(vault, secret_name)

    @staticmethod
    def _create_secret_func(secret_store, secret_name):
        return partial(secret_store.get_secret, secret_name=secret_name)

    @staticmethod
    def read_secret(secret_object: Any) -> Any:
        """
        Return the secret value.

        Parameters
        ----------
        secret_object : Any
            If it is a func, call and return the return value
            of that func. Otherwise just return the object.

        Returns
        -------
        Any
            The secret value

        """
        return secret_object() if callable(secret_object) else secret_object

    def refresh_keyring(self):
        """Reload keyring values from Key Vault."""
        if not self._use_keyring or not self._keyring_client.is_keyring_available():
            return
        for kv_client in self.kv_vaults.values():
            for secret_name in kv_client.secrets:
                if not self._keyring_client.get_secret(secret_name):
                    continue
                self._keyring_client.set_secret(
                    secret_name=secret_name,
                    secret_value=kv_client.get_secret(secret_name),
                )

    def clear_keyring_secrets(self):
        """Clear any cached secrets from keyring."""
        if not self._use_keyring or not self._keyring_client.is_keyring_available():
            return
        for kv_client in self.kv_vaults.values():
            for secret_name in kv_client.secrets:
                if not self._keyring_client.get_secret(secret_name):
                    continue
                self._keyring_client.delete_secret(secret_name=secret_name)
