# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Settings provider for secrets."""
from functools import partial
import re
from typing import Any, Callable, Dict, Tuple, Optional, Set

import keyring
from keyring.errors import KeyringError, KeyringLocked

from .keyvault_client import (
    BHKeyVaultClient,
    KeyVaultSettings,
    MsticpyKeyVaultConfigError,
)

from .utility import export
from . import pkg_config as config
from .._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"


@export
class KeyringClient:
    """Keyring client wrapper."""

    def __init__(self, name: str = "key-cache", debug: bool = False):
        """
        Initialize the keyring client.

        Parameters
        ----------
        name : str, optional
            Name of the credential group, by default "system"
        debug : bool, optional
            Output debug info, by default False

        """
        self.debug = debug
        self.keyring = name
        self._secret_names: Set[str] = set()

    def __getitem__(self, key: str):
        """Get key name."""
        cred = self.get_secret(key)
        if cred:
            return cred
        raise KeyError

    def get_secret(self, secret_name: str) -> Any:
        """
        Retrieve a secret from the keyring.

        Parameters
        ----------
        secret_name : str
            Secret name.

        Returns
        -------
        Any
            Secret value.

        """
        if self.debug:
            print(f"Fetching {secret_name} from keyring")
        try:
            secret = keyring.get_password(self.keyring, secret_name)
        except (KeyringError, KeyringLocked):
            if self.debug:
                print(
                    "Keyring error retrieving credentials",
                    f"for {secret_name} from keyring {self.keyring}",
                )
        if not secret:
            if self.debug:
                print(
                    "No credentials", f"for {secret_name} from keyring {self.keyring}"
                )
        return secret

    def set_secret(self, secret_name: str, secret_value: Any):
        """
        Set a secret in the keyring group.

        Parameters
        ----------
        secret_name : str
            Name of the secret
        secret_value : Any
            Secret value

        """
        if self.debug:
            print(f"Saving {secret_name} to keyring {self.keyring}")
        self._secret_names.add(secret_name)
        keyring.set_password(self.keyring, secret_name, secret_value)


@export
class SecretsClient:
    """Secrets client - manages keyvault and keyring secrets."""

    def __init__(self, tenant_id: str = None, use_keyring: bool = True):
        """
        Initialize SecretsClient instance.

        Parameters
        ----------
        tenant_id : str, optional
            TenantID, by default None
        use_keyring : bool, optional
            If True use keyring to cache secrets, by default True

        Raises
        ------
        MsticpyKeyVaultConfigError
            Missing or invalid configuration settings.

        Notes
        -----
        Requires KeyVault settings to be defined in msticpyconfig.yaml

        """
        self._kv_settings = KeyVaultSettings()

        self.tenant_id = tenant_id or self._kv_settings.get("tenantid")
        if not self.tenant_id:
            raise MsticpyKeyVaultConfigError(
                "Could not get TenantId from function parameters or configuration.",
                "Please add this to the KeyVault section of msticpyconfig.yaml",
                title="missing tenant ID value.",
            )
        self.kv_secret_vault: Dict[str, str] = {}
        self.kv_vaults: Dict[str, BHKeyVaultClient] = {}
        self._use_keyring = use_keyring or self._kv_settings.get("UseKeyring", False)
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
            return lambda: secret_name if secret_name else ""
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
        if self._use_keyring:
            if self._keyring_client.get_secret(secret_name):
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
        if callable(secret_object):
            return secret_object()
        return secret_object
