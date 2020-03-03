# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Settings provider for secrets."""
from functools import partial
import re
from typing import Any, Union, Callable

import keyring
from keyring.errors import KeyringError, KeyringLocked


from .keyvault_client import BHKeyVaultClient


from ..nbtools.utility import export, MsticpyConfigException
from ..nbtools import pkg_config as config
from .._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"


class KeyringClient:
    def __init__(self, name: str = "system", debug: bool = False):
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
        keyring.set_password(self.keyring, secret_name, secret_value)


class SecretsClient:
    def __init__(self, tenant_id: str = None, use_keyring: bool = True):
        try:
            self._def_kv_settings = config.get_config_path("KeyVault")
        except KeyError:
            self._def_kv_settings = {}
        self.tenant_id = tenant_id or self._def_kv_settings.get("TenantId")
        if not self.tenant_id:
            raise MsticpyConfigException(
                "TenantID must be specified in KeyVault settings section",
                "in msticpyconfig.yaml",
            )
        self.kv_secrets = {}
        self.kv_vaults = {}
        if use_keyring or self._def_kv_settings.get("UseKeyring"):
            self._keyring_client = KeyringClient("Providers")
        self._use_keyring = use_keyring

    def add_keyvault_setting(self, setting_path: str) -> Callable[[None], Any]:
        vault_name, secret_name = self._get_kv_vault_and_name(setting_path)
        self._add_kv_setting(vault_name, secret_name)
        return self.get_secret_func(secret_name)

    def _add_kv_setting(self, vault_name: str, secret_name: str):
        vault = self.kv_vaults.get(vault_name)
        if not vault:
            vault = BHKeyVaultClient(self.tenant_id, vault_name=vault_name)
            self.kv_vaults[vault_name] = vault
        self.kv_secrets[secret_name] = vault

    @staticmethod
    def format_kv_name(setting_path):
        return re.sub("[^0-9a-zA-Z-]", "-", setting_path)

    def _get_kv_vault_and_name(self, setting_path: str):
        setting_item = config.get_config_path(setting_path)

        if "KeyVault" in setting_item:
            kv_val = setting_item.get("KeyVault")
            def_vault_name = self._def_kv_settings.get("VaultName")
            if not kv_val:
                if not def_vault_name:
                    raise ValueError("No VaultName defined in KeyVault settings.")
                secret_name = self.format_kv_name(setting_path)
                return def_vault_name, secret_name
            if "/" in kv_val:
                vault_name, secret_name = kv_val.split("/")
                return vault_name, self.format_kv_name(secret_name)
            if not def_vault_name:
                raise ValueError("No VaultName defined in KeyVault settings.")
            return def_vault_name, self.format_kv_name(kv_val)

    def get_secret_func(self, secret_name: str) -> Callable[[None], Any]:
        if self._use_keyring:
            if self._keyring_client.get_secret(secret_name):
                return self._create_secret_func(self._keyring_client, secret_name)

        vault = self.kv_secrets.get(secret_name)
        if not vault:
            raise MsticpyConfigException(
                f"{secret_name} was not registered for a KeyVault"
            )
        if self._use_keyring:
            self._keyring_client.set_secret(secret_name, vault.get_secret(secret_name))
            return self._create_secret_func(self._keyring_client, secret_name)
        return self._create_secret_func(vault, secret_name)

    @staticmethod
    def _create_secret_func(secret_store, secret_name):
        return partial(secret_store.get_secret, secret_name=secret_name)

    @staticmethod
    def read_secret(secret_object: Union[str, Callable]) -> Any:
        if isinstance(secret_object, str):
            return secret_object
        # TODO - if this fails with keyring we need to fall back to KeyVault
        if callable(secret_object):
            return secret_object()
        return None
