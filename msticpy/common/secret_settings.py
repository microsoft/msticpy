# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Settings provider for secrets."""
from functools import partial
from typing import Any, Union, Callable

import base64
import keyring
from keyring.errors import KeyringError, KeyringLocked
import datetime

from .keyvault_client import BHKeyVaultClient
import requests

from ..nbtools.utility import export
from ..nbtools import pkg_config as config
from .._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"


from typing import Callable


class KeyringClient:
    def __init__(self, name: str = "system", debug: bool = False):
        self.debug = debug
        self.keyring = name
        self._get_creds = self._get_creds_keyring
        self._cache_creds = self._cache_creds_keyring

    def __getitem__(self, key: str):
        """Get key name."""
        cred = self.get_credential(key)
        if cred:
            return cred
        raise KeyError

    def get_secret(self, secret_name: str) -> Any:
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
        if self.debug:
            print(f"Saving {secret_name} to keyring {self.keyring}")
        keyring.set_password(self.keyring, secret_name, secret_value)


kv_uri = "mstic-ianhelle"

_KV_CLIENT = BHKeyVaultClient(vault_uri=kv_uri)
_KR_CLIENT = KeyringClient("Providers")


class SecretsClient:
    def __init__(self, use_keyring=True):
        self._use_keyring = use_keyring

    def get_secret(self, secret_name: str):
        sec_value = None
        if self._use_keyring:
            sec_value = _KR_CLIENT.get_secret(secret_name)
        if not sec_value:
            sec_value = _KV_CLIENT.get_secret(secret_name)
            if self._use_keyring:
                _KR_CLIENT.set_secret(secret_name, sec_value)
        return sec_value


_secrets_client = SecretsClient()


def create_secret_func(secret_name):
    return partial(_secrets_client.get_secret, secret_name=secret_name)


def read_secret(secret_object: Union[str, Callable]):
    if isinstance(secret_object, str):
        return secret_object
    if callable(secret_object):
        return secret_object()
