# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Settings provider for secrets."""
import random
from typing import Any, Set

import keyring
from keyring.errors import KeyringError, KeyringLocked, NoKeyringError

from .._version import VERSION
from ..common.utility import export

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
            Name of the credential group, by default "key-cache"
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
        secret = None
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
        if not secret and self.debug:
            print("No credentials", f"for {secret_name} from keyring {self.keyring}")
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

    def delete_secret(self, secret_name: str):
        """
        Delete a secret in the keyring group.

        Parameters
        ----------
        secret_name : str
            Name of the secret

        """
        if self.debug:
            print(f"Deleting {secret_name} from keyring {self.keyring}")
        self._secret_names.remove(secret_name)
        keyring.delete_password(self.keyring, secret_name)

    @staticmethod
    def is_keyring_available() -> bool:
        """
        Test if valid keyring backend is available.

        Returns
        -------
        bool
            True if Keyring has a usable backend, False if not.

        """
        char_list = list("abcdefghijklm1234567890")
        random.shuffle(char_list)
        test_value = "".join(char_list)
        try:
            keyring.set_password("test", test_value, test_value)
            # If no exception clear the test key
            try:
                keyring.delete_password("test", test_value)
            except keyring.errors.PasswordDeleteError:
                pass
            return True
        except NoKeyringError:
            return False
