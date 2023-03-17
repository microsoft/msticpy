# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""MSAL delegated app authentication class."""
import json
import logging
from sys import platform

import msal
from msal_extensions import (
    FilePersistence,
    FilePersistenceWithDataProtection,
    KeychainPersistence,
    LibsecretPersistence,
    PersistedTokenCache,
)
from msal_extensions.persistence import PersistenceNotFound

from .._version import VERSION

__version__ = VERSION
__author__ = "Pete Bryan"


class MSALDelegatedAuth:
    """Class for handling MSAL based delegated auth."""

    def __init__(
        self,
        client_id: str,
        authority: str,
        username: str,
        scopes: list,
        debug: bool = False,
        **kwargs,
    ):
        """
        Instantiate MSAL authentication class.

        Parameters
        ----------
        client_id : str
            ID of the client to connect to.
        authority : str
            The authentication authority to authenticate against.
        username : str
            The username to authenticate with.
        scopes : list
            A list of scopes to authenicate with
        debug : bool, optional
            Set True to get additional logging ouput, by default False

        """
        self.token_cache = None
        self.location = kwargs.get("location", "token_cache.bin")
        self.auth_type = kwargs.get("auth_type", "interactive")
        self.username = username
        self.scopes = scopes
        self.result = None

        persistence = self._create_cache()
        if persistence:
            self.token_cache = PersistedTokenCache(persistence)

        self.app = msal.PublicClientApplication(
            client_id=client_id, authority=authority, token_cache=self.token_cache
        )

        if "connect" in kwargs:
            self.get_token()

        if not debug:
            logging.getLogger("msal").setLevel(logging.ERROR)

    def get_token(self):
        """Get an authneticaiton token."""
        chosen_account = self.app.get_accounts(username=self.username)
        if chosen_account:
            self.result = self.app.acquire_token_silent_with_error(
                scopes=self.scopes, account=chosen_account[0]
            )
            if not self.result:
                self.result = self._app_auth(self.auth_type)
        else:
            self.result = self._app_auth(self.auth_type)
        self.refresh_token()

    def refresh_token(self):
        """Refresh the authentication token."""
        self.result = None
        chosen_account = self.app.get_accounts(username=self.username)
        if chosen_account:
            self.result = self.app.acquire_token_silent_with_error(
                scopes=self.scopes, account=chosen_account[0], force_refresh=True
            )
        if not self.result:
            self.get_token()

    def _app_auth(self, auth_type):  # pylint: disable=inconsistent-return-statements
        """Conduct authentication against the application."""
        if auth_type == "interactive":
            return self.app.acquire_token_interactive(scopes=self.scopes)
        if auth_type == "device":
            flow = self.app.initiate_device_flow(scopes=self.scopes)
            if "user_code" not in flow:
                raise ValueError(
                    f"Fail to create device flow. Err: {json.dumps(flow, indent=4)}"
                )
            print(flow["message"])
            return self.app.acquire_token_by_device_flow(flow)

    def _create_cache(self):
        """Build a suitable token persistence instance based your current OS."""
        if platform.startswith("win"):
            return FilePersistenceWithDataProtection(self.location)
        if platform.startswith("darwin"):
            return KeychainPersistence(self.location, "msal_token", "msal_token_value")
        if platform.startswith("linux"):
            try:
                return LibsecretPersistence(
                    self.location,
                    schema_name="msal_token",
                    attributes={
                        "msal_token1": "msal_token_values",
                        "msal_token2": "msal_token_values",
                    },
                )
            except (PersistenceNotFound, ImportError, ValueError):
                print("Unable to create encrypted token cache - using in memory cache.")
                return None

        return FilePersistence(self.location)

    @property
    def token(self):
        """Return the token."""
        return self.result["access_token"]
