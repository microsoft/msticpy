# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Msticpy MSAL authentication test class."""
from unittest.mock import patch

from msticpy.auth.msal_auth import MSALDelegatedAuth


class MSALAppMock:
    """Mock the MSAL PublicClientApplicaiton for tests"""

    def __init__(self):
        self.authed = False

    def get_accounts(self, **kwargs):
        if kwargs["username"] and kwargs["username"] == "test@test.com" or self.authed:
            return ["test@test.com"]
        else:
            return None

    def acquire_token_silent_with_error(self, **kwargs):
        self.authed = True
        return {
            "access_token": "aHR0cHM6Ly9yZWFkdGhlZG9jcy5vcmcvcHJvamVjdHMvbXN0aWNweS8="
        }

    def acquire_token_interactive(self, **kwargs):
        self.authed = True
        return {
            "access_token": "aHR0cHM6Ly9yZWFkdGhlZG9jcy5vcmcvcHJvamVjdHMvbXN0aWNweS8="
        }

    def acquire_token_by_device_flow(self, flow, **kwargs):
        self.authed = True
        return {
            "access_token": "aHR0cHM6Ly9yZWFkdGhlZG9jcy5vcmcvcHJvamVjdHMvbXN0aWNweS8="
        }

    def initiate_device_flow(self, **kwargs):
        return {"message": "Your Device Code is ABCDEF", "user_code": "ABCDEF"}


@patch("msal.PublicClientApplication")
def test_msal_auth_device(msal_mock):
    """Test MSAL auth with a Device Code."""
    msal_mock.return_value = MSALAppMock()
    auth = MSALDelegatedAuth(
        client_id="461d2b50-8c8a-4ac4-b78c-6110144d93ce",
        authority="https://login.microsoftonline.com",
        username="test@test.com",
        scopes=["User.Read"],
        auth_type="device",
    )
    auth.get_token()
    assert auth.token == "aHR0cHM6Ly9yZWFkdGhlZG9jcy5vcmcvcHJvamVjdHMvbXN0aWNweS8="


@patch("msal.PublicClientApplication")
def test_msal_auth(msal_mock):
    """Test MSAL auth with Interactive Authentication."""
    msal_mock.return_value = MSALAppMock()
    auth = MSALDelegatedAuth(
        client_id="461d2b50-8c8a-4ac4-b78c-6110144d93ce",
        authority="https://login.microsoftonline.com",
        username="test@test.com",
        scopes=["User.Read"],
    )
    auth.get_token()
    assert auth.token == "aHR0cHM6Ly9yZWFkdGhlZG9jcy5vcmcvcHJvamVjdHMvbXN0aWNweS8="


@patch("msal.PublicClientApplication")
def test_msal_auth_unkown_user(msal_mock):
    """Test MSAL auth with Interactive Authentication with an unkown user."""
    msal_mock.return_value = MSALAppMock()
    auth = MSALDelegatedAuth(
        client_id="461d2b50-8c8a-4ac4-b78c-6110144d93ce",
        authority="https://login.microsoftonline.com",
        username="test@test2.com",
        scopes=["User.Read"],
    )
    auth.get_token()
    assert auth.token == "aHR0cHM6Ly9yZWFkdGhlZG9jcy5vcmcvcHJvamVjdHMvbXN0aWNweS8="
