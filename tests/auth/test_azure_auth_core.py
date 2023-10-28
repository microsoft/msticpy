# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Module docstring."""
from datetime import datetime, timedelta
from unittest.mock import MagicMock, patch

import pytest
import pytest_check as check
from msrestazure import azure_cloud

from msticpy.auth.azure_auth_core import (
    AzureCliStatus,
    AzureCloudConfig,
    _build_env_client,
    check_cli_credentials,
    default_auth_methods,
)

from ..unit_test_lib import custom_mp_config, get_test_data_path

__author__ = "Ian Hellen"

# pylint: disable=redefined-outer-name, protected-access


@pytest.fixture(scope="module")
def mp_config_file():
    """Fixture_docstring."""
    return get_test_data_path().joinpath("msticpyconfig.yaml")


def test_default_auth_methods(mp_config_file):
    """Test default auth methods function."""
    with custom_mp_config(mp_config_file):
        check.is_in("env", default_auth_methods())
        check.is_in("msi", default_auth_methods())
        check.is_in("cli", default_auth_methods())
        check.is_in("interactive", default_auth_methods())


def test_azure_cloud_config(mp_config_file):
    """Test the Azure cloud config."""
    with custom_mp_config(mp_config_file):
        az_config = AzureCloudConfig()
    check.equal(az_config.cloud, "global")
    check.is_in("env", az_config.auth_methods)
    check.is_in("msi", az_config.auth_methods)
    check.is_in("cli", az_config.auth_methods)
    check.is_in("interactive", az_config.auth_methods)
    glob_rm_uri = azure_cloud.AZURE_PUBLIC_CLOUD.endpoints.resource_manager
    check.equal(f"{glob_rm_uri}.default", az_config.token_uri)


_TOKEN_WRAPPER = ["Bearer", "__b64_str__"]

_TOKEN = {
    "tokenType": "Bearer",
    "expiresIn": 3000,
    "expiresOn": str(datetime.now() + timedelta(0.1)),
    "resource": "https://management.core.windows.net/",
    "accessToken": "_b64_token_string_",
    "refreshToken": "_b64_token_string2_",
}

_CLI_TESTS = [
    (({}, None), AzureCliStatus.CLI_OK),
    (
        ({"expiresOn": str(datetime.now() - timedelta(0.1))}, None),
        AzureCliStatus.CLI_TOKEN_EXPIRED,
    ),
    (({}, ImportError), AzureCliStatus.CLI_NOT_INSTALLED),
    (
        ({}, ValueError("AADSTS70043: The refresh token has expired")),
        AzureCliStatus.CLI_TOKEN_EXPIRED,
    ),
    (
        ({}, ValueError("Please run 'az login' to setup account")),
        AzureCliStatus.CLI_NEEDS_SIGN_IN,
    ),
    (({}, ValueError("Another error")), AzureCliStatus.CLI_UNKNOWN_ERROR),
]


def _test_ids(test_cases):
    return [test[1].name for test in test_cases]


class CliProfile:
    """Mock Azure CLI profile class."""

    def __init__(self, token):
        """Initialize class with raw token."""
        self.token = token

    def get_raw_token(self):
        """Return raw token"""
        return (*_TOKEN_WRAPPER, self.token), None, None


@patch(check_cli_credentials.__module__ + ".get_cli_profile")
@pytest.mark.parametrize("test, expected", _CLI_TESTS, ids=_test_ids(_CLI_TESTS))
def test_check_cli_credentials(get_cli_profile, test, expected):
    # sourcery skip: use-fstring-for-concatenation
    """Test checking Azure CLI credentials."""
    test_tok = {**_TOKEN}
    test_tok.update(test[0])

    get_cli_profile.return_value = CliProfile(test_tok)
    if test[1]:
        get_cli_profile.side_effect = test[1]

    check.equal(check_cli_credentials()[0], expected)


_CLI_ID = "d8d9d2f2-5d2d-4d7e-9c5c-5d6d9d1d8d9d"
_TENANT_ID = "f8d9d2f2-5d2d-4d7e-9c5c-5d6d9d1d8d9e"

_TEST_ENV_VARS = (
    (
        {
            "AZURE_CLIENT_ID": _CLI_ID,
            "AZURE_CLIENT_SECRET": "[PLACEHOLDER]",  # nosec
            "AZURE_TENANT_ID": _TENANT_ID,
        },
        True,
    ),
    (
        {
            "AZURE_CLIENT_ID": _CLI_ID,
            "AZURE_CLIENT_CERTIFICATE_PATH": "test_cert_path",
            "AZURE_TENANT_ID": _TENANT_ID,
        },
        True,
    ),
    (
        {
            "AZURE_CLIENT_ID": _CLI_ID,
            "AZURE_USERNAME": "test_user_name",
            "AZURE_PASSWORD": "[PLACEHOLDER]",  # nosec
            "AZURE_TENANT_ID": _TENANT_ID,
        },
        True,
    ),
    (
        {
            "AZURE_CLIENT_ID": _CLI_ID,
            "AZURE_TENANT_ID": _TENANT_ID,
        },
        False,
    ),
    (
        {
            "AZURE_CLIENT_CERTIFICATE_PATH": "test_cert_path",
            "AZURE_TENANT_ID": _TENANT_ID,
        },
        False,
    ),
    ({}, False),
)


@pytest.mark.parametrize("env_vars, expected", _TEST_ENV_VARS)
def test_build_env_client(env_vars, expected, monkeypatch):
    """Test with different environment variables."""
    # sourcery skip: no-loop-in-tests
    for env_var, env_val in env_vars.items():
        monkeypatch.setenv(env_var, env_val)

    mock_credential = MagicMock()
    with patch(
        "msticpy.auth.azure_auth_core.EnvironmentCredential",
        return_value=mock_credential,
    ) as mock_env_cred:
        credential = _build_env_client(aad_uri="test_aad_uri")

        check.is_true((credential is not None) == expected)
        check.is_true(
            mock_env_cred.called_once_with(authority="test_aad_uri") or not expected
        )
