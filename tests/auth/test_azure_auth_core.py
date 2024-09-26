# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Module docstring."""
from __future__ import annotations
import logging
import os
from datetime import datetime, timedelta
from unittest.mock import MagicMock, patch

import pytest
import pytest_check as check
from azure.identity import ChainedTokenCredential, DeviceCodeCredential

from msticpy.auth.azure_auth_core import (
    AzCredentials,
    AzureCliStatus,
    AzureCloudConfig,
    MsticpyAzureConfigError,
    _az_connect_core,
    _build_certificate_client,
    _build_cli_client,
    _build_client_secret_client,
    _build_device_code_client,
    _build_env_client,
    _build_interactive_client,
    _build_msi_client,
    _build_powershell_client,
    _build_vscode_client,
    _create_chained_credential,
    _filter_all_warnings,
    _filter_credential_warning,
    check_cli_credentials,
)
from msticpy.auth.cloud_mappings import default_auth_methods

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
    check.is_in(".default", az_config.token_uri)


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

_TEST_ENV_VARS: list[tuple[dict[str, str], bool]] = [
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
]


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


@pytest.mark.parametrize(
    "auth_methods, cloud, tenant_id, silent, region, credential",
    [
        (["env", "cli"], "global", "tenant1", False, "region1", None),
        (["msi", "interactive"], "usgov", "tenant2", True, "region2", None),
        (None, None, None, False, None, DeviceCodeCredential()),
    ],
)
def test_az_connect_core(auth_methods, cloud, tenant_id, silent, region, credential):
    """Test _az_connect_core function with different parameters."""
    # Call the function with the test parameters
    result = _az_connect_core(
        auth_methods=auth_methods,
        cloud=cloud,
        tenant_id=tenant_id,
        silent=silent,
        region=region,
        credential=credential,
    )

    # Assert that the result matches the expected credential
    assert isinstance(result, AzCredentials)
    assert result.legacy is not None
    assert result.modern is not None


@pytest.mark.parametrize(
    "env_vars, expected_credential",
    [
        (
            {
                "AZURE_CLIENT_ID": "test_client_id",
                "AZURE_TENANT_ID": "test_tenant_id",
                "AZURE_CLIENT_SECRET": "[PLACEHOLDER]",
            },
            "EnvironmentCredential",
        ),
        (
            {
                "AZURE_CLIENT_ID": "test_client_id",
                "AZURE_TENANT_ID": "test_tenant_id",
                "AZURE_CLIENT_CERTIFICATE_PATH": "[PLACEHOLDER]",
            },
            "EnvironmentCredential",
        ),
        (
            {
                "AZURE_CLIENT_ID": "test_client_id",
                "AZURE_TENANT_ID": "test_tenant_id",
                "AZURE_USERNAME": "test_user",
                "AZURE_PASSWORD": "[PLACEHOLDER]",
            },
            "EnvironmentCredential",
        ),
        (
            {
                "AZURE_CLIENT_ID": "test_client_id",
                "AZURE_CLIENT_CERTIFICATE_PATH": "[PLACEHOLDER]",
            },
            None,
        ),
        (
            {
                "AZURE_TENANT_ID": "test_tenant_id",
                "AZURE_USERNAME": "test_user",
                "AZURE_PASSWORD": "[PLACEHOLDER]",
            },
            None,
        ),
        ({}, None),
    ],
)
@patch.dict(os.environ, {}, clear=True)
@patch("msticpy.auth.azure_auth_core.EnvironmentCredential", autospec=True)
def test_build_env_client_alt(
    mock_env_credential, env_vars, expected_credential, monkeypatch
):
    """Test _build_env_client function."""
    for env_var, env_val in env_vars.items():
        monkeypatch.setenv(env_var, env_val)
    result = _build_env_client()
    if expected_credential:
        # assert isinstance(result, mock_env_credential)
        mock_env_credential.assert_called_once()
    else:
        mock_env_credential.assert_not_called()
        assert result is None


@patch("msticpy.auth.azure_auth_core.AzureCliCredential", autospec=True)
def test_build_cli_client(mock_cli_credential):
    """Test _build_cli_client function."""
    result = _build_cli_client()
    # assert isinstance(result, mock_cli_credential)
    mock_cli_credential.assert_called_once()


@pytest.mark.parametrize(
    "env_vars, expected_kwargs, tenant_id, aad_uri, client_id",
    [
        (
            {"AZURE_CLIENT_ID": "test_client_id"},
            {},
            "test_tenant_id",
            "test_aad_uri",
            "test_client_id",
        ),
        ({}, {}, None, None, None),
    ],
)
@patch.dict(os.environ, {}, clear=True)
@patch("msticpy.auth.azure_auth_core.ManagedIdentityCredential", autospec=True)
def test_build_msi_client(
    mock_msi_credential,
    env_vars,
    expected_kwargs,
    tenant_id,
    aad_uri,
    client_id,
):
    """Test _build_msi_client function."""
    os.environ.update(env_vars)
    result = _build_msi_client(
        tenant_id=tenant_id, aad_uri=aad_uri, client_id=client_id
    )
    # assert isinstance(result, mock_msi_credential)
    mock_msi_credential.assert_called_once_with(
        tenant_id=tenant_id, authority=aad_uri, client_id=client_id, **expected_kwargs
    )


@pytest.mark.parametrize(
    "tenant_id, aad_uri",
    [
        ("test_tenant_id", "test_aad_uri"),
        (None, None),
    ],
)
@patch("msticpy.auth.azure_auth_core.VisualStudioCodeCredential", autospec=True)
def test_build_vscode_client(mock_vscode_credential, tenant_id, aad_uri):
    """Test _build_vscode_client function."""
    result = _build_vscode_client(tenant_id=tenant_id, aad_uri=aad_uri)
    # assert isinstance(result, mock_vscode_credential)
    mock_vscode_credential.assert_called_once_with(
        tenant_id=tenant_id, authority=aad_uri
    )


@pytest.mark.parametrize(
    "tenant_id, aad_uri, kwargs",
    [
        ("test_tenant_id", "test_aad_uri", {"param": "value"}),
        (None, None, {}),
    ],
)
@patch("msticpy.auth.azure_auth_core.InteractiveBrowserCredential", autospec=True)
def test_build_interactive_client(
    mock_interactive_credential, tenant_id, aad_uri, kwargs
):
    """Test _build_interactive_client function."""
    _ = _build_interactive_client(tenant_id=tenant_id, aad_uri=aad_uri, **kwargs)
    # assert isinstance(result, mock_interactive_credential)
    mock_interactive_credential.assert_called_once_with(
        tenant_id=tenant_id, authority=aad_uri, **kwargs
    )


@pytest.mark.parametrize(
    "tenant_id, aad_uri, kwargs",
    [
        ("test_tenant_id", "test_aad_uri", {"param": "value"}),
        (None, None, {}),
    ],
)
@patch("msticpy.auth.azure_auth_core.DeviceCodeCredential", autospec=True)
def test_build_device_code_client(
    mock_device_code_credential, tenant_id, aad_uri, kwargs
):
    """Test _build_device_code_client function."""
    _ = _build_device_code_client(tenant_id=tenant_id, aad_uri=aad_uri, **kwargs)
    mock_device_code_credential.assert_called_once_with(
        tenant_id=tenant_id, authority=aad_uri, **kwargs
    )


@pytest.mark.parametrize(
    "tenant_id, aad_uri, client_id, client_secret, expected_credential",
    [
        (
            "test_tenant_id",
            "test_aad_uri",
            "test_client_id",
            "test_client_secret",
            "ClientSecretCredential",
        ),
        ("test_tenant_id", "test_aad_uri", None, "test_client_secret", None),
        ("test_tenant_id", "test_aad_uri", "test_client_id", None, None),
    ],
)
@patch("msticpy.auth.azure_auth_core.ClientSecretCredential", autospec=True)
def test_build_client_secret_client(
    mock_client_secret_credential,
    tenant_id,
    aad_uri,
    client_id,
    client_secret,
    expected_credential,
):
    """Test _build_client_secret_client function."""
    kwargs = {"client_id": client_id, "client_secret": client_secret}
    result = _build_client_secret_client(tenant_id=tenant_id, aad_uri=aad_uri, **kwargs)
    if expected_credential:
        mock_client_secret_credential.assert_called_once_with(
            tenant_id=tenant_id, authority=aad_uri, **kwargs
        )
    else:
        assert result is None


@pytest.mark.parametrize(
    "tenant_id, aad_uri, client_id, expected_credential",
    [
        ("test_tenant_id", "test_aad_uri", "test_client_id", "CertificateCredential"),
        ("test_tenant_id", "test_aad_uri", None, None),
    ],
)
@patch("msticpy.auth.azure_auth_core.CertificateCredential", autospec=True)
def test_build_certificate_client(
    mock_certificate_credential, tenant_id, aad_uri, client_id, expected_credential
):
    """Test _build_certificate_client function."""
    kwargs = {"client_id": client_id}
    result = _build_certificate_client(tenant_id=tenant_id, aad_uri=aad_uri, **kwargs)
    if expected_credential:
        mock_certificate_credential.assert_called_once_with(
            tenant_id=tenant_id, authority=aad_uri, **kwargs
        )
    else:
        assert result is None


@patch("msticpy.auth.azure_auth_core.AzurePowerShellCredential", autospec=True)
def test_build_powershell_client(mock_powershell_credential):
    """Test _build_powershell_client function."""
    result = _build_powershell_client()
    # assert isinstance(result, mock_powershell_credential)
    mock_powershell_credential.assert_called_once()


@pytest.mark.parametrize(
    "requested_clients, tenant_id, aad_uri, kwargs, expected_cred_types, expected_exception",
    [
        (
            None,
            "test_tenant_id",
            "test_aad_uri",
            {},
            [
                "AzureCliCredential",
                "ManagedIdentityCredential",
                "InteractiveBrowserCredential",
            ],
            None,
        ),
        (
            ["env", "cli"],
            "test_tenant_id",
            "test_aad_uri",
            {},
            ["AzureCliCredential"],
            None,
        ),
        (
            ["unknown"],
            "test_tenant_id",
            "test_aad_uri",
            {},
            [],
            MsticpyAzureConfigError,
        ),
        (
            ["env-test", "cli", "invalid"],
            "test_tenant_id",
            "test_aad_uri",
            {},
            ["AzureCliCredential"],
            None,
        ),
    ],
)
@patch("msticpy.auth.azure_auth_core.EnvironmentCredential", autospec=True)
@patch("msticpy.auth.azure_auth_core.AzureCliCredential", autospec=True)
@patch("msticpy.auth.azure_auth_core.ManagedIdentityCredential", autospec=True)
@patch("msticpy.auth.azure_auth_core.InteractiveBrowserCredential", autospec=True)
def test_create_chained_credential(
    mock_interactive_credential,
    mock_msi_credential,
    mock_cli_credential,
    mock_env_credential,
    requested_clients,
    tenant_id,
    aad_uri,
    kwargs,
    expected_cred_types,
    expected_exception,
):
    """
    Test _create_chained_credential function.

    Parameters
    ----------
    mock_interactive_credential : MagicMock
        Mocked InteractiveBrowserCredential class.
    mock_msi_credential : MagicMock
        Mocked ManagedIdentityCredential class.
    mock_cli_credential : MagicMock
        Mocked AzureCliCredential class.
    mock_env_credential : MagicMock
        Mocked EnvironmentCredential class.
    mock_clients : dict
        Mocked _CLIENTS dictionary.
    requested_clients : list[str]
        List of clients to chain.
    tenant_id : str
        The tenant ID to connect to.
    aad_uri : str
        The URI of the Azure AD cloud to connect to.
    kwargs : dict
        Additional keyword arguments.
    expected_cred_types : list[str]
        Expected credential types to be included in the chained credential.
    expected_exception : Exception
        Expected exception to be raised.

    Returns
    -------
    None
    """
    if expected_exception:
        with pytest.raises(expected_exception):
            _create_chained_credential(
                aad_uri=aad_uri,
                requested_clients=requested_clients,
                tenant_id=tenant_id,
                **kwargs
            )
    else:
        result = _create_chained_credential(
            aad_uri=aad_uri,
            requested_clients=requested_clients,
            tenant_id=tenant_id,
            **kwargs
        )
        assert isinstance(result, ChainedTokenCredential)
        cred_classes = {cred.__class__.__name__ for cred in result.credentials}
        assert all(expected in cred_classes for expected in expected_cred_types)


@pytest.mark.parametrize(
    "record_name, record_level, record_message, expected_output",
    [
        ("azure.identity", logging.WARNING, "EnvironmentCredential.get_token", False),
        ("azure.identity", logging.WARNING, "AzureCliCredential.get_token", False),
        (
            "azure.identity",
            logging.WARNING,
            "ManagedIdentityCredential.get_token",
            False,
        ),
        ("azure.identity", logging.WARNING, "SomeOtherCredential.get_token", False),
        ("azure.identity", logging.INFO, "EnvironmentCredential.get_token", True),
        ("some.other.logger", logging.WARNING, "EnvironmentCredential.get_token", True),
    ],
)
def test_filter_credential_warning(
    record_name, record_level, record_message, expected_output
):
    """
    Test _filter_credential_warning function.

    Parameters
    ----------
    record_name : str
        The name of the log record.
    record_level : int
        The level of the log record.
    record_message : str
        The message of the log record.
    expected_output : bool
        The expected output of the function.

    Returns
    -------
    None
    """
    record = MagicMock()
    record.name = record_name
    record.levelno = record_level
    record.getMessage.return_value = record_message

    result = _filter_credential_warning(record)
    assert result == expected_output


@pytest.mark.parametrize(
    "record_name, record_level, record_message, expected_output",
    [
        ("azure.identity", logging.WARNING, "EnvironmentCredential.get_token", False),
        ("azure.identity", logging.WARNING, "AzureCliCredential.get_token", False),
        (
            "azure.identity",
            logging.WARNING,
            "ManagedIdentityCredential.get_token",
            False,
        ),
        ("azure.identity", logging.WARNING, "SomeOtherCredential.get_token", False),
        ("azure.identity", logging.WARNING, "Some other warning message", True),
        ("azure.identity", logging.INFO, "EnvironmentCredential.get_token", True),
        ("some.other.logger", logging.WARNING, "EnvironmentCredential.get_token", True),
    ],
)
def test_filter_all_warnings(
    record_name, record_level, record_message, expected_output
):
    """
    Test _filter_all_warnings function.

    Parameters
    ----------
    record_name : str
        The name of the log record.
    record_level : int
        The level of the log record.
    record_message : str
        The message of the log record.
    expected_output : bool
        The expected output of the function.

    Returns
    -------
    None
    """
    record = MagicMock()
    record.name = record_name
    record.levelno = record_level
    record.getMessage.return_value = record_message

    result = _filter_all_warnings(record)
    assert result == expected_output
