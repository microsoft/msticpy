from unittest.mock import MagicMock, patch

import pytest

from msticpy.auth.azure_auth import (
    az_connect,
    az_user_connect,
    fallback_devicecode_creds,
    get_default_resource_name,
)
from msticpy.auth.azure_auth_core import AzCredentials


@pytest.fixture
def mock_az_credentials():
    return MagicMock(spec=AzCredentials)


@patch("msticpy.auth.azure_auth.os")
@patch("msticpy.auth.azure_auth.get_provider_settings")
@patch("msticpy.auth.azure_auth.az_connect_core")
@patch("msticpy.auth.azure_auth.SubscriptionClient")
def test_az_connect(
    mock_sub_client,
    mock_az_connect_core,
    mock_get_provider_settings,
    mock_os,
    mock_az_credentials,
):
    mock_az_credentials.modern = MagicMock()
    mock_az_credentials.modern.__bool__.return_value = True
    mock_az_credentials.legacy = MagicMock()
    mock_az_connect_core.return_value = mock_az_credentials
    mock_sub_client.return_value = MagicMock()
    mock_os.environ = MagicMock()
    az_cli_args = MagicMock()
    az_cli_args.get.return_value = "test_value"
    az_cli_args.__bool__.return_value = True
    az_cli_config = MagicMock()
    az_cli_config.__bool__.return_value = True
    az_cli_config.args = az_cli_args

    data_provs = MagicMock(spec=dict)
    data_provs.get.return_value = az_cli_config
    mock_get_provider_settings.return_value = data_provs

    result = az_connect(auth_methods=["env"], tenant_id="test_tenant", silent=True)

    assert result == mock_az_credentials
    mock_az_connect_core.assert_called_once_with(
        auth_methods=["env"], tenant_id="test_tenant", silent=True
    )
    mock_sub_client.assert_called_once()


@patch("msticpy.auth.azure_auth.az_connect_core")
def test_az_user_connect(mock_az_connect_core, mock_az_credentials):
    mock_az_connect_core.return_value = mock_az_credentials

    result = az_user_connect(tenant_id="test_tenant", silent=True)

    assert result == mock_az_credentials
    mock_az_connect_core.assert_called_once_with(
        auth_methods=["cli", "interactive"], tenant_id="test_tenant", silent=True
    )


@patch("msticpy.auth.azure_auth.AzureCloudConfig")
@patch("msticpy.auth.azure_auth.DeviceCodeCredential")
@patch("msticpy.auth.azure_auth.CredentialWrapper")
def test_fallback_devicecode_creds(
    mock_cred_wrapper, mock_device_code_cred, mock_azure_cloud_config
):
    mock_azure_cloud_config.return_value = MagicMock()
    mock_device_code_cred.return_value = MagicMock()
    mock_cred_wrapper.return_value = MagicMock()

    result = fallback_devicecode_creds(cloud="test_cloud", tenant_id="test_tenant")

    assert isinstance(result, AzCredentials)
    mock_device_code_cred.assert_called_once()
    mock_cred_wrapper.assert_called_once()


def test_get_default_resource_name():
    resource_uri = "https://example.com/resource"
    expected_result = "https://example.com/resource/.default"

    result = get_default_resource_name(resource_uri)

    assert result == expected_result
