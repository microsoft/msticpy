# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Microsoft Sentinel core unit tests."""
from unittest.mock import MagicMock, Mock, patch

import pandas as pd
import pytest
from azure.core.exceptions import ClientAuthenticationError

import msticpy.context.azure
from msticpy.common.wsconfig import WorkspaceConfig
from msticpy.context.azure.azure_data import AzureData
from msticpy.context.azure.sentinel_core import MicrosoftSentinel

from ...unit_test_lib import custom_mp_config, get_test_data_path

# pylint: disable=redefined-outer-name, protected-access

_RESOURCES = pd.DataFrame(
    {
        "resource_type": [
            "Microsoft.OperationsManagement/solutions",
            "Microsoft.OperationsManagement/solutions",
            "Microsoft.Insights/components",
        ],
        "name": ["SecurityInsightsTest1", "Test2", "Test3"],
        "resource_id": ["123", "456", "789"],
    }
)

_RES_ID = (
    "/subscriptions/123/resourcegroups/RG/providers/"
    "Microsoft.OperationalInsights/workspaces/WSName"
)

_RESOURCE_DETAILS = {"properties": {"workspaceResourceId": _RES_ID}}


def test_azuresent_init():
    """Test class initialization."""
    with custom_mp_config(
        get_test_data_path().parent.joinpath("msticpyconfig-test.yaml")
    ):
        sentinel_inst = MicrosoftSentinel(sub_id="123", res_grp="RG", ws_name="WSName")
        assert isinstance(sentinel_inst, MicrosoftSentinel)
        assert sentinel_inst.default_subscription_id == "123"
        assert sentinel_inst.default_resource_group == "RG"
        assert sentinel_inst.default_workspace_name == "WSName"
        sentinel_inst = MicrosoftSentinel(
            subscription_id="123", resource_group="RG", workspace_name="WSName"
        )
        assert isinstance(sentinel_inst, MicrosoftSentinel)
        assert sentinel_inst.default_subscription_id == "123"
        assert sentinel_inst.default_resource_group == "RG"
        assert sentinel_inst.default_workspace_name == "WSName"
        sentinel_inst = MicrosoftSentinel(res_id=_RES_ID)
        assert isinstance(sentinel_inst, MicrosoftSentinel)
        assert sentinel_inst.default_subscription_id == "123"
        assert sentinel_inst.default_resource_group == "RG"
        assert sentinel_inst.default_workspace_name == "WSName"


@patch.object(AzureData,"connect")
@patch(MicrosoftSentinel.__module__ + ".get_token")
def test_azuresent_connect_token(get_token: Mock, az_data_connect: Mock):
    """Test connect success."""
    token = "12398120398"

    with custom_mp_config(
        get_test_data_path().parent.joinpath("msticpyconfig-test.yaml")
    ):
        sentinel_inst = MicrosoftSentinel(res_id=_RES_ID)

        setattr(sentinel_inst, "set_default_workspace", MagicMock())
        setattr(sentinel_inst, "credentials", MagicMock())
        sentinel_inst.connect(auth_methods=["env"], token=token)

        assert sentinel_inst._token == token
        az_data_connect.assert_called_once_with(auth_methods=["env"], silent=False)

        get_token.return_value = token
        token = "12398120398"
        sentinel_inst = MicrosoftSentinel(
            res_id=_RES_ID,
        )
        setattr(sentinel_inst, "set_default_workspace", MagicMock())
        setattr(sentinel_inst, "credentials", MagicMock())
        sentinel_inst.connect(auth_methods=["env"], tenant_id="12345")

        assert sentinel_inst._token == token
        get_token.assert_called_once_with(
            sentinel_inst.credentials,
            tenant_id="12345",
            cloud=sentinel_inst.cloud,
        )


@patch.object(AzureData, "connect")
def test_azuresent_connect_fail(az_data_connect: Mock):
    """Test connect failure."""
    az_data_connect.side_effect = ClientAuthenticationError("Could not authenticate.")
    with pytest.raises(ClientAuthenticationError):
        sentinel_inst = MicrosoftSentinel(res_id=_RES_ID)
        sentinel_inst.connect(auth_methods=["env"])


@pytest.fixture(scope="module")
@patch(MicrosoftSentinel.__module__ + ".MicrosoftSentinel.connect")
def sentinel_inst_loader(mock_creds):
    """Generate MicrosoftSentinel for testing."""
    mock_creds.return_value = None
    with custom_mp_config(
        get_test_data_path().parent.joinpath("msticpyconfig-test.yaml")
    ):
        sentinel_inst = MicrosoftSentinel(sub_id="123", res_grp="RG", ws_name="WSName")
        sentinel_inst.connect()
        sentinel_inst.connected = True
        sentinel_inst._token = "123"
        return sentinel_inst


@patch(AzureData.__module__ + ".AzureData.get_resources")
@patch(AzureData.__module__ + ".AzureData.get_resource_details")
def test_azuresent_workspaces(mock_res_dets, mock_res, sentinel_inst_loader):
    """Test Sentinel workspaces feature."""
    mock_res.return_value = _RESOURCES
    mock_res_dets.return_value = _RESOURCE_DETAILS
    with custom_mp_config(
        get_test_data_path().parent.joinpath("msticpyconfig-test.yaml")
    ):
        workspaces = sentinel_inst_loader.get_sentinel_workspaces(sub_id="123")
        assert isinstance(workspaces, dict)
        assert workspaces["WSName"] == _RES_ID


@patch(AzureData.__module__ + ".AzureData.get_resources")
@patch(AzureData.__module__ + ".AzureData.get_resource_details")
def test_set_default_workspace(mock_res_dets, mock_res, sentinel_inst_loader):
    """Test setting default workspace."""
    mock_res.return_value = _RESOURCES
    mock_res_dets.return_value = _RESOURCE_DETAILS
    with custom_mp_config(
        get_test_data_path().parent.joinpath("msticpyconfig-test.yaml")
    ):
        ws_name_config = WorkspaceConfig("WSName")
        sentinel_inst_loader.set_default_workspace(workspace="WSName")
        assert (
            sentinel_inst_loader.default_workspace_name
            == ws_name_config[WorkspaceConfig.CONF_WS_NAME_KEY]
        )
        assert (
            sentinel_inst_loader.default_subscription_id
            == ws_name_config[WorkspaceConfig.CONF_SUB_ID_KEY]
        )
        assert (
            sentinel_inst_loader.default_resource_group
            == ws_name_config[WorkspaceConfig.CONF_RES_GROUP_KEY]
        )

        with pytest.warns(UserWarning):
            sentinel_inst_loader.set_default_workspace(sub_id="123")

        sentinel_inst_loader.set_default_workspace(resource_id=_RES_ID)
        assert sentinel_inst_loader.default_workspace_name == "WSName"
        assert sentinel_inst_loader.default_subscription_id in _RES_ID
        assert sentinel_inst_loader.default_resource_group in _RES_ID
        assert sentinel_inst_loader.default_workspace_name in _RES_ID

        default_workspace = WorkspaceConfig()
        sentinel_inst_loader._resource_id = None
        sentinel_inst_loader.set_default_workspace()
        assert (
            sentinel_inst_loader.default_workspace_name
            == default_workspace[WorkspaceConfig.CONF_WS_NAME_KEY]
        )
        assert (
            sentinel_inst_loader.default_subscription_id
            == default_workspace[WorkspaceConfig.CONF_SUB_ID_KEY]
        )
        assert (
            sentinel_inst_loader.default_resource_group
            == default_workspace[WorkspaceConfig.CONF_RES_GROUP_KEY]
        )


_RES_ID_2 = (
    "/subscriptions/456/resourcegroups/RG2/providers/"
    "Microsoft.OperationalInsights/workspaces/WSName2"
)

_EXP_URL = f"https://management.azure.com{_RES_ID_2}"
_EXP_URL_2 = f"https://management.azure.com{_RES_ID}"


_CONNECT_TESTS = [
    (_RES_ID_2, None, None, None, _EXP_URL),
    (None, "WSName2", "456", "RG2", _EXP_URL),
    (None, "WSName2", None, None, _EXP_URL_2.replace("WSName", "WSName2")),
    (None, None, "456", None, _EXP_URL_2.replace("123", "456")),
    (None, None, None, "RG2", _EXP_URL_2.replace("RG", "RG2")),
    (None, None, None, None, _EXP_URL_2),
]


@pytest.mark.parametrize(
    "resource_id, workspace_name, subscription_id, resource_group, expected_url",
    _CONNECT_TESTS,
)
@patch.object(AzureData, "connect")
def test_sentinel_connect(
    mock_connect,
    resource_id,
    workspace_name,
    subscription_id,
    resource_group,
    expected_url,
    sentinel_inst_loader,
):
    """Test connect method with different parameter combinations."""
    del mock_connect
    # reset the workspace to known values
    sentinel_inst_loader.set_default_workspace(resource_id=_RES_ID)
    # Mock the logger to avoid actual logging
    with patch(MicrosoftSentinel.__module__ + ".logger") as mock_logger:
        # Mock the get_token function to return a test token
        with patch(
            MicrosoftSentinel.__module__ + ".get_token", return_value="test_token"
        ):
            sentinel_inst_loader.credentials = MagicMock()
            # Call the connect method with test parameters
            sentinel_inst_loader.connect(
                tenant_id="test_tenant_id",
                resource_id=resource_id,
                workspace_name=workspace_name,
                subscription_id=subscription_id,
                resource_group=resource_group,
            )

            # Assert that the URLs were set correctly
            assert sentinel_inst_loader.url == expected_url

            # Assert that the logger was called with the correct tenant_id
            mock_logger.info.assert_any_call("Using tenant id %s", "test_tenant_id")
            mock_logger.info.assert_any_call("Getting token for %s", "test_tenant_id")

            # test with parameters with None value removed
            non_null_params = {
                key: val
                for key, val in {
                    "resource_id": resource_id,
                    "workspace_name": workspace_name,
                    "subscription_id": subscription_id,
                    "resource_group": resource_group,
                }.items()
                if val
            }
            sentinel_inst_loader.connect(tenant_id="test_tenant_id", **non_null_params)
            # Assert that the URLs were set correctly
            assert sentinel_inst_loader.url == expected_url

            # Assert that the logger was called with the correct tenant_id
            mock_logger.info.assert_any_call("Using tenant id %s", "test_tenant_id")
            mock_logger.info.assert_any_call("Getting token for %s", "test_tenant_id")


_CONNECT_TESTS_2 = [
    (_RES_ID_2, None, None, None, _EXP_URL),
    (None, "WSName2", "456", "RG2", _EXP_URL),
    (None, "WSName2", None, None, ("WSName", "WSName2")),
    (None, None, "456", None, ("123", "456")),
    (None, None, None, "RG2", ("RG", "RG2")),
    (None, None, None, None, ("", "")),
]


@pytest.mark.parametrize(
    "resource_id, workspace_name, subscription_id, resource_group, expected_url",
    _CONNECT_TESTS_2,
)
@patch.object(AzureData, "connect")
def test_sentinel_connect_no_init_params(
    mock_connect,
    resource_id,
    workspace_name,
    subscription_id,
    resource_group,
    expected_url,
):
    """Test connect method where no initial parameters are set."""
    del mock_connect
    # reset the workspace to known values
    sentinel_inst = MicrosoftSentinel()
    # Mock the get_token function to return a test token
    with patch(MicrosoftSentinel.__module__ + ".get_token", return_value="test_token"):
        connect_kwargs = {
            "tenant_id": "test_tenant_id",
            "resource_id": resource_id,
            "workspace_name": workspace_name,
            "subscription_id": subscription_id,
            "resource_group": resource_group,
        }
        connect_kwargs = {key: val for key, val in connect_kwargs.items() if val}

        # Call the connect method with test parameters
        setattr(sentinel_inst, "credentials", MagicMock())
        sentinel_inst.connect(**connect_kwargs)
        if isinstance(expected_url, str):
            assert sentinel_inst.url == expected_url
        else:
            expected_url = sentinel_inst.url.replace(*expected_url)
