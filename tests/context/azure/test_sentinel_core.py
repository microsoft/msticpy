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

from msticpy.context.azure import AzureData, MicrosoftSentinel

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
    "subscriptions/123/resourceGroups/RG/providers/"
    "Microsoft.OperationalInsights/workspaces/WSNAME"
)

_RESOURCE_DETAILS = {"properties": {"workspaceResourceId": _RES_ID}}


def test_azuresent_init():
    """Test class initialization."""
    with custom_mp_config(
        get_test_data_path().parent.joinpath("msticpyconfig-test.yaml")
    ):
        sentinel_inst = MicrosoftSentinel(sub_id="123", res_grp="RG", ws_name="WSName")
        assert isinstance(sentinel_inst, MicrosoftSentinel)
        sentinel_inst = MicrosoftSentinel(res_id=_RES_ID)
        assert isinstance(sentinel_inst, MicrosoftSentinel)


@patch(MicrosoftSentinel.__module__ + ".AzureData.connect")
@patch(MicrosoftSentinel.__module__ + ".get_token")
def test_azuresent_connect_token(get_token: Mock, az_data_connect: Mock):
    """Test connect success."""
    token = "12398120398"

    with custom_mp_config(
        get_test_data_path().parent.joinpath("msticpyconfig-test.yaml")
    ):
        sentinel_inst = MicrosoftSentinel(res_id=_RES_ID)

        setattr(sentinel_inst, "set_default_workspace", MagicMock())
        sentinel_inst.connect(auth_methods=["env"], token=token)

        tenant_id = sentinel_inst._check_config(["tenant_id"])["tenant_id"]
        assert sentinel_inst._token == token
        az_data_connect.assert_called_once_with(
            auth_methods=["env"], tenant_id=tenant_id, silent=False
        )

        get_token.return_value = token
        token = "12398120398"
        sentinel_inst = MicrosoftSentinel(
            res_id=_RES_ID,
        )
        setattr(sentinel_inst, "set_default_workspace", MagicMock())
        sentinel_inst.connect(auth_methods=["env"])

        assert sentinel_inst._token == token
        get_token.assert_called_once_with(
            sentinel_inst.credentials,
            tenant_id=tenant_id,
            cloud=sentinel_inst.cloud,
        )


@patch(MicrosoftSentinel.__module__ + ".AzureData.connect")
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
        assert workspaces["WSNAME"] == _RES_ID
