# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Microsoft Sentinel core unit tests."""
from typing import Any, Dict, NamedTuple
from unittest.mock import MagicMock, Mock, patch

import pandas as pd
import pytest
import pytest_check as check
from azure.core.exceptions import ClientAuthenticationError

from msticpy.common.exceptions import MsticpyParameterError
from msticpy.common.wsconfig import WorkspaceConfig
from msticpy.context.azure import AzureData, MicrosoftSentinel

from ...unit_test_lib import custom_mp_config

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
_RESOURCE_DETAILS = {"properties": {"workspaceResourceId": "ABC"}}

_RES_ID = (
    "subscriptions/123/resourceGroups/RG/providers/"
    "Microsoft.OperationalInsights/workspaces/WSNAME"
)


@patch(MicrosoftSentinel.__module__ + ".AzureData.connect")
@patch(MicrosoftSentinel.__module__ + ".get_token")
def test_azuresent_connect_token(get_token: Mock, az_data_connect: Mock):
    """Test connect success."""
    token = "12398120398"

    sentinel_inst = MicrosoftSentinel(res_id=_RES_ID)

    setattr(sentinel_inst, "set_default_workspace", MagicMock())
    sentinel_inst.connect(auth_methods=["env"], token=token)

    tenant_id = sentinel_inst._check_config(["tenant_id"])["tenant_id"]
    assert sentinel_inst.token == token
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

    assert sentinel_inst.token == token
    get_token.assert_called_once_with(
        sentinel_inst.credentials, tenant_id=tenant_id, cloud=sentinel_inst.user_cloud
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
    sentinel_inst = MicrosoftSentinel(sub_id="123", res_grp="RG", ws_name="WSName")
    sentinel_inst.connect()
    sentinel_inst.connected = True
    sentinel_inst.token = "123"
    return sentinel_inst


@pytest.mark.filterwarnings("ignore::DeprecationWarning")
@patch(AzureData.__module__ + ".AzureData.get_resources")
@patch(AzureData.__module__ + ".AzureData.get_resource_details")
def test_azuresent_workspaces(mock_res_dets, mock_res, sentinel_inst_loader):
    """Test Sentinel workspaces feature."""
    mock_res.return_value = _RESOURCES
    mock_res_dets.return_value = _RESOURCE_DETAILS
    workspaces = sentinel_inst_loader.get_sentinel_workspaces(sub_id="123")
    assert isinstance(workspaces, dict)
    assert workspaces["ABC"] == "ABC"


class SentinelOption(NamedTuple):
    """Test cases for MSSentinel init and connect."""

    name: str
    init_args: Dict[str, str]
    connect_args: Dict[str, str]
    exception: Any = None


_RES_ID = (
    "subscriptions/cd928da3-dcde-42a3-aad7-d2a1268c2f48/resourceGroups/RG/providers/"
    "Microsoft.OperationalInsights/workspaces/WSName"
)
_WS_PARAMS = {
    "sub_id": "cd928da3-dcde-42a3-aad7-d2a1268c2f48",
    "res_grp": "RG",
    "workspace": "WSName",
}
_TEST_DEFAULT_WS = {
    "WorkspaceId": "52b1ab41-869e-4138-9e40-2a4457f09bf3",
    "TenantId": "72f988bf-86f1-41af-91ab-2d7cd011db49",
    "SubscriptionId": "cd928da3-dcde-42a3-aad7-d2a1268c2f48",
    "ResourceGroup": "ABC",
    "WorkspaceName": "Workspace1",
}

_SENTINEL_INPUTS = [
    SentinelOption("init-default", {}, {}, None),
    SentinelOption("init-res_id", {"res_id": _RES_ID}, {}, None),
    SentinelOption("init-ws_name", {"ws_name": "WSName"}, {}, None),
    SentinelOption("init-params", _WS_PARAMS, {}, None),
    SentinelOption("conn-res_id", {}, {"res_id": _RES_ID}, None),
    SentinelOption(
        "conn-ws_name", {"ws_name": "WSName"}, {"ws_name": "MyTestWS"}, None
    ),
    SentinelOption("conn-params", {}, _WS_PARAMS, None),
    SentinelOption(
        "init-params-fail", {"sub_id": _WS_PARAMS["sub_id"]}, {}, MsticpyParameterError
    ),
]


@patch(MicrosoftSentinel.__module__ + ".AzureData.connect")
@patch(MicrosoftSentinel.__module__ + ".get_token")
@pytest.mark.parametrize(
    "test_opts", _SENTINEL_INPUTS, ids=[t.name for t in _SENTINEL_INPUTS]
)
@pytest.mark.filterwarnings("ignore::DeprecationWarning")
def test_sentinel_connect_options(get_token: Mock, az_data_connect: Mock, test_opts):
    """Test initialization and connect success with diff parameters."""
    if test_opts.exception:
        with pytest.raises(MsticpyParameterError):
            sentinel = MicrosoftSentinel(**(test_opts.init_args))
        return

    default_ws_name = _TEST_DEFAULT_WS["WorkspaceName"]
    with custom_mp_config("tests/msticpyconfig-test.yaml"):
        sentinel = MicrosoftSentinel(**(test_opts.init_args))
        if test_opts.init_args:
            check.equal(sentinel.default_subscription, _WS_PARAMS["sub_id"])
            check.equal(sentinel._default_resource_group, _WS_PARAMS["res_grp"])
            check.equal(sentinel.default_workspace_name, _WS_PARAMS["workspace"])
            check.is_not_none(sentinel.url)
            check.greater_equal(len(sentinel.sent_urls), 4)
            check.is_instance(sentinel.workspace_config, WorkspaceConfig)
            check.equal(sentinel.default_subscription, _WS_PARAMS["sub_id"])
            check.equal(sentinel._default_resource_group, _WS_PARAMS["res_grp"])
            check.equal(sentinel.default_workspace_name, _WS_PARAMS["workspace"])
            default_ws_name = sentinel.default_workspace_name
        else:
            check.is_instance(sentinel.workspace_config, WorkspaceConfig)
            check.equal(
                sentinel.default_workspace_name, _TEST_DEFAULT_WS["WorkspaceName"]
            )
            # check.equal(sentinel.workspace_config["WorkspaceName"], _TEST_DEFAULT_WS["WorkspaceName"])
            # check.equal(sentinel.workspace_config["SubscriptionId"], _TEST_DEFAULT_WS["SubscriptionId"])

        sentinel.connect(**(test_opts.connect_args))
        if test_opts.connect_args:
            if test_opts.connect_args.get("ws_name") == "MyTestWS":
                expected = WorkspaceConfig("MyTestWS")
                check.equal(
                    sentinel.default_subscription,
                    expected[WorkspaceConfig.CONF_SUB_ID_KEY],
                )
                check.equal(
                    sentinel._default_resource_group,
                    expected[WorkspaceConfig.CONF_RES_GROUP_KEY],
                )
                check.equal(
                    sentinel.default_workspace_name,
                    expected[WorkspaceConfig.CONF_WS_NAME_KEY],
                )
                check.is_not_none(sentinel.url)
                check.greater_equal(len(sentinel.sent_urls), 4)
                check.is_instance(sentinel.workspace_config, WorkspaceConfig)
                check.equal(
                    sentinel.default_subscription,
                    expected[WorkspaceConfig.CONF_SUB_ID_KEY],
                )
                check.equal(
                    sentinel._default_resource_group,
                    expected[WorkspaceConfig.CONF_RES_GROUP_KEY],
                )
                check.equal(
                    sentinel.default_workspace_name,
                    expected[WorkspaceConfig.CONF_WS_NAME_KEY],
                )

            else:
                check.equal(sentinel.default_subscription, _WS_PARAMS["sub_id"])
                check.equal(sentinel._default_resource_group, _WS_PARAMS["res_grp"])
                check.equal(sentinel.default_workspace_name, _WS_PARAMS["workspace"])
                check.is_not_none(sentinel.url)
                check.greater_equal(len(sentinel.sent_urls), 4)
                check.is_instance(sentinel.workspace_config, WorkspaceConfig)
                check.equal(sentinel.default_subscription, _WS_PARAMS["sub_id"])
                check.equal(sentinel._default_resource_group, _WS_PARAMS["res_grp"])
                check.equal(sentinel.default_workspace_name, _WS_PARAMS["workspace"])
        else:
            check.is_instance(sentinel.workspace_config, WorkspaceConfig)
            check.equal(sentinel.default_workspace_name, default_ws_name)
    print(test_opts)
