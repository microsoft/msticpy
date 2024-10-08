# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""WorkspaceConfig test class."""
from pathlib import Path
from typing import NamedTuple

import pytest
import pytest_check as check

from msticpy.common import pkg_config
from msticpy.common.wsconfig import WorkspaceConfig

from ..unit_test_lib import custom_mp_config, get_test_data_path

_TEST_DATA = get_test_data_path()

# pylint: disable=protected-access


def test_wsconfig_default_ws():
    """Test WorkspaceConfig - no parameter."""
    test_config1 = Path(_TEST_DATA).joinpath(pkg_config._CONFIG_FILE)
    with custom_mp_config(test_config1):
        # Default workspace
        _default_ws = {
            "WorkspaceId": "52b1ab41-869e-4138-9e40-2a4457f09bf3",
            "TenantId": "72f988bf-86f1-41af-91ab-2d7cd011db49",
        }
        ws_config = WorkspaceConfig()
        check.is_in(WorkspaceConfig.CONF_WS_ID, ws_config)
        check.equal(ws_config[WorkspaceConfig.CONF_WS_ID], _default_ws["WorkspaceId"])
        check.is_in(WorkspaceConfig.CONF_TENANT_ID, ws_config)
        check.equal(ws_config[WorkspaceConfig.CONF_TENANT_ID], _default_ws["TenantId"])
        check.is_not_none(ws_config.code_connect_str)
        check.is_true(
            ws_config.code_connect_str.startswith("loganalytics://code().tenant(")
            and _default_ws["WorkspaceId"] in ws_config.code_connect_str
            and _default_ws["TenantId"] in ws_config.code_connect_str
        )


class WsTestCase(NamedTuple):
    """Test case for named workspaces."""

    ws_name: str
    ws_id: str
    tenant_id: str


Workspace2 = WsTestCase(
    "Workspace2",
    "a927809c-8142-43e1-96b3-4ad87cfe95a4",
    "69d28fd7-42a5-48bc-a619-af56397b9f28",
)
DefaultWS = WsTestCase(
    "Workspace1",
    "52b1ab41-869e-4138-9e40-2a4457f09bf3",
    "72f988bf-86f1-41af-91ab-2d7cd011db49",
)

WS_TEST_CASES = (
    ("MyTestWS2", Workspace2),
    ("mytestws2", Workspace2),
    ("Workspace2", Workspace2),
    ("workspace2", Workspace2),
    (Workspace2.ws_id, Workspace2),
    (None, DefaultWS),
)


@pytest.mark.parametrize("name, expected", WS_TEST_CASES)
def test_wsconfig_named_ws(name, expected):
    """Test WorkspaceConfig."""
    test_config1 = get_test_data_path().parent.joinpath("msticpyconfig-test.yaml")
    with custom_mp_config(test_config1):
        # Named workspace

        wstest_config = WorkspaceConfig(workspace=name)
        check.is_in(WorkspaceConfig.CONF_WS_ID, wstest_config)
        check.equal(wstest_config[WorkspaceConfig.CONF_WS_ID], expected.ws_id)
        check.equal(wstest_config[WorkspaceConfig.CONF_TENANT_ID], expected.tenant_id)
        check.equal(wstest_config[WorkspaceConfig.CONF_WS_NAME], expected.ws_name)
        check.is_not_none(wstest_config.code_connect_str)
        check.is_true(
            wstest_config.code_connect_str.startswith("loganalytics://code().tenant(")
            and expected.ws_id in wstest_config.code_connect_str
            and expected.tenant_id in wstest_config.code_connect_str
        )


def test_wsconfig_misc_funcs():
    """Test miscellaneous functions."""
    test_config1 = Path(_TEST_DATA).joinpath(pkg_config._CONFIG_FILE)
    with custom_mp_config(test_config1):
        ws_dict = WorkspaceConfig.list_workspaces()
        check.is_in("Default", ws_dict)
        check.equal(
            ws_dict["Default"]["WorkspaceId"],
            "52b1ab41-869e-4138-9e40-2a4457f09bf3",
        )
        check.equal(
            ws_dict["MyTestWS"]["WorkspaceId"],
            "a927809c-8142-43e1-96b3-4ad87cfe95a3",
        )
        ws_config = WorkspaceConfig()
        ws_config.prompt_for_ws()


def test_wsconfig_single_ws():
    """Test single workspace config."""
    test_config3 = Path(_TEST_DATA).joinpath("msticpyconfig-SingleAzSentSettings.yaml")
    with custom_mp_config(test_config3):
        # Single workspace
        _named_ws = {
            "WorkspaceId": "a927809c-8142-43e1-96b3-4ad87cfe95a3",
            "TenantId": "69d28fd7-42a5-48bc-a619-af56397b9f28",
        }
        wstest_config = WorkspaceConfig()
        check.is_in(WorkspaceConfig.CONF_WS_ID, wstest_config)
        check.is_not_none(wstest_config[WorkspaceConfig.CONF_WS_ID])
        check.equal(wstest_config[WorkspaceConfig.CONF_WS_ID], _named_ws["WorkspaceId"])
        check.is_in(WorkspaceConfig.CONF_TENANT_ID, wstest_config)
        check.equal(
            wstest_config[WorkspaceConfig.CONF_TENANT_ID], _named_ws["TenantId"]
        )
        check.is_not_none(wstest_config.code_connect_str)
        check.is_true(
            wstest_config.code_connect_str.startswith("loganalytics://code().tenant(")
            and _named_ws["WorkspaceId"] in wstest_config.code_connect_str
            and _named_ws["TenantId"] in wstest_config.code_connect_str
        )


_TENANT = "d8d9d2f2-5d2d-4d7e-9c5c-5d6d9d1d8d9d"
_WS_ID = "f8d9d2f2-5d2d-4d7e-9c5c-5d6d9d1d8d9e"
_CLI_ID = "18d9d2f2-5d2d-4d7e-9c5c-5d6d9d1d8d9f"
_WS_NAME = "Workspace"
_CONFIG_STR_TEST_CASES = (
    (
        f"loganalytics://code;workspace='{_WS_ID}';alias='{_WS_NAME}';tenant='{_TENANT}'",
        True,
    ),
    (
        (
            f"loganalytics://tenant='{_TENANT}';clientid='{_CLI_ID}';"
            f"clientsecret='[PLACEHOLDER]';workspace='{_WS_ID}';alias='{_WS_NAME}'"
        ),
        True,
    ),
    (
        (
            "loganalytics://username='User';password='[PLACEHOLDER]';"
            f"workspace='{_WS_ID}';alias='{_WS_NAME}';tenant='{_TENANT}'"
        ),
        True,
    ),
    (
        f"loganalytics://anonymous;workspace='{_WS_ID}';alias='{_WS_NAME}';tenant='{_TENANT}'",
        True,
    ),
    (
        f"loganalytics://code().tenant('{_TENANT}').workspace('{_WS_ID}').alias('{_WS_NAME}')",
        True,
    ),
    (f"loganalytics://code;workspace='{_WS_ID}';alias='{_WS_NAME}'", False),
    (f"loganalytics://code;alias='{_WS_NAME}';tenant='{_TENANT}'", False),
)


@pytest.mark.parametrize("config_str, is_valid", _CONFIG_STR_TEST_CASES)
def test_wsconfig_config_str(config_str, is_valid):
    """Test capture of config from connections strings."""
    if is_valid:
        ws = WorkspaceConfig.from_connection_string(config_str)
        if "workspace" in config_str:
            check.equal(ws["workspace_id"], _WS_ID)
        if "tenant" in config_str:
            check.equal(ws["tenant_id"], _TENANT)
        if "alias" in config_str:
            check.equal(ws["workspace_name"], _WS_NAME)
    else:
        with pytest.raises(ValueError):
            WorkspaceConfig.from_connection_string(config_str)
