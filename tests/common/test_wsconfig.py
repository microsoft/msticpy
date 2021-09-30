# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""WorkspaceConfig test class."""
import io
import unittest
from contextlib import redirect_stdout
from pathlib import Path

from msticpy.common import pkg_config
from msticpy.common.wsconfig import WorkspaceConfig

from ..unit_test_lib import get_test_data_path, custom_mp_config

_TEST_DATA = get_test_data_path()

# pylint: disable=protected-access


class TestPkgConfig(unittest.TestCase):
    """Unit test class."""

    def test_wsconfig_default_ws(self):
        """Test WorkspaceConfig."""
        test_config1 = Path(_TEST_DATA).joinpath(pkg_config._CONFIG_FILE)
        with custom_mp_config(test_config1):
            # Default workspace
            _DEF_WS = {
                "WorkspaceId": "52b1ab41-869e-4138-9e40-2a4457f09bf3",
                "TenantId": "72f988bf-86f1-41af-91ab-2d7cd011db49",
            }
            ws_config = WorkspaceConfig()
            self.assertIn("workspace_id", ws_config)
            self.assertEqual(ws_config["workspace_id"], _DEF_WS["WorkspaceId"])
            self.assertIn("tenant_id", ws_config)
            self.assertEqual(ws_config["tenant_id"], _DEF_WS["TenantId"])
            self.assertIsNotNone(ws_config.code_connect_str)
            self.assertTrue(
                ws_config.code_connect_str.startswith("loganalytics://code().tenant(")
                and _DEF_WS["WorkspaceId"] in ws_config.code_connect_str
                and _DEF_WS["TenantId"] in ws_config.code_connect_str
            )

    def test_wsconfig_named_ws(self):
        """Test WorkspaceConfig."""
        test_config1 = Path(_TEST_DATA).joinpath(pkg_config._CONFIG_FILE)
        with custom_mp_config(test_config1):
            # Named workspace
            _NAMED_WS = {
                "WorkspaceId": "a927809c-8142-43e1-96b3-4ad87cfe95a3",
                "TenantId": "69d28fd7-42a5-48bc-a619-af56397b9f28",
            }
            wstest_config = WorkspaceConfig(workspace="MyTestWS")
            self.assertIn("workspace_id", wstest_config)
            self.assertIsNotNone(wstest_config["workspace_id"])
            self.assertEqual(wstest_config["workspace_id"], _NAMED_WS["WorkspaceId"])
            self.assertIn("tenant_id", wstest_config)
            self.assertEqual(wstest_config["tenant_id"], _NAMED_WS["TenantId"])
            self.assertIsNotNone(wstest_config.code_connect_str)
            self.assertTrue(
                wstest_config.code_connect_str.startswith(
                    "loganalytics://code().tenant("
                )
                and _NAMED_WS["WorkspaceId"] in wstest_config.code_connect_str
                and _NAMED_WS["TenantId"] in wstest_config.code_connect_str
            )

    def test_wsconfig_config_json_fallback(self):
        # Fallback to config.json
        test_config2 = Path(_TEST_DATA).joinpath("msticpyconfig-noAzSentSettings.yaml")
        with custom_mp_config(test_config2):
            _NAMED_WS = {
                "WorkspaceId": "9997809c-8142-43e1-96b3-4ad87cfe95a3",
                "TenantId": "99928fd7-42a5-48bc-a619-af56397b9f28",
            }
            wrn_mssg = io.StringIO()
            with redirect_stdout(wrn_mssg):
                wstest_config = WorkspaceConfig()
            self.assertIn("Could not find Azure Sentinel settings", wrn_mssg.getvalue())
            self.assertIn("workspace_id", wstest_config)
            self.assertIsNotNone(wstest_config["workspace_id"])
            self.assertEqual(wstest_config["workspace_id"], _NAMED_WS["WorkspaceId"])
            self.assertIn("tenant_id", wstest_config)
            self.assertEqual(wstest_config["tenant_id"], _NAMED_WS["TenantId"])
            self.assertIsNotNone(wstest_config.code_connect_str)
            self.assertTrue(
                wstest_config.code_connect_str.startswith(
                    "loganalytics://code().tenant("
                )
                and _NAMED_WS["WorkspaceId"] in wstest_config.code_connect_str
                and _NAMED_WS["TenantId"] in wstest_config.code_connect_str
            )

    def test_wsconfig_misc_funcs(self):
        """Test miscellaneous functions."""
        test_config1 = Path(_TEST_DATA).joinpath(pkg_config._CONFIG_FILE)
        with custom_mp_config(test_config1):
            ws_dict = WorkspaceConfig.list_workspaces()
            self.assertIn("Default", ws_dict)
            self.assertEqual(
                ws_dict["Default"]["WorkspaceId"],
                "52b1ab41-869e-4138-9e40-2a4457f09bf3",
            )
            self.assertEqual(
                ws_dict["MyTestWS"]["WorkspaceId"],
                "a927809c-8142-43e1-96b3-4ad87cfe95a3",
            )
            ws_config = WorkspaceConfig()
            ws_config.prompt_for_ws()
