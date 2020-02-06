# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""datq query test class."""
import unittest
import os
from pathlib import Path
from typing import Union, Any, Tuple
import warnings

import yaml

from ..msticpy.nbtools import pkg_config
from ..msticpy.nbtools.wsconfig import WorkspaceConfig
from ..msticpy.sectools.geoip import IPStackLookup, GeoLiteLookup

_test_data_folders = [
    d for d, _, _ in os.walk(os.getcwd()) if d.endswith("/tests/testdata")
]
if len(_test_data_folders) == 1:
    _TEST_DATA = _test_data_folders[0]
else:
    _TEST_DATA = "./tests/testdata"


class TestPkgConfig(unittest.TestCase):
    """Unit test class."""

    def test_load_default(self):
        self.assertTrue(hasattr(pkg_config, "settings"))
        self.assertTrue(hasattr(pkg_config, "default_settings"))
        self.assertTrue(hasattr(pkg_config, "custom_settings"))
        settings = pkg_config.settings
        self.assertIn("QueryDefinitions", settings)
        self.assertIn("Default", settings["QueryDefinitions"])
        self.assertEqual(1, len(settings["QueryDefinitions"]["Default"]))
        for path in settings["QueryDefinitions"]["Default"]:
            self.assertTrue(type(path), str)
            path = "msticpy/data/" + path
            self.assertTrue(
                Path(__file__).resolve().parent.parent.joinpath(path).is_dir()
            )

    def test_custom_config(self):
        test_config1 = Path(_TEST_DATA).joinpath(pkg_config._CONFIG_FILE)
        os.environ[pkg_config._CONFIG_ENV_VAR] = str(test_config1)
        pkg_config.refresh_config()
        self.assertTrue(hasattr(pkg_config, "settings"))
        self.assertTrue(hasattr(pkg_config, "default_settings"))
        self.assertTrue(hasattr(pkg_config, "custom_settings"))
        settings = pkg_config.settings

        # Query Definitions
        self.assertIn("QueryDefinitions", settings)
        self.assertIn("Default", settings["QueryDefinitions"])
        self.assertEqual(1, len(settings["QueryDefinitions"]["Custom"]))
        for path in settings["QueryDefinitions"]["Custom"]:
            self.assertTrue(type(path), str)
            self.assertTrue(Path(__file__).resolve().parent.joinpath(path).is_dir())

        # TI Providers
        self.assertGreaterEqual(len(settings["TIProviders"]), 4)
        self.assertIsInstance(settings["TIProviders"], dict)
        for _, prov in settings["TIProviders"].items():

            self.assertIn("Primary", prov)
            self.assertIn("Provider", prov)
            if "Args" in prov:
                self.assertIsInstance(prov["Args"], dict)
                for arg_name, arg_val in prov["Args"].items():
                    self.assertIn(
                        arg_name, ["ApiID", "AuthKey", "WorkspaceID", "TenantID"]
                    )
                    self.assertTrue(
                        isinstance(arg_val, str)
                        or "EnvironmentVar" in arg_val
                        or "KeyVaultURI" in arg_val
                    )

    def test_wsconfig(self):
        test_config1 = Path(_TEST_DATA).joinpath(pkg_config._CONFIG_FILE)
        os.environ[pkg_config._CONFIG_ENV_VAR] = str(test_config1)
        pkg_config.refresh_config()

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
            wstest_config.code_connect_str.startswith("loganalytics://code().tenant(")
            and _NAMED_WS["WorkspaceId"] in wstest_config.code_connect_str
            and _NAMED_WS["TenantId"] in wstest_config.code_connect_str
        )

    def test_geo_ip_settings(self):
        test_config1 = Path(_TEST_DATA).joinpath(pkg_config._CONFIG_FILE)
        os.environ[pkg_config._CONFIG_ENV_VAR] = str(test_config1)
        if "MAXMIND_AUTH" not in os.environ:
            os.environ["MAXMIND_AUTH"] = "Testkey"

        pkg_config.refresh_config()

        with open(test_config1) as f_handle:
            config_settings = yaml.safe_load(f_handle)
        conf_dbpath = (
            config_settings.get("OtherProviders", {})
            .get("GeoIPLite", {})
            .get("Args", {})
            .get("DBFolder")
        )
        conf_dbpath = str(Path(conf_dbpath).expanduser())

        with warnings.catch_warnings():
            # We want to ignore warnings from failure to download DB file
            warnings.simplefilter("ignore", category=UserWarning)
            geoip_lite = GeoLiteLookup()
        self.assertIsInstance(geoip_lite._api_key, str)
        self.assertEqual(geoip_lite._api_key, os.environ["MAXMIND_AUTH"])

        self.assertEqual(geoip_lite._dbfolder, conf_dbpath)

        ipstack = IPStackLookup()
        self.assertEqual(ipstack._api_key, "987654321-222")
