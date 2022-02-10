# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Pkg_config test class."""
import unittest
import os
from pathlib import Path
import warnings

import pytest
import yaml

from msticpy.common import pkg_config
from msticpy.data.context.geoip import IPStackLookup, GeoLiteLookup

from ..unit_test_lib import get_test_data_path, custom_mp_config

_TEST_DATA = get_test_data_path()

# pylint: disable=protected-access


class TestPkgConfig(unittest.TestCase):
    """Unit test class."""

    def test_load_default(self):
        """Test load default settings."""
        self.assertTrue(hasattr(pkg_config, "settings"))
        self.assertTrue(hasattr(pkg_config, "default_settings"))
        self.assertTrue(hasattr(pkg_config, "custom_settings"))
        settings = pkg_config.settings
        self.assertIn("QueryDefinitions", settings)
        self.assertIn("Default", settings["QueryDefinitions"])
        self.assertEqual(1, len(settings["QueryDefinitions"]["Default"]))
        for path in settings["QueryDefinitions"]["Default"]:
            self.assertTrue(type(path), str)
            path = "data/" + path
            self.assertTrue(
                Path(pkg_config.__file__)
                .resolve()
                .parent.parent.joinpath(path)
                .is_dir()
            )

    def test_custom_config(self):
        """Test load queries from custom path."""
        test_config1 = Path(_TEST_DATA).joinpath(pkg_config._CONFIG_FILE)
        with custom_mp_config(test_config1):

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
                self.assertTrue(
                    Path(__file__).resolve().parent.parent.joinpath(path).is_dir()
                )

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

    def test_geo_ip_settings(self):
        """Test get geo_ip_settings."""
        if "MAXMIND_AUTH" not in os.environ:
            os.environ["MAXMIND_AUTH"] = "Testkey"
        test_config1 = Path(_TEST_DATA).joinpath(pkg_config._CONFIG_FILE)
        with custom_mp_config(test_config1):

            with open(test_config1, encoding="utf-8") as f_handle:
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

    @pytest.mark.skipif(
        os.environ.get("MSTICPY_BUILD_SOURCE", "").casefold() == "fork",
        reason="External fork.",
    )
    def test_validate_config(self):
        """Test config validation function."""
        test_config1 = Path(_TEST_DATA).joinpath(pkg_config._CONFIG_FILE)
        with custom_mp_config(test_config1):
            results = pkg_config.validate_config()
            self.assertGreater(len(results[0]), 1)
            # save env vars
            vt_auth_save = os.environ.get("VTAUTHKEY", "")
            xf_id__save = os.environ.get("XFORCE_ID", "")
            xf_auth_save = os.environ.get("XFORCE_KEY", "")
            xf_auth_save = os.environ.get("MAXMIND_AUTH", "")
            # set to some value
            os.environ["VTAUTHKEY"] = "myXfId"
            os.environ["XFORCE_ID"] = "myXfId"
            os.environ["XFORCE_KEY"] = "myXfId"
            os.environ["MAXMIND_AUTH"] = "myXfId"
            pkg_config.refresh_config()
            results = pkg_config.validate_config()
            self.assertEqual(results, ([], []))

            # restore env vars to original
            os.environ["VTAUTHKEY"] = vt_auth_save
            os.environ["XFORCE_ID"] = xf_id__save
            os.environ["XFORCE_KEY"] = xf_auth_save
            os.environ["MAXMIND_AUTH"] = xf_auth_save
