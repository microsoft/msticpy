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

from ..msticpy.nbtools import pkg_config

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
        self.assertEqual(4, len(settings["TIProviders"]))
        self.assertIsInstance(settings["TIProviders"], dict)
        for _, prov in settings["TIProviders"].items():
            self.assertIn("Args", prov)
            self.assertIn("Primary", prov)
            self.assertIn("Provider", prov)
            self.assertIsInstance(prov["Args"], dict)
            self.assertIn("ApiID", prov["Args"])
            self.assertIn("AuthKey", prov["Args"])
            if isinstance(prov["Args"]["ApiID"], dict):
                self.assertTrue(
                    "EnvironmentVar" in prov["Args"]["ApiID"]
                    or "KeyVaultURI" in prov["Args"]["ApiID"]
                )
            else:
                self.assertIsInstance(prov["Args"]["ApiID"], str)
