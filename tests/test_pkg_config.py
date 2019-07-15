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

from .. msticpy.nbtools import pkg_config

_test_data_folders = [d for d, _, _ in os.walk(os.getcwd()) if d.endswith('/tests/testdata')]
if len(_test_data_folders) == 1:
    _TEST_DATA = _test_data_folders[0]
else:
    _TEST_DATA = './tests/testdata'


class TestPkgConfig(unittest.TestCase):
    """Unit test class."""
    
    def test_load_default(self):
        self.assertTrue(hasattr(pkg_config, "settings"))
        self.assertTrue(hasattr(pkg_config, "default_settings"))
        self.assertTrue(hasattr(pkg_config, "custom_settings"))

        settings = pkg_config.settings
        self.assertIn("QueryDefinitions", settings)
        self.assertIn("Default", settings["QueryDefinitions"])

        for path in settings["QueryDefinitions"]["Default"]:
            self.assertTrue(Path(path).is_dir())
            yml_files = Path(path).glob("*.yaml")
            self.assertGreaterEqual(len(yml_files), 4)

    def test_custom_config(self):
        os.environ[] 