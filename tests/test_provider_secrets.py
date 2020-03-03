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

from ..msticpy.common.secret_settings import SecretsClient
from ..msticpy.nbtools import pkg_config
from ..msticpy.sectools.provider_settings import get_provider_settings
from ..msticpy.sectools.geoip import IPStackLookup, GeoLiteLookup
from ..msticpy.sectools.tiproviders import ProviderSettings, get_provider_settings


_test_data_folders = [
    d for d, _, _ in os.walk(os.getcwd()) if d.endswith("/tests/testdata")
]
if len(_test_data_folders) == 1:
    _TEST_DATA = _test_data_folders[0]
else:
    _TEST_DATA = "./tests/testdata"


class TestSecretsConfig(unittest.TestCase):
    """Unit test class."""

    def test_config_load(self):
        test_config = Path(_TEST_DATA).joinpath("msticpyconfig-kv.yaml")
        os.environ[pkg_config._CONFIG_ENV_VAR] = str(test_config)

        with warnings.catch_warnings():
            # We want to ignore warnings from missing config
            warnings.simplefilter("ignore", category=UserWarning)

            pkg_config.refresh_config()
            # ti_settings = get_provider_settings()

    # def test_geo_ip_settings(self):
    #     test_config1 = Path(_TEST_DATA).joinpath(pkg_config._CONFIG_FILE)
    #     os.environ[pkg_config._CONFIG_ENV_VAR] = str(test_config1)
    #     if "MAXMIND_AUTH" not in os.environ:
    #         os.environ["MAXMIND_AUTH"] = "Testkey"

    #     pkg_config.refresh_config()

    #     with open(test_config1) as f_handle:
    #         config_settings = yaml.safe_load(f_handle)
    #     conf_dbpath = (
    #         config_settings.get("OtherProviders", {})
    #         .get("GeoIPLite", {})
    #         .get("Args", {})
    #         .get("DBFolder")
    #     )
    #     conf_dbpath = str(Path(conf_dbpath).expanduser())

    #     with warnings.catch_warnings():
    #         # We want to ignore warnings from failure to download DB file
    #         warnings.simplefilter("ignore", category=UserWarning)
    #         geoip_lite = GeoLiteLookup()
    #     self.assertIsInstance(geoip_lite._api_key, str)
    #     self.assertEqual(geoip_lite._api_key, os.environ["MAXMIND_AUTH"])

    #     self.assertEqual(geoip_lite._dbfolder, conf_dbpath)

    #     ipstack = IPStackLookup()
    #     self.assertEqual(ipstack._api_key, "987654321-222")
