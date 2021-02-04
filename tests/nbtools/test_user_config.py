# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Module docstring."""
from unittest.mock import patch, Mock

import pytest
import pytest_check as check
import yaml

import msticpy
from msticpy.data import QueryProvider
from msticpy.nbtools import user_config

# pylint: disable=redefined-outer-name, unused-import, ungrouped-imports
try:
    import msticnb

    _NOTEBOOKLETS = True
except ImportError:
    _NOTEBOOKLETS = False

try:
    from msticpy.datamodel.pivot import Pivot

    _PIVOT = True
except ImportError:
    _PIVOT = False

__author__ = "Ian Hellen"


CONFIG_TEXT = """
UserDefaults:
  # List of query providers to load
  QueryProviders:
    AzureSentinel:
      Default:
        alias: asi
        connect: False
      CyberSoc:
        alias: soc
        connect: False
    Splunk:
      connect: False
    LocalData:
      alias: local
  # List of other providers/components to load
  LoadComponents:
    TILookup:
    GeoIpLookup:
      provider: GeoLiteLookup
    Notebooklets:
      query_provider:
        LocalData:
          workspace: CyberSoc
          some_param: some_value
    Pivot:
    AzureData:
      auth_methods: ['cli','interactive']
      connect: False
    AzureSentinelAPI:
      auth_methods: ['env','interactive']
      connect: False
"""


@pytest.fixture(scope="module")
def mp_settings():
    """Return test settings."""
    settings_dict = yaml.safe_load(CONFIG_TEXT)
    if not _NOTEBOOKLETS:
        del settings_dict["LoadComponents"]["Notebooklets"]
    if not _PIVOT:
        del settings_dict["LoadComponents"]["Pivot"]
    return settings_dict


@patch("msticpy.nbtools.user_config.settings")
def test_user_config(settings, mp_settings):
    """Test user config."""
    settings.get = Mock()
    settings.get.return_value = mp_settings.get("UserDefaults")
    prov_dict = user_config.load_user_defaults()

    check.is_in("qry_asi", prov_dict)
    check.is_instance(prov_dict["qry_asi"], QueryProvider)
    check.equal(prov_dict["qry_asi"].environment, "AzureSentinel")
    check.is_in("qry_soc", prov_dict)
    check.is_instance(prov_dict["qry_soc"], QueryProvider)
    check.equal(prov_dict["qry_asi"].environment, "AzureSentinel")
    check.is_in("qry_splunk", prov_dict)
    check.is_instance(prov_dict["qry_splunk"], QueryProvider)
    check.equal(prov_dict["qry_splunk"].environment, "Splunk")
    check.is_in("qry_local", prov_dict)
    check.is_instance(prov_dict["qry_local"], QueryProvider)
    check.is_true(prov_dict["qry_local"].connected)
    check.equal(prov_dict["qry_local"].environment, "LocalData")

    check.is_in("ti_lookup", prov_dict)
    check.is_in("geoip", prov_dict)
    check.is_in("az_data", prov_dict)
    check.is_in("azs_api", prov_dict)

    check.is_true(hasattr(msticpy, "current_providers"))
    check.equal(getattr(msticpy, "current_providers"), prov_dict)
