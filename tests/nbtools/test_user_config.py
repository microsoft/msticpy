# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Module docstring."""
import os
import pytest
import pytest_check as check
import yaml

import msticpy
from msticpy.data import QueryProvider
from msticpy.nbtools import user_config
from msticpy.common.pkg_config import settings

# pylint: disable=redefined-outer-name, unused-import, ungrouped-imports
try:
    import msticnb  # noqa: F401

    _NOTEBOOKLETS = True
except ImportError:
    _NOTEBOOKLETS = False

try:
    from msticpy.datamodel.pivot import Pivot  # noqa: F401

    _PIVOT = True
except ImportError:
    _PIVOT = False

from ..unit_test_lib import custom_mp_config

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
      provider: IpStackLookup
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
      auth_methods: ['cli','interactive']
      connect: False
"""


@pytest.fixture(scope="module")
def mp_settings():
    """Return test settings."""
    settings_dict = yaml.safe_load(CONFIG_TEXT)
    if not _NOTEBOOKLETS and settings_dict.get("LoadComponents", {}).get(
        "Notebooklets"
    ):
        del settings_dict["LoadComponents"]["Notebooklets"]
    if not _PIVOT and settings_dict.get("LoadComponents", {}).get("Pivot"):
        del settings_dict["LoadComponents"]["Pivot"]
    return settings_dict


def test_user_config(mp_settings):
    """Test user config."""
    mpcfg_path = os.environ.get("MSTICPYCONFIG")
    with custom_mp_config(mp_path=mpcfg_path):
        settings["UserDefaults"] = mp_settings.get("UserDefaults")
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
