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
from msticpy.common.pkg_config import set_config
from msticpy.data import QueryProvider
from msticpy.init import user_config

# pylint: disable=redefined-outer-name, unused-import, ungrouped-imports
try:
    import msticnb  # noqa: F401

    _NOTEBOOKLETS = True
except ImportError:
    _NOTEBOOKLETS = False

try:
    from msticpy.init.pivot import Pivot  # noqa: F401

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
    AzureData:
      auth_methods: ['cli','interactive']
      connect: False
    AzureSentinelAPI:
      auth_methods: ['cli','interactive']
      res_id: "subscriptions/bab8ff42-bb7b-42ca-873e-d13c5eb5ffb8/resourceGroups/TestRG/providers/Microsoft.OperationalInsights/workspaces/SentinelWorkspace"
      connect: False
"""

_NOTEBOOKLETS = False


@pytest.fixture(scope="module")
def mp_settings():
    """Return test settings."""
    settings_dict = yaml.safe_load(CONFIG_TEXT)
    if not _NOTEBOOKLETS and settings_dict["UserDefaults"].get(
        "LoadComponents", {}
    ).get("Notebooklets"):
        del settings_dict["UserDefaults"]["LoadComponents"]["Notebooklets"]
    return settings_dict


@pytest.mark.filterwarnings("ignore::UserWarning")
def test_user_config(mp_settings):
    """Test user config."""
    mpcfg_path = os.environ.get("MSTICPYCONFIG")
    with custom_mp_config(mp_path=mpcfg_path):
        set_config("UserDefaults", mp_settings.get("UserDefaults"))
        prov_dict = user_config.load_user_defaults()

    check.is_in("qry_asi", prov_dict)
    check.is_instance(prov_dict["qry_asi"], QueryProvider)
    check.equal(prov_dict["qry_asi"].environment, "MSSentinel")
    check.is_in("qry_soc", prov_dict)
    check.is_instance(prov_dict["qry_soc"], QueryProvider)
    check.equal(prov_dict["qry_asi"].environment, "MSSentinel")
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
