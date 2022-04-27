# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Pkg_config test class."""
import os
from pathlib import Path
import warnings

import httpx
import pytest
import pytest_check as check
import yaml

from msticpy.common import pkg_config
from msticpy.sectools.geoip import IPStackLookup, GeoLiteLookup

from ..unit_test_lib import get_test_data_path, custom_mp_config

_TEST_DATA = get_test_data_path()

# pylint: disable=protected-access


def test_load_default():
    """Test load default settings."""
    check.is_true(hasattr(pkg_config, "settings"))
    check.is_true(hasattr(pkg_config, "default_settings"))
    check.is_true(hasattr(pkg_config, "custom_settings"))
    settings = pkg_config.settings
    check.is_in("QueryDefinitions", settings)
    check.is_in("Default", settings["QueryDefinitions"])
    check.equal(1, len(settings["QueryDefinitions"]["Default"]))
    for path in settings["QueryDefinitions"]["Default"]:
        check.is_true(type(path), str)
        path = f"data/{path}"
        check.is_true(
            Path(pkg_config.__file__).resolve().parent.parent.joinpath(path).is_dir()
        )


def test_custom_config(self):
    """Test load queries from custom path."""
    test_config1 = Path(_TEST_DATA).joinpath(pkg_config._CONFIG_FILE)
    with custom_mp_config(test_config1):

        check.is_true(hasattr(pkg_config, "settings"))
        check.is_true(hasattr(pkg_config, "default_settings"))
        check.is_true(hasattr(pkg_config, "custom_settings"))
        settings = pkg_config.settings

        # Query Definitions
        check.is_in("QueryDefinitions", settings)
        check.is_in("Default", settings["QueryDefinitions"])
        check.equal(1, len(settings["QueryDefinitions"]["Custom"]))
        for path in settings["QueryDefinitions"]["Custom"]:
            check.is_true(type(path), str)
            check.is_true(
                Path(__file__).resolve().parent.parent.joinpath(path).is_dir()
            )

        # TI Providers
        check.greater_equal(len(settings["TIProviders"]), 4)
        check.is_instance(settings["TIProviders"], dict)
        for _, prov in settings["TIProviders"].items():

            check.is_in("Primary", prov)
            check.is_in("Provider", prov)
            if "Args" in prov:
                self.assertIsInstance(prov["Args"], dict)
                for arg_name, arg_val in prov["Args"].items():
                    check.is_in(
                        arg_name, ["ApiID", "AuthKey", "WorkspaceID", "TenantID"]
                    )
                    check.is_true(
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
        check.equal(geoip_lite._api_key, os.environ["MAXMIND_AUTH"])

        check.equal(geoip_lite._db_folder, conf_dbpath)

        ipstack = IPStackLookup()
        check.equal(ipstack._api_key, "987654321-222")


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
        check.equal(results, ([], []))

        # restore env vars to original
        os.environ["VTAUTHKEY"] = vt_auth_save
        os.environ["XFORCE_ID"] = xf_id__save
        os.environ["XFORCE_KEY"] = xf_auth_save
        os.environ["MAXMIND_AUTH"] = xf_auth_save


_HTTP_TEST_METHOD = [
    "timeout-kwarg",
    "def_timeout-kwarg",
    "settings",
]
_TEST_HTTP_TIMEOUT = [
    (1, httpx.Timeout(1), "int-1"),
    (httpx.Timeout(30), httpx.Timeout(30), "httpxTO30"),
    ({"connect": 30, "timeout": 20}, httpx.Timeout(20, connect=30), "dict"),
    ([20, 30], httpx.Timeout(20, connect=30), "list"),
    ((20, 30), httpx.Timeout(20, connect=30), "tuple"),
    ("something_else", httpx.Timeout(None), "other"),
    (None, httpx.Timeout(None), "none"),
]

_TO_IDS = [item[2] for item in _TEST_HTTP_TIMEOUT]


@pytest.mark.parametrize("method", _HTTP_TEST_METHOD, ids=_HTTP_TEST_METHOD)
@pytest.mark.parametrize("value, expected, test_id", _TEST_HTTP_TIMEOUT, ids=_TO_IDS)
def test_get_http_timeout(method, value, expected, test_id, monkeypatch):
    """Test get_http_timeout function with combo of parameters."""
    del test_id
    if method == "timeout-kwarg":
        check.equal(expected, pkg_config.get_http_timeout(**{"timeout": value}))
    if method == "def_timeout-kwarg":
        check.equal(expected, pkg_config.get_http_timeout(**{"def_timeout": value}))
    if method == "settings":
        if value is None:
            # also test with no settings
            monkeypatch.setattr(pkg_config, "settings", {"http_timeout": value})
            check.equal(expected, pkg_config.get_http_timeout(**{"def_timeout": value}))
        monkeypatch.setattr(pkg_config, "settings", {"http_timeout": value})
        check.equal(expected, pkg_config.get_http_timeout(**{"def_timeout": value}))
