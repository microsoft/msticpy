# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Pkg_config test class."""
import os
import warnings
from pathlib import Path

import httpx
import pytest
import pytest_check as check
import yaml

from msticpy.common import pkg_config
from msticpy.context.geoip import GeoLiteLookup, IPStackLookup

from ..unit_test_lib import custom_mp_config, get_test_data_path
from ..data.queries.test_query_files import validate_queries_file_structure

_TEST_DATA = get_test_data_path()

# pylint: disable=protected-access


def test_load_default():
    """Test load default settings."""
    settings = pkg_config._settings
    check.is_in("QueryDefinitions", settings)
    check.is_in("Default", settings["QueryDefinitions"])
    check.equal(1, len(settings["QueryDefinitions"]["Default"]))
    for path in settings["QueryDefinitions"]["Default"]:
        check.is_true(type(path), str)
        path = (
            Path(pkg_config.__file__).resolve().parent.parent.joinpath(f"data/{path}")
        )
        check.is_true(path.is_dir())
        for query_file in path.rglob("*.yaml"):
            validate_queries_file_structure(query_file)
    for path in settings["QueryDefinitions"].get("Custom", []):
        check.is_true(type(path), str)
        path = Path(path)
        if not path.is_absolute():
            path = Path(__file__).resolve().parent.parent.joinpath(path)
        check.is_true(path.is_dir())
        for query_file in Path(path).resolve().rglob("*.yaml"):
            if "tests" not in [parent.name for parent in query_file.absolute().parents]:
                validate_queries_file_structure(query_file)


def test_custom_config():
    """Test load queries from custom path."""
    test_config1 = Path(_TEST_DATA).joinpath(pkg_config._CONFIG_FILE)
    with custom_mp_config(test_config1):
        settings = pkg_config._settings

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
                check.is_instance(prov["Args"], dict)
                for arg_name, arg_val in prov["Args"].items():
                    check.is_in(
                        arg_name, ["ApiID", "AuthKey", "WorkspaceID", "TenantID"]
                    )
                    check.is_true(
                        isinstance(arg_val, str)
                        or "EnvironmentVar" in arg_val
                        or "KeyVaultURI" in arg_val
                    )


def test_geo_ip_settings():
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
            geoip_lite._check_initialized()
        check.is_instance(geoip_lite._api_key, str)
        check.equal(geoip_lite._api_key, os.environ["MAXMIND_AUTH"])

        check.equal(geoip_lite._db_folder, conf_dbpath)

        ipstack = IPStackLookup()
        ipstack._check_initialized()
        check.equal(ipstack._api_key, "987654321-222")


@pytest.mark.skipif(
    os.environ.get("MSTICPY_BUILD_SOURCE", "").casefold() == "fork",
    reason="External fork.",
)
def test_validate_config():
    """Test config validation function."""
    test_config1 = Path(_TEST_DATA).joinpath(pkg_config._CONFIG_FILE)
    with custom_mp_config(test_config1):
        results = pkg_config.validate_config()
        check.greater(len(results[0]), 1)
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


_HTTP_TEST_METHOD = (
    "timeout-kwarg",
    "def_timeout-kwarg",
    "settings",
)
_TEST_HTTP_TIMEOUT = (
    (1, httpx.Timeout(1), "int-1"),
    (httpx.Timeout(30), httpx.Timeout(30), "httpxTO30"),
    ({"connect": 30, "timeout": 20}, httpx.Timeout(20, connect=30), "dict"),
    ([20, 30], httpx.Timeout(20, connect=30), "list"),
    ((20, 30), httpx.Timeout(20, connect=30), "tuple"),
    ("something_else", httpx.Timeout(None), "other"),
    (None, httpx.Timeout(None), "none"),
)
_TO_IDS = [item[2] for item in _TEST_HTTP_TIMEOUT]


@pytest.mark.parametrize("method", _HTTP_TEST_METHOD, ids=_HTTP_TEST_METHOD)
@pytest.mark.parametrize("value, expected, test_id", _TEST_HTTP_TIMEOUT, ids=_TO_IDS)
def test_get_http_timeout(method, value, expected, test_id):
    """Test get_http_timeout function with combo of parameters."""
    del test_id
    if method == "timeout-kwarg":
        check.equal(expected, pkg_config.get_http_timeout(**{"timeout": value}))
    if method == "def_timeout-kwarg":
        check.equal(expected, pkg_config.get_http_timeout(**{"def_timeout": value}))
    if method == "settings":
        pkg_config.set_config("http_timeout", value)
        check.equal(expected, pkg_config.get_http_timeout(**{"def_timeout": value}))


_TEST_GET_SETTINGS = (
    (("AzureSentinel", None), dict),
    (
        ("AzureSentinel.Workspaces.Default.WorkspaceId", None),
        "52b1ab41-869e-4138-9e40-2a4457f09bf3",
    ),
    (("AzureSentinel.Workspaces.NotExist.WorkspaceId", None), KeyError),
    (("AzureSentinel.Workspaces.NotExist.WorkspaceId", "testval"), "testval"),
)


@pytest.mark.parametrize("test, expected", _TEST_GET_SETTINGS)
def test_get_config(test, expected):
    """Test pkg_config.get_config."""
    key, default = test
    test_config = get_test_data_path().parent.joinpath("msticpyconfig-test.yaml")
    result = None
    with custom_mp_config(test_config):
        if default:
            result = pkg_config.get_config(key, default)
        elif isinstance(expected, type) and issubclass(expected, Exception):
            with pytest.raises(expected):
                pkg_config.get_config(key)
        else:
            result = pkg_config.get_config(key)

    if result:
        if isinstance(expected, type):
            check.is_instance(result, expected)
        else:
            check.equal(result, expected)


_TEST_SET_SETTINGS = (
    (
        "AzureSentinel.Workspaces.Default.WorkspaceId",
        "{TEST-GUID}",
        False,
        "{TEST-GUID}",
    ),
    ("AzureSentinel.Workspaces.NotExist.WorkspaceId", "{TEST-GUID}", False, KeyError),
    (
        "AzureSentinel.Workspaces.NotExist.WorkspaceId",
        "{TEST-GUID}",
        True,
        "{TEST-GUID}",
    ),
)


@pytest.mark.parametrize("key, value, create, expected", _TEST_SET_SETTINGS)
def test_set_config(key, value, create, expected):
    """Test pkg_config.set_config."""
    test_config = get_test_data_path().parent.joinpath("msticpyconfig-test.yaml")
    result = None
    with custom_mp_config(test_config):
        if isinstance(expected, type) and issubclass(expected, Exception):
            with pytest.raises(expected):
                pkg_config.set_config(key, value, create_path=create)
        else:
            result = pkg_config.set_config(key, value, create)
    if result:
        check.equal(result, expected)
