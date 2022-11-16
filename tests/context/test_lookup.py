# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Lookup test class."""
import warnings
from pathlib import Path

import pandas as pd
import pytest
import pytest_check as check

from msticpy.common import pkg_config
from msticpy.common.provider_settings import get_provider_settings
from msticpy.context.lookup_result import LookupStatus
from msticpy.context.tilookup import TILookup
from msticpy.context.contextlookup import ContextLookup
from msticpy.context.preprocess_observable import (
    _clean_url,
    preprocess_observable,
)
from msticpy.context.provider_base import generate_items

from ..unit_test_lib import custom_mp_config, get_test_data_path

_TEST_DATA = get_test_data_path()

_IOC_IPS = [
    "185.92.220.35",
    "213.159.214.86",
    "77.222.54.202",
    "91.219.29.81",
    "193.9.28.254",
    "89.108.83.196",
    "91.219.28.44",
    "188.127.231.124",
    "192.42.116.41",
    "91.219.31.18",
    "46.4.239.76",
    "188.166.168.250",
    "195.154.241.208",
    "51.255.172.55",
    "93.170.169.52",
    "104.215.148.63",
    "13.77.161.179",
]


# pylint: disable=protected-access, redefined-outer-name
@pytest.fixture
def ti_lookup():
    """Return TILookup instance."""
    config_path = Path(_TEST_DATA).joinpath(pkg_config._CONFIG_FILE)
    with custom_mp_config(config_path):
        with warnings.catch_warnings():
            # We want to ignore warnings from missing config
            warnings.simplefilter("ignore", category=UserWarning)
            return TILookup()


@pytest.fixture
def context_lookup():
    """Return TILookup instance."""
    config_path = Path(_TEST_DATA).joinpath(pkg_config._CONFIG_FILE)
    with custom_mp_config(config_path):
        with warnings.catch_warnings():
            # We want to ignore warnings from missing config
            warnings.simplefilter("ignore", category=UserWarning)
            return ContextLookup()


@pytest.mark.filterwarnings("ignore::UserWarning")
def test_ti_config_and_load(ti_lookup):
    """Test loading TI providers."""
    config_path = Path(_TEST_DATA).parent.joinpath("msticpyconfig-test.yaml")
    with custom_mp_config(config_path):
        with warnings.catch_warnings():
            # We want to ignore warnings from missing config
            warnings.simplefilter("ignore", category=UserWarning)
            ti_settings = get_provider_settings()

            check.is_instance(ti_settings, dict)
            check.greater_equal(len(ti_settings), 4)

    # Try to load TIProviders - should throw a warning on
    # missing provider class
    config_path = Path(_TEST_DATA).joinpath(pkg_config._CONFIG_FILE)
    with custom_mp_config(config_path):
        with pytest.warns(UserWarning):
            ti_lookup = TILookup()

    # should have 2 successfully loaded providers
    check.greater_equal(len(ti_lookup.loaded_providers), 3)
    check.greater_equal(len(ti_lookup.provider_status), 3)

    av_provs = ti_lookup.available_providers
    check.greater_equal(len(av_provs), 1)
    ti_lookup.provider_usage()
    ti_lookup.list_available_providers(show_query_types=True)

    ti_lookup.reload_providers()
    check.greater_equal(len(ti_lookup.available_providers), 1)


@pytest.mark.filterwarnings("ignore::UserWarning")
def test_context_config_and_load(context_lookup):
    """Test loading Context providers."""
    config_path = Path(_TEST_DATA).parent.joinpath("msticpyconfig-test.yaml")
    with custom_mp_config(config_path):
        with warnings.catch_warnings():
            # We want to ignore warnings from missing config
            warnings.simplefilter("ignore", category=UserWarning)
            context_settings = get_provider_settings(config_section="ContextProviders")

            check.is_instance(context_settings, dict)
            check.greater_equal(len(context_settings), 1)

    # Try to load ContextProviders - should throw a warning on
    # missing provider class
    config_path = Path(_TEST_DATA).joinpath(pkg_config._CONFIG_FILE)
    with custom_mp_config(config_path):
        with pytest.warns(UserWarning):
            context_lookup = ContextLookup()

    # should have 2 successfully loaded providers
    check.greater_equal(len(context_lookup.loaded_providers), 1)
    check.greater_equal(len(context_lookup.provider_status), 1)

    av_provs = context_lookup.available_providers
    check.greater_equal(len(av_provs), 1)
    context_lookup.provider_usage()
    context_lookup.list_available_providers(show_query_types=True)

    context_lookup.reload_providers()
    check.greater_equal(len(context_lookup.available_providers), 1)


def test_ti_lookup_enable_disable(ti_lookup):
    """Test enable and disable a provider."""
    check.greater(len(ti_lookup.configured_providers), 4)

    check.is_in("OTX", ti_lookup._providers)
    ti_lookup.disable_provider("OTX")
    check.is_in("OTX", ti_lookup._secondary_providers)
    check.is_not_in("OTX", ti_lookup._providers)
    ti_lookup.enable_provider("OTX")
    check.is_in("OTX", ti_lookup._providers)
    check.is_not_in("OTX", ti_lookup._secondary_providers)

    ti_lookup.set_provider_state({"OTX": False})
    check.is_in("OTX", ti_lookup._secondary_providers)
    check.is_not_in("OTX", ti_lookup._providers)
    ti_lookup.set_provider_state({"OTX": True})
    check.is_in("OTX", ti_lookup._providers)
    check.is_not_in("OTX", ti_lookup._secondary_providers)


def test_context_lookup_enable_disable(context_lookup):
    """Test enable and disable a provider."""
    check.greater_equal(len(context_lookup.configured_providers), 1)

    check.is_in("ServiceNow", context_lookup._providers)
    context_lookup.disable_provider("ServiceNow")
    check.is_in("ServiceNow", context_lookup._secondary_providers)
    check.is_not_in("ServiceNow", context_lookup._providers)
    context_lookup.enable_provider("ServiceNow")
    check.is_in("ServiceNow", context_lookup._providers)
    check.is_not_in("ServiceNow", context_lookup._secondary_providers)

    context_lookup.set_provider_state({"ServiceNow": False})
    check.is_in("ServiceNow", context_lookup._secondary_providers)
    check.is_not_in("ServiceNow", context_lookup._providers)
    context_lookup.set_provider_state({"ServiceNow": True})
    check.is_in("ServiceNow", context_lookup._providers)
    check.is_not_in("ServiceNow", context_lookup._secondary_providers)


def test_check_ioc_type(ti_lookup):
    """Check IOC types."""
    provider = ti_lookup.loaded_providers["OTX"]
    lu_result = provider._check_ioc_type(ioc="a.b.c.d", ioc_type="ipv4")
    check.equal(lu_result["Status"], 2)
    lu_result = provider._check_ioc_type(ioc="a.b.c.d", ioc_type="ipv6")
    check.equal(lu_result["Status"], 2)
    lu_result = provider._check_ioc_type(ioc="url", ioc_type="ipv4")
    check.equal(lu_result["Status"], 2)
    lu_result = provider._check_ioc_type(ioc="123", ioc_type="dns")
    check.equal(lu_result["Status"], 2)
    lu_result = provider._check_ioc_type(ioc="424246", ioc_type="file_hash")
    check.equal(lu_result["Status"], 2)


def test_check_observable_type(context_lookup):
    """Check IOC types."""
    provider = context_lookup.loaded_providers["ServiceNow"]
    lu_result = provider._check_observable_type(obs="a.b.c.d", obs_type="ipv4")
    check.equal(lu_result["Status"], LookupStatus.BAD_FORMAT.value)
    lu_result = provider._check_observable_type(obs="a.b.c.d", obs_type="ipv6")
    check.equal(lu_result["Status"], LookupStatus.BAD_FORMAT.value)
    lu_result = provider._check_observable_type(obs="url", obs_type="ipv4")
    check.equal(lu_result["Status"], LookupStatus.BAD_FORMAT.value)
    lu_result = provider._check_observable_type(obs="123", obs_type="dns")
    check.equal(lu_result["Status"], LookupStatus.NOT_SUPPORTED.value)
    lu_result = provider._check_observable_type(obs="424246", obs_type="file_hash")
    check.equal(lu_result["Status"], LookupStatus.NOT_SUPPORTED.value)


_OBS_TYPES = [
    ("ipv4", "127.0.0.1", "IP address is not global"),
    (
        "ipv4",
        "not an ip address",
        "Observable does not match expected pattern for ipv4",
    ),
    ("ipv4", "185.92.220.35", "ok"),
    ("ipv6", "185.92.220.35", "Observable does not match expected pattern for ipv6"),
    (
        "ipv4",
        "2001:0db8:85a3:0000:0000:8a2e:0370:7334",
        "Observable does not match expected pattern for ipv4",
    ),
    ("ipv6", "2001:0db8:85a3:0000:0000:8a2e:0370:7334", "IP address is not global"),
    ("ipv6", "2345:0425:2CA1:0000:0000:0567:5673:23b5", "ok"),
    ("dns", "www.python.com", "ok"),
    ("dns", "localhost", "Observable does not match expected pattern for dns"),
    ("dns", "185.92.220.35", "Observable does not match expected pattern for dns"),
    (
        "md5_hash",
        "AAAAAAAAAAAAAAAA",
        "Observable does not match expected pattern for md5_hash",
    ),
    (
        "md5_hash",
        "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
        "String has too low an entropy to be a hash",
    ),
    ("md5_hash", "A123008438B9C391A123008438B9C391", "ok"),
    ("other_type", "A123008438B9C391", "ok"),
]


@pytest.mark.parametrize("obs_type, obs, expected", _OBS_TYPES)
def test_preprocess_observables(obs_type, obs, expected):
    """Test observable pre-processing."""
    result = preprocess_observable(obs, ioc_type=obs_type)
    check.equal(result.status, expected)


_TEST_URLS = [
    (
        "https://me@www.microsoft.com:443/test1?testparam=x",
        "https://me@www.microsoft.com:443/test1",
    ),
    ("https://www.microsoft.com", "https://www.microsoft.com"),
]


@pytest.mark.parametrize("url, expected", _TEST_URLS)
def test_clean_url(url, expected):
    """Test clean URL function."""
    check.equal(_clean_url(url), expected)


def test_iterable_generator():
    """Test iterable generator."""
    test_df = pd.DataFrame({"col1": _IOC_IPS, "col2": _IOC_IPS})

    # DataFrames
    for ioc, _ in generate_items(test_df, item_col="col1", item_type_col="col2"):
        check.is_in(ioc, _IOC_IPS)

    # Iterables
    for ioc, ioc_type in generate_items(test_df[["col1"]], item_col="col1"):
        check.is_in(ioc, _IOC_IPS)
        check.equal(ioc_type, "ipv4")

    # Used for local testing only
    # def test_interactive(self):
    #     saved_env = os.environ[pkg_config._CONFIG_ENV_VAR]
    #     os.environ[pkg_config._CONFIG_ENV_VAR] = "e:\\src\\microsoft\\msticpyconfig.yaml"
    #     pkg_config.refresh_config()
    #     if "AzureSentinel" in pkg_config.custom_settings["TIProviders"]:
    #         pkg_config.custom_settings["TIProviders"].pop("AzureSentinel")
    #     ti_lookup = TILookup()

    #     result = ti_lookup.lookup_ioc(
    #         observable="www.401k.com", providers=["OPR", "VirusTotal", "XForce"]
    #         )

    #     os.environ[pkg_config._CONFIG_ENV_VAR] = saved_env
