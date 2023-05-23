# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Tests for mp_plugins module."""
import re

import pandas as pd
import pytest
import pytest_check as check
import respx

from msticpy.common import pkg_config
from msticpy.context.contextlookup import ContextLookup
from msticpy.context.contextproviders.context_provider_base import ContextProvider
from msticpy.context.tilookup import TILookup
from msticpy.context.tiproviders.ti_provider_base import TIProvider
from msticpy.data import QueryProvider, drivers
from msticpy.data.drivers import DriverBase
from msticpy.init.mp_plugins import read_plugins

from ..unit_test_lib import custom_mp_config, get_test_data_path

__author__ = "Ian Hellen"


_TI_PROV_SETTINGS = {
    "Provider1": {
        "Args": {"AuthKey": "12345"},
        "Primary": True,
        "Provider": "Provider1",
    },
    "Provider2": {
        "Args": {"AuthKey": "12345"},
        "Primary": True,
        "Provider": "Provider2",
    },
}

# pylint: disable=redefined-outer-name


@pytest.fixture(scope="module")
def load_plugins():
    """Fixture_docstring."""
    plugin_path = get_test_data_path().joinpath("plugins")
    read_plugins(plugin_paths=str(plugin_path))


def test_load_plugins(load_plugins):
    """Load plugins from test data."""

    for provider in ("Provider1", "Provider2"):
        check.is_in(provider, TILookup.CUSTOM_PROVIDERS)
        check.is_true(issubclass(TILookup.CUSTOM_PROVIDERS[provider], TIProvider))

    check.is_true(issubclass(TILookup.CUSTOM_PROVIDERS["Provider2"], TIProvider))
    check.is_in("ContextProviderTest", ContextLookup.CUSTOM_PROVIDERS)
    check.is_true(
        issubclass(
            ContextLookup.CUSTOM_PROVIDERS["ContextProviderTest"], ContextProvider
        )
    )
    for provider in ("CustomDataProvA", "SQLTestProvider", "SQLProdProvider"):
        check.is_in(provider, drivers.CUSTOM_PROVIDERS)
        check.is_true(issubclass(drivers.CUSTOM_PROVIDERS[provider], DriverBase))


def test_custom_data_provider(load_plugins):
    """Test data provider operations."""
    qry_prov = QueryProvider(
        "SQLProdProvider", query_paths=[str(get_test_data_path().joinpath("plugins"))]
    )
    qry_prov.connect()
    results = qry_prov.exec_query("Get test query data")
    check.is_true(results.shape, (1, 4))
    check.is_true(hasattr(qry_prov, "CustomSQL"))
    check.is_true(hasattr(qry_prov.CustomSQL, "accessibility_persistence"))

    results = qry_prov.CustomSQL.accessibility_persistence()
    check.is_true(results.shape, (1, 4))

    qry_prov2 = QueryProvider(
        "CustomDataProvA", query_paths=["tests/testdata/plugins/"]
    )
    qry_prov2.connect()
    results = qry_prov2.Custom.file_path(path="foo")


# pylint: disable=protected-access
@respx.mock
def test_custom_ti_provider(load_plugins):
    """Test TI plugin."""
    config_path = get_test_data_path().parent.joinpath("msticpyconfig-test.yaml")
    with custom_mp_config(config_path):
        pkg_config._settings["TIProviders"].update(_TI_PROV_SETTINGS)
        print("Settings", pkg_config._settings["TIProviders"])
        ti = TILookup()

        check.is_in("Provider1", ti.available_providers)
        print("Loaded provs", ti.loaded_providers.keys())
        check.is_in("Provider1", ti.loaded_providers)
        check.is_in("Provider2", ti.loaded_providers)

        results = ti.lookup_ioc(ioc="20.1.2.3", providers=["Provider1"])
        check.equal(results.shape, (1, 6))

        _df_results = pd.DataFrame(
            [
                {
                    "ID": "tests results",
                    "Status": 0,
                    **{name: str(idx) for idx, name in enumerate("ABC")},
                }
            ]
        )
        respx.get(re.compile("https://api\.service\.com/.*")).respond(
            200, json=_df_results.iloc[0].to_dict()
        )

        results = ti.lookup_ioc(ioc="20.1.2.3", providers=["Provider2"])
        check.equal(results.shape, (1, 11))
        check.is_in(
            "https://api.service.com/api/v1/indicators/IPv4", results.iloc[0].Reference
        )
