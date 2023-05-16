# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Pivot main library test."""

import contextlib
import warnings
from typing import Optional

import pytest

from msticpy.context.geoip import GeoLiteLookup
from msticpy.context.tilookup import TILookup
from msticpy.data import QueryProvider
from msticpy.init.pivot import Pivot

from ...unit_test_lib import custom_mp_config, get_test_data_path

__author__ = "Ian Hellen"

# pylint: disable=redefined-outer-name, protected-access

_KQL_IMP_OK = False
with contextlib.suppress(ImportError):
    # pylint: disable=unused-import
    from msticpy.data.drivers import kql_driver

    del kql_driver
    _KQL_IMP_OK = True
_SPLUNK_IMP_OK = False
with contextlib.suppress(ImportError):
    from msticpy.data.drivers import splunk_driver

    del splunk_driver
    _SPLUNK_IMP_OK = True

_IPSTACK_IMP_OK = False
ip_stack_cls: Optional[type]
try:
    from msticpy.context.geoip import IPStackLookup as ip_stack_cls

    _IPSTACK_IMP_OK = True
except ImportError:
    ip_stack_cls = None


# pylint: disable=protected-access
def exec_connect(provider):
    """Mock the connection succeeding."""
    provider._query_provider._loaded = True
    provider._query_provider._connected = True
    provider.connect()


@pytest.fixture(scope="session")
def create_data_providers():
    """Return dict of providers."""
    prov_dict = {}
    with custom_mp_config(
        get_test_data_path().parent.joinpath("msticpyconfig-test.yaml")
    ):
        with warnings.catch_warnings():
            warnings.simplefilter("ignore", category=UserWarning)
            if _KQL_IMP_OK:
                prov_dict["az_sent_prov"] = QueryProvider("MSSentinel")
            prov_dict["mdatp_prov"] = QueryProvider("MDE")
            if _SPLUNK_IMP_OK:
                prov_dict["splunk_prov"] = QueryProvider("Splunk")
            prov_dict["ti_lookup"] = TILookup()
            prov_dict["geolite"] = GeoLiteLookup()

        if _IPSTACK_IMP_OK:
            prov_dict["ip_stack"] = ip_stack_cls()
    return prov_dict


def mock_connect(*args, **kwargs):
    """Mock driver.connect."""
    del args, kwargs


@pytest.fixture
def data_providers(create_data_providers, monkeypatch):
    """Return patch providers."""
    for provider in create_data_providers.values():
        if isinstance(provider, QueryProvider):
            monkeypatch.setattr(provider._query_provider, "connect", mock_connect)
            exec_connect(provider)
    return create_data_providers


@pytest.fixture
def create_pivot(data_providers):
    """Return Pivot instance with initialized data providers."""
    with warnings.catch_warnings():
        warnings.simplefilter("ignore", category=UserWarning)
        pivot = Pivot(namespace=data_providers)
        pivot.reload_pivots(namespace=data_providers, clear_existing=True)
    for provider in data_providers.values():
        if isinstance(provider, QueryProvider):
            exec_connect(provider)
    return pivot


# @pytest.fixture
# def create_pivot(data_providers):
#     """Return Pivot instance with initialized data providers."""
#     with warnings.catch_warnings():
#         warnings.simplefilter("ignore", category=UserWarning)
#         pivot = Pivot(namespace=data_providers)
#         pivot.reload_pivots(namespace=data_providers, clear_existing=True)
#     for provider in data_providers.values():
#         if isinstance(provider, QueryProvider):
#             exec_connect(provider)
#     return pivot
