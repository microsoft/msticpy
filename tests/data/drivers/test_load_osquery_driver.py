# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Module docstring."""
import pandas as pd
import pytest
import pytest_check as check

from msticpy.data.core.data_providers import QueryProvider
from msticpy.data.drivers.local_osquery_driver import OSQueryLogDriver

from ...unit_test_lib import get_test_data_path

__author__ = "Ian Hellen"

# pylint: disable=redefined-outer-name, protected-access


# change this for actual data

_OSQUERY_LOG_PATH = "osquery"
_EXPECTED_TABLES = 21
_LOG_TOTAL_LINES = 309
_EXPECTED_KEYS = [
    "pack_osquery-snapshots-pack_platform_info",
    "pack_osquery-snapshots-pack_python_packages",
    "pack_osquery-snapshots-pack_dns_resolvers",
]


def test_read_log_file():
    """Test reading OSQuery logs."""
    os_query_path = str(get_test_data_path().joinpath(_OSQUERY_LOG_PATH))
    osq_driver = OSQueryLogDriver(data_paths=[os_query_path])

    osq_driver.connect()
    check.equal(len(osq_driver._data_cache), _EXPECTED_TABLES)
    for df in osq_driver._data_cache.values():
        check.is_instance(df, pd.DataFrame)
    check.equal(
        sum(len(df) for df in osq_driver._data_cache.values()), _LOG_TOTAL_LINES
    )
    for event_type in _EXPECTED_KEYS:
        check.is_in(event_type, osq_driver._data_cache)

    check.is_instance(osq_driver.schema, dict)
    check.equal(len(osq_driver.schema), _EXPECTED_TABLES)
    for col_dict in osq_driver.schema.values():
        check.greater(len(col_dict), 5)
        check.is_in("datetime64[ns, UTC]", col_dict.values())
    for event_type in _EXPECTED_KEYS:
        check.is_in(event_type, osq_driver.schema)


def test_cache_file(tmp_path):
    """Test reading and writing cache file."""
    os_query_path = str(get_test_data_path().joinpath(_OSQUERY_LOG_PATH))
    cache_path = tmp_path.joinpath("osq_cache.pkl")
    check.is_false(cache_path.is_file())
    osq_driver = OSQueryLogDriver(data_paths=[os_query_path], cache_file=cache_path)
    osq_driver.connect()

    check.is_true(cache_path.is_file())

    # instantiate using the cache file
    osq_driver_cached = OSQueryLogDriver(
        data_paths=[os_query_path], cache_file=cache_path
    )
    osq_driver_cached.connect()

    check.equal(len(osq_driver_cached._data_cache), _EXPECTED_TABLES)
    for df in osq_driver_cached._data_cache.values():
        check.is_instance(df, pd.DataFrame)

    for name, df in osq_driver._data_cache.items():
        check.is_in(name, osq_driver_cached._data_cache)
        check.equal(df.shape, osq_driver_cached._data_cache[name].shape)


def test_query():
    """Test loading and querying osquery data."""
    os_query_path = str(get_test_data_path().joinpath(_OSQUERY_LOG_PATH))
    qry_prov = QueryProvider("OSQueryLogs", data_paths=[os_query_path])
    qry_prov.connect()
    check.equal(len(qry_prov.osquery), 19)
    check.equal(len(qry_prov.osquery.dns_resolvers()), 2)
    check.equal(len(qry_prov.osquery.platform_info()), 1)
    check.equal(len(qry_prov.osquery.python_packages()), 40)
    check.equal(len(qry_prov.osquery.processes()), 21)
    check.equal(len(qry_prov.osquery.shell_history()), 15)
