# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Module docstring."""
import pandas as pd
import pytest
import pytest_check as check

from msticpy.data.drivers.local_osquery_driver import OSQueryLogDriver

from ...unit_test_lib import get_test_data_path

__author__ = "Ian Hellen"

# pylint: disable=redefined-outer-name, protected-access


# change this for actual data
EXPECTED_TABLES = 5


def test_read_log_file():
    """Test reading OSQuery logs."""
    os_query_path = str(get_test_data_path().joinpath("os_query_data.log"))
    osq_driver = OSQueryLogDriver(data_paths=[os_query_path])

    osq_driver.connect()
    check.equal(len(osq_driver._data_cache), EXPECTED_TABLES)
    for df in osq_driver._data_cache.values():
        check.is_instance(df, pd.DataFrame)

    check.is_instance(osq_driver.schema, dict)
    check.equal(len(os_query_path.schema), EXPECTED_TABLES)
    for col_dict in osq_driver.schema.values():
        check.is_greater(len(col_dict), 5)
        check.is_in("datetime64[ns, UTC]", col_dict.values())


def test_cache_file(tmp_path):
    """Test reading and writing cache file."""
    check.is_false(cache_path.is_file())
    os_query_path = str(get_test_data_path().joinpath("os_query_data.log"))
    cache_path = tmp_path.join_path("osq_cache.pkl")
    osq_driver = OSQueryLogDriver(data_paths=[os_query_path], cache_file=cache_path)
    osq_driver.connect()

    check.is_true(cache_path.is_file())

    # instantiate using the cache file
    osq_driver_cached = OSQueryLogDriver(
        data_paths=[os_query_path], cache_file=cache_path
    )
    osq_driver_cached.connect()

    check.equal(len(osq_driver_cached._data_cache), EXPECTED_TABLES)
    for df in osq_driver_cached._data_cache.values():
        check.is_instance(df, pd.DataFrame)

    for name, df in osq_driver._data_cache.items():
        check.is_in(name, osq_driver_cached._data_cache)
        check.equal(df.shape, osq_driver_cached._data_cache[name].shape)
