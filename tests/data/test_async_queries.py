# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Test async connections and split queries."""

from datetime import datetime, timedelta, timezone

import pandas as pd
import pytest_check as check

from msticpy.data.core.data_providers import QueryProvider
from msticpy.data.core.query_provider_connections_mixin import _calc_split_ranges
from msticpy.data.drivers.driver_base import DriverProps

from ..unit_test_lib import get_test_data_path

_LOCAL_DATA_PATHS = [str(get_test_data_path().joinpath("localdata"))]

# pylint: disable=protected-access


def test_multiple_connections_sync():
    """Test adding connection instance to provider."""
    prov_args = dict(query_paths=_LOCAL_DATA_PATHS, data_paths=_LOCAL_DATA_PATHS)
    # create local provider and run a query
    local_prov = QueryProvider("LocalData", **prov_args)
    start = datetime.now(timezone.utc) - timedelta(days=1)
    end = datetime.now(timezone.utc)
    single_results = local_prov.WindowsSecurity.list_host_logons(
        host_name="DESKTOP-12345", start=start, end=end
    )

    # add another connection (to same folder)
    local_prov.add_connection(alias="SecondInst", **prov_args)
    connections = local_prov.list_connections()
    # verify second connection is listed
    check.equal(len(connections), 2)
    check.is_in("Default:", connections[0])
    check.is_in("SecondInst:", connections[1])

    # run query again
    multi_results = local_prov.WindowsSecurity.list_host_logons(
        host_name="DESKTOP-12345", start=start, end=end
    )
    # verify len of result is 2x single_result
    check.equal(single_results.shape[0] * 2, multi_results.shape[0])
    # verify columns/schema is the same.
    check.equal(list(single_results.columns), list(multi_results.columns))


def test_multiple_connections_threaded():
    """Test adding connection instance to provider."""
    prov_args = dict(query_paths=_LOCAL_DATA_PATHS, data_paths=_LOCAL_DATA_PATHS)
    # create local provider and run a query
    local_prov = QueryProvider("LocalData", **prov_args)
    local_prov._query_provider.set_driver_property(
        DriverProps.SUPPORTS_THREADING, value=True
    )
    start = datetime.now(timezone.utc) - timedelta(days=1)
    end = datetime.now(timezone.utc)
    single_results = local_prov.WindowsSecurity.list_host_logons(
        host_name="DESKTOP-12345", start=start, end=end
    )

    # add another 2 named connections
    for idx in range(1, 3):
        local_prov.add_connection(alias=f"Instance {idx}", **prov_args)
    # add another 2 unnamed connections
    for _ in range(2):
        local_prov.add_connection(**prov_args)

    connections = local_prov.list_connections()
    # verify second connection is listed
    check.equal(len(connections), 5)
    check.is_in("Default:", connections[0])
    check.is_in("Instance 1", connections[1])

    # run query again
    multi_results = local_prov.WindowsSecurity.list_host_logons(
        host_name="DESKTOP-12345", start=start, end=end
    )
    # verify len of result is 2x single_result
    check.equal(single_results.shape[0] * 5, multi_results.shape[0])
    # verify columns/schema is the same.
    check.equal(list(single_results.columns), list(multi_results.columns))


def test_split_queries_sync():
    """Test queries split into time segments."""
    prov_args = dict(query_paths=_LOCAL_DATA_PATHS, data_paths=_LOCAL_DATA_PATHS)
    local_prov = QueryProvider("LocalData", **prov_args)

    start = datetime.now(timezone.utc) - pd.Timedelta("5H")
    end = datetime.now(timezone.utc) + pd.Timedelta("5min")
    delta = pd.Timedelta("1H")

    ranges = _calc_split_ranges(start, end, delta)
    local_prov.WindowsSecurity.list_host_logons(
        host_name="DESKTOP-12345", start=start, end=end
    )
    result_queries = local_prov.WindowsSecurity.list_host_logons(
        "print", host_name="DESKTOP-12345", start=start, end=end, split_query_by="1H"
    )
    queries = result_queries.split("\n\n")
    check.equal(len(queries), 5)

    for idx, (st_time, e_time) in enumerate(ranges):
        check.is_in(st_time.isoformat(sep=" "), queries[idx])
        check.is_in(e_time.isoformat(sep=" "), queries[idx])
    check.is_in(start.isoformat(sep=" "), queries[0])
    check.is_in(end.isoformat(sep=" "), queries[-1])

    single_results = local_prov.WindowsSecurity.list_host_logons(
        host_name="DESKTOP-12345", start=start, end=end
    )
    result_queries = local_prov.WindowsSecurity.list_host_logons(
        host_name="DESKTOP-12345", start=start, end=end, split_query_by="1H"
    )
    # verify len of result is 2x single_result
    check.equal(single_results.shape[0] * 5, result_queries.shape[0])
    # verify columns/schema is the same.
    check.equal(list(single_results.columns), list(result_queries.columns))


def test_split_queries_async():
    """Test queries split into time segments threaded execution."""
    prov_args = dict(query_paths=_LOCAL_DATA_PATHS, data_paths=_LOCAL_DATA_PATHS)
    local_prov = QueryProvider("LocalData", **prov_args)
    local_prov._query_provider.set_driver_property(
        DriverProps.SUPPORTS_THREADING, value=True
    )

    start = datetime.now(timezone.utc) - pd.Timedelta("5H")
    end = datetime.now(timezone.utc) + pd.Timedelta("5min")
    delta = pd.Timedelta("1H")

    ranges = _calc_split_ranges(start, end, delta)
    local_prov.WindowsSecurity.list_host_logons(
        host_name="DESKTOP-12345", start=start, end=end
    )
    result_queries = local_prov.WindowsSecurity.list_host_logons(
        "print", host_name="DESKTOP-12345", start=start, end=end, split_query_by="1H"
    )
    queries = result_queries.split("\n\n")
    check.equal(len(queries), 5)

    for idx, (st_time, e_time) in enumerate(ranges):
        check.is_in(st_time.isoformat(sep=" "), queries[idx])
        check.is_in(e_time.isoformat(sep=" "), queries[idx])
    check.is_in(start.isoformat(sep=" "), queries[0])
    check.is_in(end.isoformat(sep=" "), queries[-1])

    single_results = local_prov.WindowsSecurity.list_host_logons(
        host_name="DESKTOP-12345", start=start, end=end
    )
    result_queries = local_prov.WindowsSecurity.list_host_logons(
        host_name="DESKTOP-12345", start=start, end=end, split_query_by="1H"
    )
    # verify len of result is 2x single_result
    check.equal(single_results.shape[0] * 5, result_queries.shape[0])
    # verify columns/schema is the same.
    check.equal(list(single_results.columns), list(result_queries.columns))
