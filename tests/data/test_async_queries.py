# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Test async connections and split queries."""

import asyncio
import time
from datetime import datetime, timedelta, timezone
from functools import partial
from unittest.mock import patch

import pandas as pd
import pytest_check as check

from msticpy.common.exceptions import MsticpyDataQueryError
from msticpy.data.core.data_providers import QueryProvider
from msticpy.data.drivers.local_data_driver import LocalDataDriver
from msticpy.data.core.query_provider_connections_mixin import (
    QueryProviderConnectionsMixin,
    _calc_split_ranges,
)
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

    # Check that with parameter progress = False, the result is still the same.
    multi_results_no_progress = local_prov.WindowsSecurity.list_host_logons(
        host_name="DESKTOP-12345",
        start=start,
        end=end,
        progress=False,
    )
    check.is_true(multi_results_no_progress.equals(multi_results))

    # Check that even if only exceptions are returned, the result will be an empty dataframe.
    with patch.object(
        LocalDataDriver, "query", side_effect=MsticpyDataQueryError
    ) as patched_query_exception:
        multi_results_exception_raised: pd.DataFrame = (
            local_prov.WindowsSecurity.list_host_logons(
                host_name="DESKTOP-12345",
                start=start,
                end=end,
            )
        )
        check.is_true(patched_query_exception.called)
        check.equal(patched_query_exception.call_count, len(connections))
        check.is_instance(multi_results_exception_raised, pd.DataFrame)
        check.is_true(multi_results_exception_raised.empty)


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

    # Check that with parameter progress = False, the result is still the same.
    multi_results_no_progress = local_prov.WindowsSecurity.list_host_logons(
        host_name="DESKTOP-12345",
        start=start,
        end=end,
        progress=False,
    )
    check.is_true(multi_results_no_progress.equals(multi_results))

    # Check that even if the query returns an empty dataframe, the result will be ok.
    with patch.object(
        LocalDataDriver, "query", return_value=pd.DataFrame()
    ) as patched_query_empty_df:
        multi_results_no_result: pd.DataFrame = (
            local_prov.WindowsSecurity.list_host_logons(
                host_name="DESKTOP-12345",
                start=start,
                end=end,
            )
        )
        check.is_true(patched_query_empty_df.called)
        check.equal(patched_query_empty_df.call_count, len(connections))
        check.is_instance(multi_results_no_result, pd.DataFrame)
        check.is_true(multi_results_no_result.empty)

    # Check that even if only exceptions are returned, the result will be an empty dataframe.
    with patch.object(
        LocalDataDriver, "query", side_effect=MsticpyDataQueryError
    ) as patched_query_exception:
        multi_results_exception_raised: pd.DataFrame = (
            local_prov.WindowsSecurity.list_host_logons(
                host_name="DESKTOP-12345",
                start=start,
                end=end,
            )
        )
        check.is_true(patched_query_exception.called)
        check.equal(patched_query_exception.call_count, len(connections))
        check.is_instance(multi_results_exception_raised, pd.DataFrame)
        check.is_true(multi_results_exception_raised.empty)

    # Check if retry parameter works as expected when only exceptions are raised.
    with patch.object(
        LocalDataDriver,
        "query",
        side_effect=MsticpyDataQueryError,
    ) as patched_query_exception_then_exception:
        multi_results_exception_raised_retried: pd.DataFrame = (
            local_prov.WindowsSecurity.list_host_logons(
                host_name="DESKTOP-12345",
                start=start,
                end=end,
                retry_on_error=True,
            )
        )
        check.is_true(patched_query_exception_then_exception.called)
        check.equal(
            patched_query_exception_then_exception.call_count, len(connections) * 2
        )
        check.is_instance(multi_results_exception_raised_retried, pd.DataFrame)
        check.is_true(multi_results_exception_raised_retried.empty)

    # Check if retry parameter works as expected.
    # Exceptions will be raised for the first executions, then returns a dummy dataframe
    with patch.object(
        LocalDataDriver,
        "query",
        side_effect=[MsticpyDataQueryError for _ in connections]
        + [single_results for _ in connections],
    ) as patched_query_exception_then_ok:
        multi_results_exception_raised_retried_success: pd.DataFrame = (
            local_prov.WindowsSecurity.list_host_logons(
                host_name="DESKTOP-12345",
                start=start,
                end=end,
                retry_on_error=True,
            )
        )
        check.is_true(patched_query_exception_then_ok.called)
        check.equal(patched_query_exception_then_ok.call_count, len(connections) * 2)
        check.is_instance(multi_results_exception_raised_retried_success, pd.DataFrame)
        check.is_false(multi_results_exception_raised_retried_success.empty)
        check.is_true(
            multi_results_exception_raised_retried_success.equals(multi_results)
        )

    # Check if retry parameter works as expected.
    # Exceptions will be raised for the one driver for all executions
    # but for the other drivers, only during the first execution
    with patch.object(
        LocalDataDriver,
        "query",
        side_effect=[MsticpyDataQueryError]
        + [MsticpyDataQueryError for _ in range(len(connections) - 1)]
        + [MsticpyDataQueryError]
        + [single_results for _ in range(len(connections) - 1)],
    ) as patched_query_exception_partial_success:
        multi_results_exception_raised_retried_partial_success: pd.DataFrame = (
            local_prov.WindowsSecurity.list_host_logons(
                host_name="DESKTOP-12345",
                start=start,
                end=end,
                retry_on_error=True,
            )
        )
        check.is_true(patched_query_exception_partial_success.called)
        check.equal(
            patched_query_exception_partial_success.call_count, len(connections) * 2
        )
        check.is_instance(
            multi_results_exception_raised_retried_partial_success, pd.DataFrame
        )
        check.is_true(
            multi_results_exception_raised_retried_partial_success.equals(
                pd.concat(
                    [single_results for _ in range(len(connections) - 1)],
                    ignore_index=True,
                )
            )
        )

    # Check if retry parameter works as expected.
    # Exceptions will be raised for the one driver for all executions
    # but for the other drivers, no exceptions will occur
    with patch.object(
        LocalDataDriver,
        "query",
        side_effect=[MsticpyDataQueryError]
        + [single_results for _ in range(len(connections) - 1)]
        + [MsticpyDataQueryError],
    ) as patched_query_exception_partial_sucess_v2:
        multi_results_exception_raised_retried_partial_success_v2: pd.DataFrame = (
            local_prov.WindowsSecurity.list_host_logons(
                host_name="DESKTOP-12345",
                start=start,
                end=end,
                retry_on_error=True,
            )
        )
        check.is_true(patched_query_exception_partial_sucess_v2.called)
        check.equal(
            patched_query_exception_partial_sucess_v2.call_count, len(connections) + 1
        )
        check.is_instance(
            multi_results_exception_raised_retried_partial_success_v2, pd.DataFrame
        )
        check.is_true(
            multi_results_exception_raised_retried_partial_success_v2.equals(
                pd.concat(
                    [single_results for _ in range(len(connections) - 1)],
                    ignore_index=True,
                )
            )
        )

    # Check if running in ipython yields the same result.
    with patch(
        "msticpy.data.core.query_provider_connections_mixin.is_ipython",
        return_value=True,
    ) as patched_is_ipython:
        multi_results_exception_raised_retried_success: pd.DataFrame = (
            local_prov.WindowsSecurity.list_host_logons(
                host_name="DESKTOP-12345",
                start=start,
                end=end,
            )
        )
        check.is_true(patched_is_ipython.called)
        check.equal(patched_is_ipython.call_count, 2)
        check.is_instance(multi_results_exception_raised_retried_success, pd.DataFrame)
        check.is_false(multi_results_exception_raised_retried_success.empty)
        check.is_true(
            multi_results_exception_raised_retried_success.equals(multi_results)
        )


def test_exec_queries_threaded_preserves_identity_out_of_order():
    """Results must stay attached to the query_id that actually produced them.

    asyncio.as_completed() yields tasks in completion order, not the order
    they were submitted in. A correct implementation cannot recover which
    result belongs to which query_id by position (e.g. zipping the
    submission-ordered keys against the completion-ordered results) --
    that silently mislabels results whenever tasks don't finish in
    submission order, which is the common case under real concurrency.
    """

    def _tagged_result(label: str, delay: float) -> pd.DataFrame:
        time.sleep(delay)
        return pd.DataFrame({"query_id": [label]})

    # "slow" is submitted first but finishes last.
    query_tasks = {
        "slow": partial(_tagged_result, "slow", 0.2),
        "fast": partial(_tagged_result, "fast", 0.0),
    }

    result: pd.DataFrame = asyncio.run(
        QueryProviderConnectionsMixin._exec_queries_threaded(
            query_tasks, progress=False
        )
    )

    # Each row must carry the query_id of the task that actually produced
    # it, and the final frame must preserve submission order (slow before
    # fast) regardless of which one completed first.
    check.equal(list(result["query_id"]), ["slow", "fast"])


def test_exec_queries_threaded_retries_the_task_that_actually_failed():
    """The retry mechanism must retry the query_id that failed, not
    whichever query_id happened to occupy the same position once results
    were (mis)reconstructed."""

    def _fast_ok(label: str) -> pd.DataFrame:
        return pd.DataFrame({"query_id": [label]})

    def _slow_fail(label: str) -> pd.DataFrame:
        time.sleep(0.2)
        raise MsticpyDataQueryError(label)

    call_counts: dict[str, int] = {"a": 0, "b": 0}

    def _tracked(label: str, should_fail: bool) -> pd.DataFrame:
        call_counts[label] += 1
        if should_fail and call_counts[label] == 1:
            return _slow_fail(label)
        return _fast_ok(label)

    # "a" is submitted first and fails on its first attempt; "b" is
    # submitted second, is quick, and always succeeds. "a" finishes after
    # "b" since it is the one with the artificial delay.
    query_tasks = {
        "a": partial(_tracked, "a", True),
        "b": partial(_tracked, "b", False),
    }

    result: pd.DataFrame = asyncio.run(
        QueryProviderConnectionsMixin._exec_queries_threaded(
            query_tasks, progress=False, retry=True
        )
    )

    # "a" must have been retried (called twice); "b" never failed, so it
    # must only have been called once.
    check.equal(call_counts["a"], 2)
    check.equal(call_counts["b"], 1)
    check.equal(sorted(result["query_id"]), ["a", "b"])


def test_split_queries_sync():
    """Test queries split into time segments."""
    prov_args = dict(query_paths=_LOCAL_DATA_PATHS, data_paths=_LOCAL_DATA_PATHS)
    local_prov = QueryProvider("LocalData", **prov_args)

    start = datetime.now(timezone.utc) - pd.Timedelta("5h")
    end = datetime.now(timezone.utc) + pd.Timedelta("5min")
    delta = pd.Timedelta("1h")

    ranges = _calc_split_ranges(start, end, delta)
    local_prov.WindowsSecurity.list_host_logons(
        host_name="DESKTOP-12345", start=start, end=end
    )
    result_queries = local_prov.WindowsSecurity.list_host_logons(
        "print", host_name="DESKTOP-12345", start=start, end=end, split_query_by="1H"
    )
    queries = result_queries.split("\n\n")
    check.equal(len(queries), 6)

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
    check.equal(single_results.shape[0] * 6, result_queries.shape[0])
    # verify columns/schema is the same.
    check.equal(list(single_results.columns), list(result_queries.columns))


def test_split_queries_async():
    """Test queries split into time segments threaded execution."""
    prov_args = dict(query_paths=_LOCAL_DATA_PATHS, data_paths=_LOCAL_DATA_PATHS)
    local_prov = QueryProvider("LocalData", **prov_args)
    local_prov._query_provider.set_driver_property(
        DriverProps.SUPPORTS_THREADING, value=True
    )

    start = datetime.now(timezone.utc) - pd.Timedelta("5h")
    end = datetime.now(timezone.utc) + pd.Timedelta("5min")
    delta = pd.Timedelta("1h")

    ranges = _calc_split_ranges(start, end, delta)
    local_prov.WindowsSecurity.list_host_logons(
        host_name="DESKTOP-12345", start=start, end=end
    )
    result_queries = local_prov.WindowsSecurity.list_host_logons(
        "print", host_name="DESKTOP-12345", start=start, end=end, split_query_by="1H"
    )
    queries = result_queries.split("\n\n")
    check.equal(len(queries), 6)

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
    check.equal(single_results.shape[0] * 6, result_queries.shape[0])
    # verify columns/schema is the same.
    check.equal(list(single_results.columns), list(result_queries.columns))
