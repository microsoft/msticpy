# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Test data query pivot functon handling of different input types."""
import warnings
from collections import namedtuple
from pathlib import Path

import pandas as pd
import pytest
import pytest_check as check

from msticpy.data import QueryProvider
from msticpy.datamodel import entities

# pylint: disable=unused-import
from msticpy.init.pivot import Pivot
from msticpy.init.pivot_core.pivot_container import PivotContainer

from ...unit_test_lib import get_test_data_path
from .pivot_fixtures import create_pivot, data_providers

__author__ = "Ian Hellen"

# pylint: disable=redefined-outer-name, unused-argument


@pytest.fixture(scope="session")
def create_data_providers():
    """Return dict of providers."""
    data_path = Path(get_test_data_path()) / "localdata"
    with warnings.catch_warnings():
        warnings.simplefilter("ignore", category=UserWarning)
        return {
            "LocalData": QueryProvider(
                "LocalData", data_paths=[str(data_path)], query_paths=[str(data_path)]
            ),
        }


PivotQuery = namedtuple(
    "PivotQuery",
    "entity, attrib, value, provider, pivot_func, func_param, src_df_col, exp_count",
)

_IP_LIST = [
    "104.211.30.1",
    "104.211.30.2",
    "192.168.0.1",
    "127.0.0.1",
]

_HOST_LIST = ["host1", "host2", "host3"]

_ACCOUNT_LIST = ["user1", "user2", "user3"]

_PIVOT_QUERIES = [
    pytest.param(
        PivotQuery(
            entity=entities.IpAddress,
            attrib="Address",
            value=_IP_LIST,
            provider="LocalData",
            pivot_func="list_azure_network_flows_by_ip",
            func_param="ip_address_list",
            src_df_col="ip",
            exp_count=1,
        ),
        id="IpAddress-list_azure_network_flows_by_ip",
    ),
    pytest.param(
        PivotQuery(
            entity=entities.Host,
            attrib="HostName",
            value=_HOST_LIST,
            provider="LocalData",
            pivot_func="list_host_logons",
            func_param="host_name",
            src_df_col="cmdline",
            exp_count=len(_HOST_LIST),
        ),
        id="Host-list_host_logons",
    ),
    pytest.param(
        PivotQuery(
            entity=entities.Account,
            attrib="Name",
            value=_ACCOUNT_LIST,
            provider="LocalData",
            pivot_func="list_logons_by_account",
            func_param="account_name",
            src_df_col="cmdline",
            exp_count=len(_ACCOUNT_LIST),
        ),
        id="Account-list_logons_by_account",
    ),
]


@pytest.mark.parametrize("test_case", _PIVOT_QUERIES)
def test_data_query_entity(create_pivot, test_case):
    """Test calling function with entity attributes."""
    # Test entity
    first_val = next(iter(test_case.value))
    init_args = {test_case.attrib: first_val}
    entity = test_case.entity(**init_args)
    func = getattr(getattr(entity, test_case.provider), test_case.pivot_func)
    # Test entity input
    # result_df = entity.LocalData.list_logons_by_account()
    result_df = func(entity)
    check.is_instance(result_df, pd.DataFrame)


@pytest.mark.parametrize("test_case", _PIVOT_QUERIES)
def test_data_query_value(create_pivot, test_case):
    """Test calling function with value."""
    func = getattr(getattr(test_case.entity, test_case.provider), test_case.pivot_func)
    # Test value input
    val = next(iter(test_case.value))
    params = {test_case.func_param: val}
    result_df = func(**params)
    check.is_instance(result_df, pd.DataFrame)


@pytest.mark.parametrize("test_case", _PIVOT_QUERIES)
def test_data_query_itbl(create_pivot, test_case):
    """Test calling function with iterable input."""
    func = getattr(getattr(test_case.entity, test_case.provider), test_case.pivot_func)

    val = next(iter(test_case.value))
    params = {test_case.func_param: val}
    single_val_result_df = func(**params)

    # Test iterable input
    val = test_case.value
    params = {test_case.func_param: val}
    result_df = func(**params)

    check.is_instance(result_df, pd.DataFrame)
    # For local data we are reading and returning the same data set each time
    # for multi input values, we expect to get that number
    # mulitplied by the number of input values, except in cases
    # where the query supports "list" parameters. In that case we
    # should just get 1x the data set.
    check.equal(len(single_val_result_df) * test_case.exp_count, len(result_df))


@pytest.mark.parametrize("test_case", _PIVOT_QUERIES)
def test_data_query_df(create_pivot, test_case):
    """Test calling function with DF input attributes."""
    func = getattr(getattr(test_case.entity, test_case.provider), test_case.pivot_func)

    val = next(iter(test_case.value))
    params = {test_case.func_param: val}
    single_val_result_df = func(**params)

    # Test DF input
    val = test_case.value
    in_df = pd.DataFrame(val, columns=[test_case.src_df_col])
    params = {test_case.func_param: test_case.src_df_col}
    result_df = func(data=in_df, **params)
    check.is_instance(result_df, pd.DataFrame)
    # For local data we are reading and returning the same data set each time
    # for multi input values, we expect to get that number
    # multiplied by the number of input values, except in cases
    # where the query supports "list" parameters. In that case we
    # should just get 1x the data set.
    check.equal(len(single_val_result_df) * test_case.exp_count, len(result_df))


@pytest.mark.parametrize("join_type", ["left", "inner", "right"])
@pytest.mark.parametrize("test_case", _PIVOT_QUERIES)
def test_pivot_funcs_df_merge(create_pivot, join_type, test_case):
    """Test calling function with DF input attributes."""
    func = getattr(getattr(test_case.entity, test_case.provider), test_case.pivot_func)
    # Test DF input
    val = test_case.value
    in_df = pd.DataFrame(val, columns=[test_case.src_df_col])
    params = {test_case.func_param: test_case.src_df_col}
    in_df["extra_col1"] = "test1"
    in_df["extra_col2"] = "test2"
    result_no_merge_df = func(data=in_df, **params)

    if test_case.entity not in (entities.Account, entities.Host):
        # The IP test uses a list param so we cannot do index joins
        # with it
        with pytest.warns(UserWarning):
            result_df = func(data=in_df, **params, join=join_type)
        return

    # should work ok with Account and Host
    result_df = func(data=in_df, **params, join=join_type)

    in_cols = in_df.shape[1]
    no_merge_cols = result_no_merge_df.shape[1]
    merge_cols = result_df.shape[1]
    # merged DF should have result + input cols - join key col
    check.greater_equal(no_merge_cols + in_cols, merge_cols)

    if join_type in ("left", "inner"):
        # inner and left joins should have same or greater length as input
        check.greater_equal(result_df.shape[0], in_df.shape[0])
        # all the keys from the input should be in the merged output
        for row_val in in_df[test_case.src_df_col]:
            check.is_in(row_val, result_df[test_case.src_df_col].values)
    if join_type == "right":
        # We don't know how many results we get back from right join
        # (although should not be zero)
        check.greater(len(result_df), 0)
        # but all of its key values should be present in input
        for row_val in result_df[test_case.src_df_col].values:
            check.is_in(row_val, in_df[test_case.src_df_col].values)

    join_in_data = {
        0: "0x3e7",
        1: "0xc90e957",
        2: "0xc90ea44",
        3: "0xc912d62",
        4: "0xc913737",
        10: "0x3e3",
        14: "0x3e4",
        15: "0xaddd",
        16: "0xafff",
        17: "0x3e5",
        23: "no_match",
    }
    in_df = pd.DataFrame(
        pd.Series(join_in_data), columns=["TargetLogonId"]
    ).reset_index()
    result_no_merge_df = func(data=in_df, **params)
    result_df = func(
        data=in_df,
        **params,
        join=join_type,
        left_on="TargetLogonId",
        right_on="TargetLogonId",
    )
    check.is_not_none(result_df)

    if join_type in ("inner", "right"):
        check.equal(len(result_df), len(result_no_merge_df))
        for val in join_in_data.values():
            if val != "no_match":
                check.is_in(val, result_df["TargetLogonId"].values)
            else:
                check.is_not_in(val, result_df["TargetLogonId"].values)
    if join_type == "left":
        check.equal(len(result_df), len(result_no_merge_df) + 1)
        for val in join_in_data.values():
            check.is_in(val, result_df["TargetLogonId"].values)
