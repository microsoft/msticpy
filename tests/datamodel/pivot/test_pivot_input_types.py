# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Test pivot function handling of different input types."""
import warnings
from collections import namedtuple

import pandas as pd
import pytest
import pytest_check as check

from msticpy.context.geoip import GeoLiteLookup
from msticpy.context.tilookup import TILookup
from msticpy.datamodel import entities
from msticpy.init.pivot_core.pivot_container import PivotContainer

# pylint: disable=redefined-outer-name, unused-import, unused-argument
from .pivot_fixtures import create_pivot

__author__ = "Ian Hellen"

# pylint: disable=redefined-outer-name


@pytest.fixture(scope="session")
def data_providers():
    """Return dict of providers."""
    with warnings.catch_warnings():
        warnings.simplefilter("ignore", category=UserWarning)
        return {
            "ti_lookup": TILookup(),
            "geolite": GeoLiteLookup(),
            #  "ip_stack": IPStackLookup(),
        }


PivotQuery = namedtuple(
    "PivotQuery",
    "entity, value, provider, pivot_func, func_param, src_df_col, key_col, exp_col",
)

_IP_LIST = {
    "104.211.30.1": "Public",
    "104.211.30.2": "Public",
    "192.168.0.1": "Private",
    "127.0.0.1": "Loopback",
}

# pylint: disable=line-too-long
_B64_ENCODINGS = {
    "VGhpcyBpcyBhIHRlc3Qgb2YgYmFzZTY0IGVuY29kZWQgc3RyaW5n": "This is a test of base64 encoded string",
    "QSBLdXN0byBxdWVyeSBpcyBhIHJlYWQtb25seSByZXF1ZXN0IHRvIHByb2Nlc3MgZGF0YS"
    + "BhbmQgcmV0dXJuIHJlc3VsdHMu": "A Kusto query is a read-only request to process data and return results.",
    "VGhpcyBpcyBhbiBlbWJlZGRlZCBCNjQgVkdocGN5QnBjeUJoSUhSbGMzUWdiMllnWW1GelpU"
    + "WTBJR1Z1WTI5a1pXUWdjM1J5YVc1bg==": "This is an embedded B64 VGhpcyBpcyBhIHRlc3Qgb2YgYmFzZTY0IGVuY29kZWQgc3RyaW5n",
}
# pylint: enable=line-too-long

_URLS = {
    "https://www.contoso.com/path?p1=test&p2=10.2.4.5&hash=00236a2ae558018ed13b5222ef1bd987": {
        "dns": "www.contoso.com",
        "url": "input",
        "ipv4": "10.2.4.5",
        "md5_hash": "00236a2ae558018ed13b5222ef1bd987",
    },
    "https://www.microsoft.com/path?p1=test&p2=10.2.4.5&"
    + "hash=EE35D33B6F6A069CE82E45C83FBDE97A267261E9": {
        "dns": "www.microsoft.com",
        "url": "input",
        "ipv4": "10.2.4.5",
        "sha1_hash": "EE35D33B6F6A069CE82E45C83FBDE97A267261E9",
    },
}

_PIVOT_QUERIES = [
    pytest.param(
        PivotQuery(
            entity=entities.IpAddress,
            value=_IP_LIST,
            provider="util",
            pivot_func="ip_type",
            func_param="ip_str",
            src_df_col="ip",
            key_col="ip",
            exp_col="result",
        ),
        id="IpAddress-ip_type",
    ),
    pytest.param(
        PivotQuery(
            entity=entities.Process,
            value=_B64_ENCODINGS,
            provider="util",
            pivot_func="b64decode",
            func_param="value",
            src_df_col="cmdline",
            key_col="original_string",
            exp_col="decoded_string",
        ),
        id="Process-b64decode",
    ),
    pytest.param(
        PivotQuery(
            entity=entities.Url,
            value=_URLS,
            provider="util",
            pivot_func="extract_iocs",
            func_param="value",
            src_df_col="cmdline",
            key_col="Input",
            exp_col="Observable",
        ),
        id="Url-extract_iocs",
    ),
]


@pytest.mark.parametrize("test_case", _PIVOT_QUERIES)
def test_pivot_funcs_value(create_pivot, test_case):
    """Test calling function with value."""
    func = getattr(getattr(test_case.entity, test_case.provider), test_case.pivot_func)
    # Test value input
    val = next(iter(test_case.value.keys()))
    params = {test_case.func_param: val}
    result_df = func(**params)
    expected = next(iter(test_case.value.values()))
    if isinstance(expected, dict):
        for exp_value in expected.values():
            if exp_value == "input":
                exp_value = val
            check.is_in(exp_value, result_df[test_case.exp_col].values)
    else:
        check.is_in(expected, result_df.iloc[0][test_case.exp_col])


@pytest.mark.parametrize("test_case", _PIVOT_QUERIES)
def test_pivot_funcs_itbl(create_pivot, test_case):
    """Test calling function with iterable input."""
    func = getattr(getattr(test_case.entity, test_case.provider), test_case.pivot_func)
    # Test value input
    val = test_case.value.keys()

    params = {test_case.func_param: val}
    result_df = func(**params)

    for key, expected in test_case.value.items():
        key_results = result_df[result_df[test_case.key_col] == key]
        if isinstance(expected, dict):
            for exp_value in expected.values():
                if exp_value == "input":
                    exp_value = key
                check.is_in(exp_value, key_results[test_case.exp_col].values)
        else:
            check.is_in(expected, key_results.iloc[0][test_case.exp_col])


@pytest.mark.parametrize("test_case", _PIVOT_QUERIES)
def test_pivot_funcs_df(create_pivot, test_case):
    """Test calling function with DF input attributes."""
    func = getattr(getattr(test_case.entity, test_case.provider), test_case.pivot_func)
    # Test DF input
    val = test_case.value.keys()
    in_df = pd.DataFrame(val, columns=[test_case.src_df_col])
    result_df = func(data=in_df, src_column=test_case.src_df_col)
    for key, expected in test_case.value.items():
        key_results = result_df[result_df[test_case.key_col] == key]
        if isinstance(expected, dict):
            for exp_value in expected.values():
                if exp_value == "input":
                    exp_value = key
                check.is_in(exp_value, key_results[test_case.exp_col].values)
        else:
            check.is_in(expected, key_results.iloc[0][test_case.exp_col])


@pytest.mark.parametrize("join_type", ["left", "inner", "right"])
@pytest.mark.parametrize("test_case", _PIVOT_QUERIES)
def test_pivot_funcs_df_merge(create_pivot, join_type, test_case):
    """Test calling function with DF input attributes."""
    func = getattr(getattr(test_case.entity, test_case.provider), test_case.pivot_func)
    # Test DF input
    val = enumerate(test_case.value.keys())
    in_df = pd.DataFrame(val, columns=["idx", test_case.src_df_col])
    in_df["extra_col1"] = "test1"
    in_df["extra_col2"] = "test2"
    result_no_merge_df = func(data=in_df, src_column=test_case.src_df_col)
    result_df = func(data=in_df, src_column=test_case.src_df_col, join=join_type)

    in_cols = in_df.shape[1]
    no_merge_cols = result_no_merge_df.shape[1]
    merge_cols = result_df.shape[1]
    # merged DF should have result + input cols - join key col
    check.greater_equal(no_merge_cols + in_cols, merge_cols)

    if join_type in ("left", "inner"):
        # inner and left joins should have same or greater length as input
        check.greater_equal(result_df.shape[0], in_df.shape[0])
        # all the keys from the input should be in the merged output
        for key in in_df[test_case.src_df_col]:
            check.is_in(key, result_df[test_case.key_col].values)
    if join_type == "right":
        # We don't know how many results we get back from right join
        # (although should not be zero)
        check.greater(len(result_df), 0)
        # but all of its key values should be present in input
        for key in result_df[test_case.key_col].values:
            check.is_in(key, in_df[test_case.src_df_col].values)
