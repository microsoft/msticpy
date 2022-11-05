# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""data obfuscation tests."""
from collections import Counter
from pathlib import Path
from typing import Dict, Iterable

import pandas as pd
import pytest
import pytest_check as check

from msticpy.data import data_obfus
from msticpy.init import mp_pandas_accessors

from ..unit_test_lib import TEST_DATA_PATH

_TEST_DATA: Dict[str, Iterable] = {
    "hash_string": [("string", True), ("42424", True), (["42424"], True)],
    "hash_item": [
        (("no-delim", None), True),
        (("one-delim", "-"), True),
        (("mul.ti.del-im", "-.@"), True),
        ((42424, "-.@"), False),
    ],
    "hash_ip": [
        (42424, False),
        ("192.168.1.2", True),
        ("2a00:23c4:4780:ca00:90ab:c7fa:f451:de61", True),
        (
            [
                "93.165.156.37",
                "165.225.81.16",
                "165.225.197.26",
                "165.225.17.25",
                "85.255.234.226",
                "165.225.81.104",
                "165.225.81.22",
                "176.129.52.104",
                "185.13.49.14",
                "185.13.49.1",
                "165.225.81.37",
                "165.225.81.37",
                "165.225.80.134",
            ],
            True,
        ),
        ("not_an_ip_192.168.1.2", True),
        ("127.000.000.001", False),
        ("0.0.0.0", False),  # nosec
        ("10.1.23.456", True),
        ("17.16.23.456", True),
        ("17.1.23.456", True),
        ("192.168.1.20", True),
        ("1.1.1.1", True),
    ],
    "hash_list": [
        (["a", "b", "c"], True),
        (["a", "b", ["c", "d", "e"]], True),
        (["a", {"b": "name"}, ["c", "d", "e"]], True),
    ],
    "hash_dict": [
        ({"a": "name", "b": "noname"}, True),
        ({"a": "name", "b": ["one", "two"]}, True),
        ({"a": "name", "b": {1: "one", 2: "two"}}, True),
    ],
    "hash_sid": [
        ("S-1-5-21-996632719-2361334927-4038480536-500", True),
        ("S-1-5-18", False),
        ("S-1-5-21-996632719-2361334927-4038480536-1066", True),
        ("not-a-sid", False),
    ],
    "repl_uuid": [
        ("b53bdbb0-d4ae-48b1-9b78-bc6cad35b95f", True),
        ("c9275714-d709-45ae-afae-9edd756ee1a1", True),
        ("efe2f687-0581-4726-887c-898855a66e39", True),
        ("f93786b9-870f-4681-a4e7-2f81b10964a2", True),
        ("352f9e97-7d04-4f2b-b27f-2d9625c78dd1", True),
        ("be3d40b1-f317-4843-9010-a6cbe10dff00", True),
    ],
    "hash_account": [
        ("NT AUTHORITY/SYSTEM", False),
        ("someone@domain.com", True),
        ("root", False),
        ("CONTOSO\\my_admin", True),
        ("NT AUTHORITY", False),
        ("LOCAL SERVICE", False),
        ("network service", False),
        ("network disservice", True),
    ],
}


@pytest.mark.parametrize("test_input, expected", _TEST_DATA["hash_string"])
def test_hash_string(test_input, expected):
    """Test basic hash string function."""
    check.is_true((data_obfus.hash_string(test_input) != test_input) == expected)
    if isinstance(test_input, str) and test_input.isnumeric():
        check.is_true(data_obfus.hash_string(test_input).isnumeric())


@pytest.mark.parametrize("test_input, expected", _TEST_DATA["hash_item"])
def test_hash_item(test_input, expected):
    """Test hash delimited function."""
    test_str, delims = test_input
    result = data_obfus.hash_item(test_str, delims)
    check.is_true((result != test_str) == expected)
    if isinstance(test_str, str) and delims:
        delim_count = Counter(test_str)
        result_count = Counter(result)
        for delim in delims:
            check.equal(delim_count[delim], result_count[delim])


@pytest.mark.parametrize("test_input, expected", _TEST_DATA["hash_ip"])
def test_hash_ip(test_input, expected):
    """Test hash ip function."""
    result = data_obfus.hash_ip(test_input)
    if isinstance(test_input, list):
        for orig_ip, hashed_ip in zip(test_input, result):
            check.equal((orig_ip != hashed_ip), expected)
    else:
        check.equal((test_input != result), expected)

    if not isinstance(test_input, str):
        return
    if test_input.startswith("10."):
        check.is_true(result.startswith("10."))
    if test_input.startswith("17.16."):
        check.is_true(result.startswith("17.16."))
    if test_input.startswith("17.1."):
        check.is_false(result.startswith("17.1."))
    if test_input.startswith("192.168"):
        check.is_true(result.startswith("192.168"))
    if test_input == "1.1.1.1":
        check.is_true(len(set(result.split("."))) > 1)


@pytest.mark.parametrize("test_input, expected", _TEST_DATA["hash_list"])
def test_hash_list(test_input, expected):
    """Test hash list function."""
    result = data_obfus.hash_list(test_input)
    for orig_ip, hashed_ip in zip(test_input, result):
        check.equal(orig_ip != hashed_ip, expected)


@pytest.mark.parametrize("test_input, expected", _TEST_DATA["hash_dict"])
def test_hash_dict(test_input, expected):
    """Test hash dict function."""
    result = data_obfus.hash_dict(test_input)
    for key in test_input:
        check.equal(test_input[key] != result[key], expected)


@pytest.mark.parametrize("test_input, expected", _TEST_DATA["hash_sid"])
def test_hash_sid(test_input, expected):
    """Test hash SID function."""
    result = data_obfus.hash_sid(test_input)
    check.equal(test_input != result, expected)

    if test_input != result:
        check.equal(test_input.split("-")[-1], result.split("-")[-1])


@pytest.mark.parametrize("test_input, expected", _TEST_DATA["hash_account"])
def test_hash_acct(test_input, expected):
    """Test hash Account function."""
    result = data_obfus.hash_account(test_input)
    check.equal(test_input != result, expected)

    if test_input != result:
        check.is_in("account-#", result)


@pytest.mark.parametrize("test_input, expected", _TEST_DATA["repl_uuid"])
def test_hash_uuid(test_input, expected):
    """Test hash UUI function."""
    result = data_obfus.replace_guid(test_input)
    check.equal(test_input != result, expected)

    # test that source Guid maps onto same result
    result2 = data_obfus.replace_guid(test_input)
    check.equal(result, result2)


def test_obfuscate_df():
    """Test obfuscation on DataFrame."""
    win_procs = pd.read_pickle(Path(TEST_DATA_PATH).joinpath("win_proc_test.pkl"))

    out_df = data_obfus.mask_df(win_procs)

    check.equal(len(out_df), len(win_procs))
    for idx, row in win_procs.loc[:5].iterrows():
        for mapped_col in win_procs.columns:
            if data_obfus.OBFUS_COL_MAP.get(mapped_col) == "sid":
                # SIDs can be unchanged if well-known SID
                continue
            if mapped_col in data_obfus.OBFUS_COL_MAP:
                check.not_equal(row[mapped_col], out_df.loc[idx][mapped_col])
            else:
                check.equal(row[mapped_col], out_df.loc[idx][mapped_col])

        comp_uc, comp_ch = data_obfus.check_masking(out_df, win_procs, index=idx)
        n_changed = len(
            [col for col in win_procs.columns if col in data_obfus.OBFUS_COL_MAP]
        )
        n_unchanged = len(win_procs.columns) - n_changed
        # number of unchanged might be one less since some SIDs are not hashed
        check.is_true(len(comp_uc) in [n_unchanged, n_unchanged + 1])
        check.is_true(len(comp_ch) in [n_changed, n_changed - 1])


def test_pandas_accessor():
    """Test obfuscation with pandas accessor."""
    win_procs = pd.read_pickle(Path(TEST_DATA_PATH).joinpath("win_proc_test.pkl"))

    out_df = win_procs.mp.mask()
    check.equal(len(out_df), len(win_procs))
    for idx, row in win_procs.loc[:2].iterrows():
        for mapped_col in win_procs.columns:
            if data_obfus.OBFUS_COL_MAP.get(mapped_col) == "sid":
                # SIDs can be unchanged if well-known SID
                continue
            if mapped_col in data_obfus.OBFUS_COL_MAP:
                check.not_equal(row[mapped_col], out_df.loc[idx][mapped_col])
            else:
                check.equal(row[mapped_col], out_df.loc[idx][mapped_col])
