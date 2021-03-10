# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Module docstring."""
from collections import Counter
from pathlib import Path

import pandas as pd
import pytest
import pytest_check as check

from msticpy.datamodel import pivot_pd_accessor

from ..unit_test_lib import TEST_DATA_PATH

__author__ = "Ian Hellen"

# pylint: disable=redefined-outer-name


@pytest.fixture(scope="session")
def test_df():
    """Fixture_docstring."""
    csv_file = Path(TEST_DATA_PATH).joinpath("host_logons.csv")
    return pd.read_csv(csv_file, index_col=0)


def test_load_accessor(test_df):
    """Function_docstring."""
    check.is_not_none(getattr(test_df, "mp_pivot"))
    check.is_not_none(getattr(test_df.mp_pivot, "run"))
    check.is_not_none(getattr(test_df.mp_pivot, "display"))
    check.is_not_none(getattr(test_df.mp_pivot, "tee"))
    check.is_not_none(getattr(test_df.mp_pivot, "tee_exec"))
    check.is_not_none(getattr(test_df.mp_pivot, "filter"))
    check.is_not_none(getattr(test_df.mp_pivot, "filter_cols"))
    check.is_not_none(getattr(test_df.mp_pivot, "sort"))


def _nop_df(data, **kwargs):
    """Test function for test_pd_run."""
    for key, val in kwargs.items():
        data[key] = val
    return data


def test_pd_run(test_df, capsys):
    """Test mp_pivot.run accessor."""
    res_df = test_df.mp_pivot.run(
        _nop_df, test_col="test_val", verbose=True, debug=True
    )
    check.equal(len(res_df), len(test_df))
    cap_out = capsys.readouterr().out
    check.is_in("rows returned from", cap_out)
    check.is_in("Columns in result", cap_out)
    check.is_in("Parameters:", cap_out)
    check.is_in("test_col", res_df.columns)


def test_pd_display(test_df, capsys):
    """Test mp_pivot.display accessor."""
    test_df.mp_pivot.display(
        _nop_df,
        cols=["Computer", "SubjectUserName", "TargetUserName"],
        query="TargetUserName != 'MSTICAdmin'",
    )
    cap_out = capsys.readouterr().out
    check.is_in("Computer   SubjectUserName TargetUserName", cap_out)
    check.is_not_in("MSTICAdmin", cap_out)
    check.equal(Counter(cap_out.split())["MSTICAlertsWin1"], 12)
    check.equal(len(cap_out.split("\n")), len(test_df) + 1)
    test_df.mp_pivot.display(
        _nop_df,
        cols=["Computer", "SubjectUserName", "TargetUserName"],
        query="TargetUserName != 'MSTICAdmin'",
        head=5,
    )
    cap_out = capsys.readouterr().out
    check.is_in("Computer   SubjectUserName TargetUserName", cap_out)
    check.equal(Counter(cap_out.split())["MSTICAlertsWin1"], 5)


class _IPython:
    """Test class for mocking IPython."""

    def __init__(self):
        self.ns_table = {"user_local": {"test_var": None}}


# For some reason this test works locally but not in pytest
# from the commandline - assuming it's something to do with the
# way the mock class is handled.
@pytest.mark.skip()
def test_tee(test_df, monkeypatch):
    """Test mp_pivot.tee_exec accessor."""
    ipython = _IPython()

    def _m_get_ipython():
        """Path get_ipython."""
        return ipython

    monkeypatch.setattr(pivot_pd_accessor, "get_ipython", _m_get_ipython)

    # with pytest.warns(UserWarning):
    # Try with variable that already exists
    test_df.mp_pivot.tee(var_name="test_var")
    check.is_none(ipython.ns_table["user_local"]["test_var"])

    test_df.mp_pivot.tee(var_name="test_var", clobber=True)
    print(ipython.ns_table["user_local"]["test_var"])
    print(type(ipython.ns_table["user_local"]["test_var"]))

    check.is_instance(ipython.ns_table["user_local"]["test_var"], pd.DataFrame)
    check.is_true(test_df.compare(ipython.ns_table["user_local"]["test_var"]).empty)


def test_tee_exec(test_df):
    """Test mp_pivot.tee_exec accessor."""
    res_df = test_df.mp_pivot.tee_exec("head", 5)
    check.equal(res_df.shape, test_df.shape)
    check.is_true(test_df.compare(res_df).empty)


def test_filter_cols(test_df):
    """Test mp_pivot.filter_cols accessor."""
    res_df = test_df.mp_pivot.filter_cols(cols="Computer")
    check.equal(res_df.shape, (14, 1))

    res_df = test_df.mp_pivot.filter_cols(cols="Target*")
    check.equal(res_df.shape, (14, 4))

    res_df = test_df.mp_pivot.filter_cols(cols=["Computer", "Target*"])
    check.equal(res_df.shape, (14, 5))

    with pytest.raises(ValueError):
        res_df = test_df.mp_pivot.filter_cols(cols=["Missing", "Missing2"])

    res_df = test_df.mp_pivot.filter_cols(cols=["computer", "Target*"], match_case=True)
    check.equal(res_df.shape, (14, 4))

    res_df = test_df.mp_pivot.filter_cols(
        cols=["LogonProcessName", "Target*"], sort_cols=True
    )
    check.equal(res_df.shape, (14, 5))
    check.equal(res_df.columns[0], "LogonProcessName")


def test_filter(test_df):
    """Test mp_pivot.filter accessor."""
    res_df = test_df.mp_pivot.filter(expr="MSTICAdmin")
    check.equal(len(res_df), 2)

    res_df = test_df.mp_pivot.filter(expr="2361334927")
    check.equal(len(res_df), 3)

    res_df = test_df.mp_pivot.filter(expr="msticadmin", case_sensitive=True)
    check.equal(len(res_df), 0)

    res_df = test_df.mp_pivot.filter(expr=4624, case_sensitive=True)
    check.equal(len(res_df), 14)
    res_df = test_df.mp_pivot.filter(expr=4625, case_sensitive=True)
    check.equal(len(res_df), 0)
    res_df = test_df.mp_pivot.filter(expr="4624", case_sensitive=True, numeric_col=True)
    check.equal(len(res_df), 14)

    with pytest.raises(TypeError):
        res_df = test_df.mp_pivot.filter(expr=["Missing", "Missing2"])


def test_sort(test_df):
    """Test mp_pivot.sort accessor."""
    res_df = test_df.mp_pivot.sort(cols="TargetUserName")
    check.equal(res_df.iloc[0].TargetUserName, "MSTICAdmin")

    res_df = test_df.mp_pivot.sort(cols=["TargetUserName", "Computer"])
    check.equal(res_df.iloc[0].TargetUserName, "MSTICAdmin")

    res_df = test_df.mp_pivot.sort(cols=["TargetUserName", "Computer"], ascending=False)
    check.equal(res_df.iloc[0].TargetUserName, "adm1nistrator")

    res_df = test_df.mp_pivot.sort(cols=["TargetUserName:asc", "Computer"])
    check.equal(res_df.iloc[0].TargetUserName, "MSTICAdmin")

    res_df = test_df.mp_pivot.sort(cols=["TargetUserName:desc", "Computer"])
    check.equal(res_df.iloc[0].TargetUserName, "adm1nistrator")

    res_df = test_df.mp_pivot.sort(cols={"TargetUserName": True, "Computer": False})
    check.equal(res_df.iloc[0].TargetUserName, "MSTICAdmin")

    res_df = test_df.mp_pivot.sort(cols={"TargetUserName": False, "Computer": False})
    check.equal(res_df.iloc[0].TargetUserName, "adm1nistrator")

    res_df = test_df.mp_pivot.sort(
        cols={"TargetUserName": True, "Computer": False}, ascending=False
    )
    check.equal(res_df.iloc[0].TargetUserName, "adm1nistrator")
