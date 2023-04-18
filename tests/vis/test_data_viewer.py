# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Module docstring."""
from datetime import datetime
from pathlib import Path

import pandas as pd
import pytest
import pytest_check as check
from bokeh.models import DateFormatter
from traitlets import TraitError

from msticpy.vis.data_viewer import DataTableColumnChooser, DataTableFilter
from msticpy.vis.data_viewer import DataViewerBokeh as DataViewer
from msticpy.vis.data_viewer import _get_cols_from_df

from ..unit_test_lib import TEST_DATA_PATH

__author__ = "Ian Hellen"

# pylint: disable=redefined-outer-name, protected-access


@pytest.fixture(scope="module")
def dataframe():
    """Get dataframe and columns."""
    df_path = Path(TEST_DATA_PATH).joinpath("host_logons.csv")
    return pd.read_csv(
        str(df_path),
        index_col=0,
        parse_dates=["TimeGenerated"],
        infer_datetime_format=True,
    )


def _apply_filter(control, column, oper, value, not_true=False):
    control._col_select.value = column
    check.equal(column, control.current_col)
    control._oper_sel.value = oper
    control._filter_value.value = value
    control._not_cb.value = not_true
    control._add_filter(btn=None)


def test_data_viewer(dataframe):
    """Test DataView functions."""
    dt_viewer = DataViewer(data=dataframe)

    check.equal(len(dataframe), len(dt_viewer.filtered_data))
    _apply_filter(dt_viewer.data_filter, "Account", "contains", "MSTICAlertsWin1")
    dt_viewer._apply_filter(btn=None)
    check.equal(3, len(dt_viewer.filtered_data))

    col_subset = list(dataframe.columns)[:5]
    dt_viewer.column_chooser = DataTableColumnChooser(
        data=dataframe, selected_cols=col_subset
    )
    dt_viewer._update_columns(btn=None)

    check.equal((3, 5), dt_viewer.filtered_data.shape)


def test_datatable_filter(dataframe):
    """Test basic data table filter functionality."""
    dt_filter = DataTableFilter(data=dataframe)
    for col in dataframe.columns:
        check.is_in(col, dt_filter._col_select.options)

    check.is_instance(dt_filter.filtered_dataframe, pd.DataFrame)
    check.equal(len(dataframe), len(dt_filter.filtered_dataframe))

    dt_filter._col_select.value = "Account"
    check.equal("Account", dt_filter.current_col)
    for oper in dt_filter._OPERATORS["string"]:
        check.is_in(oper, dt_filter._oper_sel.options)
    dt_filter._oper_sel.value = "contains"
    dt_filter._filter_value.value = "MSTICAlertsWin1"
    dt_filter._add_filter(btn=None)

    check.equal(1, len(dt_filter._curr_filters.options))
    check.equal(3, len(dt_filter.filtered_dataframe))

    dt_filter._col_select.value = "EventID"
    for oper in dt_filter._OPERATORS["other"]:
        check.is_in(oper, dt_filter._oper_sel.options)
    dt_filter._oper_sel.value = "=="
    dt_filter._filter_value.value = "4624"
    dt_filter._add_filter(btn=None)

    check.equal(2, len(dt_filter._curr_filters.options))
    check.equal(3, len(dt_filter.filtered_dataframe))

    check.equal(len(dataframe), len(dt_filter.bool_filters))

    dt_filter._curr_filters.value = dt_filter._curr_filters.options[0]
    dt_filter._col_select.value = "Account"
    dt_filter._oper_sel.value = "contains"
    dt_filter._filter_value.value = "SYSTEM"
    dt_filter._update_filter(btn=None)
    check.equal(2, len(dt_filter._curr_filters.options))
    check.equal(len(dataframe) - 3, len(dt_filter.filtered_dataframe))

    dt_filter._curr_filters.value = dt_filter._curr_filters.options[1]
    dt_filter._del_filter(btn=None)
    check.equal(1, len(dt_filter._curr_filters.options))
    check.equal(len(dataframe), len(dt_filter.filtered_dataframe))

    dt_filter._clear_filters(btn=None)
    check.equal(0, len(dt_filter._curr_filters.options))
    check.equal(len(dataframe), len(dt_filter.filtered_dataframe))


_FILTER_DEFS = [
    pytest.param("Account", False, "query", "EventID == 4624", 14, id="query"),
    pytest.param("Account", False, "==", "MSTICAlertsWin1\\MSTICAdmin", 2, id="str_eq"),
    pytest.param("Account", False, "matches", ".*Win1.*", 3, id="matches"),
    pytest.param("Account", False, "contains", "Win1", 3, id="contains"),
    pytest.param("TargetUserName", False, "in", "adm1nistrator, Something", 1, id="in"),
    pytest.param("EventID", False, "==", "4624", 14, id="num_eq"),
    pytest.param("EventID", False, ">", "4623", 14, id="gt"),
    pytest.param("EventID", False, "<", "4624", 0, id="lt"),
    pytest.param("EventID", False, ">=", "4624", 14, id="gte"),
    pytest.param("EventID", False, "<=", "4624", 14, id="lte"),
    pytest.param("EventID", False, "in", "4624, 4625", 14, id="num_in"),
    pytest.param("EventID", False, "between", "4623, 4625", 14, id="num_between"),
    pytest.param(
        "TimeGenerated", False, "<", datetime.utcnow().isoformat(), 14, id="date_lt"
    ),
    pytest.param(
        "EventID",
        False,
        "between",
        "4623, 4625, 4626",
        ValueError,
        id="fail_num_between",
    ),
    pytest.param("Account", False, ">", "ZZZZ", TraitError, id="str_gt"),
    pytest.param(
        "Account", True, "==", "MSTICAlertsWin1\\MSTICAdmin", 12, id="not_str_eq"
    ),
    pytest.param("Account", True, "matches", ".*Win1.*", 11, id="not_matches"),
    pytest.param("EventID", True, "==", "4624", 0, id="not_num_eq"),
    pytest.param("EventID", True, ">", "4623", 0, id="not_gt"),
    pytest.param("EventID", True, "between", "4623, 4625", 0, id="not_num_in"),
]


@pytest.mark.parametrize("col, not_true, oper, expr, result", _FILTER_DEFS)
def test_datatable_types(dataframe, col, not_true, oper, expr, result):
    """Check filter handling of different col types and operators."""
    dt_filter = DataTableFilter(data=dataframe)
    if isinstance(result, type) and issubclass(result, Exception):
        data = None
        with pytest.raises(result):
            _apply_filter(dt_filter, col, oper, expr, not_true)
            data = dt_filter.filtered_dataframe
        check.is_none(data)
    else:
        _apply_filter(dt_filter, col, oper, expr, not_true)
        check.equal(result, len(dt_filter.filtered_dataframe))


def test_datatable_column_chooser(dataframe):
    """Test basic data column chooser functionality."""
    dt_col = DataTableColumnChooser(data=dataframe)
    check.equal(len(dataframe.columns), len(dt_col.datatable_columns))
    for col in dataframe.columns:
        check.is_in(col, dt_col.dataframe_columns)

    col_subset = list(dataframe.columns)[:5]
    dt_col = DataTableColumnChooser(data=dataframe, selected_cols=col_subset)
    check.equal(len(col_subset), len(dt_col.datatable_columns))
    for col in col_subset:
        check.is_in(col, dt_col.dataframe_columns)


def test_get_cols_from_df(dataframe):
    """Test creating bokeh columns from dataframe."""
    col_dict = _get_cols_from_df(dataframe)
    check.equal(len(dataframe.columns), len(col_dict))
    check.is_instance(col_dict["TimeGenerated"].formatter, DateFormatter)
