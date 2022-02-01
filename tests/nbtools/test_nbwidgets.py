# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Test module for nbwidgets."""
from datetime import datetime, timedelta
from pathlib import Path

import ipywidgets as widgets
import nbformat
from nbconvert.preprocessors import ExecutePreprocessor, CellExecutionError
import pytest
import pytest_check as check

from msticpy.common.timespan import TimeSpan
from msticpy.nbtools import nbwidgets as nbw
from msticpy.nbtools.nbwidgets.core import parse_time_unit, default_max_buffer, TimeUnit

__author__ = "Ian Hellen"

# pylint: disable=redefined-outer-name, protected-access


_PARSE_TIME_UNIT_TESTS = [
    ("minute", TimeUnit.MINUTE),
    ("hour", TimeUnit.HOUR),
    ("second", TimeUnit.SECOND),
    ("day", TimeUnit.DAY),
    ("week", TimeUnit.WEEK),
    ("not_known", TimeUnit.MINUTE),
]


def ptu_ids(tests):
    """Return IDs for parse time unit tests."""
    return [item[0] for item in tests]


@pytest.mark.parametrize(
    "test, expected", _PARSE_TIME_UNIT_TESTS, ids=ptu_ids(_PARSE_TIME_UNIT_TESTS)
)
def test_parse_time_unit(test, expected):
    """Parse time unit."""
    check.equal(parse_time_unit(test), expected)


_DEF_MAX_BUFFER_TESTS = [
    ((40, 3, TimeUnit.MINUTE), 40),
    ((6, -3, TimeUnit.MINUTE), 12),
    ((None, 8, TimeUnit.DAY), 32),
    ((None, 4, TimeUnit.DAY), 28),
    ((None, 24, TimeUnit.HOUR), 96),
    ((None, 4, TimeUnit.HOUR), 72),
    ((None, 24, TimeUnit.WEEK), 96),
    ((None, 4, TimeUnit.WEEK), 20),
    ((None, 10.5, TimeUnit.WEEK), 42),
    ((None, 100, TimeUnit.MINUTE), 400),
]


def _dmb_ids(tests):
    """Return IDs for default max buffer tests."""
    return ["-".join(str(sub) for sub in item[0]) for item in tests]


@pytest.mark.parametrize(
    "test, expected", _DEF_MAX_BUFFER_TESTS, ids=_dmb_ids(_DEF_MAX_BUFFER_TESTS)
)
def test_default_max_buffer(test, expected):
    """Test max buffer function."""
    check.equal(default_max_buffer(*test), expected)


_END_TIME = datetime.utcnow()
_START_TIME = _END_TIME - timedelta(1)

_QT_PARAM_TESTS = [
    (
        {"origin_time": _START_TIME},
        {"_w_origin_dt": _START_TIME.date(), "_w_origin_tm": str(_START_TIME.time())},
    ),
    ({"label": "The title"}, {"_label": "The title"}),
    ({"description": "The title"}, {"_label": "The title"}),
    ({"before": 1, "after": 1}, {"_w_tm_range": (-1, 1)}),
    (
        {"start": _START_TIME, "end": _END_TIME},
        {"_query_start": _START_TIME, "_query_end": _END_TIME},
    ),
    (
        {"timespan": TimeSpan(start=_START_TIME, end=_END_TIME)},
        {"_query_start": _START_TIME, "_query_end": _END_TIME},
    ),
    (
        {"max_before": 90, "max_after": 90, "units": "hour"},
        {"max_before": 90, "max_after": 90},
    ),
    ({"units": "hour"}, {"max_before": 72, "max_after": 72, "before": 6, "after": 6}),
    ({"units": "day"}, {"max_before": 28, "max_after": 28, "before": 1, "after": 1}),
]


def _qtp_ids(tests):
    """Return IDs for QueryTime params tests."""
    return ["-".join(item[0].keys()) for item in tests]


@pytest.mark.parametrize(
    "test, expected", _QT_PARAM_TESTS, ids=_qtp_ids(_QT_PARAM_TESTS)
)
def test_query_time_params(test, expected):
    """Test parameters for QueryTime widget."""
    qt = nbw.QueryTime(**test)
    for attr_name, attr_value in expected.items():
        attrib = getattr(qt, attr_name)
        if isinstance(attrib, widgets.Widget):
            check.equal(attr_value, getattr(attrib, "value"))
        else:
            check.equal(attrib, attr_value)


def test_query_time_events():
    """Test QueryTime events."""
    qt = nbw.QueryTime(
        units="day", origin_time=_END_TIME - timedelta(1), end=_END_TIME + timedelta(3)
    )
    q_start = qt.start
    q_end = qt.end
    qt._w_tm_range.value = (-2, 2)

    check.equal(qt.before, 2)
    check.equal(qt.after, 2)
    check.equal(q_start - timedelta(1), qt.start)
    check.equal(q_end + timedelta(1), qt.end)

    qt._w_time_unit.value = "Hour"
    check.equal(qt.units, "Hour")
    check.equal(qt.before, 6)
    check.equal(qt.after, 6)
    check.equal(qt.max_before, 72)
    check.equal(qt.max_after, 72)

    new_origin = _END_TIME + timedelta(1)
    qt._w_origin_dt.value = new_origin
    check.equal(qt.origin_time, new_origin)
    qt._w_origin_tm.value = "invalid time"
    check.equal(qt.origin_time, new_origin)


class _TestSelectAction:
    """Mock action class."""

    value = "nothing"

    def action(self, value):
        """Mock action method."""
        self.value = value


_SEL_ITEM_PARAMS = [
    ({"item_list": ["one", "two", "three"]}, ("one", "two", ("two", "three"))),
    ({"options": ["one", "two", "three"]}, ("one", "two", ("two", "three"))),
    (
        {"item_dict": {"one": "one-val", "two": "two-val", "three": "three-val"}},
        ("one-val", "two-val", ("two", "three")),
    ),
    (
        {"options": {"one": "one-val", "two": "two-val", "three": "three-val"}},
        ("one-val", "two-val", ("two", "three")),
    ),
]


@pytest.mark.parametrize("args, expected", _SEL_ITEM_PARAMS)
def test_select_item(args, expected):
    """Test SelectItem widget events."""
    act_obj = _TestSelectAction()

    sel_item = nbw.SelectItem(**args, action=act_obj.action)
    check.equal(sel_item.value, expected[0])

    sel_item.value = "two"
    check.equal(sel_item.value, expected[1])
    check.equal(act_obj.value, expected[1])

    sel_item._w_filter.value = "t"
    for item in sel_item._wgt_select.options:
        if isinstance(item, tuple):
            check.is_in(item[0], expected[2])
        else:
            check.is_in(item, expected[2])

    sel_item._ipython_display_()


def test_select_subset():
    """Test SelectSubset widget events."""
    src_items = ["one", "two", "three"]
    sel_subs = nbw.SelectSubset(source_items=src_items, default_selected=["one"])
    check.equal(sel_subs._select_list.options, ("one",))

    sel_subs._w_filter.value = "t"
    for item in sel_subs._source_list.options:
        check.is_in(item, ("two", "three"))
    sel_subs._w_filter.value = ""

    sel_subs._select_list.value = ["one"]
    sel_subs._b_del.click()
    check.is_false(sel_subs._select_list.options)
    sel_subs._source_list.value = ["two"]
    sel_subs._b_add.click()
    check.equal(sel_subs._select_list.options, ("two",))

    sel_subs._b_add_all.click()
    check.equal(len(sel_subs._select_list.options), len(src_items))
    for opt in src_items:
        check.is_in(opt, sel_subs._select_list.options)

    sel_subs._b_del_all.click()
    check.is_false(sel_subs._select_list.options)

    sel_subs._ipython_display_()


_NBWIDGETS_ATTR_TEST = [
    (nbw.Lookback, ["lookback"], None, {}),
    (nbw.QueryTime, None, None, {}),
    (nbw.GetText, None, None, {}),
    (nbw.Progress, None, None, {"completed_len": 10}),
    (nbw.OptionButtons, None, None, {}),
]


def _nbw_ids(tests):
    """Return IDs for widget attribute tests."""
    return [test[0].__name__ for test in tests]


@pytest.mark.parametrize(
    "widget, w_props, w_funcs, args",
    _NBWIDGETS_ATTR_TEST,
    ids=_nbw_ids(_NBWIDGETS_ATTR_TEST),
)
def test_widget_attribs(widget, w_props, w_funcs, args):
    """Check widgets expected properties."""
    def_props = ["value", "layout"]
    def_funcs = ["display", "_ipython_display_"]

    wgt = widget(**args)
    test_props = def_props + w_props if w_props else def_props
    for prop in test_props:
        getattr(wgt, prop)

    test_funcs = def_funcs + w_funcs if w_funcs else def_funcs
    for func in test_funcs:
        f_attr = getattr(wgt, func)
        f_attr()


_NB_FOLDER = "docs/notebooks"
_NB_NAME = "NotebookWidgets.ipynb"


# @pytest.mark.skipif(
#     not os.environ.get("MSTICPY_TEST_NOSKIP"), reason="Skipped for local tests."
# )
def test_widgets_notebook():
    """Run widgets notebook."""
    nb_path = Path(_NB_FOLDER).joinpath(_NB_NAME)
    abs_path = Path(_NB_FOLDER).absolute()
    with open(nb_path, encoding="utf-8") as f:
        nb = nbformat.read(f, as_version=4)
    ep = ExecutePreprocessor(timeout=600, kernel_name="python3")

    try:
        ep.preprocess(nb, {"metadata": {"path": abs_path}})
    except CellExecutionError:
        nb_err = str(nb_path).replace(".ipynb", "-err.ipynb")
        msg = f"Error executing the notebook '{nb_path}'.\n"
        msg += f"See notebook '{nb_err}' for the traceback."
        print(msg)
        with open(nb_err, mode="w", encoding="utf-8") as f:
            nbformat.write(nb, f)
        raise
