# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Test module for nbwidgets."""
import os
from datetime import datetime, timedelta
from pathlib import Path

import ipywidgets as widgets
import pytest
import pytest_check as check

from msticpy import nbwidgets as nbw
from msticpy.common.timespan import TimeSpan
from msticpy.nbwidgets.core import TimeUnit, default_max_buffer, parse_time_unit

from ..unit_test_lib import exec_notebook, get_test_data_path

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


_QT_SET_TIME_TESTS = [
    ((1,), (3,), (4, 0, TimeUnit.DAY)),
    ((4,), (6,), (10, 0, TimeUnit.DAY)),
    ((0.5,), (0,), (12, 0, TimeUnit.HOUR)),
    ((0.1,), (0.1,), (4, 0, TimeUnit.HOUR)),
    ((0, 120), (0, 120), (4, 0, TimeUnit.MINUTE)),
]


def _qt_set_time_ids(tests):
    """Return IDs for set time tests."""
    return [f"bef:{exp[0]}-aft:{exp[1]}-unit:{exp[2].name}" for _, _, exp in tests]


@pytest.mark.parametrize(
    "start, end, expected", _QT_SET_TIME_TESTS, ids=_qt_set_time_ids(_QT_SET_TIME_TESTS)
)
def test_query_time_set_time(start, end, expected):
    """Test setting time range from set_time method."""
    # Default starting time
    init_start = (1,)  # use tuples of (Day, Sec,..) to create time values
    init_end = (3,)
    qt = nbw.QueryTime(
        units="day",
        start=_END_TIME - timedelta(*init_start),
        end=_END_TIME + timedelta(*init_end),
    )

    # Set a new time
    new_time = TimeSpan((_END_TIME - timedelta(*start), _END_TIME + timedelta(*end)))
    qt.timespan = new_time

    # check before and after are expected values
    check.equal(qt.before, expected[0])
    check.equal(qt.after, expected[1])

    # current start/end should be original minus/plus
    # the difference between their offsets
    check.equal(_END_TIME - timedelta(*start), qt.start)
    check.equal(_END_TIME + timedelta(*end), qt.end)

    qt.value = new_time
    # check UI values updated correctly
    check.equal(qt._w_tm_range.value, (-expected[0], expected[1]))
    check.equal(qt._w_start_time_txt.value, new_time.start.isoformat(sep=" "))
    check.equal(qt._w_end_time_txt.value, new_time.end.isoformat(sep=" "))
    check.equal(qt._w_origin_dt.value, new_time.end.date())
    check.equal(qt._w_origin_tm.value, new_time.end.time().isoformat())
    check.equal(qt._w_time_unit.value, expected[2].name.capitalize())


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
_MP_CONFIG_PATH = get_test_data_path().parent.joinpath("msticpyconfig-test.yaml")


@pytest.mark.skipif(
    not os.environ.get("MSTICPY_TEST_NOSKIP"), reason="Skipped for local tests."
)
def test_widgets_notebook():
    """Run widgets notebook."""
    nb_path = Path(_NB_FOLDER).joinpath(_NB_NAME)
    exec_notebook(nb_path=nb_path, mp_config=_MP_CONFIG_PATH)
