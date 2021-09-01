# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Test module for nbwidgets."""
from datetime import datetime, timedelta

import ipywidgets as widgets
import pytest
import pytest_check as check

from msticpy.common.timespan import TimeSpan
from msticpy.nbtools import nbwidgets as nbw

__author__ = "Ian Hellen"

# pylint: disable=redefined-outer-name, protected-access


@pytest.fixture(scope="module")
def fixture_name():
    """Fixture_docstring."""


_PARSE_TIME_UNIT_TESTS = [
    ("minute", nbw.TimeUnit.minute),
    ("hour", nbw.TimeUnit.hour),
    ("second", nbw.TimeUnit.second),
    ("day", nbw.TimeUnit.day),
    ("week", nbw.TimeUnit.week),
    ("not_known", nbw.TimeUnit.minute),
]


def ptu_ids(tests):
    return [item[0] for item in tests]


@pytest.mark.parametrize(
    "test, expected", _PARSE_TIME_UNIT_TESTS, ids=ptu_ids(_PARSE_TIME_UNIT_TESTS)
)
def test_parse_time_unit(test, expected):
    """Parse time unit."""
    check.equal(nbw._parse_time_unit(test), expected)


_DEF_MAX_BUFFER_TESTS = [
    ((40, 3, nbw.TimeUnit.minute), 40),
    ((6, -3, nbw.TimeUnit.minute), 12),
    ((None, 8, nbw.TimeUnit.day), 32),
    ((None, 4, nbw.TimeUnit.day), 28),
    ((None, 24, nbw.TimeUnit.hour), 96),
    ((None, 4, nbw.TimeUnit.hour), 72),
    ((None, 24, nbw.TimeUnit.week), 96),
    ((None, 4, nbw.TimeUnit.week), 20),
    ((None, 10.5, nbw.TimeUnit.week), 42),
    ((None, 100, nbw.TimeUnit.minute), 400),
]


def _dmb_ids(tests):
    return ["-".join(str(sub) for sub in item[0]) for item in tests]


@pytest.mark.parametrize(
    "test, expected", _DEF_MAX_BUFFER_TESTS, ids=_dmb_ids(_DEF_MAX_BUFFER_TESTS)
)
def test_default_max_buffer(test, expected):
    """Test max buffer function."""
    check.equal(nbw._default_max_buffer(*test), expected)


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
    return ["-".join(item[0].keys()) for item in tests]


@pytest.mark.parametrize(
    "test, expected", _QT_PARAM_TESTS, ids=_qtp_ids(_QT_PARAM_TESTS)
)
def test_query_time_params(test, expected):
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

    qt._w_time_unit.value = "hour"
    check.equal(qt.units, "hour")
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
    value = "nothing"

    def action(self, value):
        self.value = value


def test_select_item():
    """Test SelectItem events."""
    act_obj = _TestSelectAction()

    sel_item = nbw.SelectItem(item_list=["one", "two", "three"], action=act_obj.action)
    check.equal(sel_item.value, "one")

    sel_item._wgt_select.value = "two"
    check.equal(sel_item.value, "two")
    check.equal(act_obj.value, "two")

    sel_item._w_filter.value = "t"
    for item in sel_item._wgt_select.options:
        check.is_in(item, ("two", "three"))

    sel_item._ipython_display_()


def test_select_subset():
    """Test SelectItem events."""
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
    return [test[0].__name__ for test in tests]


@pytest.mark.parametrize(
    "widget, w_props, w_funcs, args",
    _NBWIDGETS_ATTR_TEST,
    ids=_nbw_ids(_NBWIDGETS_ATTR_TEST),
)
def test_widget_attribs(widget, w_props, w_funcs, args):
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
