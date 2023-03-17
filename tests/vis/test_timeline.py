# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Test timeline."""
import os
from pathlib import Path
from typing import Any, Dict, List, Tuple

import pandas as pd
import pytest

from msticpy.common.exceptions import MsticpyParameterError
from msticpy.vis.timeline import display_timeline, display_timeline_values
from msticpy.vis.timeline_duration import display_timeline_duration

from ..unit_test_lib import TEST_DATA_PATH, exec_notebook, get_test_data_path

__author__ = "Ian Hellen"

# pylint: disable=redefined-outer-name


@pytest.fixture(scope="module")
def data():
    """Return DataFrame."""
    f_path = Path(TEST_DATA_PATH).joinpath("processes_on_host.csv")
    return _update_timestamps(
        pd.read_csv(
            str(f_path), parse_dates=["TimeGenerated", "TimeCreatedUtc"], index_col=0
        )
    ).head(100)


@pytest.fixture(scope="module")
def data_net():
    """Return DataFrame."""
    f_path = Path(TEST_DATA_PATH).joinpath("az_net_flows.csv")
    return _update_timestamps(
        pd.read_csv(
            str(f_path),
            parse_dates=["TimeGenerated", "FlowStartTime", "FlowEndTime"],
            index_col=0,
        ).head(100)
    )


def _update_timestamps(data):
    date_cols = data.select_dtypes("datetime").columns
    for col in date_cols:
        now_delta = pd.Timestamp("now") - data[col].max()
        data[col] = data[col] + now_delta
    return data


@pytest.fixture(scope="module")
def data_dict(data, data_net):
    """Return timeline dict parameter."""
    return {
        "Primary": {
            "data": data,
            "time_column": "TimeGenerated",
            "source_columns": ["Computer", "NewProcessName"],
            "color": "navy",
        },
        "Secondary": {
            "data": data_net,
            "time_column": "TimeGenerated",
            "source_columns": ["VMRegion", "AllExtIPs"],
            "color": "green",
        },
    }


def _get_event(data, n_events):
    return data.sample(n_events)


def _get_data_time(data, n_times):
    if n_times == 1:
        return data.sample(n_times).iloc[0]["TimeGenerated"]
    else:
        return [
            (time, "label") for time in data.sample(n_times)["TimeGenerated"].tolist()
        ]


TIMELINE_COMMON_ARGS: List[Tuple[Dict[str, List[Any]], List[Any]]] = [
    ({"color": ["green", "navy"]}, [True, True]),
    ({"group_by": [None, "Account"]}, [True, True]),
    ({"height": [None, 500, 1000]}, [True, True, True]),
    ({"legend": ["inline", "left", "right", "none"]}, [True, True, True, True]),
    ({"range_tool": [True, False]}, [True, True]),
    ({"time_column": ["TimeGenerated", "TimeCreatedUtc"]}, [True, True]),
    ({"title": [None, "Test timeline"]}, [True, True]),
    ({"width": [0, 300, 1000]}, [True, True, True]),
    ({"yaxis": [True, False]}, [True, True]),
]

TL_SRC_COLS = [
    (
        {"source_columns": [None, ["Computer", "NewProcessName"], ["OtherCol"]]},
        [True, True, (KeyError, MsticpyParameterError)],
    )
]

TL_ONLY_ARGS: List[Tuple[Dict[str, List[Any]], List[Any]]] = [
    ({"overlay_color": [None, "green"]}, [True, True]),
    ({"overlay_data": ["overlay_data"]}, [True]),
    ({"ygrid": [True, False]}, [True, True]),
    ({"xgrid": [True, False]}, [True, True]),
    ({"hide": [True, False]}, [True, True]),
]

TIMELINE_ALL = TIMELINE_COMMON_ARGS + TL_ONLY_ARGS + TL_SRC_COLS


def _get_test_ids(test_list):
    return [next(iter(test[0])) for test in test_list]


@pytest.mark.parametrize(
    "param, expected", TIMELINE_ALL, ids=_get_test_ids(TIMELINE_ALL)
)
def test_timeline(data, data_net, param, expected):
    """Test display_timeline."""
    for param, p_vals in param.items():
        for idx, p_val in enumerate(p_vals):
            expect_result = expected[idx]
            print(idx, expect_result)

            params = {param: p_val}
            if p_val == "overlay_data":
                params["overlay_data"] = data_net
            print(params)
            if isinstance(expect_result, bool):
                display_timeline(data, **params)
                data.mp_plot.timeline(**params)
            else:
                with pytest.raises(expect_result):
                    display_timeline(data, **params)
                    data.mp_plot.timeline(**params)


TIME_LINE_REF_ARGS = [
    ("ref_event", {"ref_event": (_get_event, 1)}),
    ("ref_time", {"ref_time": (_get_data_time, 1)}),
    (
        "ref_events",
        {
            "ref_events": (_get_event, 5),
            "ref_time_col": "TimeGenerated",
            "ref_col": "Computer",
        },
    ),
    ("ref_times", {"ref_times": (_get_data_time, 5)}),
    ("alert", {"alert": (_get_event, 1)}),
]


def _get_dict_test_ids(test_list):
    return [test[0] for test in test_list]


@pytest.mark.parametrize(
    "param", TIME_LINE_REF_ARGS, ids=_get_dict_test_ids(TIME_LINE_REF_ARGS)
)
def test_timeline_refs(data, param):
    """Test display_timeline with reference items."""
    params = {}
    test_name, test_def = param
    for p_name, p_def in test_def.items():
        if isinstance(p_def, tuple):
            func, f_param = p_def
            p_value = func(data, f_param)
            params[p_name] = p_value
        else:
            params[p_name] = p_def
        print(test_name, ",  params:", params)
        display_timeline(data, **params)


def test_timeline_dict(data_dict):
    """Test display_timeline_dict."""
    display_timeline(data_dict)


TL_VALUES_ARGS: List[Tuple[Dict[str, List[Any]], List[Any]]] = [
    ({"kind": ["circle", ["circle", "line", "vbar"]]}, [True, True]),
    ({"kind_grp": ["circle", ["circle", "line", "vbar"]]}, [True, True]),
    ({"leg_grp": ["inline", "left", "right", "none"]}, [True, True, True, True]),
]

TIMELINE_VALUES = TIMELINE_COMMON_ARGS + TL_SRC_COLS + TL_VALUES_ARGS


@pytest.mark.parametrize(
    "param, expected", TIMELINE_VALUES, ids=_get_test_ids(TIMELINE_VALUES)
)
def test_timeline_values(data, param, expected):
    """Test display_timeline_values."""
    for param, p_vals in param.items():
        for idx, p_val in enumerate(p_vals):
            expect_result = expected[idx]
            if param == "kind_grp":
                params = {"kind": p_val, "group_by": "Account"}
            elif param == "leg_grp":
                params = {"legend": p_val, "group_by": "Account"}
            else:
                params = {param: p_val}
            print(idx, expect_result)
            print("params:", params)
            if isinstance(expect_result, bool):
                display_timeline_values(data, value_col="EventID", **params)
                data.mp_plot.timeline_values(value_col="EventID", **params)
            else:
                with pytest.raises(expect_result):
                    display_timeline_values(data, value_col="EventID", **params)
                    data.mp_plot.timeline_values(value_col="EventID", **params)


TIMELINE_DURATION_ARGS: List[Tuple[Dict[str, List[Any]], List[Any]]] = [
    ({"color": ["green", "navy"]}, [True, True]),
    ({"height": [None, 500, 1000]}, [True, True, True]),
    ({"range_tool": [True, False]}, [True, True]),
    ({"time_column": ["TimeGenerated", "TimeCreatedUtc"]}, [True, True]),
    ({"title": [None, "Test timeline"]}, [True, True]),
    ({"width": [0, 300, 1000]}, [True, True, True]),
    ({"yaxis": [True, False]}, [True, True]),
    ({"ygrid": [True, False]}, [True, True]),
    ({"xgrid": [True, False]}, [True, True]),
    ({"hide": [True, False]}, [True, True]),
]


@pytest.mark.parametrize(
    "param, expected", TIMELINE_DURATION_ARGS, ids=_get_test_ids(TIMELINE_DURATION_ARGS)
)
def test_timeline_duration(data, param, expected):
    """Test display_timeline_duration."""
    for param, p_vals in param.items():
        for idx, p_val in enumerate(p_vals):
            expect_result = expected[idx]
            params = {param: p_val}
            print(idx, expect_result)
            print("params:", params)
            if isinstance(expect_result, bool):
                display_timeline_duration(data, group_by="Account", **params)
                data.mp_plot.timeline_duration(group_by="Account", **params)
            else:
                with pytest.raises(expect_result):
                    display_timeline_duration(data, value_col="Account", **params)
                    data.mp_plot.timeline_duration(group_by="Account", **params)


_NB_FOLDER = "docs/notebooks"
_NB_NAME = "EventTimeline.ipynb"
_MP_CONFIG_PATH = get_test_data_path().parent.joinpath("msticpyconfig-test.yaml")


@pytest.mark.skipif(
    not os.environ.get("MSTICPY_TEST_NOSKIP"), reason="Skipped for local tests."
)
def test_timeline_controls():
    """Test timeline notebook."""
    nb_path = Path(_NB_FOLDER).joinpath(_NB_NAME)
    exec_notebook(nb_path=nb_path, mp_config=_MP_CONFIG_PATH)
