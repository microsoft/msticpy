# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Test module for matrix_plot."""
from typing import Any, Dict

import pandas as pd
import pytest

# pylint: disable=unused-import
from msticpy.vis import mp_pandas_plot  # noqa: F401
from msticpy.vis.matrix_plot import plot_matrix

from ..unit_test_lib import get_test_data_path

__author__ = "Ian Hellen"

# pylint: disable=redefined-outer-name


@pytest.fixture(scope="module")
def network_data():
    """Get network data."""
    pd_file = get_test_data_path().joinpath("az_net_flows.csv")
    return pd.read_csv(
        pd_file,
        index_col=0,
        parse_dates=[
            "TimeGenerated",
            "FlowStartTime",
            "FlowEndTime",
            "FlowIntervalEndTime",
        ],
    )


_XY_PARAMS: Dict[str, Any] = {"x": "L7Protocol", "y": "AllExtIPs"}
_XYCOL_PARAMS: Dict[str, Any] = {"x_col": "L7Protocol", "y_col": "AllExtIPs"}

_TEST_PARAMS = [
    pytest.param({}, ValueError, id="no_xy_params"),
    pytest.param(_XY_PARAMS, None, id="basic_xy_params"),
    pytest.param(_XYCOL_PARAMS, None, id="basic_xycol_params"),
    pytest.param(
        {**_XY_PARAMS, "value_col": "TotalAllowedFlows"}, None, id="value_col"
    ),
    pytest.param(
        {**_XY_PARAMS, "value_col": "TotalAllowedFlows", "dist_count": True},
        None,
        id="value_col + dist count",
    ),
    pytest.param(
        {**_XY_PARAMS, "value_col": "TotalAllowedFlows", "invert": True},
        None,
        id="value_col + invert",
    ),
    pytest.param(
        {**_XY_PARAMS, "value_col": "TotalAllowedFlows", "log_size": True},
        None,
        id="value_col + log_size",
    ),
    pytest.param({**_XY_PARAMS, "dist_count": True}, None, id="value_col + dist count"),
    pytest.param({**_XY_PARAMS, "invert": True}, None, id="value_col + invert"),
    pytest.param({**_XY_PARAMS, "log_size": True}, None, id="value_col + log_size"),
    pytest.param({**_XY_PARAMS, "title": "Custom title"}, None, id="title"),
    pytest.param({**_XY_PARAMS, "intersect": True}, None, id="intersect"),
    pytest.param(
        {**_XY_PARAMS, "sort": "asc"},
        None,
        id="sort asc",
    ),
    pytest.param(
        {**_XY_PARAMS, "sort": "desc"},
        None,
        id="sort desc",
    ),
    pytest.param(
        {**_XY_PARAMS, "sort": True},
        None,
        id="sort True",
    ),
    pytest.param(
        {**_XY_PARAMS, "sort": None},
        None,
        id="sort None",
    ),
    pytest.param(
        {**_XY_PARAMS, "sort": "some string"},
        None,
        id="sort Other",
    ),
    pytest.param(
        {**_XY_PARAMS, "sort_x": "asc"},
        None,
        id="sort_x",
    ),
    pytest.param(
        {**_XY_PARAMS, "sort": "asc", "sort_y": "desc"},
        None,
        id="sort_y",
    ),
    pytest.param({**_XY_PARAMS, "hide": True}, None, id="hide"),
]


@pytest.mark.parametrize("test_data, exception", _TEST_PARAMS)
def test_matrix_plot(network_data, test_data, exception):
    """Function_docstring."""
    if exception:
        with pytest.raises(exception):
            plot_matrix(network_data, **test_data)
    else:
        plot_matrix(network_data, **test_data)


def test_matrix_plot_no_cols(network_data):
    """Function_docstring."""
    data = network_data[["L7Protocol", "AllExtIPs"]]
    plot_matrix(data, **_XY_PARAMS)


@pytest.mark.parametrize("test_data, exception", _TEST_PARAMS)
def test_matrix_plot_pd(network_data, test_data, exception):
    """Function_docstring."""
    if exception:
        with pytest.raises(exception):
            network_data.mp_plot.matrix(**test_data)
    else:
        network_data.mp_plot.matrix(**test_data)
