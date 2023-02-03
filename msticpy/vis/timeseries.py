# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Module for common display functions."""
from itertools import zip_longest
from typing import Any, Dict

import pandas as pd
from bokeh.io import output_notebook, show
from bokeh.layouts import column
from bokeh.models import ColumnDataSource, HoverTool, NumeralTickFormatter

# pylint: disable=no-name-in-module
from bokeh.plotting import figure, reset_output
from deprecated.sphinx import deprecated

from .._version import VERSION
from ..common.utility import check_kwargs, export
from .timeline_common import (
    calc_auto_plot_height,
    create_range_tool,
    create_tool_tips,
    get_ref_event_time,
    get_tick_formatter,
    plot_ref_line,
)

__version__ = VERSION
__author__ = "Ashwin Patil"

_DEFAULT_KWARGS = [
    "color",
    "data",
    "group_by",
    "height",
    "legend",
    "range_tool",
    "ref_event",
    "ref_time",
    "source_columns",
    "time_column",
    "title",
    "width",
    "yaxis",
    "ref_events",
    "ref_time_col",
    "ref_col",
    "ref_times",
    "size",
]
_TL_VALUE_KWARGS = ["kind", "y", "x"]
_TS_KWARGS = ["xgrid", "ygrid"]


# pylint: disable=invalid-name, too-many-locals, too-many-statements
# pylint: disable=too-many-branches, too-many-function-args, too-many-arguments
@export  # noqa: C901, MC0001
def display_timeseries_anomalies(
    data: pd.DataFrame,
    y: str = "Total",
    time_column: str = "TimeGenerated",
    anomalies_column: str = "anomalies",
    period: int = 30,
    **kwargs,
) -> figure:
    """
    Display time series anomalies visualization.

    Parameters
    ----------
    data : pd.DataFrame
        DataFrame as a time series data set retrieved from KQL time series
        functions. Dataframe must have columns specified in `y`, `time_column`
        and `anomalies_column` parameters
    value_column : str, optional
        Name of column holding numeric values to plot against time series to
        determine anomalies
        (the default is 'Total')
    y : str, optional
        alias for "value_column"
    time_column : str, optional
        Name of the timestamp column
        (the default is 'TimeGenerated')
    anomalies_column : str, optional
        Name of the column holding binary status(1/0) for anomaly/benign
        (the default is 'anomalies')
    period : int, optional
        Period of the dataset for hourly-no of days, for daily-no of weeks.
        This is used to correctly calculate the plot height.
        (the default is 30)

    Other Parameters
    ----------------
    ref_time : datetime, optional
        Input reference line to display (the default is None)
    title : str, optional
        Title to display (the default is None)
    legend: str, optional
        Where to position the legend
        None, left, right or inline (default is None)
    yaxis : bool, optional
        Whether to show the yaxis and labels
    range_tool : bool, optional
        Show the the range slider tool (default is True)
    height : int, optional
        The height of the plot figure
        (the default is auto-calculated height)
    width : int, optional
        The width of the plot figure (the default is 900)
    xgrid : bool, optional
        Whether to show the xaxis grid (default is True)
    ygrid : bool, optional
        Whether to show the yaxis grid (default is False)
    color : list, optional
        List of colors to use in 3 plots as specified in order
        3 plots- line(observed), circle(baseline), circle_x/user specified(anomalies).
        (the default is ["navy", "green", "firebrick"])

    Returns
    -------
    figure
        The bokeh plot figure.

    """
    check_kwargs(kwargs, _DEFAULT_KWARGS + _TL_VALUE_KWARGS + _TS_KWARGS)

    reset_output()
    output_notebook()
    height: int = kwargs.pop("height", None)
    width: int = kwargs.pop("width", 1200)
    title: str = kwargs.pop("title", None)
    time_column = kwargs.get("x", time_column)
    show_range: bool = kwargs.pop("range_tool", True)
    color: list = kwargs.get("color", ["navy", "green", "firebrick"])
    color = [
        col1 or col2
        for col1, col2 in zip_longest(color[:3], ["navy", "green", "firebrick"])
    ]
    legend_pos: str = kwargs.pop("legend", "top_left")
    xgrid: bool = kwargs.pop("xgrid", False)
    ygrid: bool = kwargs.pop("ygrid", False)
    kind: str = kwargs.pop("kind", "circle_x")
    value_column = y or kwargs.pop("value_column", "Total")

    ref_time, ref_label = get_ref_event_time(**kwargs)

    source = ColumnDataSource(data)

    series_count = len(data) // period

    # Filtering anomalies to create new dataframe
    source_columns = [col for col in data.columns if col not in [anomalies_column]]
    data_anomaly = data[data[anomalies_column] == 1][source_columns].reset_index()

    hover = HoverTool(**(create_tool_tips(data, source_columns)))

    # Create the Plot figure
    title = title or "Time Series Anomalies Visualization"
    min_time = data[time_column].min()
    max_time = data[time_column].max()
    start_range = min_time - ((max_time - min_time) * 0.05)
    end_range = max_time + ((max_time - min_time) * 0.05)
    height = height or calc_auto_plot_height(series_count)

    plot = figure(
        x_range=(start_range, end_range),
        min_border_left=50,
        plot_height=height,
        plot_width=width,
        x_axis_label=time_column,
        x_axis_type="datetime",
        y_axis_label=value_column,
        x_minor_ticks=10,
        tools=[hover, "xwheel_zoom", "box_zoom", "reset", "save", "xpan"],
        toolbar_location="above",
        title=title,
    )

    if xgrid:
        plot.xgrid.minor_grid_line_color = "navy"
        plot.xgrid.minor_grid_line_alpha = 0.1
        plot.xgrid.grid_line_color = "navy"
        plot.xgrid.grid_line_alpha = 0.3
    else:
        plot.xgrid.grid_line_color = None
    if ygrid:
        plot.ygrid.minor_grid_line_color = "navy"
        plot.ygrid.minor_grid_line_alpha = 0.1
        plot.ygrid.grid_line_color = "navy"
        plot.ygrid.grid_line_alpha = 0.3
    else:
        plot.ygrid.grid_line_color = None

    # set the tick formatter
    plot.xaxis[0].formatter = get_tick_formatter()
    plot.yaxis.formatter = NumeralTickFormatter(format="00")

    plot.circle(
        time_column,
        value_column,
        line_color=color[0],
        size=4,
        source=source,
        legend_label="observed",
    )
    plot.line(
        time_column,
        "baseline",
        line_color=color[1],
        source=source,
        legend_label="baseline",
    )

    # create default plot args
    # pylint: disable=use-dict-literal
    arg_dict: Dict[str, Any] = {
        "x": time_column,
        "y": value_column,
        "size": 12,
        "color": color[2],
        "fill_alpha": 0.2,
        "legend_label": "anomalies",
        "source": ColumnDataSource(data_anomaly),
    }

    # setting the visualization types for anomalies based on user input to kind
    if kind == "cross":
        plot.cross(**arg_dict)
    elif kind == "diamond":
        plot.diamond(**arg_dict)
    elif kind == "diamond_cross":
        plot.diamond_cross(**arg_dict)
    else:
        plot.circle_x(**arg_dict)

    # interactive legend to hide single/multiple plots if selected
    plot.legend.location = legend_pos
    plot.legend.click_policy = "hide"

    # Create plot for the score column to act as as range selector
    rng_select = create_range_tool(
        data=data,
        y="score",
        min_time=min_time,
        max_time=max_time,
        plot_range=plot.x_range,
        width=width,
        height=height,
        time_column=time_column,
    )

    # if we have a reference timestamp, plot the time as a line
    if ref_time is not None:
        plot_ref_line(plot, ref_time, ref_label, data[value_column].max())

    if show_range:
        show(column(plot, rng_select))
        return column(plot, rng_select)

    show(plot)
    return plot


# Keep misspelled name for backward compatibility
@export
@deprecated(
    "Will be removed in version 2.0.0. Please use display_timeseries_anomalies",
    version="1.7.0",
)
def display_timeseries_anomolies(*args, **kwargs):
    """Call display_timeseries_anomalies."""
    return display_timeseries_anomalies(*args, **kwargs)
