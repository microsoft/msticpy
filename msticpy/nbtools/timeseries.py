# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Module for common display functions."""
from typing import Any, Dict
from itertools import zip_longest

import pandas as pd
from statsmodels.tsa.seasonal import STL
from scipy import stats
from bokeh.io import output_notebook, show
from bokeh.models import ColumnDataSource, NumeralTickFormatter, HoverTool

# pylint: disable=no-name-in-module
from bokeh.plotting import figure, reset_output
from bokeh.layouts import column

from .._version import VERSION
from ..common.utility import export, check_kwargs
from .timeline import (
    _create_tool_tips,
    _calc_auto_plot_height,
    _create_range_tool,
    _get_tick_formatter,
    _add_ref_line,
    _get_ref_event_time,
    _DEFAULT_KWARGS,
    _TL_VALUE_KWARGS,
)

__version__ = VERSION
__author__ = "Ashwin Patil"


# pylint: disable=invalid-name, too-many-locals, too-many-statements
# pylint: disable=too-many-branches, too-many-function-args, too-many-arguments
@export  # noqa: C901, MC0001
def display_timeseries_anomolies(
    data: pd.DataFrame,
    y: str = "Total",
    time_column: str = "TimeGenerated",
    anomalies_column: str = "anomalies",
    source_columns: list = None,
    period: int = 30,
    **kwargs,
) -> figure:
    """
    Display time series anomalies visualization.

    Parameters
    ----------
    data : pd.DataFrame
        DataFrame as a time series data set retreived from KQL time series
        functions. Dataframe must have columns specified in `y`, `time_column`
        and `anomalies_column` parameters
    y : str, optional
        Name of column holding numeric values to plot against time series to
        determine anomolies
        (the default is 'Total')
    time_column : str, optional
        Name of the timestamp column
        (the default is 'TimeGenerated')
    anomalies_column : str, optional
        Name of the column holding binary status(1/0) for anomaly/benign
        (the default is 'anomalies')
    source_columns : list, optional
        List of default source columns to use in tooltips
        (the default is None)
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
    check_kwargs(kwargs, _DEFAULT_KWARGS + _TL_VALUE_KWARGS)

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

    ref_time, ref_label = _get_ref_event_time(**kwargs)

    source = ColumnDataSource(data)

    series_count = int(len(data) / period)

    # Filtering anomalies to create new dataframe
    source_columns = [col for col in data.columns if col not in [anomalies_column]]
    data_anomaly = data[data[anomalies_column] == 1][source_columns].reset_index()

    hover = HoverTool(
        tooltips=_create_tool_tips(data, source_columns),
        formatters={time_column: "datetime", "Tooltip": "printf"},
    )

    # Create the Plot figure
    title = title if title else "Time Series Anomalies Visualization"
    min_time = data[time_column].min()
    max_time = data[time_column].max()
    start_range = min_time - ((max_time - min_time) * 0.05)
    end_range = max_time + ((max_time - min_time) * 0.05)
    height = height if height else _calc_auto_plot_height(series_count)

    plot = figure(
        x_range=(start_range, end_range),
        min_border_left=50,
        plot_height=height,
        plot_width=width,
        x_axis_label=time_column,
        x_axis_type="datetime",
        y_axis_label=y,
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
    plot.xaxis[0].formatter = _get_tick_formatter()
    plot.yaxis.formatter = NumeralTickFormatter(format="00")

    plot.circle(
        time_column,
        y,
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
    arg_dict: Dict[str, Any] = dict(
        x=time_column,
        y=y,
        size=12,
        color=color[2],
        fill_alpha=0.2,
        legend_label="anomalies",
        source=ColumnDataSource(data_anomaly),
    )

    # setting the visualization types for anomalies based on user input to kind
    if kind == "diamond_cross":
        plot.diamond_cross(**arg_dict)
    elif kind == "cross":
        plot.cross(**arg_dict)
    elif kind == "diamond":
        plot.diamond(**arg_dict)
    else:
        plot.circle_x(**arg_dict)

    # interactive legend to hide single/multiple plots if selected
    plot.legend.location = legend_pos
    plot.legend.click_policy = "hide"

    # Create plot for the score column to act as as range selector
    rng_select = _create_range_tool(
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
        _add_ref_line(plot, ref_time, ref_label, data[y].max())

    if show_range:
        show(column(plot, rng_select))
        return column(plot, rng_select)

    show(plot)
    return plot


@export
def timeseries_anomalies_stl(data: pd.DataFrame, **kwargs,) -> pd.DataFrame:
    """ Discover anomalies in Timeseries data using
    STL (Seasonal-Trend Decomposition using LOESS) method using statsmodels package.

    Parameters
    ----------
    data : pd.DataFrame
        DataFrame as a time series data set retrived from data connector or external data source.
        Dataframe must have 2 columns with time column set as index and other numeric value.

    Other Parameters
    ----------------
    seasonal : int, optional
        Seasonality period of the input data required for STL. 
        Must be an odd integer, and should normally be >= 7 (default).
    period: int, optional
        Periodicity of the the input data. by default 24 (Hourly).
    score_threshold : float, optional
        standard deviation threshold value calculated using Z-score used to flag anomalies,
        by default 3

    Returns
    -------
    pd.DataFrame
        Returns a dataframe with additional columns by decomposing time series data
        into residual, trend, seasonal, weights, baseline, score and anomalies.
        The anomalies column will have 0, 1,-1 values based on score_threshold set.
    """
    seasonal: int = kwargs.get("seasonal", 7)
    period: int = kwargs.get("period", 24)
    score_threshold: float = kwargs.get("score_threshold", 3.0)

    if isinstance(data, pd.DataFrame) and not data.empty:
        # STL method does Season-Trend decomposition using LOESS. Accepts timeseries dataframe
        stl = STL(data, seasonal=seasonal, period=period)
        # Fitting the data - Estimate season, trend and residuals components.
        res = stl.fit()

        result = data.copy()

        # Create dataframe columns from decomposition results
        result["residual"] = res.resid
        result["trend"] = res.trend
        result["seasonal"] = res.seasonal
        result["weights"] = res.weights

        # Baseline is generally seasonal + trend
        result["baseline"] = result["seasonal"] + result["trend"]

        # Type cast and replace na values with 0
        result = result.fillna(0).astype("int64")

        # Calculate zscore based on residual column
        # this column does not contain seasonal/trend components
        result["score"] = stats.zscore(result["residual"])

        # create spikes(1) and dips(-1) based on threshold and seasonal columns
        result.loc[
            (result["score"] > score_threshold) & (result["seasonal"] > 0), "anomalies"
        ] = 1
        result.loc[
            (result["score"] > score_threshold) & (result["seasonal"] < 0), "anomalies"
        ] = -1
        result.loc[(result["score"] < score_threshold), "anomalies"] = 0

        # Datatype casting
        result["anomalies"] = result["anomalies"].astype("int64")
        result = result.reset_index()

        return result
