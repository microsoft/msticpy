# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Timeline duration plot."""
from datetime import datetime
from typing import Iterable, List, Optional, Tuple, Union

import attr
import pandas as pd
from bokeh.io import output_notebook, show
from bokeh.layouts import column
from bokeh.models import ColumnDataSource, HoverTool, LayoutDOM

# pylint: enable=no-name-in-module
from bokeh.plotting import figure, reset_output
from bokeh.transform import dodge

from .._version import VERSION
from ..common.data_utils import ensure_df_datetimes
from ..common.utility import check_kwargs, export
from .timeline_common import (
    calc_auto_plot_height,
    check_df_columns,
    create_range_tool,
    create_tool_tips,
    get_tick_formatter,
    get_time_bounds,
    plot_ref_events,
    set_axes_and_grids,
)

# pylint: disable=unused-import
# Importing to activate pandas accessors
from .timeline_pd_accessor import TimeLineAccessor  # noqa F401

# pylint: enable=unused-import


__version__ = VERSION
__author__ = "Ian Hellen"

_TIMELINE_HELP = (
    "https://msticpy.readthedocs.io/en/latest/msticpy.init.html"
    "#msticpy.init.timeline_duration.{plot_type}"
)


@attr.s(auto_attribs=True)
class PlotParams:
    """Plot params for time_duration."""

    height: Optional[int] = None
    width: int = 900
    title: Optional[str] = None
    yaxis: bool = True
    range_tool: bool = True
    xgrid: bool = True
    ygrid: bool = False
    hide: bool = False
    color: str = "navy"
    ylabel_cols: Iterable[str] = attr.Factory(list)
    ref_events: Optional[pd.DataFrame] = None
    ref_col: Optional[str] = None
    ref_times: Optional[List[Tuple[datetime, str]]] = None
    source_columns: List = []

    @classmethod
    def field_list(cls) -> List[str]:
        """Return field names as a list."""
        return list(attr.fields_dict(cls).keys())


# pylint: disable=too-many-locals


@export
def display_timeline_duration(
    data: pd.DataFrame,
    group_by: Union[Iterable[str], str],
    time_column: str = "TimeGenerated",
    end_time_column: Optional[str] = None,
    **kwargs,
) -> LayoutDOM:  # noqa: C901, MC0001
    """
    Display a duration timeline of events grouped by one or more columns.

    Parameters
    ----------
    data : pd.DataFrame
        Data to plot
    group_by : Union[Iterable[str], str]
        The column name or iterable of column names to group the data by.
    time_column : str
        Primary time column - will be used to calculate the
        start time of the duration for each group.
        If `end_time_column` is not specified it will also be used to
        calculate the end time.
    end_time_column : Optional[str]
        If supplied, it will be used to calculate the end time
        of the duration for each group.

    Other Parameters
    ----------------
    title : str, optional
        Title to display (the default is None)
    ylabel_cols : Optional[Iterable[str]], optional
        The subset of the group columns to use for the y-axis labels.
    yaxis : bool, optional
        Whether to show the yaxis and labels
    range_tool : bool, optional
        Show the the range slider tool (default is True)
    source_columns : list, optional
        List of default source columns to use in tooltips
        (the default is None)
    height : int, optional
        The height of the plot figure
        (the default is auto-calculated height)
    width : int, optional
        The width of the plot figure (the default is 900)
    color : str
        Default series color (default is "navy")
    ref_events : pd.DataFrame, optional
        Add references line/label using the event times in the dataframe.
        (the default is None)
    ref_col : str, optional
        The column name to use for the label from `ref_events`
        (the default is None)
    ref_times : List[Tuple[datetime, str]], optional
        Add one or more reference line/label using (the default is None)

    Returns
    -------
    LayoutDOM
        The bokeh plot figure.

    """
    reset_output()
    output_notebook()

    check_kwargs(kwargs, PlotParams.field_list())
    param = PlotParams(**kwargs)

    group_by = [group_by] if isinstance(group_by, str) else list(group_by)
    end_time_column = end_time_column or time_column
    data = ensure_df_datetimes(data, columns=list(set([time_column, end_time_column])))
    check_df_columns(
        data,
        group_by + [end_time_column, time_column],
        _TIMELINE_HELP,
        "display_timeline_duration",
    )
    grouped_data = _group_durations(data, group_by, time_column, end_time_column)
    min_time = grouped_data["start_time"].min()
    max_time = grouped_data["end_time"].max()

    # Create Bokeh Column Data Sources
    col_src = ColumnDataSource(grouped_data)
    # Re-join original data to grouped data
    all_data = data.merge(grouped_data, on=group_by)
    all_data_src = ColumnDataSource(all_data)

    tool_tip_cols = [*group_by, "start_time", "end_time"]
    if "source_columns" in kwargs:
        tool_tip_cols += kwargs["source_columns"]

    hover = HoverTool(**(create_tool_tips(data, tool_tip_cols)))

    title = (
        f"Timeline: {param.title}"
        if param.title
        else f"Event Duration Timeline for {', '.join(group_by)}"
    )

    start_range, end_range, min_time, max_time = get_time_bounds(min_time, max_time)
    height = param.height or calc_auto_plot_height(len(grouped_data))
    # Concatenate ylabel columns to display on y-axis
    if len(group_by) > 1:
        y_range = grouped_data[group_by[0]].str.cat(
            grouped_data[group_by[1:]], sep=" / "
        )
    else:
        y_range = grouped_data[group_by[0]]

    plot = figure(
        x_range=(start_range, end_range),
        y_range=y_range,
        min_border_left=50,
        plot_height=height,
        plot_width=param.width,
        x_axis_label="Event Time",
        y_axis_label=", ".join(group_by),
        x_axis_type="datetime",
        x_minor_ticks=10,
        tools=[hover, "xwheel_zoom", "box_zoom", "reset", "save", "xpan"],
        title=title,
    )

    # Plot the duration rectangles
    rect_plot_params = {
        "height": 0.3,
        "source": col_src,
        "fill_alpha": 0.4,
        "color": param.color,
    }
    plot.rect(x="Center", y=dodge("Row", 0.5), width="Width", **rect_plot_params)

    # Plot the individual events as diamonds
    plot.diamond(
        x=time_column,
        y=dodge("Row", 0.5),
        color=param.color,
        alpha=0.5,
        size=5,
        source=all_data_src,
    )

    # Set grid parameters
    set_axes_and_grids(None, plot, param.yaxis, param.ygrid, param.xgrid)

    # Create plot bar to act as as range selector
    rng_select = create_range_tool(
        data=all_data,
        min_time=min_time,
        max_time=max_time,
        plot_range=plot.x_range,
        width=param.width,
        height=height,
        time_column=time_column,
        y="Row",
    )

    # set the tick datetime formatter
    plot.xaxis[0].formatter = get_tick_formatter()
    plot_ref_events(
        plot=plot,
        ref_events=param.ref_events,
        time_col=time_column,
        group_count=len(grouped_data),
        ref_col=param.ref_col,
        ref_times=param.ref_times,
    )

    plot_layout = column(plot, rng_select) if param.range_tool else plot
    if not param.hide:
        show(plot_layout)

    return plot_layout


def _group_durations(
    data: pd.DataFrame, group_by: List[str], time_column: str, end_time_column: str
):
    """Group the data and calculate start and end times."""
    grouped_data = data.groupby(group_by).agg(
        start_time=pd.NamedAgg(time_column, "min"),
        end_time=pd.NamedAgg(end_time_column, "max"),
    )
    # If we don't have an endtime for certain types just consider them to be single time events
    for row in grouped_data.iterrows():
        if pd.isnull(row[1]["end_time"]):
            grouped_data.at[row[0], "end_time"] = row[1]["start_time"]
    grouped_data = grouped_data.reset_index()
    grouped_data.index.name = "Row"
    grouped_data = grouped_data.reset_index()
    grouped_data["Width"] = grouped_data["end_time"] - grouped_data["start_time"]
    grouped_data["Center"] = grouped_data["start_time"] + (grouped_data["Width"] / 2)
    return grouped_data
