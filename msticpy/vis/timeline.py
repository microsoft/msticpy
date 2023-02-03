# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Timeline base plot."""
from datetime import datetime
from typing import Any, Dict, Iterable, List, Optional, Set, Tuple, Union

import attr
import pandas as pd
from bokeh.io import output_notebook, show
from bokeh.layouts import column
from bokeh.models import ColumnDataSource, HoverTool, LayoutDOM, Legend
from bokeh.models.annotations import LegendItem
from bokeh.plotting import figure, reset_output

from .._version import VERSION
from ..common.data_utils import ensure_df_datetimes
from ..common.utility import check_kwargs, export
from .timeline_common import (
    calc_auto_plot_height,
    create_range_tool,
    create_tool_tips,
    get_color_palette,
    get_def_source_cols,
    get_ref_event_time,
    get_tick_formatter,
    get_time_bounds,
    plot_ref_events,
    plot_ref_line,
    set_axes_and_grids,
)

# pylint: disable=unused-import
# Importing to activate pandas accessors
from .timeline_pd_accessor import TimeLineAccessor  # noqa F401
from .timeline_values import display_timeline_values  # noqa F401

# pylint: enable=unused-import

__version__ = VERSION
__author__ = "Ian Hellen"


@attr.s(auto_attribs=True)
class PlotParams:
    """Plot params for time_duration."""

    time_column: str = "TimeGenerated"
    height: Optional[int] = None
    width: int = 900
    title: str = "Events"
    yaxis: bool = True
    range_tool: bool = True
    group_by: Optional[str] = None
    legend: Optional[str] = None
    xgrid: bool = True
    ygrid: bool = False
    hide: bool = False
    color: str = "navy"
    size: int = 10
    ylabel_cols: Iterable[str] = attr.Factory(list)
    ref_event: Optional[Any] = None
    ref_time: Optional[datetime] = None
    ref_events: Optional[pd.DataFrame] = None
    ref_col: Optional[str] = None
    ref_time_col: Optional[str] = None
    ref_times: Optional[List[Tuple[datetime, str]]] = None
    ref_label: str = "Ref time"
    source_columns: List[str] = []
    alert: Any = None
    overlay_color: Optional[str] = None
    overlay_data: Optional[pd.DataFrame] = None
    overlay_columns: Iterable[str] = attr.Factory(list)

    @classmethod
    def field_list(cls) -> List[str]:
        """Return field names as a list."""
        return list(attr.fields_dict(cls).keys())

    @property
    def fmt_title(self):
        """Return formatted title."""
        return f"Timeline: {self.title}"


@export
def display_timeline(
    data: Union[pd.DataFrame, dict],
    time_column: str = "TimeGenerated",
    source_columns: Optional[List[str]] = None,
    **kwargs,
) -> LayoutDOM:
    """
    Display a timeline of events.

    Parameters
    ----------
    data : Union[dict, pd.DataFrame]
        Either
        dict of data sets to plot on the timeline with the following structure::

            Key (str) - Name of data set to be displayed in legend
            Value (Dict[str, Any]) - containing:
                data (pd.DataFrame) - Data to plot
                time_column (str, optional) - Name of the timestamp column
                source_columns (list[str], optional) - source columns to use
                    in tooltips
                color (str, optional) - color of data points for this data
                size (int) - size of plotted event glyphs
            If any of the last values are omitted, they default to the values
            supplied as parameters to the function (see below)

        Or
        DataFrame as a single data set or grouped into individual
        plot series using the `group_by` parameter
    time_column : str, optional
        Name of the timestamp column
        (the default is 'TimeGenerated')
    source_columns : Optional[List[str]]
        List of default source columns to use in tooltips
        (the default is None)

    Other Parameters
    ----------------
    title : str, optional
        Title to display (the default is None)
    alert : SecurityAlert, optional
        Add a reference line/label using the alert time (the default is None)
    ref_event : Any, optional
        Add a reference line/label using the alert time (the default is None)
    ref_time : datetime, optional
        Add a reference line/label using `ref_time` (the default is None)
    group_by : str
        (where `data` is a DataFrame)
        The column to group timelines on
    size : Union[int, str]
        The size of the event glyph.
        If a string the size is taken as a column in the input data.
        If an integer, this is used as the fixed size.
    legend: str, optional
        "left", "right", "inline" or "none"
        (the default is to show a legend when plotting multiple series
        and not to show one when plotting a single series)
    yaxis : bool, optional
        Whether to show the yaxis and labels (default is False)
    ygrid : bool, optional
        Whether to show the yaxis grid (default is False)
    xgrid : bool, optional
        Whether to show the xaxis grid (default is True)
    range_tool : bool, optional
        Show the the range slider tool (default is True)
    height : int, optional
        The height of the plot figure
        (the default is auto-calculated height)
    width : int, optional
        The width of the plot figure (the default is 900)
    color : str
        Default series color (default is "navy")
    overlay_data : pd.DataFrame:
        A second dataframe to plot as a different series.
    overlay_color : str
        Overlay series color (default is "green")
    hide : bool, optional
        If True, create but do not display the plot.
        By default, False.
    ref_events : pd.DataFrame, optional
        Add references line/label using the event times in the dataframe.
        (the default is None)
    ref_time_col : str, optional
        Add references line/label using the this column in `ref_events`
        for the time value (x-axis).
        (this defaults the value of the `time_column` parameter or 'TimeGenerated'
        `time_column` is None)
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
    # Get args
    check_kwargs(kwargs, PlotParams.field_list())
    param = PlotParams(
        time_column=time_column, source_columns=source_columns or [], **kwargs
    )
    param.ref_time, param.ref_label = get_ref_event_time(**kwargs)

    if isinstance(data, pd.DataFrame):
        if param.overlay_data is not None:
            aggr_data = {
                "Primary": {
                    "data": data,
                    "time_column": param.time_column,
                    "source_columns": source_columns,
                    "color": param.color,
                    "size": param.size,
                },
                "Secondary": {
                    "data": param.overlay_data,
                    "time_column": time_column,
                    "source_columns": param.overlay_columns,
                    "color": param.overlay_color,
                    "size": param.size,
                },
            }
            return _display_timeline_dict(data=aggr_data, param=param)

        # Create a dictionary from a grouped or simple series
        series_dict = _create_dict_from_grouping(
            data=data,
            source_columns=param.source_columns,
            time_column=param.time_column,
            group_by=param.group_by,
            color=param.color,
            size=param.size,
        )
        return _display_timeline_dict(data=series_dict, param=param)

    if isinstance(data, dict):
        return _display_timeline_dict(data, param=param)
    return None


def _display_timeline_dict(
    data: dict, param: PlotParams
) -> figure:  # noqa: C901, MC0001
    """
    Display a timeline of events.

    Parameters
    ----------
    data : dict
        Data points to plot on the timeline.
        The dict should contain the following structure:
            Key - Name of data type to be displayed in legend
            Value - dict of data containing:
                data : pd.DataFrame
                    Data to plot
                time_column : str
                    Name of the timestamp column
                source_columns : list
                    List of source columns to use in tooltips
                color: str
                    Color of data points for this data
    param : PlotParams
        Plot parameters object

    Returns
    -------
    figure
        The bokeh plot figure.

    """
    reset_output()
    output_notebook()

    tool_tip_columns, min_time, max_time = _unpack_data_series_dict(data, param)
    if min_time is None or max_time is None:
        print("No data to plot.")
        return figure()

    series_count = len(data)

    hover = HoverTool(**(create_tool_tips(data, tool_tip_columns)))

    start_range, end_range, min_time, max_time = get_time_bounds(min_time, max_time)
    height = param.height or calc_auto_plot_height(len(data))
    y_range = ((-1 / series_count), series_count - 1 + (1 / series_count))
    plot = figure(
        x_range=(start_range, end_range),
        y_range=y_range,
        min_border_left=50,
        plot_height=height,
        plot_width=param.width,
        x_axis_label="Event Time",
        x_axis_type="datetime",
        x_minor_ticks=10,
        tools=[hover, "xwheel_zoom", "box_zoom", "reset", "save", "xpan"],
        title=param.fmt_title,
    )

    set_axes_and_grids(data, plot, param.yaxis, param.ygrid, param.xgrid)

    # Create plot bar to act as as range selector
    rng_select = create_range_tool(
        data=data,
        min_time=min_time,
        max_time=max_time,
        plot_range=plot.x_range,
        width=param.width,
        height=height,
    )

    # set the tick datetime formatter
    plot.xaxis[0].formatter = get_tick_formatter()
    # plot the data
    _plot_series(data, plot, param.legend)

    if param.ref_time is not None:
        plot_ref_line(plot, param.ref_time, param.ref_label, len(data))  # type: ignore
    elif param.ref_events is not None or param.ref_times is not None:
        plot_ref_events(
            plot=plot,
            ref_events=param.ref_events,
            time_col=param.ref_time_col or param.time_column,
            group_count=series_count,
            ref_col=param.ref_col,
            ref_times=param.ref_times,
        )

    plot_layout = column(plot, rng_select) if param.range_tool else plot
    if not param.hide:
        show(plot_layout)

    return plot_layout


def _plot_series(data, plot, legend_pos):
    """Plot data series and add legend."""
    # plot groups individually so that we can create an interactive legend
    # if legend_pos is "inline", we add add the normal legend inside the plot
    # if legend_pos is "left" or "right", we add the legend to the side
    if len(data) > 1 and not legend_pos:
        legend_pos = "left"
    legend_items = []
    for ser_name, series_def in data.items():
        size_param = series_def.get("size", 10)
        glyph_size: Union[pd.Series, int]
        if isinstance(size_param, str):
            if size_param in series_def["data"].columns:
                glyph_size = series_def["data"][size_param]
            else:
                glyph_size = 10
        else:
            glyph_size = size_param
        time_col = "TimeGenerated"
        if "time_column" in series_def:
            time_col = series_def["time_column"]
        if legend_pos == "inline":
            p_series = plot.diamond(
                x=time_col,
                y="y_index",
                color=series_def["color"],
                alpha=0.5,
                size=glyph_size,
                source=series_def["source"],
                legend_label=str(ser_name),
            )
        else:
            p_series = plot.diamond(
                x=time_col,
                y="y_index",
                color=series_def["color"],
                alpha=0.5,
                size=glyph_size,
                source=series_def["source"],
            )
        if legend_pos in ["left", "right"]:
            legend_items.append(
                LegendItem(
                    label=str(ser_name),
                    renderers=[p_series],
                )
            )

    if legend_pos == "inline":
        # Position the inline legend
        plot.legend.location = "center_left"
        plot.legend.click_policy = "hide"
    elif legend_pos in ["left", "right"]:
        # Create the legend box outside of the plot area
        ext_legend = Legend(
            items=legend_items[::-1],  # the legend is in the wrong order otherwise
            location="center",
            click_policy="hide",
            label_text_font_size="8pt",
        )
        plot.add_layout(ext_legend, legend_pos)


# pylint: disable=too-many-locals
def _unpack_data_series_dict(data, param: PlotParams):
    """Unpack each series from the data series dictionary."""
    # Process the input dictionary
    # Take each item that is passed and fill in blanks and add a y_index
    tool_tip_columns: Set[str] = set()
    min_time = None
    max_time = None
    y_index = 0

    # Create a color map in case colors have not been specified
    # (Shift the Viridis palette so we lose the top, harder-to-see colors)
    series_count = len(data)
    colors, palette_size = get_color_palette(series_count)

    for ser_name, series_def in data.items():
        data_columns: Set[str] = set()
        series_data = series_def["data"]

        if (
            series_data is None
            or not isinstance(series_data, pd.DataFrame)
            or series_data.empty
        ):
            print(f"No data to plot for series {ser_name}.")
            continue

        # if the series has source columns, use those
        # or fall back to global source cols or defaults
        series_def_src_cols = get_def_source_cols(series_data, param.source_columns)
        src_cols = series_def.get("source_columns", series_def_src_cols)
        data_columns.update(src_cols or series_def_src_cols)

        time_col = series_def.get("time_column", None)
        if not time_col:
            time_col = param.time_column
            series_def["time_column"] = time_col
        # ensure named time columns are in datetime format
        series_data = ensure_df_datetimes(data=series_data, columns=time_col)
        data_columns.update([time_col])
        # add the data columns to the tool tip column set
        tool_tip_columns.update(data_columns)

        # Create the Column data source to plot
        graph_df = series_data[list(data_columns)].copy()
        graph_df["y_index"] = y_index
        series_def["y_index"] = y_index
        ser_color = series_def.get("color", None)
        if not ser_color:
            ser_color = colors[y_index % palette_size]
            series_def["color"] = ser_color
        # Wrap tooltip lines longer than 50 chars
        _wrap_df_columns(graph_df, 50)
        series_def["source"] = ColumnDataSource(graph_df)
        y_index += 1

        # calculate min/max time from this set
        if min_time is None:
            min_time = series_data[time_col].min()
            max_time = series_data[time_col].max()
        else:
            min_time = min(min_time, series_data[time_col].min())
            max_time = max(max_time, series_data[time_col].max())

    return tool_tip_columns, min_time, max_time


# pylint: enable=too-many-locals


def _create_dict_from_grouping(
    data, source_columns, time_column, group_by, color, size=10
):
    """Return data groupings as a dictionary."""
    data_columns = get_def_source_cols(data, source_columns)
    # If the time column not explicitly specified in source_columns, add it
    data_columns.add(time_column)

    series_dict: Dict[str, Dict] = {}
    # create group frame so that we can color each group separately
    if group_by:
        data_columns.add(group_by)
        grouped_data = data[list(data_columns)].groupby(group_by)

        series_count = len(grouped_data)
        colors, palette_size = get_color_palette(series_count)
        for color_index, (group_name, group_df) in enumerate(grouped_data):
            series_dict[str(group_name)] = {
                "data": group_df,
                "time_column": time_column,
                "source_columns": source_columns,
                "color": colors[color_index % palette_size],
                "size": size,
            }
    else:
        group_df = data[list(data_columns)].copy()
        series_dict["unnamed series"] = {
            "data": group_df,
            "time_column": time_column,
            "source_columns": source_columns,
            "color": color,
            "size": size,
        }

    return series_dict


def _wrap_df_columns(data: pd.DataFrame, wrap_len: int = 50):
    """Wrap any string columns."""
    if not data.empty:
        for col in data.columns:
            if isinstance(data[col].iloc[0], str):
                data[col] = data[col].str.wrap(wrap_len)
