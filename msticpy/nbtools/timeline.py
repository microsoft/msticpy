# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Module for common display functions."""
from typing import Any, Union, Set

import pandas as pd
from bokeh.io import output_notebook, show
from bokeh.models import (
    ColumnDataSource,
    DatetimeTickFormatter,
    HoverTool,
    Label,
    Legend,
    RangeTool,
)

# pylint: disable=no-name-in-module
from bokeh.palettes import viridis

# pylint: enable=no-name-in-module
from bokeh.plotting import figure, reset_output
from bokeh.layouts import column

from .._version import VERSION
from .utility import export

__version__ = VERSION
__author__ = "Ian Hellen"


# Constants
_WRAP = 50
_WRAP_CMDL = "WrapCmdl"


def display_timeline(
    data: Union[pd.DataFrame, dict],
    time_column: str = "TimeGenerated",
    source_columns: list = None,
    **kwargs,
) -> figure:
    """
    Display a timeline of events.

    Parameters
    ----------
    data : Union[dict, pd.DataFrame]
        Data points to plot on the timeline.
            Need to contain:
                Key - Name of data type to be displayed in legend
                Value - dict of data containing:
                    data : pd.DataFrame
                        Data to plot
                    time_column : str
                        Name of the timestamp column
                    source_columns : list
                        List of source columns to use in tooltips
                    color: str
                        Color of datapoints for this data
        Or
        DataFrame as a single data set or grouped into individual
        plot series using the `group_by` parameter
    time_column : str, optional
        Name of the timestamp column
        (the default is 'TimeGenerated')
    source_columns : list, optional
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
    sort_by : str
        (where `data` is a DataFrame)
        The column to order timelines on
    legend: str, optional
        left, right or inline
        (the default is None/no legend)
    legend_column : str, optional
        (where `data` is a DataFrame)
        Name of the column used to generate the legend labels if a legend is
        to be displayed. Default is `group_by` parameter.
    yaxis : bool, optional
        Whether to show the yaxis and labels (default is False)
    range_tool : bool, optional
        Show the the range slider tool (default is True)
    height : int, optional
        The height of the plot figure
        (the default is auto-calculated height)
    width : int, optional
        The width of the plot figure (the default is 900)
    color : str
        Default series color (default is "navy")
    overlay_color : str
        Overlay series color (default is "green")

    Returns
    -------
    figure
        The bokeh plot figure.

    """
    # Get args
    overlay_data: pd.DataFrame = kwargs.pop("overlay_data", None)
    overlay_columns: list = kwargs.pop("overlay_columns", source_columns)
    color: str = kwargs.get("color", "navy")  # don't remove this from kwargs
    overlay_color: str = kwargs.pop("overlay_color", "green")

    kwargs_sub = kwargs.copy()
    kwargs_sub["time_column"] = time_column
    kwargs_sub["source_columns"] = source_columns
    ref_event = kwargs.get("alert", kwargs.get("ref_event", None))
    if ref_event is not None:
        kwargs_sub["ref_time"] = getattr(ref_event, "StartTimeUtc", None)
        if not kwargs_sub["ref_time"]:
            kwargs_sub["ref_time"] = getattr(ref_event, "TimeGenerated", None)
    else:
        kwargs_sub["ref_time"] = kwargs.get("ref_time", None)

    if isinstance(data, pd.DataFrame):
        if overlay_data is not None:
            aggr_data = {
                "Primary": {
                    "data": data,
                    "time_column": time_column,
                    "source_columns": source_columns,
                    "color": color,
                },
                "Secondary": {
                    "data": overlay_data,
                    "time_column": time_column,
                    "source_columns": overlay_columns,
                    "color": overlay_color,
                },
            }
            return display_timeline_dict(data=aggr_data, **kwargs_sub)
        return display_timeline_grouped(data=data, **kwargs_sub)
    if isinstance(data, dict):
        return display_timeline_dict(data, **kwargs_sub)
    return None


# pylint: disable=too-many-arguments, too-many-locals
# pylint: disable=too-many-statements, too-many-branches
@export  # noqa: C901, MC0001
def display_timeline_dict(data: dict, **kwargs) -> figure:
    """
    Display a timeline of events.

    Parameters
    ----------
    data : dict
        Data points to plot on the timeline.
            Need to contain:
                Key - Name of data type to be displayed in legend
                Value - dict of data containing:
                    data : pd.DataFrame
                        Data to plot
                    time_column : str
                        Name of the timestamp column
                    source_columns : list
                        List of source columns to use in tooltips
                    color: str
                        Color of datapoints for this data
    Other Parameters
    ----------------
    ref_time : datetime, optional
        Input reference line to display (the default is None)
    title : str, optional
        Title to display (the default is None)
    time_column : str, optional
        Name of the timestamp column
        (the default is 'TimeGenerated')
    legend: str, optional
        Where to position the legend
        None, left, right or inline (default is None)
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

    Returns
    -------
    figure
        The bokeh plot figure.

    """
    reset_output()
    output_notebook()

    height: int = kwargs.pop("height", None)
    width: int = kwargs.pop("width", 900)
    ref_time: Any = kwargs.pop("ref_time", None)
    title: str = kwargs.pop("title", None)
    time_column: str = kwargs.pop("time_column", "TimeGenerated")
    source_columns: list = kwargs.pop("source_columns", None)
    legend_pos: str = kwargs.pop("legend", None)
    show_yaxis: bool = kwargs.pop("yaxis", False)
    show_range: bool = kwargs.pop("range_tool", True)

    if not source_columns:
        source_columns = ["NewProcessName", "EventID", "CommandLine"]

    def_source_columns = set(source_columns) if source_columns else set()

    # Process the input dictionary
    # Take each item that is passed and fill in blanks and add a y_index
    tool_tip_columns: Set[str] = set()
    min_time = pd.Timestamp(pd.Timestamp.max)
    max_time = pd.Timestamp(pd.Timestamp.min)
    y_index = 0
    # Shift the Viridis palatte so we lose the top, harder-to-see colors
    series_count = len(data)
    palette_size = min(256, series_count + int(series_count / 2))
    colors = viridis(palette_size)
    for ser_name, series_def in data.items():
        data_columns: Set[str] = set()
        series_data = series_def["data"]

        # if the series has source columns, use those
        src_cols = series_def.get("source_columns", def_source_columns)
        data_columns.update(src_cols if src_cols else def_source_columns)

        # add these columns to the tool tip column set
        tool_tip_columns.update(data_columns)

        time_col = series_def.get("time_column", None)
        if not time_col:
            time_col = time_column
            series_def["time_column"] = time_col

        min_time = min(min_time, series_data[time_col].min())
        max_time = max(max_time, series_data[time_col].max())
        data_columns.update([time_col])
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

    # build the tool tips from all specified columns
    tool_tip_items = [(f"{col}", f"@{col}") for col in tool_tip_columns]
    hover = HoverTool(tooltips=tool_tip_items, formatters={"Tooltip": "printf"})

    title = f"Timeline: {title}" if title else "Event Timeline"
    start_range = min_time - ((max_time - min_time) * 0.1)
    end_range = max_time + ((max_time - min_time) * 0.1)
    height = height if height else _calc_auto_plot_height(len(data))
    plot = figure(
        x_range=(start_range, end_range),
        min_border_left=50,
        plot_height=height,
        plot_width=width,
        x_axis_label="Event Time",
        x_axis_type="datetime",
        x_minor_ticks=10,
        tools=[hover, "xwheel_zoom", "box_zoom", "reset", "save", "xpan"],
        title=title,
    )
    if show_yaxis:
        plot.yaxis.visible = True
        if data:
            y_labels = {ser_def["y_index"]: str(lbl) for lbl, ser_def in data.items()}
            plot.yaxis.major_label_overrides = y_labels
    plot.ygrid.minor_grid_line_color = "navy"
    plot.ygrid.minor_grid_line_alpha = 0.3
    plot.xgrid.minor_grid_line_color = "navy"
    plot.xgrid.minor_grid_line_alpha = 0.3
    # Create plot bar to act as as range selector
    rng_select = _create_range_tool(
        data=data,
        min_time=min_time,
        max_time=max_time,
        plot_range=plot.x_range,
        width=width,
    )

    # set the tick datetime formatter
    plot.xaxis[0].formatter = _get_tick_formatter()

    # plot groups individually so that we can create an interactive legend
    # if legend_pos is "inline", we add add the normal legend inside the plot
    # if legend_pos is "left" or "right", we add the legend to the side
    legend_items = []
    for ser_name, series_def in data.items():
        inline_legend = ser_name if legend_pos == "inline" else None
        p_series = plot.diamond(
            x=series_def["time_column"],
            y="y_index",
            color=series_def["color"],
            alpha=0.5,
            size=10,
            source=series_def["source"],
            legend=inline_legend,
        )
        if legend_pos in ["left", "right"]:
            legend_items.append((ser_name, [p_series]))

    if legend_pos == "inline":
        # Position the inline legend
        plot.legend.location = "top_left"
        plot.legend.click_policy = "hide"
    elif legend_pos in ["left", "right"]:
        # Create the legend box outside of the plot area
        ext_legend = Legend(
            items=legend_items,
            location="center",
            click_policy="hide",
            label_text_font_size="8pt",
        )
        plot.add_layout(ext_legend, legend_pos)

    if ref_time is not None:
        _add_ref_line(plot, ref_time, len(data))

    if show_range:
        show(column(plot, rng_select))
    else:
        show(plot)
    return plot


@export  # noqa: C901, MC0001
def display_timeline_grouped(data: pd.DataFrame, **kwargs) -> figure:
    """
    Display a timeline of events.

    Parameters
    ----------
    data : pd.DataFrame
        Input DataFrame

    Other Parameters
    ----------------
    group_by : str
        The column to group timelines on
    sort_by : str
        (where `data` is a DataFrame)
        The column to order timelines on
    ref_time : datetime, optional
        Input reference line to display (the default is None)
    title : str, optional
        Title to display (the default is None)
    time_column : str, optional
        Name of the timestamp column
        (the default is 'TimeGenerated')
    legend_column : str, optional
        Name of the column used to generate the legend lables
        (the default is None/no legend)
    legend: str, optional
        Where to position the legend
        None, left, right or inline (default is None)
    yaxis : bool, optional
        Whether to show the yaxis and labels
    source_columns : list
        List of source columns to use in tooltips
        (the default is None)
    height : int, optional
        The height of the plot figure
        (the default is auto-calculated height)
    width : int, optional
        The width of the plot figure (the default is 900)
    color : str
        Default series color (default is "navy")

    Returns
    -------
    figure
        The bokeh plot figure.

    """
    reset_output()
    output_notebook()

    height: int = kwargs.pop("height", None)
    width: int = kwargs.pop("width", 900)
    ref_time: Any = kwargs.pop("ref_time", None)
    title: str = kwargs.pop("title", None)
    time_column: str = kwargs.get("time_column", "TimeGenerated")
    source_columns: list = kwargs.pop("source_columns", None)
    group_by: str = kwargs.get("group_by", None)
    sort_by: str = kwargs.pop("sort_by", None)
    show_yaxis: bool = kwargs.pop("yaxis", False)
    show_range: bool = kwargs.pop("range_tool", True)
    color: str = kwargs.get("color", "navy")

    graph_df, group_count_df, tool_tip_columns, series_count = _create_data_grouping(
        data, source_columns, time_column, group_by, sort_by, color
    )

    # build the tool tips from columns (excluding these)
    tool_tip_items = [(f"{col}", f"@{col}") for col in tool_tip_columns]
    hover = HoverTool(tooltips=tool_tip_items, formatters={"Tooltip": "printf"})

    title = f"Timeline: {title}" if title else "Event Timeline"
    min_time = graph_df[time_column].min()
    max_time = graph_df[time_column].max()
    start_range = min_time - ((max_time - min_time) * 0.1)
    end_range = max_time + ((max_time - min_time) * 0.1)
    height = height if height else _calc_auto_plot_height(series_count)

    plot = figure(
        x_range=(start_range, end_range),
        min_border_left=50,
        plot_height=height,
        plot_width=width,
        x_axis_label="Event Time",
        x_axis_type="datetime",
        x_minor_ticks=10,
        y_axis_type="auto" if show_yaxis else None,
        tools=[hover, "xwheel_zoom", "box_zoom", "reset", "save", "xpan"],
        toolbar_location="above",
        title=title,
    )
    if show_yaxis:
        plot.yaxis.visible = True
        if group_by:
            y_labels = {
                idx: str(lbl)
                for idx, lbl in group_count_df[[group_by]].to_dict()[group_by].items()
            }
            plot.yaxis.major_label_overrides = y_labels
    plot.ygrid.minor_grid_line_color = "navy"
    plot.ygrid.minor_grid_line_alpha = 0.3
    plot.xgrid.minor_grid_line_color = "navy"
    plot.xgrid.minor_grid_line_alpha = 0.3

    # set the tick datetime formatter
    plot.xaxis[0].formatter = _get_tick_formatter()

    if group_by:
        # plot grouped series
        _plot_grouped_series(
            graph_df=graph_df, group_count_df=group_count_df, plot=plot, **kwargs
        )
    else:
        # plot single series
        plot.diamond(
            x=time_column,
            y="y_index",
            color="color",
            alpha=0.5,
            size=10,
            source=ColumnDataSource(graph_df),
        )

    # if we have a reference time, plot the time as a line
    if ref_time is not None:
        _add_ref_line(plot, ref_time, series_count)

    if show_range:
        # Create plot bar to act as as range selector
        rng_select = _create_range_tool(
            data=graph_df,
            min_time=min_time,
            max_time=max_time,
            plot_range=plot.x_range,
            width=width,
            time_column=time_column,
        )
        show(column(plot, rng_select))
    else:
        show(plot)
    return plot


def _create_data_grouping(data, source_columns, time_column, group_by, sort_by, color):
    if not source_columns:
        data_columns = set(["NewProcessName", "EventID", "CommandLine"])
    else:
        data_columns = set(source_columns)
    tool_tip_columns = data_columns.copy()
    # If the time column not explicity specified in source_columns, add it
    data_columns.add(time_column)
    # create group frame so that we can color each group separately
    if group_by:
        if not sort_by:
            sort_by = group_by
        group_count_df = (
            data[[group_by, time_column]]
            .groupby(group_by)
            .count()
            .sort_values(sort_by)
            .reset_index()
            .rename(columns={time_column: "count"})
        )
        group_count_df["y_index"] = group_count_df.index

        # Shift the Viridis palatte so we lose the top, harder-to-see colors
        series_count = len(group_count_df)
        palette_size = min(256, series_count + int(series_count / 2))
        colors = viridis(palette_size)
        group_count_df["color"] = group_count_df.apply(
            lambda x: colors[x.y_index % palette_size], axis=1
        )
        # re-join with the original data
        data_columns.update([group_by, "y_index", "color"])
        clean_data = data.drop(columns=["y_index", "color"], errors="ignore")
        graph_df = clean_data.merge(group_count_df, on=group_by)[list(data_columns)]
    else:
        graph_df = data[list(data_columns)].copy()
        graph_df["color"] = color
        graph_df["y_index"] = 1
        series_count = 1
        group_count_df = None
    return graph_df, group_count_df, tool_tip_columns, series_count


def _plot_dict_series(data, plot, legend_pos):
    """Plot series from dict."""
    # If legend_pos is outside the graph we need to create the legend
    # seperately.
    # We plot groups individually so that we can create an interactive legend.
    legend_items = []
    for ser_name, series_def in data.items():
        inline_legend = ser_name if legend_pos == "inline" else None
        p_series = plot.diamond(
            x=series_def["time_column"],
            y="y_index",
            color=series_def["color"],
            alpha=0.5,
            size=10,
            source=series_def["source"],
            legend=inline_legend,
        )
        if legend_pos in ["left", "right"]:
            legend_items.append((ser_name, [p_series]))

    if legend_pos == "inline":
        # Position the inline legend
        plot.legend.location = "top_left"
        plot.legend.click_policy = "hide"
    elif legend_pos in ["left", "right"]:
        # Create the legend box outside of the plot area
        ext_legend = Legend(
            items=legend_items,
            location="center",
            click_policy="hide",
            label_text_font_size="8pt",
        )
        plot.add_layout(ext_legend, legend_pos)


def _plot_grouped_series(graph_df, group_count_df, plot, **kwargs):
    """Plot series from grouped DataFrame."""
    time_column: str = kwargs.pop("time_column", "TimeGenerated")
    group_by: str = kwargs.pop("group_by", None)
    legend_column: str = kwargs.pop("legend_column", None)
    legend_pos: str = kwargs.pop("legend", None)

    # If legend_pos is outside the graph we need to create the legend
    # seperately.
    # We plot groups individually so that we can create an interactive legend.
    legend_items = []
    for _, group_id in group_count_df[group_by].items():
        first_group_item = graph_df[graph_df[group_by] == group_id].iloc[0]
        if legend_column:
            legend_label = first_group_item[legend_column]
        else:
            legend_label = first_group_item[group_by]
        inline_legend = legend_label if legend_pos == "inline" else None
        row_source = ColumnDataSource(graph_df[graph_df[group_by] == group_id])
        p_series = plot.diamond(
            x=time_column,
            y="y_index",
            color="color",
            alpha=0.5,
            size=10,
            source=row_source,
            legend=inline_legend,
        )
        if legend_column:
            legend_items.append((legend_label, [p_series]))

    if legend_pos == "inline":
        # Position the inline legend
        plot.legend.location = "top_left"
        plot.legend.click_policy = "hide"
    elif legend_pos in ["left", "right"]:
        # Create the legend box outside of the plot area
        ext_legend = Legend(
            items=legend_items,
            location="center",
            click_policy="hide",
            label_text_font_size="8pt",
        )
        plot.add_layout(ext_legend, legend_pos)


def _wrap_df_columns(data: pd.DataFrame, wrap_len: int = 50):
    """Wrap any string columns."""
    for col in data.columns:
        if isinstance(data[col].iloc[0], str):
            data[col] = data[col].str.wrap(wrap_len)


def _get_tick_formatter() -> DatetimeTickFormatter:
    """Return tick formatting for different zoom levels."""
    # '%H:%M:%S.%3Nms
    tick_format = DatetimeTickFormatter()
    tick_format.days = ["%m-%d %H:%M"]
    tick_format.hours = ["%H:%M:%S"]
    tick_format.minutes = ["%H:%M:%S"]
    tick_format.seconds = ["%H:%M:%S"]
    tick_format.milliseconds = ["%H:%M:%S.%3N"]
    return tick_format


def _calc_auto_plot_height(group_count):
    """Dynamic calculation of plot height."""
    ht_per_row = 40
    if group_count > 15:
        ht_per_row = 25
    return max(ht_per_row * group_count, 300)


def _create_range_tool(
    data, min_time, max_time, plot_range, width, time_column: str = None
):
    """Create plot bar to act as as range selector."""
    ext_min = min_time - ((max_time - min_time) * 0.15)
    ext_max = max_time + ((max_time - min_time) * 0.15)
    rng_select = figure(
        x_range=(ext_min, ext_max),
        title="Drag the middle and edges of the selection box to change the range above",
        plot_height=100,
        plot_width=width,
        x_axis_type="datetime",
        y_axis_type=None,
        tools="",
        toolbar_location=None,
    )
    rng_select.xaxis[0].formatter = _get_tick_formatter()
    if isinstance(data, dict):
        for _, series_def in data.items():
            rng_select.circle(
                x=series_def["time_column"],
                y="y_index",
                color=series_def["color"],
                source=series_def["source"],
            )
    elif isinstance(data, pd.DataFrame):
        rng_select.circle(
            x=time_column, y="y_index", color="blue", source=ColumnDataSource(data)
        )

    range_tool = RangeTool(x_range=plot_range)
    range_tool.overlay.fill_color = "navy"
    range_tool.overlay.fill_alpha = 0.2
    rng_select.ygrid.grid_line_color = None
    rng_select.add_tools(range_tool)
    rng_select.toolbar.active_multi = range_tool
    return rng_select


def _add_ref_line(plot, ref_time, series_count=1):
    """Add a reference marker line and label at `ref_time`."""
    ref_label_tm = pd.Timestamp(ref_time)
    plot.line(x=[ref_label_tm, ref_label_tm], y=[0, series_count])
    ref_label = Label(
        x=ref_label_tm,
        y=0,
        y_offset=10,
        x_units="data",
        y_units="data",
        text="< ref",
        text_font_size="8pt",
        render_mode="css",
        border_line_color="red",
        border_line_alpha=1.0,
        background_fill_color="white",
        background_fill_alpha=0.5,
    )

    plot.add_layout(ref_label)


# pylint: disable=invalid-name
def display_timeline_values(  # noqa: C901, MC0001
    data: pd.DataFrame,
    y: str,
    time_column: str = "TimeGenerated",
    source_columns: list = None,
    **kwargs,
) -> figure:
    """
    Display a timeline of events.

    Parameters
    ----------
    data : pd.DataFrame
        DataFrame as a single data set or grouped into individual
        plot series using the `group_by` parameter
    time_column : str, optional
        Name of the timestamp column
        (the default is 'TimeGenerated')
    y : str
        The column name holding the value to plot vertically
    source_columns : list, optional
        List of default source columns to use in tooltips
        (the default is None)

    Other Parameters
    ----------------
    x : str, optional
        alias of `time_column`
    title : str, optional
        Title to display (the default is None)
    ref_event : Any, optional
        Add a reference line/label using the alert time (the default is None)
    ref_time : datetime, optional
        Add a reference line/label using `ref_time` (the default is None)
    group_by : str
        (where `data` is a DataFrame)
        The column to group timelines on
    sort_by : str
        (where `data` is a DataFrame)
        The column to order timelines on
    legend: str, optional
        left, right or inline
        (the default is None/no legend)
    legend_column : str, optional
        (where `data` is a DataFrame)
        Name of the column used to generate the legend labels if a legend is
        to be displayed. Default is `group_by` parameter.
    yaxis : bool, optional
        Whether to show the yaxis and labels
    range_tool : bool, optional
        Show the the range slider tool (default is True)
    height : int, optional
        The height of the plot figure
        (the default is auto-calculated height)
    width : int, optional
        The width of the plot figure (the default is 900)
    color : str
        Default series color (default is "navy"). This is overridden by
        automatic color assignments if plotting a grouped chart
    kind : Union[str, List[str]]
        one or more glyph types to plot., optional
        Supported types are "circle", "line" and "vbar" (default is "vbar")

    Returns
    -------
    figure
        The bokeh plot figure.

    """
    reset_output()
    output_notebook()

    height: int = kwargs.pop("height", None)
    width: int = kwargs.pop("width", 900)
    ref_time: Any = kwargs.pop("ref_time", None)
    title: str = kwargs.pop("title", None)
    time_column = kwargs.get("x", time_column)
    group_by: str = kwargs.get("group_by", None)
    sort_by: str = kwargs.pop("sort_by", None)
    show_yaxis: bool = kwargs.pop("yaxis", True)
    show_range: bool = kwargs.pop("range_tool", True)
    color: str = kwargs.get("color", "navy")
    legend_column: str = kwargs.pop("legend_column", None)
    legend_pos: str = kwargs.pop("legend", None)
    kind: Any = kwargs.pop("kind", ["vbar"])
    plot_kinds = kind if isinstance(kind, list) else [kind]

    if not ref_time:
        ref_event = kwargs.get("alert", kwargs.get("ref_event", None))
        if ref_event:
            ref_time = getattr(ref_event, "StartTimeUtc", None)
            if not ref_time:
                ref_time = getattr(ref_event, "TimeGenerated", None)

    graph_df, group_count_df, tool_tip_columns, series_count = _create_data_grouping(
        data, source_columns, time_column, group_by, sort_by, color
    )

    # build the tool tips from columns (excluding these)
    tool_tip_items = [(f"{col}", f"@{col}") for col in tool_tip_columns]
    hover = HoverTool(tooltips=tool_tip_items, formatters={"Tooltip": "printf"})

    # Create the Plot figure
    title = title if title else "Timeline"
    min_time = graph_df[time_column].min()
    max_time = graph_df[time_column].max()
    start_range = min_time - ((max_time - min_time) * 0.1)
    end_range = max_time + ((max_time - min_time) * 0.1)
    height = height if height else _calc_auto_plot_height(series_count)

    plot = figure(
        x_range=(start_range, end_range),
        min_border_left=50,
        plot_height=height,
        plot_width=width,
        x_axis_label="Event Time",
        x_axis_type="datetime",
        x_minor_ticks=10,
        y_axis_label=y,
        tools=[hover, "xwheel_zoom", "box_zoom", "reset", "save", "xpan"],
        toolbar_location="above",
        title=title,
    )
    plot.yaxis.visible = show_yaxis
    plot.ygrid.minor_grid_line_color = "navy"
    plot.ygrid.minor_grid_line_alpha = 0.3
    plot.xgrid.minor_grid_line_color = "navy"
    plot.xgrid.minor_grid_line_alpha = 0.3

    # set the tick datetime formatter
    plot.xaxis[0].formatter = _get_tick_formatter()

    # plot groups individually so that we can create an interactive legend
    if group_by:
        legend_items = []
        for _, group_id in group_count_df[group_by].items():
            inline_legend = group_id if legend_pos == "inline" else None
            row_source = ColumnDataSource(graph_df[graph_df[group_by] == group_id])
            p_series = []
            plot_args = dict(
                x=time_column, alpha=0.7, source=row_source, legend=inline_legend
            )
            if "vbar" in plot_kinds:
                p_series.append(plot.vbar(top=y, width=4, color="color", **plot_args))
            if "circle" in plot_kinds:
                p_series.append(plot.circle(y=y, size=4, color="color", **plot_args))
            if "line" in plot_kinds:
                p_series.append(
                    plot.line(y=y, line_width=4, line_color="color", **plot_args)
                )
            if legend_column:
                legend_label = graph_df[graph_df[group_by] == group_id][
                    legend_column
                ].iloc[0]
                legend_items.append((legend_label, p_series))

        if legend_pos == "inline":
            # Position the inline legend
            plot.legend.location = "top_left"
            plot.legend.click_policy = "hide"
        elif legend_pos in ["left", "right"]:
            # Create the legend box outside of the plot area
            ext_legend = Legend(
                items=legend_items,
                location="center",
                click_policy="hide",
                label_text_font_size="8pt",
            )
            plot.add_layout(ext_legend, legend_pos)
    else:
        plot_args = dict(
            x=time_column, color="color", alpha=0.7, source=ColumnDataSource(graph_df)
        )
        if "vbar" in plot_kinds:
            plot.vbar(top=y, width=4, **plot_args)
        if "circle" in plot_kinds:
            plot.circle(y=y, size=4, **plot_args)
        if "line" in plot_kinds:
            plot.line(y=y, line_width=4, **plot_args)

    # if we have a reference, plot the time as a line
    if ref_time is not None:
        _add_ref_line(plot, ref_time, series_count)

    if show_range:
        rng_select = _create_range_tool(
            data=graph_df,
            min_time=min_time,
            max_time=max_time,
            plot_range=plot.x_range,
            width=width,
            time_column=time_column,
        )
        show(column(plot, rng_select))
    else:
        show(plot)
    return plot
