# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Timeline values Bokeh plot."""
from datetime import datetime
from typing import Any, Dict, Iterable, List, Optional, Tuple, Union

import attr
import pandas as pd
from bokeh.io import output_notebook, show
from bokeh.layouts import column
from bokeh.models import ColumnDataSource, HoverTool, LayoutDOM, Legend
from bokeh.plotting import figure, reset_output

from .._version import VERSION
from ..common.data_utils import ensure_df_datetimes
from ..common.utility import check_kwargs, export
from .timeline_common import (
    TIMELINE_HELP,
    calc_auto_plot_height,
    check_df_columns,
    create_data_grouping,
    create_range_tool,
    create_tool_tips,
    get_ref_event_time,
    get_tick_formatter,
    plot_ref_events,
    plot_ref_line,
)

__version__ = VERSION
__author__ = "Ian Hellen"


@attr.s(auto_attribs=True)
class PlotParams:
    """Plot params for time_duration."""

    time_column: Optional[str] = None
    height: Optional[int] = None
    width: int = 900
    title: Optional[str] = None
    yaxis: bool = True
    range_tool: bool = True
    group_by: Optional[str] = None
    legend: Optional[str] = None
    xgrid: bool = True
    ygrid: bool = False
    hide: bool = False
    color: str = "navy"
    kind: Union[str, List[str]] = "vbar"
    ylabel_cols: Iterable[str] = attr.Factory(list)
    ref_event: Optional[Any] = None
    ref_time: Optional[datetime] = None
    ref_events: Optional[pd.DataFrame] = None
    ref_col: Optional[str] = None
    ref_time_col: Optional[str] = None
    ref_times: Optional[List[Tuple[datetime, str]]] = None
    source_columns: List = []

    @classmethod
    def field_list(cls) -> List[str]:
        """Return field names as a list."""
        return list(attr.fields_dict(cls).keys())


# pylint: disable=invalid-name, too-many-locals, too-many-statements, too-many-branches
@export  # noqa: C901, MC0001
def display_timeline_values(  # noqa: C901, MC0001
    data: pd.DataFrame,
    value_column: str = None,
    time_column: str = "TimeGenerated",
    source_columns: list = None,
    **kwargs,
) -> LayoutDOM:
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
    value_column : str
        The column name holding the value to plot vertically
    source_columns : list, optional
        List of default source columns to use in tooltips
        (the default is None)

    Other Parameters
    ----------------
    x : str, optional
        alias of `time_column`
    y : str, optional
        alias of `value_column`
    value_col : str, optional
        alias of `value_column`
    title : str, optional
        Title to display (the default is None)
    ref_event : Any, optional
        Add a reference line/label using the alert time (the default is None)
    ref_time : datetime, optional
        Add a reference line/label using `ref_time` (the default is None)
    ref_label : str, optional
        A label for the `ref_event` or `ref_time` reference item
    group_by : str
        (where `data` is a DataFrame)
        The column to group timelines on
    legend: str, optional
        "left", "right", "inline" or "none"
        (the default is to show a legend when plotting multiple series
        and not to show one when plotting a single series)
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
    # check_kwargs(kwargs, _DEFAULT_KWARGS + _TL_VALUE_KWARGS + ["y"])
    check_kwargs(kwargs, PlotParams.field_list() + ["y", "value_col"])
    value_col = value_column or kwargs.pop("y", kwargs.pop("value_col", None))
    if not value_col:
        raise ValueError("Must supply 'value_column', 'value_col' or 'y' parameter.")
    param = PlotParams(**kwargs)

    if data is None or not isinstance(data, pd.DataFrame) or data.empty:
        print("No data to plot.")
        return figure()

    reset_output()
    output_notebook()
    check_kwargs(kwargs, PlotParams.field_list())
    param = PlotParams(**kwargs)

    plot_kinds = param.kind if isinstance(param.kind, list) else [param.kind]
    param.ref_time_col = param.ref_time_col or time_column or "TimeGenerated"

    ref_time, ref_label = get_ref_event_time(**kwargs)

    if source_columns is None:
        source_columns = [value_col]
    if value_col not in source_columns:
        source_columns.append(value_col)
    check_df_columns(
        data, source_columns + [time_column], TIMELINE_HELP, "display_timeline_values"
    )
    data = ensure_df_datetimes(data=data, columns=time_column)
    graph_df, group_count_df, tool_tip_columns, series_count = create_data_grouping(
        data, source_columns, time_column, param.group_by, param.color
    )

    hover = HoverTool(**(create_tool_tips(data, tool_tip_columns)))

    # Create the Plot figure
    min_time = graph_df[time_column].min()
    max_time = graph_df[time_column].max()
    start_range = min_time - ((max_time - min_time) * 0.1)
    end_range = max_time + ((max_time - min_time) * 0.1)
    height = param.height or calc_auto_plot_height(series_count)

    plot = figure(
        x_range=(start_range, end_range),
        min_border_left=50,
        plot_height=height,
        plot_width=param.width,
        x_axis_label="Event Time",
        x_axis_type="datetime",
        x_minor_ticks=10,
        y_axis_label=value_col,
        tools=[hover, "xwheel_zoom", "box_zoom", "reset", "save", "xpan"],
        toolbar_location="above",
        title=param.title or "Timeline",
    )
    plot.yaxis.visible = param.yaxis
    _set_grid_lines(plot)

    # set the tick datetime formatter
    plot.xaxis[0].formatter = get_tick_formatter()

    # plot groups individually so that we can create an interactive legend
    if param.group_by:
        legend_items = _plot_param_group(
            value_col,
            time_column,
            param,
            plot_kinds,
            graph_df,
            group_count_df,
            plot,
        )

        if param.legend == "inline":
            # Position the inline legend
            plot.legend.location = "top_left"
            plot.legend.click_policy = "hide"
        elif param.legend in {"left", "right"}:
            # Create the legend box outside of the plot area
            ext_legend = Legend(
                items=legend_items,
                location="center",
                click_policy="hide",
                label_text_font_size="8pt",
            )
            plot.add_layout(ext_legend, param.legend)
    else:
        plot_args = {
            "x": time_column,
            "color": param.color,
            "alpha": 0.7,
            "source": ColumnDataSource(graph_df),
        }
        if "vbar" in plot_kinds:
            plot.vbar(top=value_col, width=4, **plot_args)
        if "circle" in plot_kinds:
            plot.circle(y=value_col, size=4, **plot_args)
        if "line" in plot_kinds:
            plot.line(y=value_col, line_width=2, **plot_args)

    # if we have a reference, plot the time as a line
    if ref_time is not None:
        plot_ref_line(plot, ref_time, ref_label, data[value_col].max())
    elif param.ref_events is not None or param.ref_times is not None:
        plot_ref_events(
            plot=plot,
            ref_events=param.ref_events,
            time_col=param.ref_time_col,
            group_count=series_count,
            ref_col=param.ref_col,
            ref_times=param.ref_times,
        )

    if param.range_tool:
        rng_select = create_range_tool(
            data=graph_df,
            min_time=min_time,
            max_time=max_time,
            plot_range=plot.x_range,
            width=param.width,
            height=height,
            time_column=time_column,
        )
        plot_layout = column(plot, rng_select)
    else:
        plot_layout = plot

    if not param.hide:
        show(plot_layout)
    return plot_layout


def _set_grid_lines(plot):
    """Set default grid lines."""
    plot.ygrid.minor_grid_line_color = "navy"
    plot.ygrid.minor_grid_line_alpha = 0.1
    plot.ygrid.grid_line_color = "navy"
    plot.ygrid.grid_line_alpha = 0.3
    plot.xgrid.minor_grid_line_color = "navy"
    plot.xgrid.minor_grid_line_alpha = 0.1
    plot.xgrid.grid_line_color = "navy"
    plot.xgrid.grid_line_alpha = 0.3


def _plot_param_group(
    value_col,
    time_column,
    param,
    plot_kinds,
    graph_df,
    group_count_df,
    plot,
) -> List[Tuple[str, Any]]:
    """Plot series groups."""
    legend_items: List[Tuple[str, Any]] = []
    for _, group_id in group_count_df[param.group_by].items():
        first_group_item = graph_df[graph_df[param.group_by] == group_id].iloc[0]
        legend_label = str(first_group_item[param.group_by])
        inline_legend = str(group_id)
        group_color = first_group_item["color"]
        row_source = ColumnDataSource(graph_df[graph_df[param.group_by] == group_id])
        p_series = []
        # create default plot args
        plot_args: Dict[str, Any] = {
            "x": time_column,
            "alpha": 0.7,
            "source": row_source,
        }
        if param.legend != "none":
            plot_args["legend_label"] = inline_legend

        if "vbar" in plot_kinds:
            p_series.append(
                plot.vbar(top=value_col, width=4, color="color", **plot_args)
            )
        if "circle" in plot_kinds:
            p_series.append(
                plot.circle(y=value_col, size=4, color="color", **plot_args)
            )
        if "line" in plot_kinds:
            p_series.append(
                plot.line(
                    y=value_col, line_width=2, line_color=group_color, **plot_args
                )
            )
        if not inline_legend:
            legend_items.append((legend_label, p_series))
    return legend_items
