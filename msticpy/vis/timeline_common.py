# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Module for common timeline functions."""
from datetime import datetime
from typing import Any, Dict, Iterable, List, Optional, Set, Tuple, Union

import pandas as pd
from bokeh.models import (
    ColumnDataSource,
    DatetimeTickFormatter,
    Label,
    LayoutDOM,
    Range,
    RangeTool,
    Title,
)

# pylint: disable=no-name-in-module
from bokeh.palettes import Palette, viridis

# pylint: enable=no-name-in-module
from bokeh.plotting import Figure, figure
from pandas.api.types import is_datetime64_any_dtype
from pandas.errors import OutOfBoundsDatetime

from .._version import VERSION
from ..common.exceptions import MsticpyParameterError
from ..common.utility import export

# pylint: enable=unused-import

__version__ = VERSION
__author__ = "Ian Hellen"

TIMELINE_HELP = (
    "https://msticpy.readthedocs.io/en/latest/msticpy.vis.html"
    "#msticpy.vis.timeline.{plot_type}"
)


@export
def check_df_columns(
    data: pd.DataFrame, req_columns: List[str], help_uri: str, plot_type: str
):
    """
    Check that specified columns are in the DataFrame.

    Parameters
    ----------
    data : pd.DataFrame
        [description]
    req_columns : List[str]
        [description]
    help_uri : str
        [description]
    plot_type : str
        [description]

    Raises
    ------
    MsticpyParameterError
        If one or more columns not found in `data`

    """
    missing_cols = set(req_columns) - set(data.columns)
    if missing_cols:
        raise MsticpyParameterError(
            title="Columns not found in DataFrame",
            help_uri=help_uri.format(plot_type=plot_type),
            parameter=missing_cols,
        )


def create_data_grouping(
    data: pd.DataFrame,
    source_columns: List[str],
    time_column: str,
    group_by: Optional[str],
    color: str,
) -> Tuple[pd.DataFrame, pd.DataFrame, Set[str], int]:
    """
    Group input data and add indexes and tooltips.

    Parameters
    ----------
    data : pd.DataFrame
        Input dataframe
    source_columns : List[str]
        Tooltip columns
    time_column : str
        Name of the time column
    group_by : str
        Name of the group_by column
    color : str
        Plot color

    Returns
    -------
    Tuple[pd.DataFrame, pd.DataFrame, Set[str], int]
        Tuple of grouped dataframe, group color dataframe,
        set of tooltip columns, the number of groups created.

    """
    data_columns = get_def_source_cols(data, source_columns)
    # If the time column not explicitly specified in source_columns, add it
    data_columns.add(time_column)
    tool_tip_columns = data_columns.copy()
    # create group frame so that we can color each group separately
    if group_by:
        group_count_df = (
            data[[group_by, time_column]]
            .groupby(group_by)
            .count()
            .reset_index()
            .rename(columns={time_column: "count"})
        )
        group_count_df["y_index"] = group_count_df.index

        # Shift the Viridis palette so we lose the top, harder-to-see colors
        series_count = len(group_count_df)
        colors, palette_size = get_color_palette(series_count)
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


def get_def_source_cols(data: pd.DataFrame, source_columns: Iterable[str]) -> Set[str]:
    """Get default set of columns (backward compat)."""
    if not source_columns:
        return (
            {"NewProcessName", "EventID", "CommandLine"}
            if all(
                col in data.columns
                for col in ["NewProcessName", "EventID", "CommandLine"]
            )
            else set()
        )

    return set(source_columns)


def get_color_palette(series_count: int) -> Palette:
    """Return palette based on series size."""
    palette_size = min(256, series_count + series_count // 5)
    return viridis(palette_size), palette_size


def set_axes_and_grids(
    data: pd.DataFrame, plot: Figure, show_yaxis: bool, ygrid: bool, xgrid: bool
):
    """Set the axes visibility and grids according to parameters."""
    plot.yaxis.visible = show_yaxis
    if show_yaxis and data:
        y_labels = {ser_def["y_index"]: str(lbl) for lbl, ser_def in data.items()}
        plot.yaxis.major_label_overrides = y_labels
    if ygrid:
        plot.ygrid.minor_grid_line_color = "navy"
        plot.ygrid.minor_grid_line_alpha = 0.1
        plot.ygrid.grid_line_color = "navy"
        plot.ygrid.grid_line_alpha = 0.3
    else:
        plot.ygrid.grid_line_color = None
    if xgrid:
        plot.xgrid.minor_grid_line_color = "navy"
        plot.xgrid.minor_grid_line_alpha = 0.3
    else:
        plot.xgrid.grid_line_color = None


def get_time_bounds(
    min_time: pd.Timestamp, max_time: pd.Timestamp
) -> Tuple[pd.Timestamp, pd.Timestamp, pd.Timestamp, pd.Timestamp]:
    """Return start and end range, coping with out-of-bounds error."""
    try:
        start_range = min_time - ((max_time - min_time) * 0.1)
        end_range = max_time + ((max_time - min_time) * 0.1)
    except OutOfBoundsDatetime:
        min_time = min_time.to_pydatetime()
        max_time = max_time.to_pydatetime()
        start_range = min_time - ((max_time - min_time) * 0.1)
        end_range = max_time + ((max_time - min_time) * 0.1)
    return start_range, end_range, min_time, max_time


def create_tool_tips(
    data: Union[pd.DataFrame, Dict[str, pd.DataFrame]], columns: Iterable[str]
) -> Dict[str, Any]:
    """Create formatting for tool tip columns."""
    formatters: Dict[str, str] = {}
    # if this is a dict we need to unpack each dataframe and process
    # the tooltip columns for all of the data sets.
    if isinstance(data, dict):
        tool_tip_dict = {}
        for data_set in data.values():
            data_df = data_set.get("data", {})
            for col in columns:
                disp_col, col_tooltip, col_fmt = _get_datetime_tooltip(col, data_df)
                tool_tip_dict[disp_col] = col_tooltip
                formatters.update(col_fmt)
        return {"tooltips": list(tool_tip_dict.items()), "formatters": formatters}

    # If just a dataframe we just process the columns against this
    tool_tip_items = []
    for col in columns:
        disp_col, col_tooltip, col_fmt = _get_datetime_tooltip(col, data)
        tool_tip_items.append((disp_col, col_tooltip))
        formatters.update(col_fmt)

    return {"tooltips": tool_tip_items, "formatters": formatters}


def _get_datetime_tooltip(
    col: str, dataset: pd.DataFrame
) -> Tuple[str, str, Dict[str, str]]:
    """Return tooltip and formatter entries for column."""
    if " " in col:
        disp_col = col.replace(" ", "_")
        tt_col = f"{{{col}}}"
    else:
        disp_col = tt_col = col
    if col in dataset and is_datetime64_any_dtype(dataset[col]):
        col_tooltip = f"@{tt_col}{{%F %T.%3N}}"
        col_fmt: Dict[Any, Any] = {f"@{tt_col}": "datetime"}
    else:
        col_tooltip = f"@{tt_col}"
        col_fmt = {}
    return disp_col, col_tooltip, col_fmt


def calc_auto_plot_height(group_count: int) -> int:
    """Dynamic calculation of plot height."""
    ht_per_row = 25 if group_count > 15 else 40
    return max(ht_per_row * group_count, 300)


# pylint: disable=too-many-arguments, invalid-name, too-many-locals
def create_range_tool(
    data: pd.DataFrame,
    min_time: pd.Timestamp,
    max_time: pd.Timestamp,
    plot_range: Range,
    width: int,
    height: int,
    time_column: str = None,
    y: str = "y_index",
) -> Figure:
    """Create plot bar to act as as range selector."""
    ext_min = min_time - ((max_time - min_time) * 0.15)
    ext_max = max_time + ((max_time - min_time) * 0.15)
    plot_height = max(120, int(height * 0.20))
    rng_select = figure(
        x_range=(ext_min, ext_max),
        title="Range Selector",
        plot_height=plot_height,
        plot_width=width,
        x_axis_type="datetime",
        y_axis_type=None,
        tools="",
        toolbar_location=None,
    )
    help_str = (
        "Drag the middle or edges of the selection box to change "
        + "the range in the main chart"
    )
    rng_select.add_layout(
        Title(text=help_str, align="right", text_font_size="10px"), "below"
    )
    rng_select.xaxis[0].formatter = get_tick_formatter()
    if isinstance(data, dict):
        for _, series_def in data.items():
            rng_select.circle(
                x=series_def["time_column"],
                y=y,
                color=series_def["color"],
                source=series_def["source"],
            )
    elif isinstance(data, pd.DataFrame):
        rng_select.circle(
            x=time_column, y=y, color="blue", source=ColumnDataSource(data)
        )

    range_tool = RangeTool(x_range=plot_range)
    range_tool.overlay.fill_color = "navy"
    range_tool.overlay.fill_alpha = 0.2
    rng_select.ygrid.grid_line_color = None
    rng_select.add_tools(range_tool)
    rng_select.toolbar.active_multi = range_tool
    return rng_select


# pylint: enable=too-many-arguments


def plot_ref_line(
    plot: LayoutDOM,
    ref_time: datetime,
    ref_text: str = "Ref time",
    series_count: int = 1,
    index: int = 0,
):
    """Add a reference marker line and label at `ref_time`."""
    ref_label_tm = pd.Timestamp(ref_time)
    index = min(index, series_count)
    plot.line(
        x=[ref_label_tm, ref_label_tm],
        y=[0, series_count],
        line_width=1,
        line_color="red",
        line_dash="dashed",
        line_alpha=0.5,
    )
    ref_label = Label(
        x=ref_label_tm,
        y=0,
        y_offset=10 + (10 * index),
        x_units="data",
        y_units="data",
        text=f"< {ref_text}",
        text_font_size="8pt",
        text_alpha=0.5,
        render_mode="css",
        border_line_color="red",
        border_line_alpha=0.3,
        background_fill_color="white",
        background_fill_alpha=0.3,
    )

    plot.add_layout(ref_label)


def plot_ref_events(
    plot: Figure,
    time_col: str,
    group_count: int,
    ref_events: Optional[pd.DataFrame] = None,
    ref_col: Optional[str] = None,
    ref_times: Optional[List[Tuple[datetime, str]]] = None,
):
    """Plot reference lines/labels."""
    if ref_events is not None:
        if isinstance(ref_events, pd.Series):
            ref_events = pd.DataFrame(ref_events)
        for idx, event in enumerate(ref_events.itertuples()):
            evt_time = event._asdict()[time_col]
            evt_label = (
                event._asdict()[ref_col] if ref_col else f"reference {event.Index}"
            )
            plot_ref_line(
                plot=plot,
                ref_time=evt_time,
                ref_text=evt_label,
                series_count=group_count,
                index=idx,
            )
    elif ref_times:
        for idx, (evt_time, evt_label) in enumerate(ref_times):
            evt_label = evt_label or f"reference {idx}"
            plot_ref_line(
                plot=plot,
                ref_time=evt_time,
                ref_text=evt_label,
                series_count=group_count,
                index=idx,
            )


def get_ref_event_time(**kwargs) -> Tuple[Optional[Any], Union[Any, str]]:
    """Extract the reference time from kwargs."""
    ref_alert = kwargs.get("alert", None)
    if ref_alert is not None:
        ref_event = ref_alert
        ref_label = "Alert time"
    else:
        ref_event = kwargs.get("ref_event", None)
        ref_label = "Event time"

    if ref_event is not None:
        if isinstance(ref_event, pd.DataFrame):
            ref_event = ref_event.iloc[0]
        ref_time = getattr(ref_event, "StartTimeUtc", None)
        if not ref_time:
            ref_time = getattr(ref_event, "TimeGenerated", None)
    else:
        ref_time = kwargs.get("ref_time", None)
        ref_label = "Ref time"
    return ref_time, kwargs.get("ref_label", ref_label)  # type: ignore


def get_tick_formatter() -> DatetimeTickFormatter:
    """Return tick formatting for different zoom levels."""
    # '%H:%M:%S.%3Nms
    tick_format = DatetimeTickFormatter()
    tick_format.days = ["%m-%d %H:%M"]
    tick_format.hours = ["%H:%M:%S"]
    tick_format.minutes = ["%H:%M:%S"]
    tick_format.seconds = ["%H:%M:%S"]
    tick_format.milliseconds = ["%H:%M:%S.%3N"]
    return tick_format
