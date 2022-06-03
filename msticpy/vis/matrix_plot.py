# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Bokeh matrix plot."""
import math
from typing import List, Optional, Union

import attr
import numpy as np
import pandas as pd
from bokeh.io import output_notebook, reset_output, show
from bokeh.models import ColumnDataSource, HoverTool, LayoutDOM
from bokeh.plotting import figure

from .._version import VERSION
from ..common.utility import check_kwargs

__version__ = VERSION
__author__ = "Ian Hellen"


@attr.s(auto_attribs=True)
class PlotParams:
    """Plot params for time_duration."""

    title: Optional[str] = "Interaction Plot"
    x: Optional[str] = None
    x_col: Optional[str] = None
    y: Optional[str] = None
    y_col: Optional[str] = None
    intersect: bool = False
    height: int = 700
    width: int = 900
    color: str = "red"
    value_col: Optional[str] = None
    dist_count: bool = False
    log_size: bool = False
    invert: bool = False
    sort: Optional[Union[str, bool]] = None
    sort_x: Optional[Union[str, bool]] = None
    sort_y: Optional[Union[str, bool]] = None
    hide: bool = False
    font_size: Optional[int] = None
    max_label_font_size: int = 11

    @property
    def x_column(self) -> Optional[str]:
        """Return the current x column value."""
        return self.x or self.x_col

    @property
    def y_column(self) -> Optional[str]:
        """Return the current y column value."""
        return self.y or self.y_col

    @classmethod
    def field_list(cls) -> List[str]:
        """Return field names as a list."""
        return list(attr.fields_dict(cls).keys())


def plot_matrix(data: pd.DataFrame, **kwargs) -> LayoutDOM:
    """
    Plot data as an intersection matrix.

    Parameters
    ----------
    data : pd.DataFrame
        The data to plot.
    x : str
        Column to plot on the x (horizontal) axis
    x_col : str
        Alias for 'x'
    y : str
        Column to plot on the y (vertical) axis
    y_col : str
        Alias for 'y'
    title : str, optional
        Custom title, default is 'Intersection plot'
    value_col : str, optional
        Column from the DataFrame used to size the intersection points.
    dist_count : bool, optional
        Calculates a count of distinct values (from `value_col`) and uses
        this to size the intersection points.
        Requires `value_col` to be specified.
    log_size : bool, optional
        Takes the log of the size value before calculating the intersection
        display point size.
        Can be combined with `invert`.
    invert : bool, optional
        Takes the inverse of the size value as the basis for calculating
        the intersection display point size. This is useful for highlighting
        rare interactions.
        Can be combined with `log_size`.
    intersect : bool, optional
        Plots points of a fixed size, rather than using a sizing value. This
        is useful for just showing the presence/absence of an interaction.
    height : int, optional
        The plot height. Default is 700
    width : int
        The plot width. Default is 900
    color : str
        The color of the plotted points, default is "red"
    sort : Union[str, bool], optional
        Sorts the labels of both axes, default is None.
        Acceptable values are:
        'asc' (or string starting with 'asc') - Sort ascending
        'desc' (or string starting with 'asc') - Sort descending
        False or None (no sort)
        True  - Sort ascending
    sort_x : str, optional
        Sorts the labels of the x axis (takes precedence over `sort`),
        default is None.
        Acceptable values are:
        'asc' (or string starting with 'asc') - Sort ascending
        'desc' (or string starting with 'asc') - Sort descending
        False or None (no sort)
        True  - Sort ascending
    sort_y : str, optional
        Sorts the labels of the y axis (takes precedence over `sort`),
        default is None.
        Acceptable values are:
        'asc' (or string starting with 'asc') - Sort ascending
        'desc' (or string starting with 'asc') - Sort descending
        False or None (no sort)
        True  - Sort ascending
    hide : bool, optional
        Creates and returns but does not display the plot, default
        is False.
    font_size : int, optional
        Manually specify the font size for axis labels, in points,
        the default is to automatically calculate a size based on the
        number of items in each axis.
    max_label_font_size : int, optional
        The maximum size, in points, of the X and Y labels, default is 11.


    Returns
    -------
    LayoutDOM
        The Bokeh plot

    """
    # Process/extract parameters
    check_kwargs(kwargs, PlotParams.field_list())
    param = PlotParams(**kwargs)

    if not param.x_column or not param.y_column:
        raise ValueError("Must supply `x` and `y` column parameters.")

    reset_output()
    output_notebook()

    plot_data = _prep_data(data, param)

    x_range = _sort_labels(plot_data, param.x_column, param.sort_x or param.sort)
    y_range = _sort_labels(
        plot_data, param.y_column, param.sort_y or param.sort, invert=True
    )

    # Rescale the size so that it matches the graph
    max_size = plot_data["size"].max()
    plot_data["plt_size"] = plot_data["size"] * 10 / max_size
    source = ColumnDataSource(data=plot_data)

    plot = figure(
        title=param.title,
        plot_width=param.width,
        plot_height=param.height,
        x_range=x_range,
        y_range=y_range,
        tools=["wheel_zoom", "box_zoom", "pan", "reset", "save"],
        toolbar_location="above",
    )

    tool_tips = [
        (param.x_column, f"@{param.x_column}"),
        (param.y_column, f"@{param.y_column}"),
        ("value", "@size"),
    ]
    plot.add_tools(HoverTool(tooltips=tool_tips))

    if param.intersect:
        plot.circle_cross(
            x=param.x_column,
            y=param.y_column,
            source=source,
            fill_alpha=0.6,
            line_color=param.color,
            size=5,
        )
    else:
        plot.circle(
            x=param.x_column,
            y=param.y_column,
            source=source,
            fill_alpha=0.6,
            fill_color=param.color,
            size="plt_size",
        )
    _set_plot_params(plot)

    # Calculate appropriate font size for labels
    x_label_pt_size = param.font_size or max(
        5,
        min(
            param.max_label_font_size,
            int(param.width * 0.6 / plot_data[param.x_column].nunique()),
        ),
    )
    y_label_pt_size = param.font_size or max(
        5,
        min(
            param.max_label_font_size,
            int(param.height * 0.6 / plot_data[param.y_column].nunique()),
        ),
    )
    plot.xaxis.major_label_text_font_size = f"{x_label_pt_size}pt"
    plot.yaxis.major_label_text_font_size = f"{y_label_pt_size}pt"
    plot.xaxis.axis_label = param.x_column
    plot.yaxis.axis_label = param.y_column

    if not param.hide:
        show(plot)
    return plot


def _set_plot_params(plot):
    plot.title.text_font_size = "15pt"
    plot.outline_line_color = None
    plot.grid.grid_line_color = "navy"
    plot.grid.grid_line_alpha = 0.1
    plot.axis.axis_line_color = None
    plot.axis.major_tick_line_color = None
    plot.xaxis.major_label_orientation = math.pi / 2
    plot.xgrid.visible = True
    plot.ygrid.visible = True
    plot.axis.major_label_standoff = 0


def _sort_labels(data, column, sort_arg, invert=False):
    """Sort the labels if required."""
    if sort_arg:
        if isinstance(sort_arg, str):
            sort_order = sort_arg.casefold().startswith("asc")
        else:
            sort_order = True
        sort_order = not sort_order if invert else sort_order
        return data[column].sort_values(ascending=sort_order).unique().tolist()
    return data[column].unique().tolist()


def _prep_data(data: pd.DataFrame, param: PlotParams):
    """Process the data to create size column."""

    def _size_scale(value_series, log_size, invert):
        # local function to scale values
        if invert:
            # If invert, calculate inverse values on same
            # scale as input
            max_size = value_series.max()
            # min_size = value_series.min()
            value_series = (max_size) / value_series
        if log_size:
            # calc log of values, if requested
            return np.log(value_series)
        return value_series

    if param.value_col is None:
        # calculate a count of rows in each group
        other_cols = list(set(data.columns) - set([param.x_column, param.y_column]))
        if other_cols:
            count_col = other_cols[0]
        else:
            count_col = data.index.name or "index"
            data = data.reset_index()
        count_rows_df = (
            data[[param.x_column, param.y_column, count_col]]
            .groupby([param.x_column, param.y_column])
            .count()
            .rename(columns={count_col: "row_count"})
            .reset_index()
        )
        return count_rows_df.assign(
            size=_size_scale(count_rows_df.row_count, param.log_size, param.invert)
        )

    # if value column was specified, use that
    if param.dist_count:
        # If distinct count of values required, get nunique
        tmp_df = (
            data[[param.x_column, param.y_column, param.value_col]]
            .groupby([param.x_column, param.y_column])
            .nunique()
            .reset_index()
        )
    else:
        tmp_df = (
            data[[param.x_column, param.y_column, param.value_col]]
            .groupby([param.x_column, param.y_column])
            .sum()
            .reset_index()
        )
    return tmp_df.assign(
        size=lambda x: _size_scale(
            tmp_df[param.value_col], param.log_size, param.invert
        )
    )
