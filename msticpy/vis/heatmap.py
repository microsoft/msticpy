"""Bokeh heatmap plot."""
from math import pi
from typing import List, Optional, Union

import attr
import pandas as pd
from bokeh.io import output_notebook, reset_output, show
from bokeh.models import (
    BasicTicker,
    ColorBar,
    HoverTool,
    LayoutDOM,
    LinearColorMapper,
    PrintfTickFormatter,
)
from bokeh.plotting import figure

from .._version import VERSION
from ..common.utility import check_kwargs

__version__ = VERSION
__author__ = "Ashwin Patil"


@attr.s(auto_attribs=True)
class PlotParams:
    """Plot params for heatmap."""

    title: Optional[str] = "Heatmap"
    x: Optional[str] = None
    x_col: Optional[str] = None
    y: Optional[str] = None
    y_col: Optional[str] = None
    height: int = 400
    width: int = 800
    color_pallette: List[str] = [
        "#75968f",
        "#a5bab7",
        "#c9d9d3",
        "#e2e2e2",
        "#dfccce",
        "#ddb7b1",
        "#cc7878",
        "#933b41",
        "#550b1d",
    ]
    value_col: Optional[str] = "Total"
    sort: Optional[Union[str, bool]] = None
    sort_x: Optional[Union[str, bool]] = None
    sort_y: Optional[Union[str, bool]] = None
    hide: bool = False
    font_size: Optional[int] = None
    max_label_font_size: int = 11
    major_label_text_font_size: str = "7px"

    @property
    def x_column(self) -> str:
        """Return the current x column value."""
        x_column = self.x or self.x_col
        if x_column is None:
            raise TypeError("Please supply value for x_column")
        return x_column

    @property
    def y_column(self) -> str:
        """Return the current y column value."""
        y_column = self.y or self.y_col
        if y_column is None:
            raise TypeError("Please supply value for y_column")
        return y_column

    @classmethod
    def field_list(cls) -> List[str]:
        """Return field names as a list."""
        return list(attr.fields_dict(cls).keys())


def plot_heatmap(data: pd.DataFrame, **kwargs) -> LayoutDOM:
    """
    Plot data as a heatmap.

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
        Column from the DataFrame used to categorize heatmap. Default is Total.
    height : int, optional
        The plot height. Default is 700
    width : int
        The plot width. Default is 900
    color_pallette : List, optional
        The color pallette of the heatmap, default is custom list
        ["#75968f", "#a5bab7", "#c9d9d3", "#e2e2e2", "#dfccce", "#ddb7b1",
        "#cc7878", "#933b41", "#550b1d"]
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

    if not param.x_column and not param.y_column:
        raise ValueError("Must supply `x` and `y` column parameters.")

    reset_output()
    output_notebook()

    x_range, y_range = _sort_days_hours(data, param.x_column, param.y_column)

    plot = figure(
        title=param.title,
        x_axis_location="above",
        x_range=x_range,
        y_range=y_range,
        plot_width=param.width,
        plot_height=param.height,
        tools=["wheel_zoom", "box_zoom", "pan", "reset", "save"],
        toolbar_location="above",
    )

    tool_tips = [
        (param.x_column, f"@{param.x_column} @{param.y_column}:00"),
        (param.value_col, f"@{param.value_col}"),
    ]
    plot.add_tools(HoverTool(tooltips=tool_tips))

    mapper, color_bar = _create_colorbar(data, param)

    plot.rect(
        x=param.y_column,
        y=param.x_column,
        width=1,
        height=1,
        source=data,
        fill_color={"field": param.value_col, "transform": mapper},
        line_color=None,
    )

    plot.add_layout(color_bar, "right")

    _set_plot_params(plot)

    if not param.hide:
        show(plot)
    return plot


def _set_plot_params(plot):
    plot.title.text_font_size = "15pt"
    plot.outline_line_color = None
    plot.xgrid.visible = True
    plot.ygrid.visible = True
    plot.grid.grid_line_color = None
    plot.grid.grid_line_alpha = 0.1
    plot.axis.axis_line_color = None
    plot.axis.major_tick_line_color = None
    plot.axis.major_label_standoff = 0
    plot.xaxis.major_label_orientation = pi / 3


def _sort_days_hours(data: pd.DataFrame, day_column: str, hour_column: str):
    """Sort the Week days and hour of day if required."""
    dayofweek = list(data[day_column].unique())
    hourofday = list(data[hour_column].astype(str).unique())
    correct_days = [
        "Monday",
        "Tuesday",
        "Wednesday",
        "Thursday",
        "Friday",
        "Saturday",
        "Sunday",
    ]
    correct_hours = [f"{hr}" for hr in range(0, 24)]
    days = {name: val for val, name in enumerate(correct_days)}
    hours = {name: val for val, name in enumerate(correct_hours)}
    sorted_days = sorted(dayofweek, key=days.get, reverse=True)  # type: ignore[arg-type]
    sorted_hours = sorted(hourofday, key=hours.get)  # type: ignore[arg-type]
    return sorted_hours, sorted_days


def _create_colorbar(data: pd.DataFrame, param: PlotParams):
    mapper = LinearColorMapper(
        palette=param.color_pallette,
        low=data[param.value_col].min(),
        high=data[param.value_col].max(),
    )
    color_bar = ColorBar(
        color_mapper=mapper,
        major_label_text_font_size=param.major_label_text_font_size,
        ticker=BasicTicker(desired_num_ticks=len(param.color_pallette)),
        formatter=PrintfTickFormatter(format="%d"),
        label_standoff=6,
        border_line_color=None,
    )
    return mapper, color_bar
