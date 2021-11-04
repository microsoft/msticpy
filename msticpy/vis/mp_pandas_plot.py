# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Module docstring."""
from typing import Iterable, Optional, Tuple, Union

import pandas as pd
from bokeh.models import LayoutDOM
from bokeh.plotting import figure

from .._version import VERSION
from ..common.exceptions import MsticpyUserError
from ..nbtools.process_tree import build_and_show_process_tree
from ..nbtools.timeline import display_timeline, display_timeline_values
from ..nbtools.timeline_duration import display_timeline_duration
from .entity_graph_tools import EntityGraph, req_alert_cols, req_inc_cols
from .matrix_plot import plot_matrix

__version__ = VERSION
__author__ = "Ian Hellen"


@pd.api.extensions.register_dataframe_accessor("mp_plot")
class MsticpyPlotAccessor:
    """Pandas api extension for MSTICPy visualizations."""

    def __init__(self, pandas_obj):
        """Instantiate pandas extension class."""
        self._df = pandas_obj

    def timeline(self, **kwargs) -> LayoutDOM:
        """
        Display a timeline of events.

        Parameters
        ----------
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
            The column to group timelines on.
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
        return display_timeline(data=self._df, **kwargs)

    # pylint: disable=invalid-name
    def timeline_values(self, value_col: str = None, **kwargs) -> LayoutDOM:
        """
        Display a timeline of events.

        Parameters
        ----------
        time_column : str, optional
            Name of the timestamp column
            (the default is 'TimeGenerated')
        value_col : str
            The column name holding the value to plot vertically
        source_columns : list, optional
            List of default source columns to use in tooltips
            (the default is None)

        Other Parameters
        ----------------
        x : str, optional
            alias of `time_column`
        y : str, optional
            alias of `value_col`
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
        return display_timeline_values(data=self._df, value_col=value_col, **kwargs)

    def timeline_duration(
        self,
        group_by: Union[Iterable[str], str],
        time_column: str = "TimeGenerated",
        end_time_column: Optional[str] = None,
        **kwargs,
    ) -> LayoutDOM:  # noqa: C901, MC0001
        """
        Display a duration timeline of events grouped by one or more columns.

        Parameters
        ----------
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
        return display_timeline_duration(
            data=self._df,
            group_by=group_by,
            time_column=time_column,
            end_time_column=end_time_column,
            **kwargs,
        )

    def process_tree(self, **kwargs) -> Tuple[figure, LayoutDOM]:
        """
        Build and plot a process tree.

        Parameters
        ----------
        schema : ProcSchema, optional
            The data schema to use for the data set, by default None
            (if None the schema is inferred)
        output_var : str, optional
            Output variable for selected items in the tree,
            by default None
        legend_col : str, optional
            The column used to color the tree items, by default None
        show_table: bool
            Set to True to show a data table, by default False.

        Other Parameters
        ----------------
        height : int, optional
            The height of the plot figure
            (the default is 700)
        width : int, optional
            The width of the plot figure (the default is 900)
        title : str, optional
            Title to display (the default is None)
        hide_legend : bool, optional
            Hide the legend box, even if legend_col is specified.
        pid_fmt : str, optional
            Display Process ID as 'dec' (decimal) or 'hex' (hexadecimal),
            default is 'hex'.

        Returns
        -------
        Tuple[figure, LayoutDOM]:
            figure - The main bokeh.plotting.figure
            Layout - Bokeh layout structure.

        """
        return build_and_show_process_tree(data=self._df, **kwargs)

    def matrix(self, **kwargs) -> LayoutDOM:
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
            Manually specify the font size for axis labels, the
            default is to automatically calculate a size based on the
            number of items in each axis.
        max_label_font_size : int, optional
            The maximum size, in points, of the X and Y labels, default is 11.


        Returns
        -------
        LayoutDOM
            The Bokeh plot

        """
        return plot_matrix(data=self._df, **kwargs)

    def incident_graph(
        self, timeline: bool = False, hide: bool = False, **kwargs
    ) -> LayoutDOM:
        """
        Plot an incident graph if the dataframe contains incidents or alerts.

        Parameters
        ----------
        timeline : bool, optional
            True to plot the entity timeline, by default False
        hide : bool, optional
            True to hide the plot, by default False

        Other Parameters
        ----------------
        node_size : int, optional
            Size of the nodes in pixels, by default 25
        font_size : int, optional
            Font size for node labels, by default 10
            Can be an integer (point size) or a string (e.g. "10pt")
        width : int, optional
            Width in pixels, by default 800
        height : int, optional
            Image height (the default is 800)
        scale : int, optional
            Position scale (the default is 2)

        Raises
        ------
        MsticpyUserError
            Raised if the dataframe does not contain incidents or alerts.

        """
        if not all(elem in self._df.columns for elem in req_alert_cols) and any(
            elem not in self._df.columns for elem in req_inc_cols
        ):
            raise MsticpyUserError("DataFrame must consist of Incidents or Alerts")
        graph = EntityGraph(self._df)
        return graph.plot(hide=hide, timeline=timeline, **kwargs)
