# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Pandas accessor class for timeline functions."""
import warnings
from typing import Iterable, Optional, Union

import pandas as pd
from bokeh.models import LayoutDOM
from deprecated.sphinx import deprecated

from .._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"

# pylint: disable=import-outside-toplevel, cyclic-import


@deprecated("Will be removed in version 2.2.0", version="1.7.0")
@pd.api.extensions.register_dataframe_accessor("mp_timeline")
class TimeLineAccessor:
    """Pandas api extension for Timeline."""

    def __init__(self, pandas_obj):
        """Instantiate pandas extension class."""
        from .timeline import display_timeline, display_timeline_values
        from .timeline_duration import display_timeline_duration

        self._display_timeline = display_timeline
        self._display_timeline_values = display_timeline_values
        self._display_timeline_duration = display_timeline_duration
        self._df = pandas_obj

    def plot(self, **kwargs) -> LayoutDOM:
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
        warn_message = (
            "This accessor method has been deprecated.\n"
            "Please use df.mp_plot.timeline() method instead."
            "This will be removed in MSTICPy v2.2.0"
        )
        warnings.warn(warn_message, category=DeprecationWarning)
        return self._display_timeline(data=self._df, **kwargs)

    def plot_values(self, value_column: str = None, **kwargs) -> LayoutDOM:
        """
        Display a timeline of events.

        Parameters
        ----------
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
        warn_message = (
            "This accessor method has been deprecated.\n"
            "Please use df.mp_plot.timeline_values() method instead."
            "This will be removed in MSTICPy v2.2.0"
        )
        warnings.warn(warn_message, category=DeprecationWarning)
        return self._display_timeline_values(
            data=self._df, value_column=value_column, **kwargs
        )

    def plot_duration(
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
        warn_message = (
            "This accessor method has been deprecated.\n"
            "Please use df.mp_plot.timeline_duration() method instead."
            "This will be removed in MSTICPy v2.2.0"
        )
        warnings.warn(warn_message, category=DeprecationWarning)
        return self._display_timeline_duration(
            data=self._df,
            group_by=group_by,
            time_column=time_column,
            end_time_column=end_time_column,
            **kwargs,
        )
