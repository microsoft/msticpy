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
from ..transform.network import GraphType, df_to_networkx
from ..vis.network_plot import plot_nx_graph
from .entity_graph_tools import EntityGraph, req_alert_cols, req_inc_cols
from .foliummap import plot_map
from .matrix_plot import plot_matrix
from .process_tree import build_and_show_process_tree
from .timeline import display_timeline
from .timeline_duration import display_timeline_duration
from .timeline_values import display_timeline_values

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
    def timeline_values(self, value_column: str = None, **kwargs) -> LayoutDOM:
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
        return display_timeline_values(
            data=self._df, value_column=value_column, **kwargs
        )

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
            Display Process ID as 'dec' (decimal), 'hex' (hexadecimal),
            or 'guid' (string), default is 'hex'.

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
        if any(elem not in self._df.columns for elem in req_alert_cols) and any(
            elem not in self._df.columns for elem in req_inc_cols
        ):
            raise MsticpyUserError("DataFrame must consist of Incidents or Alerts")
        graph = EntityGraph(self._df)
        return graph.plot(hide=hide, timeline=timeline, **kwargs)

    def folium_map(self, **kwargs):
        """
        Plot folium map from DataFrame.

        Parameters
        ----------
        ip_column : Optional[str], optional
            The name of the IP Address column, by default None
        lat_column : Optional[str], optional
            The name of the location 'latitude' column, by default None
        long_column : Optional[str], optional
            The name of the location 'longitude' column, by default None
        layer_column : Optional[str], optional
            The column to group markers into for displaying on different
            map layers, by default None
        icon_column : Optional[str], optional
            Optional column containing the name of the icon to use
            for the marker in this row, by default None
        icon_map : IconMapper, optional
            Mapping dictionary or function, by default None
            See Notes for more details.
        popup_columns : Optional[List[str]], optional
            List of columns to use for the popup text, by default None
        tooltip_columns : Optional[List[str]], optional
            List of columns to use for the tooltip text, by default None


        Other Parameters
        ----------------
        marker_cluster : bool, optional
            Use marker clustering, default is True.
        default_color : str, optional
            Default color for marker icons, by default "blue"
        title : str, optional
            Name of the layer (the default is 'layer1')
            (passed to FoliumMap constructor)
        zoom_start : int, optional
            The zoom level of the map (the default is 7)
            (passed to FoliumMap constructor)
        tiles : [type], optional
            Custom set of tiles or tile URL (the default is None)
            (passed to FoliumMap constructor)
        width : str, optional
            Map display width (the default is '100%')
            (passed to FoliumMap constructor)
        height : str, optional
            Map display height (the default is '100%')
            (passed to FoliumMap constructor)
        location : list, optional
            Location to center map on

        Returns
        -------
        folium.Map
            Folium Map object.

        Raises
        ------
        ValueError
            If neither `ip_column` nor `lat_column` and `long_column` are passed.
        LookupError
            If one of the passed columns does not exist in `data`

        Notes
        -----
        There are two ways of providing custom icon settings based on the
        the row of the input DataFrame.

        If `icon_map` is a dict it should contain keys that map to the
        value of `icon_col` and values that a dicts of valid
        folium Icon properties ("color", "icon_color", "icon", "angle", "prefix").
        The dict should include a "default" entry that will be used if the
        value in the DataFrame[icon_col] doesn't match any key.
        For example:

        .. code:: python

            icon_map = {
                "high": {
                    "color": "red",
                    "icon": "warning",
                },
                "medium": {
                    "color": "orange",
                    "icon": "triangle-exclamation",
                    "prefix": "fa",
                },
                "default": {
                    "color": "blue",
                    "icon": "info-sign",
                },
            }

        If icon_map is a function it should take a single str parameter
        (the item key) and return a dict of icon properties. It should
        return a default set of values if the key does not match a known
        key. The `icon_col` value for each row will be passed to this
        function and the return value used to populate the Icon arguments.

        For example:

        .. code::python

            def icon_mapper(icon_key):
                if icon_key.startswith("bad"):
                    return {
                        "color": "red",
                        "icon": "triangle-alert",
                    }
                ...
                else:
                    return {
                        "color": "blue",
                        "icon": "info-sign",
                    }

        FontAwesome icon (prefix "fa") names are available at https://fontawesome.com/
        GlyphIcons icons (prefix "glyphicon") are available at https://www.glyphicons.com/

        """
        return plot_map(data=self._df, **kwargs)

    # pylint: disable=too-many-arguments
    def network(
        self,
        source_col: str,
        target_col: str,
        title: str = "Data Graph",
        source_attrs: Optional[Iterable[str]] = None,
        target_attrs: Optional[Iterable[str]] = None,
        edge_attrs: Optional[Iterable[str]] = None,
        graph_type: GraphType = "graph",
        **kwargs,
    ):
        """
        Plot entity graph with Bokeh.

        Parameters
        ----------
        source_col : str
            Column for source nodes.
        target_col : str
            Column for target nodes.
        title : str
            Title for the plot, by default 'Data Graph'
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
        hide : bool, optional
            Don't show the plot, by default False. If True, just
            return the figure.
        source_attrs : Optional[List[str]], optional
            Optional list of source attributes to use as hover properties, by default None
        target_attrs : Optional[List[str]], optional
            Optional list of target attributes to use as hover properties, by default None
        edge_attrs : Optional[List[str]], optional
            Optional list of edge attributes to use as hover properties, by default None
        graph_type : str
            "graph" or "digraph" (for nx.DiGraph)

        Other Parameters
        ----------------
        source_color : str, optional
            The color of the source nodes, by default 'light-blue'
        target_color : str, optional
            The color of the source nodes, by default 'light-green'
        edge_color : str, optional
            The color of the edges, by default 'black'
        kwargs :
            Additional keyword arguments are passed to the networkx
            layout function.

        Returns
        -------
        bokeh.plotting.figure
            The network plot.

        """
        nx_graph = df_to_networkx(
            data=self._df,
            source_col=source_col,
            target_col=target_col,
            source_attrs=source_attrs,
            target_attrs=target_attrs,
            edge_attrs=edge_attrs,
            graph_type=graph_type,
        )
        return plot_nx_graph(
            nx_graph=nx_graph,
            title=title,
            source_attrs=source_attrs,
            target_attrs=target_attrs,
            edge_attrs=edge_attrs,
            **kwargs,
        )
