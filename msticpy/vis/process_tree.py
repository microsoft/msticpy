# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Process Tree Visualization."""
import warnings
from typing import Any, Dict, NamedTuple, Optional, Tuple, Union

import numpy as np
import pandas as pd
from bokeh.io import output_notebook, reset_output, show

# pylint: enable=no-name-in-module
from bokeh.layouts import column, row
from bokeh.models import (
    BoxSelectTool,
    ColorBar,
    ColumnDataSource,
    CustomJS,
    HoverTool,
    LayoutDOM,
    RangeTool,
)
from bokeh.models.widgets import DataTable, DateFormatter, TableColumn

# pylint: disable=no-name-in-module
from bokeh.palettes import viridis
from bokeh.plotting import figure
from bokeh.transform import dodge, factor_cmap, linear_cmap
from deprecated.sphinx import deprecated

from .._version import VERSION
from ..common.utility import check_kwargs, export
from ..transform.proc_tree_builder import build_process_tree, infer_schema
from ..transform.proc_tree_schema import ColNames as Col
from ..transform.proc_tree_schema import ProcessTreeSchemaException, ProcSchema

# pylint: disable=unused-import
# flake8: noqa F401
from ..transform.process_tree_utils import (
    get_ancestors,
    get_children,
    get_descendents,
    get_parent,
    get_process,
    get_process_key,
    get_root,
    get_root_tree,
    get_roots,
    get_siblings,
    get_summary_info,
    get_tree_depth,
)

# pylint: enable=unused-import


__version__ = VERSION
__author__ = "Ian Hellen"

_DEFAULT_KWARGS = ["height", "title", "width", "hide_legend", "pid_fmt"]


@export
def build_and_show_process_tree(
    data: pd.DataFrame,
    schema: Union[ProcSchema, Dict[str, Any]] = None,
    output_var: str = None,
    legend_col: str = None,
    **kwargs,
) -> Tuple[figure, LayoutDOM]:
    """
    Build process tree from data and plot a tree.

    Parameters
    ----------
    data : pd.DataFrame
        Window process creation or Linux Auditd events
    schema : Union[ProcSchema, Dict[str, Any]], optional
        The column schema to use, by default None.
        If supplied as a dict it must include definitions for the
        required fields in the ProcSchema class
        If None, then the schema is inferred
    output_var : str, optional
        Output variable for selected items in the tree,
        by default None
    legend_col : str, optional
        The column used to color the tree items, by default None
    kwargs : Dict[str, Any]
        Additional arguments passed to plot_process_tree

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

    Notes
    -----
    For full parameter set for process tree display see the
    help for plot_process_tree.

    See Also
    --------
    plot_process_tree

    """
    if isinstance(schema, dict):
        schema = ProcSchema(**schema)
    # Check if this table already seems to have the proc_tree metadata
    missing_cols = _check_proc_tree_schema(data)
    plot_args = {**kwargs, "legend_col": legend_col}
    if missing_cols:
        data = build_process_tree(procs=data, schema=schema, plot_args=plot_args)
    return plot_process_tree(data, schema, output_var=output_var, **plot_args)


# pylint: disable=too-many-locals, too-many-statements
@export
def plot_process_tree(  # noqa: MC0001
    data: pd.DataFrame,
    schema: Union[ProcSchema, Dict[str, Any]] = None,
    output_var: str = None,
    legend_col: str = None,
    show_table: bool = False,
    **kwargs,
) -> Tuple[figure, LayoutDOM]:
    """
    Plot a Process Tree Visualization.

    Parameters
    ----------
    data : pd.DataFrame
        DataFrame containing one or more Process Trees
    schema : Union[ProcSchema, Dict[str, Any]], optional
        The column schema to use, by default None.
        If supplied as a dict it must include definitions for the
        required fields in the ProcSchema class
        If None, then the schema is inferred
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

    Raises
    ------
    ProcessTreeSchemaException
        If the data set schema is not valid for the plot.

    Notes
    -----
    The `output_var` variable will be overwritten with any selected
    values.

    """
    check_kwargs(kwargs, _DEFAULT_KWARGS)
    reset_output()
    output_notebook()

    plot_height: int = kwargs.pop("height", 700)
    plot_width: int = kwargs.pop("width", 900)
    title: str = kwargs.pop("title", "ProcessTree")
    hide_legend = kwargs.pop("hide_legend", False)
    pid_fmt = kwargs.pop("pid_fmt", "hex")

    proc_data, schema, levels, n_rows = _pre_process_tree(data, schema, pid_fmt=pid_fmt)
    if schema is None:
        raise ProcessTreeSchemaException("Could not infer data schema from data set.")
    if levels is None:
        raise ProcessTreeSchemaException("Could create process relationships.")

    source = ColumnDataSource(data=proc_data)
    # Get legend/color bar map
    fill_map, color_bar = _create_fill_map(source, legend_col)

    max_level = max(levels) + 3
    min_level = min(levels)

    if color_bar:
        title += " (color bar = {legend_col})"
    visible_range = int(plot_height / 35)
    y_start_range = (n_rows - visible_range, n_rows + 1)
    b_plot = figure(
        title=title,
        plot_width=plot_width,
        plot_height=plot_height,
        x_range=(min_level, max_level),
        y_range=y_start_range,
        tools=["reset", "save", "tap", "ywheel_pan"],
        toolbar_location="above",
        active_scroll="ywheel_pan",
    )

    hover = HoverTool(
        tooltips=_get_tool_tips(schema),
        formatters={f"@{schema.time_stamp}": "datetime"},
    )
    b_plot.add_tools(hover)

    # dodge to align rectangle with grid
    rect_x = dodge("Level", 1.75, range=b_plot.x_range)
    rect_plot_params = {
        "width": 3.5,
        "height": 0.95,
        "source": source,
        "fill_alpha": 0.4,
        "fill_color": fill_map,
    }

    if color_bar:
        b_plot.add_layout(color_bar, "right")
    elif legend_col:
        rect_plot_params["legend_field"] = legend_col
    rect_plot = b_plot.rect(x=rect_x, y="Row", **rect_plot_params)
    if legend_col and not color_bar:
        b_plot.legend.title = legend_col
        b_plot.legend.label_text_font_size = "7pt"
    if hide_legend:
        b_plot.legend.visible = False

    text_props = {"source": source, "text_align": "left", "text_baseline": "middle"}

    def x_dodge(x_offset):
        return dodge("Level", x_offset, range=b_plot.x_range)

    def y_dodge(y_offset):
        return dodge("Row", y_offset, range=b_plot.y_range)

    b_plot.text(
        x=x_dodge(0.1),
        y=y_dodge(-0.2),
        text="__cmd_line$$",
        text_font_size="7pt",
        **text_props,
    )
    b_plot.text(
        x=x_dodge(0.1),
        y=y_dodge(0.25),
        text="__proc_name$$",
        text_font_size="8pt",
        **text_props,
    )
    b_plot.text(
        x=x_dodge(2.2),
        y=y_dodge(0.25),
        text="__proc_id$$",
        text_font_size="8pt",
        **text_props,
    )

    # Plot options
    _set_plot_option_defaults(b_plot)
    b_plot.xaxis.ticker = sorted(levels)
    b_plot.xgrid.ticker = sorted(levels)
    b_plot.hover.renderers = [rect_plot]  # only hover element boxes

    # Selection callback
    if output_var is not None:
        get_selected = _create_js_callback(source, output_var)
        b_plot.js_on_event("tap", get_selected)
        box_select = BoxSelectTool(callback=get_selected)
        b_plot.add_tools(box_select)

    range_tool = _create_vert_range_tool(
        data=source,
        min_y=0,
        max_y=n_rows,
        plot_range=b_plot.y_range,
        width=90,
        height=plot_height,
        x_col="Level",
        y_col="Row",
        fill_map=fill_map,
    )
    plot_elems = row(b_plot, range_tool)
    if show_table:
        data_table = _create_data_table(source, schema, legend_col)
        plot_elems = column(plot_elems, data_table)
    show(plot_elems)
    return b_plot, plot_elems


# pylint: enable=too-many-locals, too-many-statements


class TreeResult(NamedTuple):
    """Result tuple for _pre_process_tree."""

    proc_tree: pd.DataFrame
    schema: Optional[ProcSchema]
    levels: Optional[np.ndarray]
    n_rows: int


def _pre_process_tree(
    proc_tree: pd.DataFrame,
    schema: Union[Dict[str, Any], ProcSchema] = None,
    pid_fmt: str = "hex",
) -> TreeResult:
    """Extract dimensions and formatted values from proc_tree."""
    # Check if this table already seems to have the proc_tree metadata
    missing_cols = _check_proc_tree_schema(proc_tree)

    if isinstance(schema, dict):
        schema = ProcSchema(**schema)
    if missing_cols:
        proc_tree = build_process_tree(procs=proc_tree, schema=schema)

    if schema is None:
        schema = infer_schema(proc_tree)
    if schema is None:
        return TreeResult(proc_tree, None, None, 0)

    _validate_plot_schema(proc_tree, schema)

    proc_tree = proc_tree.sort_values(
        by=["path", schema.time_stamp], ascending=True
    ).reset_index()
    n_rows = len(proc_tree)
    proc_tree["Row"] = proc_tree.index
    proc_tree["Row"] = n_rows - proc_tree["Row"]
    proc_tree["Level"] = proc_tree["path"].str.count("/") + 1

    levels = proc_tree["Level"].unique()

    proc_tree[schema.process_name] = proc_tree[schema.process_name].fillna("unknown")
    proc_tree["__proc_name$$"] = proc_tree.apply(
        lambda x: x[schema.process_name].split(schema.path_separator)[-1], axis=1
    )
    proc_tree[schema.process_id] = proc_tree[schema.process_id].fillna("unknown")
    proc_tree["__proc_id$$"] = proc_tree[schema.process_id].apply(
        _pid_fmt, args=(pid_fmt,)
    )

    # Command line processing
    if not schema.cmd_line:
        schema = ProcSchema(**vars(schema))
        schema.cmd_line = "__cmd_line__"
        proc_tree[schema.cmd_line] = "cmdline unknown"
    # trim long command lines
    max_cmd_len = 500 // len(levels)
    proc_tree[schema.cmd_line] = proc_tree[schema.cmd_line].astype(str)
    long_cmd = proc_tree[schema.cmd_line].str.len() > max_cmd_len
    proc_tree.loc[long_cmd, "__cmd_line$$"] = (
        proc_tree[long_cmd][schema.cmd_line].str[:max_cmd_len] + "..."
    )
    # replace missing cmd lines
    proc_tree.loc[~long_cmd, "__cmd_line$$"] = proc_tree[~long_cmd][
        schema.cmd_line
    ].fillna("cmdline unknown")
    return TreeResult(proc_tree=proc_tree, schema=schema, levels=levels, n_rows=n_rows)


def _pid_fmt(pid, pid_fmt):
    if pid_fmt == "hex":
        return f"PID: {pid}" if str(pid).startswith("0x") else f"PID: 0x{int(pid):x}"
    if pid_fmt == "guid":
        return f"GUID: {pid}"
    return (
        f"PID: {pid}" if not str(pid).startswith("0x") else f"PID: {int(pid, base=16)}"
    )


def _validate_plot_schema(proc_tree: pd.DataFrame, schema):
    """Validate that we have the required columns."""
    required_cols = {"path", schema.process_name, schema.process_id}
    proc_cols = set(proc_tree.columns)
    missing = required_cols - proc_cols
    if missing:
        raise ProcessTreeSchemaException(
            f"Required columns not found in data set: {','.join(missing)}"
        )


def _set_plot_option_defaults(b_plot):
    """Set default plot options."""
    b_plot.outline_line_color = None
    b_plot.grid.grid_line_color = "navy"
    b_plot.axis.axis_line_color = None
    b_plot.axis.major_tick_line_color = "navy"
    b_plot.xaxis.visible = False
    b_plot.yaxis.visible = False
    b_plot.xgrid.visible = True
    b_plot.ygrid.visible = False
    b_plot.xgrid.minor_grid_line_color = "navy"
    b_plot.xgrid.minor_grid_line_alpha = 0.1
    b_plot.xgrid.grid_line_color = "navy"
    b_plot.xgrid.grid_line_alpha = 0.1
    b_plot.axis.major_label_standoff = 0


def _get_tool_tips(schema: ProcSchema):
    """Return tool tip formatter."""
    return [
        ("Process", f"@{schema.process_name}"),
        ("PID", f"@{schema.process_id}"),
        ("CmdLine", f"@{schema.cmd_line}"),
        ("SubjUser", f"@{schema.user_name}"),
        ("SubjLgnId", f"@{schema.logon_id}"),
        ("TgtLgnId", f"@{schema.target_logon_id}"),
        ("Time", f"@{schema.time_stamp}{{%F %T.%3N}}"),
    ]


def _create_js_callback(source: ColumnDataSource, result_var: str) -> CustomJS:
    """Create and return CustomJS callback to set Python variable."""
    ret_var_js = """
        // get data source from Callback args
        var inds = source.selected.indices;
        var output = [];
        for (var i = 0; i < inds.length; i++) {
            output.push(source.data[itemkey][inds[i]])
        }
        var out_str = JSON.stringify(output);
        var py_str = `${output_var} = ${out_str}`;
        console.log(py_str);
        IPython.notebook.kernel.execute(py_str);
    """
    return CustomJS(
        args={"source": source, "itemkey": Col.proc_key, "output_var": result_var},
        code=ret_var_js,
    )


def _create_fill_map(
    source: ColumnDataSource, source_column: str = None
) -> Tuple[Union[factor_cmap, linear_cmap], Optional[ColorBar]]:
    """Create factor map or linear map based on `source_column`."""
    fill_map = "navy"
    color_bar = None
    key_column = source_column or "Level"

    col_kind = source.data[key_column].dtype.kind
    if col_kind in ["b", "O"]:
        s_values = set(source.data[key_column])
        if np.nan in s_values:
            s_values.remove(np.nan)
        values = list(s_values)
        fill_map = factor_cmap(
            key_column, palette=viridis(max(3, len(values))), factors=values
        )
    elif col_kind in ["i", "u", "f", "M"]:
        values = [val for val in source.data[key_column] if not np.isnan(val)]
        fill_map = linear_cmap(
            field_name=key_column,
            palette=viridis(256),
            low=np.min(values),
            high=np.max(values),
        )
        if source_column is not None:
            # If user hasn't specified a legend column - don't create a bar
            color_bar = ColorBar(
                color_mapper=fill_map["transform"], width=8, location=(0, 0)  # type: ignore
            )
    return fill_map, color_bar


# pylint: disable=too-many-arguments
def _create_vert_range_tool(
    data, min_y, max_y, plot_range, width, height, x_col, y_col, fill_map="navy"
):
    """Return vertical range too for plot."""
    rng_select = figure(
        plot_width=width,
        plot_height=height,
        y_range=(min_y - 1, max_y + 1),
        toolbar_location=None,
    )

    x_dodge = dodge(x_col, -0.5)
    rng_select.rect(
        x=x_dodge,
        y=y_col,
        width=1.2,
        height=0.8,
        source=data,
        fill_alpha=0.6,
        fill_color=fill_map,
    )

    rng_select.xaxis.visible = False
    rng_select.yaxis.visible = False

    range_tool = RangeTool(y_range=plot_range)
    range_tool.overlay.fill_color = "navy"
    range_tool.overlay.fill_alpha = 0.2
    rng_select.ygrid.grid_line_color = None
    rng_select.xgrid.grid_line_color = None
    rng_select.add_tools(range_tool)
    rng_select.toolbar.active_multi = range_tool
    return rng_select


# pylint: enable=too-many-arguments


def _create_data_table(
    source: ColumnDataSource, schema: ProcSchema, legend_col: str = None
):
    """Return DataTable widget for source."""
    column_names = [
        schema.user_name,
        schema.user_id,
        schema.logon_id,
        schema.process_id,
        schema.process_name,
        schema.cmd_line,
        schema.parent_id,
        schema.parent_name,
        schema.target_logon_id,
    ]

    if legend_col and legend_col not in column_names:
        column_names.append(legend_col)

    date_fmt = "%F %T"
    columns = [
        TableColumn(
            field=schema.time_stamp,
            title=schema.time_stamp,
            formatter=DateFormatter(format=date_fmt),
        )
    ]
    columns2 = [
        TableColumn(field=col, title=col)
        for col in column_names
        if col in source.column_names
    ]

    return DataTable(source=source, columns=columns + columns2, width=950, height=150)


def _check_proc_tree_schema(data):
    """Return true if expected process tree columns are present."""
    if data.index.name != Col.proc_key:
        return {Col.proc_key}
    expected_cols = {Col.parent_key, "IsRoot", "IsLeaf", "IsBranch", "path"}
    return expected_cols - set(data.columns)


# pylint: disable=too-few-public-methods
@deprecated("Will be removed in version 2.0.0", version="1.7.0")
@pd.api.extensions.register_dataframe_accessor("mp_process_tree")
class ProcessTreeAccessor:
    """Pandas api extension for Process Tree."""

    def __init__(self, pandas_obj):
        """Instantiate pandas extension class."""
        self._df = pandas_obj

    def plot(self, **kwargs) -> Tuple[figure, LayoutDOM]:
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
        warn_message = (
            "This accessor method has been deprecated.\n"
            "Please use df.mp_plot.process_tree() method instead."
            "This will be removed in MSTICPy v2.2.0"
        )
        warnings.warn(warn_message, category=DeprecationWarning)
        return build_and_show_process_tree(data=self._df, **kwargs)

    def build(self, schema: ProcSchema = None, **kwargs) -> pd.DataFrame:
        """
        Build process trees from the process events.

        Parameters
        ----------
        procs : pd.DataFrame
            Process events (Windows 4688 or Linux Auditd)
        schema : ProcSchema, optional
            The column schema to use, by default None
            If None, then the schema is inferred
        show_summary : bool
            Shows summary of the built tree, default is False. : bool
        debug : bool
            If True produces extra debugging output,
            by default False

        Returns
        -------
        pd.DataFrame
            Process tree dataframe.

        Notes
        -----
        It is not necessary to call this before `plot`. The process
        tree is built automatically. This is only needed if you want
        to return the processed tree data as a DataFrame

        """
        warn_message = (
            "This accessor method has been deprecated.\n"
            "Please use df.mp.build_process_tree() method instead."
            "This will be removed in MSTICPy v2.2.0"
        )
        warnings.warn(warn_message, category=DeprecationWarning)
        return build_process_tree(
            procs=self._df,
            schema=schema,
            show_summary=kwargs.get("show_summary", kwargs.get("show_progress", False)),
            debug=kwargs.get("debug", False),
        )
