# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Process Tree Visualization."""
from collections import namedtuple
from typing import Optional, Tuple, Union

from bokeh.io import output_notebook, show, reset_output
from bokeh.plotting import figure
from bokeh.transform import dodge, factor_cmap, linear_cmap
from bokeh.models import (
    HoverTool,
    ColumnDataSource,
    CustomJS,
    # Legend,
    # LegendItem,
    BoxSelectTool,
    RangeTool,
    ColorBar,
)
from bokeh.palettes import Spectral, RdGy, Set3
from bokeh.models.widgets import DataTable, TableColumn, DateFormatter
from bokeh.layouts import column, row
import numpy as np
import pandas as pd

from ..sectools.process_tree_utils import (
    build_process_tree,
    ProcSchema,
    infer_schema,
    ProcessTreeSchemaException,
)

from .._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"


def show_process_tree(
    data: pd.DataFrame,
    schema: ProcSchema = None,
    output_var: str = None,
    legend_col: str = None,
):
    """[summary]

    Parameters
    ----------
    data : pd.DataFrame
        [description]

    """
    """Build process tree from data and plot a tree.

    Parameters
    ----------
    data : pd.DataFrame
        Window process creation or Linux Auditd events
    The data schema to use for the data set, by default None
        (if None the schema is inferred)
    output_var : str, optional
        Output variable for selected items in the tree,
        by default None
    legend_col : str, optional
        The column used to color the tree items, by default None
    """
    procs_with_path, schema = build_process_tree(data)
    # If we have multiple roots - ask the user to choose?
    if len(procs_with_path[procs_with_path["IsRoot"]] > 1):
        print("many roots")

    p_tree_srt = procs_with_path.sort_values("path", ascending="True")
    p_tree_srt = p_tree_srt.reset_index()
    plot_tree(p_tree_srt, schema)


# pylint: disable=too-many-locals
def plot_tree(
    proc_tree: pd.DataFrame,
    schema: ProcSchema = None,
    output_var: str = None,
    legend_col: str = None,
):
    """Plot a Process Tree Visualization.

    Parameters
    ----------
    proc_tree : pd.DataFrame
        DataFrame containing one or more Process Trees
    schema : ProcSchema, optional
        The data schema to use for the data set, by default None
        (if None the schema is inferred)
    output_var : str, optional
        Output variable for selected items in the tree,
        by default None
    legend_col : str, optional
        The column used to color the tree items, by default None

    Raises
    ------
    ProcessTreeSchemaException
        If the data set schema is not valid for the plot.

    Notes
    -----
    The `selected_keys` variable will be overwritten with any selected
    values.

    """
    reset_output()
    output_notebook()

    proc_tree, schema, levels, n_rows = _pre_process_tree(proc_tree, schema)
    if schema is None:
        raise ProcessTreeSchemaException("Could not infer schema from data set.")

    source = ColumnDataSource(data=proc_tree)

    max_level = max(levels) + 3
    min_level = min(levels)
    plot_height = 700
    visible_range = int(plot_height / 35)
    y_start_range = (n_rows - visible_range, n_rows + 1)
    b_plot = figure(
        title="ProcessTree",
        plot_width=900,
        plot_height=plot_height,
        x_range=(min_level, max_level),
        y_range=y_start_range,
        tools=["ypan", "reset", "save", "tap"],
        toolbar_location="above",
    )

    hover = HoverTool(
        tooltips=_get_tool_tips(schema), formatters={"TimeGenerated": "datetime"}
    )
    b_plot.add_tools(hover)

    fill_map, color_bar = _create_fill_map(source, legend_col)
    if legend_col:
        b_plot.legend.title = legend_col
    if color_bar:
        b_plot.add_layout(color_bar, "right")
    # dodge to align rectangle with grid
    rect_x = dodge("Level", 1.75, range=b_plot.x_range)
    rect_plot = b_plot.rect(
        x=rect_x,
        y="Row",
        width=3.5,
        height=0.95,
        source=source,
        fill_alpha=0.6,
        fill_color=fill_map,
        legend=legend_col,
    )

    text_props = {"source": source, "text_align": "left", "text_baseline": "middle"}

    def x_dodge(x_offset):
        return dodge("Level", x_offset, range=b_plot.x_range)

    def y_dodge(y_offset):
        return dodge("Row", y_offset, range=b_plot.y_range)

    b_plot.text(
        x=x_dodge(0.1), y=y_dodge(-0.2), text="cmd", text_font_size="7pt", **text_props
    )
    b_plot.text(
        x=x_dodge(0.1), y=y_dodge(0.25), text="Exe", text_font_size="8pt", **text_props
    )
    b_plot.text(
        x=x_dodge(1.8), y=y_dodge(0.25), text="PID", text_font_size="8pt", **text_props
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
    data_table = _create_data_table(source, schema)

    show(column(row(b_plot, range_tool), data_table))


# pylint: enable=too-many-locals


TreeResult = namedtuple("TreeResult", "proc_tree, schema, levels, n_rows")


def _pre_process_tree(proc_tree: pd.DataFrame, schema: ProcSchema = None):
    """Extract dimensions and formatted values from proc_tree."""
    if schema is None:
        schema = infer_schema(proc_tree)
    _validate_plot_schema(proc_tree, schema)

    n_rows = len(proc_tree)
    proc_tree["Row"] = proc_tree.index
    proc_tree["Row"] = n_rows - proc_tree["Row"]
    proc_tree["Level"] = proc_tree["path"].str.count("/") + 1

    levels = proc_tree["Level"].unique()

    max_cmd_len = int(350 / len(levels))
    long_cmd = proc_tree[schema.cmd_line].str.len() > max_cmd_len
    proc_tree.loc[long_cmd, "cmd"] = (
        proc_tree[schema.cmd_line].str[:max_cmd_len] + "..."
    )
    proc_tree.loc[~long_cmd, "cmd"] = proc_tree[schema.cmd_line].fillna(
        "cmdline unknown"
    )
    proc_tree["Exe"] = proc_tree.apply(
        lambda x: x[schema.process_name].split(schema.path_separator)[-1], axis=1
    )
    pid_fmt = (
        lambda x: f"PID: {x} ({int(x, base=16)})"
        if str(x).startswith("0x")
        else f"PID: 0x{int(x):x} ({int(x)})"
    )
    proc_tree["PID"] = proc_tree[schema.process_id].apply(pid_fmt)
    return TreeResult(proc_tree=proc_tree, schema=schema, levels=levels, n_rows=n_rows)


def _validate_plot_schema(proc_tree: pd.DataFrame, schema):
    """Validate that we have the required columns."""
    required_cols = set(
        ["path", schema.cmd_line, schema.process_name, schema.process_id]
    )
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
        ("PID", "@PID"),
        ("CmdLine", f"@{schema.cmd_line}"),
        ("SubjUser", f"@{schema.user_name}"),
        ("SubjLgnId", f"@{schema.logon_id}"),
        ("TargLgnId", f"@{schema.target_logon_id}"),
        ("Time", f"@{schema.time_stamp}{{%F %T}}"),
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
        out_str = JSON.stringify(output);
        py_str = `${output_var} = ${out_str}`;
        console.log(py_str);
        IPython.notebook.kernel.execute(py_str);
    """
    get_selected = CustomJS(
        args=dict(source=source, itemkey="proc_key", output_var=result_var),
        code=ret_var_js,
    )
    return get_selected


def _create_fill_map(
    source: ColumnDataSource, source_column: str = None
) -> Tuple[Union[factor_cmap, linear_cmap], Optional[ColorBar]]:
    """Create factor map or linear map based on `source_column`."""
    fill_map = "navy"
    color_bar = None
    if source_column is None or source_column not in source.data:
        return fill_map, color_bar

    col_kind = source.data[source_column].kind
    if col_kind in ["b", "o"]:
        src_values = np.unique(source.data[source_column])
        values = [str(val) for val in src_values if not np.isnan(val)]
        fill_map = factor_cmap(
            source_column, palette=Spectral[max(3, len(values))], factors=values
        )
    elif col_kind in ["i", "u", "f", "M"]:
        values = source.data[source_column]
        fill_map = linear_cmap(
            field_name=source_column,
            palette=Spectral[30],
            low=np.min(values),
            high=np.max(values),
        )
        color_bar = ColorBar(
            color_mapper=fill_map["transform"], width=8, location=(0, 0)
        )
    return fill_map, color_bar


# pylint: disable=too-many-arguments
def _create_vert_range_tool(
    data, min_y, max_y, plot_range, width, height, x_col, y_col, fill_map="navy"
):
    """Return vertical range too for plot."""
    rng_select = figure(
        plot_width=width, plot_height=height, y_range=(min_y - 1, max_y + 1)
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


def _create_data_table(source: ColumnDataSource, schema: ProcSchema):
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
    date_fmt = "%F %T"
    columns = [
        TableColumn(
            field=schema.time_stamp,
            title=schema.time_stamp,
            formatter=DateFormatter(format=date_fmt),
        )
    ]
    columns2 = [TableColumn(field=col, title=col) for col in column_names]
    data_table = DataTable(
        source=source, columns=columns + columns2, width=950, height=150
    )
    return data_table
