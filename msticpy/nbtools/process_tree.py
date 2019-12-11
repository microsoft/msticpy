# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Process Tree Visualization."""
from collections import namedtuple
from typing import Optional

import attr
import ipywidgets as wgt

import pandas as pd
from bokeh.io import output_notebook, show, output_file, reset_output
from bokeh.plotting import figure
from bokeh.transform import dodge, factor_cmap
from bokeh.models import (
    HoverTool,
    ColumnDataSource,
    CustomJS,
    Legend,
    LegendItem,
    BoxSelectTool,
)
from bokeh.models.callbacks import CustomJS
from bokeh.palettes import Spectral, RdGy, Set3
from bokeh.models.widgets import DataTable, TableColumn, DateFormatter
from bokeh.models import (
    ColumnDataSource,
    DatetimeTickFormatter,
    HoverTool,
    Label,
    Legend,
    RangeTool,
    Title,
)

from bokeh.layouts import column, row

from .process_tree_utils import build_tree

from .._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"


@attr.s(auto_attribs=True)
class ProcSchema:
    """Property name lookup for Process event schema."""

    process_name: str
    process_id: str
    parent_id: str
    logon_id: str
    cmd_line: str
    user_name: str
    path_separator: str
    time_stamp: str = "TimeGenerated"
    parent_name: Optional[str] = None
    target_logon_id: Optional[str] = None
    user_name: Optional[str] = None
    path_separator: Optional[str] = None
    user_id: Optional[str] = None


win_event_sch = ProcSchema(
    time_stamp="TimeGenerated",
    process_name="NewProcessName",
    process_id="NewProcessId",
    parent_name="ParentProcessName",
    parent_id="ProcessId",
    logon_id="SubjectLogonId",
    target_logon_id="TargetLogonId",
    cmd_line="CommandLine",
    user_name="SubjectUserName",
    path_separator="\\",
    user_id="SubjectUserSid",
)
lx_event_sch = ProcSchema(
    time_stamp="TimeGenerated",
    process_name="exe",
    process_id="pid",
    parent_name=None,
    parent_id="ppid",
    logon_id="ses",
    target_logon_id=None,
    cmd_line="cmdline",
    user_name="acct",
    path_separator="/",
    user_id="uid",
)

LX_TYPE_DICT = {
    "argc": "str",
    "egid": "str",
    "euid": "str",
    "gid": "str",
    "ppid": "str",
    "pid": "str",
    "ses": "str",
    "uid": "str",
}


def plot_process_tree(procs: pd.DataFrame):
    procs_with_path, schema = build_process_tree(procs)
    # TODO - if we have multiple roots - ask the user to choose?
    if len(procs_with_path[procs_with_path["IsRoot"] == True] > 1):
        print("many roots")

    p_tree_srt = procs_with_path.sort_values("path", ascending="True")
    p_tree_srt = p_tree_srt.reset_index()
    plot_tree(p_tree_srt)


def plot_tree(p_tree_srt: pd.DataFrame, schema: ProcSchema, selected_keys):
    reset_output()
    output_notebook()

    plot_rows = len(p_tree_srt)
    p_tree_srt["Row"] = p_tree_srt.index
    p_tree_srt["Row"] = plot_rows - p_tree_srt["Row"]
    p_tree_srt["PlotLevel"] = p_tree_srt["path"].str.count("/") + 1

    n_levels = p_tree_srt["PlotLevel"].unique()
    n_rows = len(p_tree_srt)

    max_cmd_len = int(350 / len(n_levels))
    long_cmd = p_tree_srt[schema.cmd_line].str.len() > max_cmd_len
    p_tree_srt.loc[long_cmd, "CommandLine_tr"] = (
        p_tree_srt[schema.cmd_line].str[:max_cmd_len] + "..."
    )
    p_tree_srt.loc[~long_cmd, "CommandLine_tr"] = p_tree_srt[schema.cmd_line].fillna(
        "cmdline unknown"
    )
    p_tree_srt["Exe"] = p_tree_srt.apply(
        lambda x: x[schema.process_name].split(schema.path_separator)[-1], axis=1
    )
    pid_fmt = (
        lambda x: f"PID: {x} ({int(x, base=16)})"
        if str(x).startswith("0x")
        else f"PID: 0x{int(x):x} ({int(x)})"
    )
    p_tree_srt["PID"] = p_tree_srt[schema.process_id].apply(pid_fmt)

    source = ColumnDataSource(data=p_tree_srt)
    TOOLTIPS = [
        ("Process", f"@{schema.process_name}"),
        ("PID", "@PID"),
        ("CmdLine", f"@{schema.cmd_line}"),
        ("SubjUser", f"@{schema.user_name}"),
        ("SubjLgnId", f"@{schema.logon_id}"),
        ("TargLgnId", f"@{schema.target_logon_id}"),
        ("Time", f"@{schema.time_stamp}{{%F %T}}"),
    ]
    max_level = max(n_levels) + 3
    min_level = min(n_levels)
    plot_height = 35 * len(p_tree_srt)
    plot_height = 700
    visible_range = int(plot_height / 35)
    y_start_range = (plot_rows - visible_range, plot_rows + 1)
    p = figure(
        title="ProcessTree",
        plot_width=900,
        plot_height=plot_height,
        x_range=(min_level, max_level),
        y_range=y_start_range,
        tools=["ypan", "reset", "save", "tap"],
        toolbar_location="above",
    )
    hover = HoverTool(tooltips=TOOLTIPS, formatters={"TimeGenerated": "datetime"})
    p.add_tools(hover)

    # Coloring and Legend
    logon_ids = p_tree_srt["EffectiveLogonId"].fillna("unknown").astype("str").unique()
    user_col = ""
    if schema.user_name in p_tree_srt.columns:
        user_col = schema.user_name
    elif schema.user_id in p_tree_srt.columns:
        user_col = schema.user_id

    if user_col:
        users = p_tree_srt[user_col].fillna("unknown").astype("str").unique()
        fill_map = factor_cmap(
            user_col, palette=Spectral[max(3, len(users))], factors=users
        )
    else:
        fill_map = "navy"

    #     users = p_tree_srt[schema.user_name].fillna("unknown").unique()
    #     fill_map = factor_cmap(schema.user_name, palette=Spectral[max(3, len(users))], factors=users)
    # else:

    line_map = factor_cmap(
        "EffectiveLogonId", palette=RdGy[max(3, len(logon_ids))], factors=logon_ids
    )

    r_x = dodge("PlotLevel", 1.75, range=p.x_range)
    r = p.rect(
        r_x,
        "Row",
        3.5,
        0.95,
        source=source,
        fill_alpha=0.6,
        line_color=line_map,
        fill_color=fill_map,
        legend=user_col,
    )
    p.legend.title = user_col

    text_props = {"source": source, "text_align": "left", "text_baseline": "middle"}

    x = dodge("PlotLevel", 0.1, range=p.x_range)

    p.text(
        x=x,
        y=dodge("Row", -0.2, range=p.y_range),
        text="CommandLine_tr",
        text_font_size="7pt",
        **text_props,
    )
    p.text(
        x=x,
        y=dodge("Row", 0.25, range=p.y_range),
        text="Exe",
        text_font_size="8pt",
        **text_props,
    )
    p.text(
        x=dodge("PlotLevel", 1.8, range=p.x_range),
        y=dodge("Row", 0.25, range=p.y_range),
        text="PID",
        text_font_size="8pt",
        **text_props,
    )

    p.outline_line_color = None
    p.grid.grid_line_color = "navy"
    p.axis.axis_line_color = None
    p.axis.major_tick_line_color = "navy"
    p.xaxis.visible = False
    p.yaxis.visible = False
    p.xgrid.visible = True
    p.ygrid.visible = False
    p.xaxis.ticker = sorted(n_levels)
    p.xgrid.minor_grid_line_color = "navy"
    p.xgrid.minor_grid_line_alpha = 0.1
    p.xgrid.grid_line_color = "navy"
    p.xgrid.ticker = sorted(n_levels)
    p.xgrid.grid_line_alpha = 0.1
    p.axis.major_label_standoff = 0
    p.hover.renderers = [r]  # only hover element boxes

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
        args=dict(source=source, itemkey="proc_key", output_var="selected_procs"),
        code=ret_var_js,
    )
    p.js_on_event("tap", get_selected)

    box_select = BoxSelectTool(callback=get_selected)
    p.add_tools(box_select)

    range_tool = create_v_range_tool(
        data=source,
        min_y=0,
        max_y=plot_rows,
        plot_range=p.y_range,
        # fixed_range,
        width=90,
        height=plot_height,
        x_col="PlotLevel",
        y_col="Row",
        fill_map=fill_map,
        hover_tool=hover,
    )
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

    # t = p.Text("")
    show(column(row(p, range_tool), data_table))


def create_v_range_tool(
    data,
    min_y,
    max_y,
    plot_range,
    width,
    height,
    x_col,
    y_col,
    fill_map="navy",
    hover_tool=None,
):
    rng_select = figure(
        plot_width=width,
        plot_height=height,
        y_range=(min_y - 1, max_y + 1),
        tools=[hover_tool],
        toolbar_location=None,
    )

    x = dodge(x_col, -0.5)
    r = rng_select.rect(
        x, y_col, 1.2, 0.8, source=data, fill_alpha=0.6, fill_color=fill_map
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
