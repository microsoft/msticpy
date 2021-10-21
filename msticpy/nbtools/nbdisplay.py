# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Module for common display functions."""
from typing import Any, Mapping, Union, Tuple, List

import networkx as nx
import pandas as pd
from bokeh.io import output_notebook
from bokeh.plotting import figure, from_networkx, show
from bokeh.models import Circle, HoverTool, Label
from deprecated.sphinx import deprecated
import IPython
from IPython.core.display import HTML, display
from IPython.display import Javascript

from .._version import VERSION
from .security_alert import SecurityAlert

# pylint: disable=unused-import
from .timeline import display_timeline, display_timeline_values  # noqa
from .process_tree import build_and_show_process_tree, plot_process_tree  # noqa

# pylint: enable=unused-import
from ..common.utility import export

__version__ = VERSION
__author__ = "Ian Hellen"


@export
def display_alert(
    alert: Union[Mapping[str, Any], SecurityAlert], show_entities: bool = False
):
    """
    Display a Security Alert.

    Parameters
    ----------
    alert : Union[Mapping[str, Any], SecurityAlert]
        The alert to display as Mapping (e.g. pd.Series)
        or SecurityAlert
    show_entities : bool, optional
        Whether to display entities (the default is False)

    """
    output = format_alert(alert, show_entities)
    if not isinstance(output, tuple):
        output = [output]
    for disp_obj in output:
        display(disp_obj)


@export
def format_alert(
    alert: Union[Mapping[str, Any], SecurityAlert], show_entities: bool = False
) -> Union[IPython.display.HTML, Tuple[IPython.display.HTML, pd.DataFrame]]:
    """
    Get IPython displayable Security Alert.

    Parameters
    ----------
    alert : Union[Mapping[str, Any], SecurityAlert]
        The alert to display as Mapping (e.g. pd.Series)
        or SecurityAlert
    show_entities : bool, optional
        Whether to display entities (the default is False)

    Returns
    -------
    Union[IPython.display.HTML, Tuple[IPython.display.HTML, pd.DataFrame]]
        Single or tuple of displayable IPython objects

    Raises
    ------
    ValueError
        If the alert object is in an unknown format

    """
    if isinstance(alert, SecurityAlert):
        return HTML(alert.to_html(show_entities=show_entities))

    # Display subset of raw properties
    if isinstance(alert, pd.Series):
        entity = alert["CompromisedEntity"] if "CompromisedEntity" in alert else ""
        title = f"""
            <h3>Selected Alert: '{alert["AlertDisplayName"]}'</h3>
            <b>Alert_time:</b> {alert["StartTimeUtc"]},&nbsp;
            <b>Compr_entity:</b> {entity},&nbsp;
            <b>Alert_id:</b> {alert["SystemAlertId"]}
            <br/>
            """
        return HTML(title), pd.DataFrame(alert)

    raise ValueError("Unrecognized alert object type " + str(type(alert)))


@export
def display_process_tree(process_tree: pd.DataFrame):
    """
    Display process tree data frame. (Deprecated).

    Parameters
    ----------
    process_tree : pd.DataFrame
        Process tree DataFrame

    The display module expects the columns NodeRole and Level to
    be populated. NoteRole is one of: 'source', 'parent', 'child'
    or 'sibling'. Level indicates the 'hop' distance from the 'source'
    node.

    """
    build_and_show_process_tree(process_tree)


@export
def exec_remaining_cells():
    """Execute all cells below currently selected cell."""
    Javascript("Jupyter.notebook.execute_cells_below()")


@deprecated(
    reason=(
        "Matplotlib version 'draw_alert_entity_graph' "
        "no longer supported - use 'plot_entity_graph'"
    ),
    version="0.3.2",
)
@export
# pylint: disable=too-many-arguments
def draw_alert_entity_graph(
    nx_graph: nx.Graph,
    font_size: int = 12,
    height: int = 8,
    width: int = 8,
    margin: float = 0.3,
    scale: int = 1,
):
    """
    Draw networkX graph with matplotlib.

    Parameters
    ----------
    nx_graph : nx.Graph
        The NetworkX graph to draw
    font_size : int, optional
        base font size (the default is 12)
    height : int, optional
        Image height (the default is 8)
    width : int, optional
        Image width (the default is 8)
    margin : float, optional
        Image margin (the default is 0.3)
    scale : int, optional
        Position scale (the default is 1)

    """
    del margin
    return plot_entity_graph(
        entity_graph=nx_graph,
        font_size=font_size,
        height=height * 100,
        width=width * 100,
        scale=scale * 2,
    )


def plot_entity_graph(
    entity_graph: nx.Graph,
    node_size: int = 25,
    font_size: Union[int, str] = 10,
    height: int = 800,
    width: int = 800,
    scale: int = 2,
    hide: bool = False,
) -> figure:
    """
    Plot entity graph with Bokeh.

    Parameters
    ----------
    entity_graph : nx.Graph
        The entity graph as a networkX graph
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

    Returns
    -------
    bokeh.plotting.figure
        The network plot.

    """
    output_notebook()
    font_pnt = f"{font_size}pt" if isinstance(font_size, int) else font_size
    node_attrs = {
        node: attrs.get("color", "green")
        for node, attrs in entity_graph.nodes(data=True)
    }
    nx.set_node_attributes(entity_graph, node_attrs, "node_color")

    plot = figure(
        title="Alert Entity graph",
        x_range=(-3, 3),
        y_range=(-3, 3),
        width=width,
        height=height,
    )

    plot.add_tools(
        HoverTool(
            tooltips=[
                ("node_type", "@node_type"),
                ("name", "@name"),
                ("description", "@description"),
                ("entitytype", "@entitytype"),
            ]
        )
    )

    graph_renderer = from_networkx(
        entity_graph, nx.spring_layout, scale=scale, center=(0, 0)
    )
    graph_renderer.node_renderer.glyph = Circle(
        size=node_size, fill_color="node_color", fill_alpha=0.5
    )
    # pylint: disable=no-member
    plot.renderers.append(graph_renderer)

    # Create labels
    for name, pos in graph_renderer.layout_provider.graph_layout.items():
        label = Label(
            x=pos[0],
            y=pos[1],
            x_offset=5,
            y_offset=5,
            text=name,
            text_font_size=font_pnt,
        )
        plot.add_layout(label)
    # pylint: enable=no-member
    if not hide:
        show(plot)
    return plot


# Constants for Windows logon
_WIN_LOGON_TYPE_MAP = {
    0: "Unknown",
    2: "Interactive",
    3: "Network",
    4: "Batch",
    5: "Service",
    7: "Unlock",
    8: "NetworkCleartext",
    9: "NewCredentials",
    10: "RemoteInteractive",
    11: "CachedInteractive",
}
_WINDOWS_SID = {
    "S-1-0-0": "Null SID",
    "S-1-5-18": "LOCAL_SYSTEM",
    "S-1-5-19": "LOCAL_SERVICE",
    "S-1-5-20": "NETWORK_SERVICE",
}
_ADMINISTRATOR_SID = "500"
_GUEST_SID = "501"
_DOM_OR_MACHINE_SID = "S-1-5-21"


@export
def display_logon_data(
    logon_event: pd.DataFrame, alert: SecurityAlert = None, os_family: str = None
):
    """
    Display logon data for one or more events as HTML table.

    Parameters
    ----------
    logon_event : pd.DataFrame
        Dataframe containing one or more logon events
    alert : SecurityAlert, optional
        obtain os_family from the security alert
        (the default is None)
    os_family : str, optional
         explicitly specify os_family (Linux or Windows)
         (the default is None)

    Notes
    -----
    Currently only Windows Logon events.

    """
    display(format_logon(logon_event, alert, os_family))


@export
def format_logon(
    logon_event: Union[pd.DataFrame, pd.Series],
    alert: SecurityAlert = None,
    os_family: str = None,
) -> IPython.display.HTML:
    """
    Return logon data for one or more events as HTML table.

    Parameters
    ----------
    logon_event : Union[pd.DataFrame, pd.Series]
        Dataframe containing one or more logon events
        or Series containing a single logon event.
    alert : SecurityAlert, optional
        obtain os_family from the security alert
        (the default is None)
    os_family : str, optional
         explicitly specify os_family (Linux or Windows)
         (the default is None)

    Returns
    -------
    IPython.display.HTML :
        HTML display object

    """
    if not os_family:
        os_family = alert.os_family if alert else "Windows"

    logon_output = []
    if isinstance(logon_event, pd.DataFrame):
        for _, logon_row in logon_event.iterrows():
            logon_record = _fmt_single_row(logon_row, os_family)
            logon_output.append(
                "<tr class='cell_logon'><td class='cell_logon'>"
                + f"{'<br>'.join(logon_record)}</td></tr>"
            )
    elif isinstance(logon_event, pd.Series):
        logon_record = _fmt_single_row(logon_event, os_family)
        logon_output.append(
            "<tr class='cell_logon'><td class='cell_logon'>"
            + f"{'<br>'.join(logon_record)}</td></tr>"
        )

    t_style = """
        <style>
            .table_logon {border-collapse: collapse; width: 50%;}
            .cell_logon {border: 1px solid #ddd !important;
                text-align: left !important; padding: 15px !important;}
        </style>
        """
    return HTML(f"{t_style}<table class='table_logon'>{''.join(logon_output)}</table>")


def _fmt_single_row(logon_row: pd.Series, os_family: str) -> List[str]:
    """Format a pandas series logon record."""
    logon_record = [
        f"<b>Account: </b>{logon_row['TargetUserName']}",
        f"<b>Account Domain: </b>{logon_row['TargetDomainName']}",
        f"<b>Logon Time: </b>{logon_row['TimeGenerated']}",
    ]

    if os_family == "Windows":
        logon_type = logon_row["LogonType"]
        logon_desc_idx = logon_type
        if logon_type not in _WIN_LOGON_TYPE_MAP:
            logon_desc_idx = 0
        logon_record.append(
            f"<b>Logon type: </b>{logon_type}"
            + f"({_WIN_LOGON_TYPE_MAP[logon_desc_idx]})"
        )

    account_id = logon_row.TargetUserSid
    logon_record.append(f"<b>User Id/SID: </b>{account_id}")
    if os_family == "Windows":
        logon_record.extend(_format_sid_info(account_id))
    else:
        logon_record.append(f"<b>Audit user: </b>{logon_row['audit_user']}")

    session_id = logon_row["TargetLogonId"]
    sess_id = f"<b>Session id: </b>'{session_id}'"
    if session_id in ["0x3e7", "-1"]:
        sess_id += "System logon session"
    logon_record.append("")

    domain = logon_row["SubjectDomainName"]
    if not domain:
        subj_account = logon_row.SubjectUserName
    else:
        subj_account = f"{domain}/{logon_row.SubjectUserName}"
    logon_record.append(f"<b>Subject (source) account: </b>{subj_account}")

    logon_record.append(f"<b>Logon process: </b>{logon_row['LogonProcessName']}")
    logon_record.append(
        f"<b>Authentication: </b>{logon_row['AuthenticationPackageName']}"
    )
    logon_record.append(f"<b>Source IpAddress: </b>{logon_row['IpAddress']}")
    logon_record.append(f"<b>Source Host: </b>{logon_row['WorkstationName']}")
    logon_record.append(f"<b>Logon status: </b>{logon_row['Status']}")
    logon_record.append("")
    return logon_record


def _format_sid_info(sid):
    sid_info = []
    if not sid:
        return sid_info
    if sid in _WINDOWS_SID:
        sid_info.append(f"&nbsp;&nbsp;SID {sid} is {_WINDOWS_SID[sid]}")
    elif sid.endswith(_ADMINISTRATOR_SID):
        sid_info.append(f"&nbsp;&nbsp;SID {sid} is administrator")
    elif sid.endswith(_GUEST_SID):
        sid_info.append(f"&nbsp;&nbsp;SID {sid} is guest")
    if sid.startswith(_DOM_OR_MACHINE_SID):
        sid_info.append(f"&nbsp;&nbsp;SID {sid} is local machine or domain account")
    return sid_info
