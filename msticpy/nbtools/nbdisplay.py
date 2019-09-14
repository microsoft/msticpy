# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Module for common display functions."""
from typing import Any, Mapping, Union

import matplotlib.pyplot as plt
import networkx as nx
import pandas as pd

from IPython.core.display import HTML, display
from IPython.display import Javascript

from .._version import VERSION
from .security_alert import SecurityAlert

# pylint: disable=unused-import
from .timeline import display_timeline, display_timeline_values  # noqa

# pylint: enable=unused-import
from .utility import export

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
    if isinstance(alert, SecurityAlert):
        display(HTML(alert.to_html(show_entities=False)))
        if show_entities:
            for entity in alert.entities:
                print(entity)
        return

    # Display subset of raw properties
    if isinstance(alert, pd.Series):
        entity = alert["CompromisedEntity"] if "CompromisedEntity" in alert else ""
        title = """
            <h3>Alert: '{name}'</h3><br>time=<b>{start}</b>,
            entity=<b>{entity}</b>, id=<b>{id}</b>
            """.format(
            start=alert["StartTimeUtc"],
            name=alert["AlertDisplayName"],
            entity=entity,
            id=alert["SystemAlertId"],
        )
        display(HTML(title))
        display(pd.DataFrame(alert))
    else:
        raise ValueError("Unrecognized alert object type " + str(type(alert)))


def _print_process(process_row: pd.Series, fmt: str = "html") -> str:
    """
    Format individual process item as text or html.

    Parameters
    ----------
    process_row : pd.Series
        Process series
    fmt : str, optional
        Format ('txt' or 'html')
        (the default is ' html')

    Returns
    -------
    str
        Formatted process summary.

    """
    if process_row.NodeRole == "parent":
        if process_row.Level > 1:
            level = 0
        else:
            level = 1
    elif process_row.NodeRole == "source":
        level = 2
    elif process_row.NodeRole == "child":
        level = 3 + process_row.Level
    else:
        level = 2

    px_spaces = 20 * level * 2
    txt_spaces = " " * (4 * int(level))

    font_col = "red" if process_row.NodeRole == "source" else "inherit"

    if fmt.lower() == "html":
        l1_span = f'<span style="color:{font_col};font-size:90%">'
        line1_w_tmplt = (
            l1_span
            + "[{NodeRole}:lev{Level}] {TimeGenerated} "
            + "<b>{NewProcessName}</b> "
            + "[PID: {NewProcessId}, "
            + "SubjSess:{SubjectLogonId}, "
            + "TargSess:{TargetLogonId}]"
        )
        line2_w_tmplt = '(Cmdline: "{CommandLine}") [Account: {Account}]</span>' ""
        output_tmplt = (
            f'<div style="margin-left:{px_spaces}px">' + f"{{line1}}<br>{{line2}}</div>"
        )
        line1_lx_tmplt = (
            l1_span
            + "[{NodeRole}:lev{Level}] {TimeGenerated} "
            + "<b>{NewProcessName}</b> "
            + "[PID: {NewProcessId}, "
            + "PPID: {ProcessId}]"
        )
        line2_lx_tmplt = '(Cmdline: "{CommandLine}") [Path: {cwd}]</span>' ""
        output_tmplt = (
            f'<div style="margin-left:{px_spaces}px">' + f"{{line1}}<br>{{line2}}</div>"
        )
    else:
        line1_w_tmplt = (
            "[{NodeRole}:lev{Level}] {TimeGenerated} "
            + "{NewProcessName} "
            + "[PID: {NewProcessId}, "
            + "SubjSess:{SubjectLogonId}, "
            + "TargSess:{TargetLogonId}]"
        )
        line2_w_tmplt = '(Cmdline: "{CommandLine}") [Account: {Account}]'
        line1_lx_tmplt = (
            "[{NodeRole}:lev{Level}] {TimeGenerated} "
            + "{NewProcessName} "
            + "[PID: {NewProcessId}, "
            + "PPID: {ProcessId}]"
        )
        line2_lx_tmplt = '(Cmdline: "{CommandLine}") [Path: {cwd}]'
        output_tmplt = f"\n{txt_spaces}{{line1}}\n{txt_spaces}{{line2}}"
    rows = process_row.to_dict()
    if "TargetLogonId" in rows:
        line1 = line1_w_tmplt.format(**(rows))
        line2 = line2_w_tmplt.format(**(rows))
    else:
        line1 = line1_lx_tmplt.format(**(rows))
        line2 = line2_lx_tmplt.format(**(rows))

    return output_tmplt.format(line1=line1, line2=line2)


def format_process_tree(process_tree: pd.DataFrame, fmt: str = "html"):
    """
    Display process tree data frame.

    Parameters
    ----------
    process_tree : pd.DataFrame
        Process tree DataFrame
    fmt : str, optional
        Format ('txt' or 'html')
        (the default is ' html')

    Returns
    -------
    str
        Formatted process tree.

    The display module expects the columns NodeRole and Level to
    be populated. NoteRole is one of: 'source', 'parent', 'child'
    or 'sibling'. Level indicates the 'hop' distance from the 'source'
    node.

    """
    if "TimeCreatedUtc" in process_tree.index:
        tree = process_tree.sort_values(by=["TimeCreatedUtc"], ascending=True)
    else:
        tree = process_tree.sort_values(by=["TimeGenerated"], ascending=True)

    if fmt.lower() == "html":
        out_string = "<h3>Process tree:</h3>"
    else:
        out_string = "Process tree:"
        out_string = out_string + "\n" + "_" * len(out_string)

    for _, line in tree.iterrows():
        out_string += _print_process(line, fmt)

    return out_string


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
    display(HTML(format_process_tree(process_tree)))


@export
def exec_remaining_cells():
    """Execute all cells below currently selected cell."""
    Javascript("Jupyter.notebook.execute_cells_below()")


@export
# pylint: disable=too-many-arguments
def draw_alert_entity_graph(
    nx_graph: nx.Graph,
    font_size: int = 12,
    height: int = 15,
    width: int = 15,
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
        Image height (the default is 15)
    width : int, optional
        Image width (the default is 15)
    margin : float, optional
        Image margin (the default is 0.3)
    scale : int, optional
        Position scale (the default is 1)

    """
    alert_node = [
        n
        for (n, node_type) in nx.get_node_attributes(nx_graph, "node_type").items()
        if node_type == "alert"
    ]
    entity_nodes = [
        n
        for (n, node_type) in nx.get_node_attributes(nx_graph, "node_type").items()
        if node_type == "entity"
    ]

    # now draw them in subsets  using the `nodelist` arg
    plt.rcParams["figure.figsize"] = (width, height)

    plt.margins(x=margin, y=margin)

    pos = nx.kamada_kawai_layout(nx_graph, scale=scale, weight="weight")
    nx.draw_networkx_nodes(
        nx_graph, pos, nodelist=alert_node, node_color="red", alpha=0.5, node_shape="o"
    )
    nx.draw_networkx_nodes(
        nx_graph,
        pos,
        nodelist=entity_nodes,
        node_color="green",
        alpha=0.5,
        node_shape="s",
        s=200,
    )
    nlabels = nx.get_node_attributes(nx_graph, "description")
    nx.relabel_nodes(nx_graph, nlabels)
    nx.draw_networkx_labels(nx_graph, pos, nlabels, font_size=font_size)
    nx.draw_networkx_edges(nx_graph, pos)
    elabels = nx.get_edge_attributes(nx_graph, "description")
    nx.draw_networkx_edge_labels(
        nx_graph, pos, edge_labels=elabels, font_size=font_size * 2 / 3, alpha=0.6
    )


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
    Display logon data for one or more events.

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

    """
    if not os_family:
        os_family = alert.os_family if alert else "Windows"

    for _, logon_row in logon_event.iterrows():
        print("### Account Logon")
        print("Account: ", logon_row["TargetUserName"])
        print("Account Domain: ", logon_row["TargetDomainName"])
        print("Logon Time: ", logon_row["TimeGenerated"])

        if os_family == "Windows":
            logon_type = logon_row["LogonType"]
            logon_desc_idx = logon_type
            if logon_type not in _WIN_LOGON_TYPE_MAP:
                logon_desc_idx = 0
            print(
                f"Logon type: {logon_type} ", f"({_WIN_LOGON_TYPE_MAP[logon_desc_idx]})"
            )

        account_id = logon_row.TargetUserSid
        print("User Id/SID: ", account_id)
        if os_family == "Windows":
            _print_sid_info(account_id)
        else:
            print("Audit user: ", logon_row["audit_user"])

        session_id = logon_row["TargetLogonId"]
        print(f"Session id '{session_id}'", end="  ")
        if session_id in ["0x3e7", "-1"]:
            print("System logon session")

        print()
        domain = logon_row["SubjectDomainName"]
        if not domain:
            subj_account = logon_row.SubjectUserName
        else:
            subj_account = f"{domain}/{logon_row.SubjectUserName}"
        print("Subject (source) account: ", subj_account)

        print("Logon process: ", logon_row["LogonProcessName"])
        print("Authentication: ", logon_row["AuthenticationPackageName"])
        print("Source IpAddress: ", logon_row["IpAddress"])
        print("Source Host: ", logon_row["WorkstationName"])
        print("Logon status: ", logon_row["Status"])
        print()


def _print_sid_info(sid):
    if sid in _WINDOWS_SID:
        print("    SID {} is {}".format(sid, _WINDOWS_SID[sid]))
    elif sid.endswith(_ADMINISTRATOR_SID):
        print("    SID {} is administrator".format(sid))
    elif sid.endswith(_GUEST_SID):
        print("    SID {} is guest".format(sid))
    if sid.startswith(_DOM_OR_MACHINE_SID):
        print("    SID {} is local machine or domain account".format(sid))
