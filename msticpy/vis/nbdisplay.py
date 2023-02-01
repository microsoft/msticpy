# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Module for common display functions."""
from typing import Any, List, Mapping, Tuple, Union

import IPython
import networkx as nx
import pandas as pd
from deprecated.sphinx import deprecated
from IPython.display import HTML, Javascript, display

from .._version import VERSION
from ..common.utility import export
from ..nbtools.security_alert import SecurityAlert
from .network_plot import plot_entity_graph

# pylint: disable=unused-import
from .timeline import display_timeline  # noqa: F401
from .timeline_values import display_timeline_values  # noqa: F401

# pylint: disable=unused-import

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
        return pd.DataFrame(alert)

    raise ValueError(f"Unrecognized alert object type {type(alert)}")


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
