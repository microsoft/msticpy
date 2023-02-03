# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""VirusTotal File Behavior functions."""
import re
from copy import deepcopy
from datetime import datetime
from pathlib import Path
from pprint import pformat
from typing import Any, Dict, List, Optional, Union

import attr
import ipywidgets as widgets
import numpy as np
import pandas as pd

from ..._version import VERSION
from ...common.exceptions import MsticpyImportExtraError, MsticpyUserError
from ...transform.proc_tree_builder import ProcSchema, build_proc_tree

try:
    import vt
except ImportError as imp_err:
    raise MsticpyImportExtraError(
        "Cannot use this feature without vt-py and vt-graph-api packages installed.",
        title="Error importing VirusTotal modules.",
        extra="vt3",
    ) from imp_err

__version__ = VERSION
__author__ = "Ian Hellen"


VT_API_NOT_FOUND = "NotFoundError"


_FB_CAT_PATTERNS = {
    "File": "file.*",
    "Process": "process.*|command.*|module.*",
    "Registry": "registry.*",
    "Network": ".*ips|dns.*|.*urls|ip.*|http.*|tls",
    "System": "mutex.*|calls.*|permissions.*|text.*",
    "Other": ".*",
}

_BORDER_LAYOUT = widgets.Layout(
    **{
        "width": "90%",
        "border": "solid gray 1px",
        "margin": "1pt",
        "padding": "5pt",
    }
)


class VTFileBehavior:
    """VirusTotal File Behavior class."""

    _SANDBOXES = [
        "f_secure_sandbox",
        "bitdam_atp",
        "vmray",
        "virustotal_zenbox",
        "sangfor_zsand",
        "virustotal_jujubox",
        "qianxin_reddrip",
        "nsfocus_poma",
        "virustotal_androbox",
        "venuseye_sandbox",
        "cyber_adapt",
        "dr_web_vxcube",
        "virustotal_observer",
        "tencent_habo",
        "yomi_hunter",
        "virustotal_jsbox",
        "virustotal_cuckoofork",
        "lastline",
        "reaqta_hive",
        "os_x_sandbox",
        "virustotal_droidy",
        "sndbox",
        "virustotal_r2dbox",
        "rising_moves",
        "virustotal_box_of_apples",
        "secondwrite",
        "malwation",
        "c2ae",
        "microsoft_sysinternals",
    ]

    _FP_ENDPOINTS = {
        "summary": "/files/{id}/behaviour_summary",
        "sandbox": "/file_behaviours/{id}_{sandbox}",
        "evtx": "/file_behaviours/{sandbox_id}/evtx",
        "pcap": "/file_behaviours/{sandbox_id}/pcap",
        "memdump": "/file_behaviours/{sandbox_id}/memdump",
    }

    @classmethod
    def list_sandboxes(cls) -> List[str]:
        """Return list of known sandbox types."""
        return list(cls._SANDBOXES)

    def __init__(
        self,
        vt_key: str = None,
        file_id: Optional[str] = None,
        file_summary: Optional[Union[pd.DataFrame, pd.Series, Dict[str, Any]]] = None,
    ):
        """
        Initialize the VTFileBehavior class.

        Parameters
        ----------
        vt_key : str, optional
            VirusTotal API key, by default None
        file_id : Optional[str], optional
            The ID of the file to look up, by default None
        file_summary : Optional[Union[pd.DataFrame, pd, Series, Dict[str, Any]]], optional
            VT file summary - this can be in one of the following formats:
            VT object dictionary
            Pandas DataFrame - first row is assumed to be the file summary
            Pandas Series
            by default None

        """
        self._vt_client = vt.Client(apikey=vt_key)
        if file_id is None and file_summary is None:
            raise MsticpyUserError(
                "You must supply either a file_id or a file_summary.",
                title="Missing required parameter.",
            )

        if isinstance(file_summary, pd.DataFrame):
            file_summary = file_summary.iloc[0]
        if isinstance(file_summary, pd.Series):
            file_summary = file_summary.to_dict()
        self.file_summary = file_summary or {}  # type: ignore
        self.file_id = file_id or self.file_summary.get("id")

        self._file_behavior: Dict[str, Any] = {}
        self.categories: Dict[str, Any] = {}
        self.behavior_links: Dict[str, Any] = {}
        self.process_tree_df: Optional[pd.DataFrame] = None

    def _reset_summary(self):
        self._file_behavior: Dict[str, Any] = {}
        self.categories: Dict[str, Any] = {}
        self.process_tree_df: Optional[pd.DataFrame] = None

    @property
    def sandbox_id(self) -> str:
        """Return sandbox ID of detonation."""
        return self.categories.get("id", "")

    @property
    def has_evtx(self) -> bool:
        """Return True if EVTX data is available (Enterprise only)."""
        return self.categories.get("has_evtx", False)

    @property
    def has_memdump(self) -> bool:
        """Return True if memory dump data is available (Enterprise only)."""
        return self.categories.get("has_memdump", False)

    @property
    def has_pcap(self) -> bool:
        """Return True if PCAP data is available (Enterprise only)."""
        return self.categories.get("has_pcap", False)

    def get_file_behavior(self, sandbox: str = None):
        """
        Retrieve the file behavior data.

        Parameters
        ----------
        sandbox : str, optional
            Name of specific sandbox to retrieve, by default None
            If None, it will retrieve the behavior summary.

        """
        if sandbox:
            endpoint = self._FP_ENDPOINTS["sandbox"].format(
                id=self.file_id,
                sandbox=sandbox,
            )
        else:
            endpoint = self._FP_ENDPOINTS["summary"].format(id=self.file_id)

        try:
            self._file_behavior = self._vt_client.get_data(endpoint)
        except vt.APIError as err:
            if err.args and err.args[0] == VT_API_NOT_FOUND:
                self._file_behavior = {"id": self.file_id, "result": VT_API_NOT_FOUND}
                return
            raise
        finally:
            self._vt_client.close()

        if "attributes" in self._file_behavior:
            self.categories = self._file_behavior.get("attributes", {})
            self.behavior_links = self._file_behavior.get("links", {})
        else:
            self.categories = self._file_behavior

    def browse(self) -> Optional[widgets.VBox]:
        """Browse the behavior categories."""
        if not self.has_behavior_data:
            self._print_no_data()
            return None
        groupings = {}
        remaining_categories = set(self.categories)
        for name, pattern in _FB_CAT_PATTERNS.items():
            groupings[name] = _extract_subcats(pattern, remaining_categories)
            remaining_categories = remaining_categories - groupings[name]

        accordion = widgets.Accordion()
        child_tabs = {}
        for group, sub_cats in groupings.items():
            sub_cat_tab = widgets.Tab()
            tab_content = {
                section: widgets.HTML(value=_format_widget_data(items))
                for section, items in self.categories.items()
                if items and section in sub_cats
            }
            sub_cat_tab.children = list(tab_content.values())
            for idx, section in enumerate(tab_content):
                sub_cat_tab.set_title(idx, section)
            child_tabs[group] = sub_cat_tab

        accordion.children = list(child_tabs.values())
        for idx, group_name in enumerate(child_tabs):
            accordion.set_title(idx, group_name)
        accordion.selected_index = 0

        html_title = widgets.HTML(
            ("<h2>VirusTotal Detonation Details</h2>" f"For file {self.file_id}"),
            layout=_BORDER_LAYOUT,
        )
        return widgets.VBox([html_title, accordion])

    @property
    def process_tree(self) -> Any:
        """Return the process tree plot."""
        if not self.has_behavior_data:
            self._print_no_data()
            return None
        if self.process_tree_df is None:
            self.process_tree_df = _build_process_tree(self.categories)
        plot, _ = self.process_tree_df.mp_plot.process_tree(
            schema=VT_PROCSCHEMA,
            legend_col="name",
            hide_legend=True,
        )
        return plot

    @property
    def has_behavior_data(self) -> bool:
        """Return true if file behavior data available."""
        return bool(self.categories)

    def _print_no_data(self):
        """Print a message if operation is tried with no data."""
        print(f"No data available for {self.file_id}.")


# Process tree extraction


# pylint: disable=too-few-public-methods
@attr.s(auto_attribs=True)
class SIProcess:
    """Data class to hold each process from detonation."""

    process_id: str
    name: str
    cmd_line: str
    parent_id: int = -1
    proc_key: Optional[str] = None
    parent_key: Optional[str] = None
    path: Optional[str] = None
    IsRoot: bool = False
    IsLeaf: bool = False
    IsBranch: bool = False
    children: list = []
    # proc_children: list = []
    time_offset: int = 0


# pylint: enable=too-few-public-methods


VT_PROCSCHEMA = ProcSchema(
    **{
        "process_name": "name",
        "process_id": "process_id",
        "parent_id": "parent_id",
        "cmd_line": "cmd_line",
        "time_stamp": "time_stamp",
        "logon_id": "logon_id",
        "path_separator": "\\",
        "user_name": "user_name",
        "host_name_column": "host",
        "event_id_column": "event_id",
    }
)


def _build_process_tree(fb_categories):
    """Top level function to create displayable DataFrame."""
    proc_tree_raw = deepcopy(fb_categories["processes_tree"])
    procs_created = {
        Path(proc).parts[-1].lower(): proc
        for proc in fb_categories["processes_created"]
    }

    si_procs = _extract_processes(proc_tree_raw, procs_created)
    process_tree_df = pd.DataFrame(_procs_to_df(si_procs)).drop(columns="children")
    process_tree_df = _try_match_commandlines(
        fb_categories["command_executions"], process_tree_df
    )
    return _fill_missing_proc_tree_values(process_tree_df)


def _extract_processes(process_data, procs_created, parent=None):
    """Convert processes_tree attribute to SIProcessObjects."""
    procs = []
    for process in process_data:
        si_proc = _create_si_proc(process, procs_created)
        # pylint: disable=invalid-name
        if parent:
            si_proc.parent_key = parent.proc_key
            si_proc.IsBranch = True
        else:
            si_proc.IsRoot = True
        child_procs_raw = process.get("children", [])
        if child_procs_raw:
            si_proc.children = _extract_processes(
                child_procs_raw, procs_created, parent=si_proc
            )
        else:
            si_proc.IsLeaf = True
            si_proc.IsBranch = False
        procs.append(si_proc)
    return procs


def _create_si_proc(raw_proc, procs_created):
    """Return an SIProcess Object from a raw VT proc definition."""
    # raw_proc = copy(raw_proc)
    name = raw_proc.get("name")
    raw_proc["cmd_line"] = name
    for proc in procs_created:
        if name.lower().endswith(proc):
            raw_proc["name"] = procs_created[proc]
            break
    raw_proc["proc_key"] = raw_proc["process_id"] + "|" + raw_proc["name"]
    # print(name, raw_proc.keys())
    return SIProcess(**raw_proc)


# Convert to DF
def _procs_to_df(procs):
    """Convert the SIProcess objects recursively to a list."""
    df_list = []
    for proc in procs:
        df_list.append(attr.asdict(proc))
        if proc.children:
            df_list.extend(_procs_to_df(proc.children))
    return df_list


# Try to Match up 'command_executions' commandline data with
# process_df.
def _try_match_commandlines(
    command_executions, procs_cmds: pd.DataFrame
) -> pd.DataFrame:
    """Return DF with matched commandlines."""
    procs_cmd = procs_cmds.copy()
    procs_cmd["cmd_line"] = np.nan
    weak_matches = 0
    for cmd in command_executions:
        for idx, row in procs_cmd.iterrows():
            # print(row["name"], cmd, row["cmd_line"], isinstance(row["cmd_line"], str))
            if (
                not isinstance(row["cmd_line"], str)
                and np.isnan(row["cmd_line"])
                and row["name"] in cmd
            ):
                # print("Found match:", row["name"], "==", cmd)
                procs_cmd.loc[idx, "cmd_line"] = cmd
                break
    for cmd in command_executions:
        for idx, row in procs_cmd.iterrows():
            # print(row["name"], cmd, row["cmd_line"], isinstance(row["cmd_line"], str))
            if (
                not isinstance(row["cmd_line"], str)
                and np.isnan(row["cmd_line"])
                and Path(row["name"]).stem.lower() in cmd.lower()
            ):
                weak_matches += 1
                # print("Found weak match:", row["name"], "~=", cmd)
                procs_cmd.loc[idx, "cmd_line"] = cmd
                break

    if weak_matches:
        print(
            f"WARNING: {weak_matches} of the {len(command_executions)} commandlines",
            "were weakly matched - some commandlines may be attributed",
            "to the wrong instance of the process.",
            end="\n",
        )
    return procs_cmd


def _fill_missing_proc_tree_values(process_df: pd.DataFrame) -> pd.DataFrame:
    # Define a schema to map Df names on to internal ProcSchema
    process_df["path"] = np.nan
    process_df.loc[process_df.IsRoot, "path"] = process_df[
        process_df.IsRoot
    ].index.astype("str")

    # Fill in some required fields with placeholder data
    process_df["time_stamp"] = datetime.utcnow()
    process_df["host"] = "sandbox"
    process_df["logon_id"] = "na"
    process_df["event_id"] = "na"
    process_df["source_index"] = process_df.index.astype("str")

    proc_tree = process_df.set_index("proc_key")

    first_unique = proc_tree.index.duplicated()
    proc_tree = proc_tree[~first_unique]
    # msticpy function to build the tree
    return build_proc_tree(proc_tree)


# Process browser helper functions
def _extract_subcats(pattern, categs):
    """Extract the category names matching `pattern`."""
    return {cat for cat in categs if re.match(pattern, cat)}


def _format_widget_data(data_item):
    if not data_item:
        return ""
    if isinstance(data_item, list):
        if isinstance(data_item[0], dict):
            return pd.DataFrame(data_item).style.hide(axis="index").to_html()
        if isinstance(data_item[0], str):
            return pd.DataFrame(pd.Series(data_item)).style.hide(axis="index").to_html()
    return f"<pre>{pformat(data_item)}</pre>"
