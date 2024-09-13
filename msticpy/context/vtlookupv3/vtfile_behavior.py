# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""VirusTotal File Behavior functions."""
from __future__ import annotations

import logging
import re
from copy import deepcopy
from datetime import datetime, timezone
from pathlib import Path
from pprint import pformat
from typing import TYPE_CHECKING, Any, ClassVar

import attr
import ipywidgets as widgets
import numpy as np
import pandas as pd
from typing_extensions import Self

from ..._version import VERSION
from ...common.exceptions import MsticpyImportExtraError, MsticpyUserError
from ...transform.proc_tree_builder import ProcSchema, build_proc_tree

if TYPE_CHECKING:
    from bokeh.plotting import figure

try:
    import vt
except ImportError as imp_err:
    err_msg: str = (
        "Cannot use this feature without vt-py and vt-graph-api packages installed."
    )
    raise MsticpyImportExtraError(
        err_msg,
        title="Error importing VirusTotal modules.",
        extra="vt3",
    ) from imp_err
logger: logging.Logger = logging.getLogger(__name__)
__version__ = VERSION
__author__ = "Ian Hellen"


VT_API_NOT_FOUND = "NotFoundError"


_FB_CAT_PATTERNS: dict[str, str] = {
    "File": "file.*",
    "Process": "process.*|command.*|module.*",
    "Registry": "registry.*",
    "Network": ".*ips|dns.*|.*urls|ip.*|http.*|tls",
    "System": "mutex.*|calls.*|permissions.*|text.*",
    "Other": ".*",
}

_BORDER_LAYOUT = widgets.Layout(
    width="90%",
    border="solid gray 1px",
    margin="1pt",
    padding="5pt",
)


class VTFileBehavior:
    """VirusTotal File Behavior class."""

    _SANDBOXES: ClassVar[list[str]] = [
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

    _FP_ENDPOINTS: ClassVar[dict[str, str]] = {
        "summary": "/files/{id}/behaviour_summary",
        "sandbox": "/file_behaviours/{id}_{sandbox}",
        "evtx": "/file_behaviours/{sandbox_id}/evtx",
        "pcap": "/file_behaviours/{sandbox_id}/pcap",
        "memdump": "/file_behaviours/{sandbox_id}/memdump",
    }

    @classmethod
    def list_sandboxes(cls: type[Self]) -> list[str]:
        """Return list of known sandbox types."""
        return list(cls._SANDBOXES)

    def __init__(
        self: Self,
        vt_key: str,
        file_id: str | None = None,
        file_summary: pd.DataFrame | pd.Series | dict[str, Any] | None = None,
    ) -> None:
        """
        Initialize the VTFileBehavior class.

        Parameters
        ----------
        vt_key : str, optional
            VirusTotal API key, by default None
        file_id : Optional[str], optional
            The ID of the file to look up, by default None
        file_summary : Optional[Union[pd.DataFrame, pd, Series, dict[str, Any]]], optional
            VT file summary - this can be in one of the following formats:
            VT object dictionary
            Pandas DataFrame - first row is assumed to be the file summary
            Pandas Series
            by default None

        """
        self._vt_client = vt.Client(apikey=vt_key)
        if file_id is None and file_summary is None:
            error_msg: str = "You must supply either a file_id or a file_summary."
            raise MsticpyUserError(
                error_msg,
                title="Missing required parameter.",
            )

        if isinstance(file_summary, pd.DataFrame):
            file_summary = file_summary.iloc[0]
        if isinstance(file_summary, pd.Series):
            file_summary = file_summary.to_dict()
        self.file_summary: dict[str, Any] = file_summary or {}
        self.file_id: str | Any | None = file_id or self.file_summary.get("id")

        self._file_behavior: dict[str, Any] = {}
        self.categories: dict[str, Any] = {}
        self.behavior_links: dict[str, Any] = {}
        self.process_tree_df: pd.DataFrame | None = None

    def _reset_summary(self: Self) -> None:
        self._file_behavior = {}
        self.categories = {}
        self.process_tree_df = None

    @property
    def sandbox_id(self: Self) -> str:
        """Return sandbox ID of detonation."""
        return self.categories.get("id", "")

    @property
    def has_evtx(self: Self) -> bool:
        """Return True if EVTX data is available (Enterprise only)."""
        return self.categories.get("has_evtx", False)

    @property
    def has_memdump(self: Self) -> bool:
        """Return True if memory dump data is available (Enterprise only)."""
        return self.categories.get("has_memdump", False)

    @property
    def has_pcap(self: Self) -> bool:
        """Return True if PCAP data is available (Enterprise only)."""
        return self.categories.get("has_pcap", False)

    def get_file_behavior(self: Self, sandbox: str | None = None) -> None:
        """
        Retrieve the file behavior data.

        Parameters
        ----------
        sandbox : str, optional
            Name of specific sandbox to retrieve, by default None
            If None, it will retrieve the behavior summary.

        """
        if sandbox:
            endpoint: str = self._FP_ENDPOINTS["sandbox"].format(
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

    def browse(self: Self) -> widgets.VBox | None:
        """Browse the behavior categories."""
        if not self.has_behavior_data:
            self._print_no_data()
            return None
        groupings: dict[str, Any] = {}
        remaining_categories: set[str] = set(self.categories)
        for name, pattern in _FB_CAT_PATTERNS.items():
            groupings[name] = _extract_subcats(pattern, remaining_categories)
            remaining_categories = remaining_categories - groupings[name]

        accordion = widgets.Accordion()
        child_tabs: dict[str, Any] = {}
        for group, sub_cats in groupings.items():
            sub_cat_tab = widgets.Tab()
            tab_content: dict[str, widgets.HTML] = {
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
            ("<h2>VirusTotal Detonation Details</h2> For file {self.file_id}"),
            layout=_BORDER_LAYOUT,
        )
        return widgets.VBox([html_title, accordion])

    @property
    def process_tree(self: Self) -> figure | None:
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
    def has_behavior_data(self: Self) -> bool:
        """Return true if file behavior data available."""
        return bool(self.categories)

    def _print_no_data(self: Self) -> None:
        """Print a message if operation is tried with no data."""
        logger.info("No data available for %s.", self.file_id)


# Process tree extraction


# pylint: disable=too-few-public-methods
@attr.s(auto_attribs=True)
class SIProcess:
    """Data class to hold each process from detonation."""

    process_id: str
    name: str
    cmd_line: str
    parent_id: int = -1
    proc_key: str | None = None
    parent_key: str | None = None
    path: str | None = None
    IsRoot: bool = False
    IsLeaf: bool = False
    IsBranch: bool = False
    children: list = attr.ib(factory=list)
    time_offset: int = 0


# pylint: enable=too-few-public-methods


VT_PROCSCHEMA = ProcSchema(
    process_name="name",
    process_id="process_id",
    parent_id="parent_id",
    cmd_line="cmd_line",
    time_stamp="time_stamp",
    logon_id="logon_id",
    path_separator="\\",
    user_name="user_name",
    host_name_column="host",
    event_id_column="event_id",
)


def _build_process_tree(fb_categories: dict[str, Any]) -> pd.DataFrame:
    """Top level function to create displayable DataFrame."""
    proc_tree_raw: list[dict[str, Any]] = deepcopy(fb_categories["processes_tree"])
    procs_created: dict[str, Any] = {
        Path(proc).parts[-1].lower(): proc
        for proc in fb_categories["processes_created"]
    }

    si_procs: list[SIProcess] = _extract_processes(proc_tree_raw, procs_created)
    process_tree_df: pd.DataFrame = pd.DataFrame(_procs_to_df(si_procs)).drop(
        columns="children",
    )
    process_tree_df = _try_match_commandlines(
        fb_categories["command_executions"],
        process_tree_df,
    )
    return _fill_missing_proc_tree_values(process_tree_df)


def _extract_processes(
    process_data: list[dict[str, Any]],
    procs_created: dict[str, Any],
    parent: SIProcess | None = None,
) -> list[SIProcess]:
    """Convert processes_tree attribute to SIProcessObjects."""
    procs: list[SIProcess] = []
    for process in process_data:
        si_proc: SIProcess = _create_si_proc(process, procs_created)
        # pylint: disable=invalid-name
        if parent:
            si_proc.parent_key = parent.proc_key
            si_proc.IsBranch = True
        else:
            si_proc.IsRoot = True
        child_procs_raw: list[dict[str, Any]] = process.get("children", [])
        if child_procs_raw:
            si_proc.children = _extract_processes(
                child_procs_raw,
                procs_created,
                parent=si_proc,
            )
        else:
            si_proc.IsLeaf = True
            si_proc.IsBranch = False
        procs.append(si_proc)
    return procs


def _create_si_proc(
    raw_proc: dict[str, Any],
    procs_created: dict[str, Any],
) -> SIProcess:
    """Return an SIProcess Object from a raw VT proc definition."""
    name: str = raw_proc["name"]
    raw_proc["cmd_line"] = name
    for proc in procs_created:
        if name.lower().endswith(proc):
            raw_proc["name"] = procs_created[proc]
            break
    raw_proc["proc_key"] = raw_proc["process_id"] + "|" + raw_proc["name"]
    return SIProcess(**raw_proc)


# Convert to DF
def _procs_to_df(procs: list[SIProcess]) -> list[dict[str, Any]]:
    """Convert the SIProcess objects recursively to a list."""
    df_list: list[dict[str, Any]] = []
    for proc in procs:
        df_list.append(attr.asdict(proc))
        if proc.children:
            df_list.extend(_procs_to_df(proc.children))
    return df_list


# Try to Match up 'command_executions' commandline data with
# process_df.
def _try_match_commandlines(
    command_executions: list,
    procs_cmds: pd.DataFrame,
) -> pd.DataFrame:
    """Return DF with matched commandlines."""
    procs_cmd: pd.DataFrame = procs_cmds.copy()
    procs_cmd["cmd_line"] = np.nan
    procs_cmd["cmd_line"] = procs_cmd["cmd_line"].astype(object)  # Set dtype to object
    weak_matches = 0
    for cmd in command_executions:
        for idx, row in procs_cmd.iterrows():
            if (
                not isinstance(row["cmd_line"], str)
                and np.isnan(row["cmd_line"])
                and row["name"] in cmd
            ):
                procs_cmd.loc[idx, "cmd_line"] = cmd  # type: ignore[reportCallIssue]
                break
    for cmd in command_executions:
        for idx, row in procs_cmd.iterrows():
            if (
                not isinstance(row["cmd_line"], str)
                and np.isnan(row["cmd_line"])
                and Path(row["name"]).stem.lower() in cmd.lower()
            ):
                weak_matches += 1
                procs_cmd.loc[idx, "cmd_line"] = cmd  # type: ignore[reportCallIssue]
                break

    if weak_matches:
        logger.warning(
            "%s of the %d commandlines were weakly matched - some commandlines may be attributed"
            "to the wrong instance of the process.",
            weak_matches,
            len(command_executions),
        )
    return procs_cmd


def _fill_missing_proc_tree_values(process_df: pd.DataFrame) -> pd.DataFrame:
    # Define a schema to map Df names on to internal ProcSchema
    process_df["path"] = np.nan
    process_df["path"] = process_df["path"].astype(object)  # Set dtype to object
    process_df.loc[process_df.IsRoot, "path"] = pd.Series(
        process_df[process_df.IsRoot].index.astype("str"),
    ).apply(lambda x: x.zfill(5))

    # Fill in some required fields with placeholder data
    process_df["time_stamp"] = datetime.now(tz=timezone.utc)
    process_df["host"] = "sandbox"
    process_df["logon_id"] = "na"
    process_df["event_id"] = "na"
    process_df["source_index"] = pd.Series(process_df.index.astype("str")).apply(
        lambda x: x.zfill(5),
    )

    proc_tree: pd.DataFrame = process_df.set_index("proc_key")

    first_unique: np.ndarray[Any, np.dtype[np.bool_]] = proc_tree.index.duplicated()
    proc_tree = proc_tree[~first_unique]
    # msticpy function to build the tree
    return build_proc_tree(proc_tree)


# Process browser helper functions
def _extract_subcats(pattern: str, categs: set[str]) -> set:
    """Extract the category names matching `pattern`."""
    return {cat for cat in categs if re.match(pattern, cat)}


def _format_widget_data(data_item: list[dict[str, Any] | str] | None) -> str:
    if not data_item:
        return ""
    if isinstance(data_item, list):
        if isinstance(data_item[0], dict):
            return pd.DataFrame(data_item).style.hide(axis="index").to_html()
        if isinstance(data_item[0], str):
            return pd.DataFrame(pd.Series(data_item)).style.hide(axis="index").to_html()
    return f"<pre>{pformat(data_item)}</pre>"
