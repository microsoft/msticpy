# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Process Tree Builder module for Process Tree Visualization."""
from typing import Any, Dict, Optional, Union

import attr
import pandas as pd

from . import proc_tree_build_winlx as winlx
from . import proc_tree_build_mde as mde
from .process_tree_utils import get_summary_info
from .._version import VERSION
from ..common.exceptions import MsticpyUserError

__version__ = VERSION
__author__ = "Ian Hellen"


class ProcessTreeSchemaException(MsticpyUserError):
    """Custom exception for Process Tree schema."""

    DEF_HELP_URI = (
        "MSTICPy Process Tree documentation",
        "https://msticpy.readthedocs.io/en/latest/visualization/ProcessTree.html",
    )


@attr.s(auto_attribs=True)
class ProcSchema:
    """
    Property name lookup for Process event schema.

    Each property maps a generic column name on to the
    schema of the input data. Most of these are mandatory,
    some are optional - not supplying them may result in
    a less complete tree.
    The `time_stamp` column should be supplied although
    defaults to 'TimeGenerated'.

    """

    process_name: str
    process_id: str
    parent_id: str
    logon_id: str
    cmd_line: str
    user_name: str
    path_separator: str
    host_name_column: str
    time_stamp: str = "TimeGenerated"
    parent_name: Optional[str] = None
    target_logon_id: Optional[str] = None
    user_id: Optional[str] = None
    event_id_column: Optional[str] = None
    event_id_identifier: Optional[Any] = None

    def __eq__(self, other):
        if not isinstance(other, ProcSchema):
            return False
        self_dict = attr.asdict(self)

        return not any(
            value and value != self_dict[field]
            for field, value in attr.asdict(other).items()
        )

    @property
    def required_columns(self):
        """Return columns required for Init."""
        return [
            "process_name",
            "process_id",
            "parent_id",
            "logon_id",
            "cmd_line",
            "user_name",
            "path_separator",
            "host_name_column",
            "time_stamp",
        ]

    @property
    def column_map(self) -> Dict[str, str]:
        """Return a dictionary that maps fields to schema names."""
        return {
            prop: str(col)
            for prop, col in attr.asdict(self).items()
            if prop not in {"path_separator", "event_id_identifier"}
        }

    @property
    def columns(self):
        """Return list of columns in schema data source."""
        return [
            col
            for prop, col in attr.asdict(self).items()
            if prop not in {"path_separator", "event_id_identifier"}
        ]

    def get_df_cols(self, data: pd.DataFrame):
        """Return the subset of columns that are present in `data`."""
        return [col for col in self.columns if col in data.columns]

    @property
    def event_type_col(self) -> str:
        """
        Return the column name containing the event identifier.

        Returns
        -------
        str
            The name of the event ID column.

        Raises
        ------
        ProcessTreeSchemaException
            If the schema is not known.

        """
        if self.event_id_column:
            return self.event_id_column
        raise ProcessTreeSchemaException(
            "Unknown schema - there is no value for the 'event_id' column."
        )

    @property
    def event_filter(self) -> Any:
        """
        Return the event type/ID to process for the current schema.

        Returns
        -------
        Any
            The value of the event ID to process.

        Raises
        ------
        ProcessTreeSchemaException
            If the schema is not known.

        """
        if self.event_id_identifier:
            return self.event_id_identifier
        raise ProcessTreeSchemaException(
            "Unknown schema - there is no value for the 'event_id_identifier' in the schema."
        )


WIN_EVENT_SCH = ProcSchema(
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
    event_id_column="EventID",
    event_id_identifier=4688,
    host_name_column="Computer",
)

LX_EVENT_SCH = ProcSchema(
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
    event_id_column="EventType",
    event_id_identifier="SYSCALL_EXECVE",
    host_name_column="Computer",
)

MDE_EVENT_SCH = ProcSchema(
    time_stamp="CreatedProcessCreationTime",
    process_name="CreatedProcessName",
    process_id="CreatedProcessId",
    parent_name="ParentProcessName",
    parent_id="CreatedProcessParentId",
    logon_id="InitiatingProcessLogonId",
    target_logon_id="LogonId",
    cmd_line="CreatedProcessCommandLine",
    user_name="CreatedProcessAccountName",
    path_separator="\\",
    user_id="CreatedProcessAccountSid",
    host_name_column="ComputerDnsName",
)

SUPPORTED_SCHEMAS = (WIN_EVENT_SCH, LX_EVENT_SCH, MDE_EVENT_SCH)


def build_process_tree(
    procs: pd.DataFrame,
    schema: Union[ProcSchema, Dict[str, Any]] = None,
    show_summary: bool = False,
    debug: bool = False,
) -> pd.DataFrame:
    """
    Build process trees from the process events.

    Parameters
    ----------
    procs : pd.DataFrame
        Process events (Windows 4688 or Linux Auditd)
    schema : Union[ProcSchema, Dict[str, Any]], optional
        The column schema to use, by default None.
        If supplied as a dict it must include definitions for the
        required fields in the ProcSchema class
        If None, then the schema is inferred
    show_summary : bool
        Shows summary of the built tree, default is False.
    debug : bool
        If True produces extra debugging output,
        by default False

    Returns
    -------
    pd.DataFrame
        Process tree dataframe.

    See Also
    --------
    ProcSchema

    """
    # If schema is none, infer schema from columns
    if not schema:
        schema = infer_schema(procs)
    if isinstance(schema, dict):
        schema = ProcSchema(**schema)

    if not schema:
        raise TypeError(
            "No matching schema for input data found.",
            "Please create a schema definition for the process data and",
            "pass it as the 'schema' parameter to this function.",
        )

    if schema in (WIN_EVENT_SCH, LX_EVENT_SCH):
        extr_proc_tree = winlx.extract_process_tree(procs, schema=schema, debug=debug)
    elif schema == MDE_EVENT_SCH:
        extr_proc_tree = mde.extract_process_tree(procs, debug=debug)
    merged_procs_keys = _add_tree_properties(extr_proc_tree)

    # Build process paths
    proc_tree = _build_proc_tree(merged_procs_keys)

    if show_summary:
        print(get_summary_info(proc_tree))
    return proc_tree


def infer_schema(data: Union[pd.DataFrame, pd.Series]) -> Optional[ProcSchema]:
    """
    Infer the correct schema to use for this data set.

    Parameters
    ----------
    data : Union[pd.DataFrame, pd.Series]
        Data set to test

    Returns
    -------
    ProcSchema
        The schema most closely matching the data set.

    """
    src_cols = data.columns if isinstance(data, pd.DataFrame) else data.index
    schema_matches = {}
    for schema in SUPPORTED_SCHEMAS:
        matching_cols = set(src_cols) & set(schema.columns)
        schema_matches[len(matching_cols)] = schema
    if max(schema_matches) > 5:
        return schema_matches[max(schema_matches)]
    return None


def _add_tree_properties(proc_tree):
    """Add root, branch, leaf properties and set proc_key as index."""
    proc_tree = proc_tree.copy()
    # Create labels based on node type
    ppids = proc_tree[["parent_key"]].set_index("parent_key")
    proc_tree = proc_tree.assign(IsRoot=False, IsLeaf=False, IsBranch=False)

    is_root = proc_tree["parent_key"].isna()
    has_child = proc_tree["proc_key"].isin(ppids.index)
    proc_tree.loc[is_root, "IsRoot"] = True
    proc_tree.loc[~has_child, "IsLeaf"] = True
    proc_tree.loc[~is_root & has_child, "IsBranch"] = True

    # Save the current numeric index as "source_index" converting to string
    proc_tree["source_index"] = proc_tree.index.astype(str)
    # Set the index of the output frame to be the proc_key
    proc_tree = proc_tree.set_index("proc_key")

    first_unique = proc_tree.index.duplicated()
    proc_tree = proc_tree[~first_unique]
    return proc_tree


def _build_proc_tree(input_tree: pd.DataFrame, max_depth=-1) -> pd.DataFrame:
    """Build process tree paths."""
    # set default path == current process ID
    input_tree["path"] = input_tree["source_index"]

    cur_level = input_tree[input_tree["IsRoot"]]
    remaining_procs = input_tree[~input_tree["IsRoot"]]

    cur_level_num = 0
    while True:
        sel_crit = remaining_procs["parent_key"].isin(cur_level.index)
        next_level = remaining_procs[sel_crit].copy()
        remaining_procs = remaining_procs[~sel_crit]

        if next_level.empty:
            break
        if max_depth != -1 and cur_level_num >= max_depth:
            print(f"max path depth reached: {cur_level_num}")
            break

        # merge next level with current level
        tmp_df = next_level.merge(
            cur_level[["source_index", "path"]],
            how="inner",
            left_on="parent_key",
            right_index=True,
        )

        # Build the path of these processes
        # = parent_path + child source_index
        next_level.loc[tmp_df.index, "path"] = (
            tmp_df["path_y"] + "/" + tmp_df["source_index_x"]
        )
        input_tree.loc[next_level.index, "path"] = next_level["path"]
        input_tree.loc[tmp_df.index, "parent_index"] = tmp_df["source_index_y"]

        cur_level = next_level
        cur_level_num += 1

    return input_tree
