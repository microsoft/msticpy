# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Process Tree Visualization."""
from typing import Optional, Dict, Iterable, Union, Any

import attr
import pandas as pd

from ..nbtools.nbwidgets import Progress
from ..nbtools.utility import MsticpyException
from .._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"


class ProcessTreeSchemaException(MsticpyException):
    """Custom exception for Process Tree schema."""


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
    user_id: Optional[str] = None

    @property
    def column_map(self) -> Dict[str, str]:
        """Return a dictionary that maps fields to schema names."""
        return {key: str(val) for key, val in attr.asdict(self).items()}

    @property
    def columns(self) -> Iterable[str]:
        """Return an interable of target column names."""
        return [str(val) for val in attr.asdict(self).values() if val]

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
        if self.process_name == "NewProcessName":
            return "EventID"
        if self.process_name == "exe":
            return "EventType"
        raise ProcessTreeSchemaException("Unknown schema.")

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
        if self.process_name == "NewProcessName":
            return 4688
        if self.process_name == "exe":
            return "SYSCALL_EXECVE"
        raise ProcessTreeSchemaException("Unknown schema.")


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
)

LX_INT_TYPES = ["argc", "egid", "euid", "gid", "auid", "ppid", "pid", "ses", "uid"]


TS_FMT_STRING = "%Y-%m-%d %H:%M:%S.%f"


def build_process_tree(
    procs: pd.DataFrame,
    schema: ProcSchema = None,
    show_progress: bool = False,
    debug: bool = False,
) -> pd.DataFrame:
    """
    Build process trees from the process events.

    Parameters
    ----------
    procs : pd.DataFrame
        Process events (Windows 4688 or Linux Auditd)
    schema : ProcSchema, optional
        The column schema to use, by default None
        If None, then the schema is inferred
    show_progress : bool
        Shows the progress of the process (helpful for
        very large data sets)
    debug : bool
        If True produces extra debugging output,
        by default False

    Returns
    -------
    pd.DataFrame
        Process tree dataframe.

    """
    # If schema is none, infer schema from columns
    if not schema:
        schema = infer_schema(procs)

    data_len = len(procs)
    section_len = int(data_len / 4)
    progress_ui = Progress(completed_len=data_len * 2, visible=show_progress)

    # Clean data
    procs_cln = _clean_proc_data(procs, schema)
    progress_ui.update_progress(delta=section_len)

    # Merge parent-child
    merged_procs = _merge_parent_by_time(procs_cln, schema)
    if debug:
        _check_merge_status(procs_cln, merged_procs, schema)
    progress_ui.update_progress(delta=section_len)

    # extract inferred parents
    merged_procs_par = _extract_inferred_parents(merged_procs, schema)
    if debug:
        _check_inferred_parents(merged_procs, merged_procs_par)
    progress_ui.update_progress(delta=section_len)

    # create parent-child keys
    merged_procs_keys = _assign_proc_keys(merged_procs_par, schema)
    if debug:
        _check_proc_keys(merged_procs_keys, schema)
    progress_ui.update_progress(delta=section_len)

    # Build process paths
    proc_tree = _build_proc_tree(merged_procs_keys, progress_ui)

    if show_progress:
        print(get_summary_info(proc_tree))
    return proc_tree


def infer_schema(data: Union[pd.DataFrame, pd.Series]) -> ProcSchema:
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
    if isinstance(data, pd.DataFrame):
        src_cols = data.columns
    else:
        src_cols = data.index
    lx_match = set(src_cols) & set(LX_EVENT_SCH.columns)
    win_match = set(src_cols) & set(WIN_EVENT_SCH.columns)
    return LX_EVENT_SCH if len(lx_match) > len(win_match) else WIN_EVENT_SCH


def _clean_proc_data(procs: pd.DataFrame, schema: ProcSchema) -> pd.DataFrame:
    """Return cleaned process data."""
    procs_cln = (
        procs.drop_duplicates().sort_values(schema.time_stamp, ascending=True).copy()
    )

    # Filter out any non-process events
    event_type_filter = procs_cln[schema.event_type_col] == schema.event_filter
    procs_cln = procs_cln[event_type_filter]

    # Change Linux int cols to force int then to string types
    type_chng_int_dict = {col: "int" for col in LX_INT_TYPES if col in procs.columns}
    if type_chng_int_dict:
        procs_cln = procs_cln.astype(type_chng_int_dict)
        type_chng_str_dict = {
            col: "str" for col in LX_INT_TYPES if col in procs.columns
        }
        procs_cln = procs_cln.astype(type_chng_str_dict)
    if "EventID" not in procs_cln.columns and "EventType" in procs_cln.columns:
        procs_cln = procs_cln.rename(columns={"EventType": "EventID"})

    procs_cln["EffectiveLogonId"] = procs_cln[schema.logon_id]
    # Create effective logon Id for Windows, if the TargetLogonId is not 0x0
    if schema.target_logon_id:
        has_tgt_logonid = (procs_cln[schema.target_logon_id] != "0x0") & (
            procs_cln[schema.target_logon_id].notna()
        )
        procs_cln.loc[has_tgt_logonid, "EffectiveLogonId"] = procs_cln[
            schema.target_logon_id
        ]
    procs_cln["new_process_lc"] = procs_cln[schema.process_name].str.lower()
    if schema.parent_name:
        no_pproc = procs_cln[schema.parent_name] == ""
        procs_cln.loc[no_pproc, schema.parent_name] = "unknown"
        procs_cln["parent_proc_lc"] = procs_cln[schema.parent_name].str.lower()
    procs_cln["source_index"] = procs_cln.index
    return procs_cln


def _merge_parent_by_time(procs: pd.DataFrame, schema: ProcSchema) -> pd.DataFrame:
    """Merge procs with parents using merge_asof."""
    parent_procs = (
        procs[
            [
                schema.process_id,
                "EffectiveLogonId",
                "new_process_lc",
                "source_index",
                schema.parent_id,
                schema.time_stamp,
                schema.process_name,
            ]
        ]
        .assign(TimeGenerated_orig_par=procs[schema.time_stamp])
        .sort_values(schema.time_stamp, ascending=True)
    )
    # if we have a parent name (Windows) - use that as part of the
    # match
    if schema.parent_name:
        par_join_cols = [schema.process_id, "new_process_lc"]
        child_join_cols = [schema.parent_id, "parent_proc_lc"]
    else:
        par_join_cols = [schema.process_id]
        child_join_cols = [schema.parent_id]
    # merge_asof merges on the "by" fields and then the closest time
    # match in the time_stamp field. The default is to look backwards
    # for a match on the right of the join (parent) that is a time earlier
    # than the corresponding row on the left.
    return pd.merge_asof(
        left=procs.sort_values(schema.time_stamp, ascending=True),
        right=parent_procs,
        on=schema.time_stamp,
        left_by=child_join_cols,
        right_by=par_join_cols,
        suffixes=("", "_par"),
    )


def _extract_inferred_parents(
    merged_procs: pd.DataFrame, schema: ProcSchema
) -> pd.DataFrame:
    """Find any inferred parents and creates rows for them."""
    # Fill in missing values for root processes
    root_procs_crit = merged_procs["source_index_par"].isna()
    merged_procs.loc[root_procs_crit, "NewProcessId_par"] = merged_procs[
        schema.parent_id
    ]
    if schema.parent_name:
        merged_procs.loc[root_procs_crit, "new_process_lc_par"] = merged_procs[
            "parent_proc_lc"
        ]
    else:
        merged_procs.loc[root_procs_crit, "new_process_lc_par"] = "unknown"
        merged_procs.loc[root_procs_crit, f"{schema.process_name}_par"] = "unknown"
        # If the schema doesn't have a ParentProcessName/parent_proc_lc - copy this value
        # from the merged data for ALL processes
        merged_procs["ParentProcessName"] = merged_procs[f"{schema.process_name}_par"]
        merged_procs["parent_proc_lc"] = merged_procs["new_process_lc_par"]
    merged_procs.loc[root_procs_crit, "EffectiveLogonId_par"] = merged_procs[
        schema.logon_id
    ]
    merged_procs.loc[root_procs_crit, "TimeGenerated_orig_par"] = pd.Timestamp(0)

    # Extract synthentic rows for the parents of root processes
    inferred_parents = (
        merged_procs[root_procs_crit][
            [
                "TenantId",
                "EventID",
                "Computer",
                schema.parent_id,
                "EffectiveLogonId_par",
                "ParentProcessName",
                "parent_proc_lc",
            ]
        ]
        .rename(
            columns={
                schema.parent_id: schema.process_id,
                "ParentProcessName": schema.process_name,
                "parent_proc_lc": "new_process_lc",
                "EffectiveLogonId_par": schema.logon_id,
            }
        )
        .assign(
            TimeGenerated=pd.Timestamp(0),
            EffectiveLogonId=merged_procs[schema.logon_id],
        )
        .drop_duplicates()
    )

    plus_parents = pd.concat(
        [merged_procs, inferred_parents], ignore_index=True, axis=0, sort=False
    )
    return plus_parents


def _assign_proc_keys(
    merged_procs_par: pd.DataFrame, schema: ProcSchema
) -> pd.DataFrame:
    """Create process and parent keys for unambiguous par-child relation."""
    # Create Process Key
    merged_procs_par["proc_key"] = (
        merged_procs_par["new_process_lc"]
        + merged_procs_par[schema.process_id].astype(str)
        + merged_procs_par[schema.time_stamp]
        .dt.round("10us")
        .dt.strftime(TS_FMT_STRING)
    )
    # Create Parent Key
    merged_procs_par["parent_key"] = (
        merged_procs_par["parent_proc_lc"]
        + merged_procs_par[schema.parent_id].astype(str)
        + merged_procs_par["TimeGenerated_orig_par"]
        .dt.round("10us")
        .dt.strftime(TS_FMT_STRING)
    )
    proc_tree = merged_procs_par.copy()
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


def _build_proc_tree(input_tree, progress: Progress, max_depth=-1):
    """Build process tree paths."""
    # set default path == current process ID
    input_tree["path"] = input_tree["source_index"]

    cur_level = input_tree[input_tree["IsRoot"]]

    cur_level_num = 0
    progress.update_progress(delta=len(cur_level))
    while True:
        sel_crit = input_tree["parent_key"].isin(cur_level.index)
        next_level = input_tree[sel_crit].copy()

        if next_level.empty:
            progress.update_progress(new_total=progress.max)
            break
        if max_depth != -1 and cur_level_num >= max_depth:
            print(f"max path depth reached: {cur_level_num}")
            print(
                f"processed {progress.value} of {progress.max} for specified depth of {max_depth}"
            )
            progress.update_progress(new_total=progress.max)
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
        progress.update_progress(delta=len(cur_level))

    return input_tree


def get_process_key(procs: pd.DataFrame, source_index: int) -> str:
    """
    Return the process key of the process given its source_index.

    Parameters
    ----------
    procs : pd.DataFrame
        Process events
    source_index : int, optional
        source_index of the process record

    Returns
    -------
    str
        The process key of the process.

    """
    return procs[procs["source_index"] == source_index].iloc[0].name


def build_process_key(source_proc: pd.Series, schema: ProcSchema = None) -> str:
    """
    Return a process key from a process event.

    Parameters
    ----------
    source_proc : pd.Series, optional
        Source process
    schema : ProcSchema, optional
        The data schema to use, by default None
        - if None the schema will be inferred

    Returns
    -------
    str
        Process key of the process

    """
    if schema is None:
        schema = infer_schema(source_proc)
    proc_path = source_proc[schema.process_name].lower()
    pid = source_proc[schema.process_id]
    tstamp = pd.to_datetime(source_proc[schema.time_stamp]).strftime(TS_FMT_STRING)
    return f"{proc_path}{pid}{tstamp}"


def get_roots(procs: pd.DataFrame) -> pd.DataFrame:
    """
    Return the process tree roots for the current data set.

    Parameters
    ----------
    procs : pd.DataFrame
        Process events (with process tree metadata)

    Returns
    -------
    pd.DataFrame
        Process Tree root processes

    """
    return procs[procs["IsRoot"]]


def get_process(procs: pd.DataFrame, source: Union[str, pd.Series]) -> pd.Series:
    """
    Return the process event as a Series.

    Parameters
    ----------
    procs : pd.DataFrame
        Process events (with process tree metadata)
    source : Union[str, pd.Series]
        source_index of process or the process row

    Returns
    -------
    pd.Series
        Process row

    Raises
    ------
    ValueError
        If unknown type is supplied as `source`

    """
    if isinstance(source, str):
        return procs.loc[source]
    if isinstance(source, pd.Series):
        return source
    raise ValueError("Unknown type for source parameter.")


def get_parent(
    procs: pd.DataFrame, source: Union[str, pd.Series]
) -> Optional[pd.Series]:
    """
    Return the parent of the source process.

    Parameters
    ----------
    procs : pd.DataFrame
        Process events (with process tree metadata)
    source : Union[str, pd.Series]
        source_index of process or the process row

    Returns
    -------
    Optional[pd.Series]
        Parent Process row or None if no parent was found.

    """
    proc = get_process(procs, source)
    if proc.parent_key in procs.index:
        return procs.loc[proc.parent_key]
    return None


def get_root(procs: pd.DataFrame, source: Union[str, pd.Series]) -> pd.Series:
    """
    Return the root process for the source process.

    Parameters
    ----------
    procs : pd.DataFrame
        Process events (with process tree metadata)
    source : Union[str, pd.Series]
        source_index of process or the process row

    Returns
    -------
    pd.Series
        Root process

    """
    proc = get_process(procs, source)
    p_path = proc.path.split("/")
    root_proc = procs[procs["source_index"] == p_path[0]]
    return root_proc.iloc[0]


def get_root_tree(procs: pd.DataFrame, source: Union[str, pd.Series]) -> pd.DataFrame:
    """
    Return the process tree to which the source process belongs.

    Parameters
    ----------
    procs : pd.DataFrame
        Process events (with process tree metadata)
    source : Union[str, pd.Series]
        source_index of process or the process row

    Returns
    -------
    pd.DataFrame
        Process Tree

    """
    proc = get_process(procs, source)
    p_path = proc.path.split("/")
    return procs[procs["path"].str.startswith(p_path[0])]


def get_tree_depth(procs: pd.DataFrame) -> int:
    """
    Return the depth of the process tree.

    Parameters
    ----------
    procs : pd.DataFrame
        Process events (with process tree metadata)

    Returns
    -------
    int
        Tree depth

    """
    return procs["path"].str.count("/").max() + 1


def get_children(
    procs: pd.DataFrame, source: Union[str, pd.Series], include_source: bool = True
) -> pd.DataFrame:
    """
    Return the child processes for the source process.

    Parameters
    ----------
    procs : pd.DataFrame
        Process events (with process tree metadata)
    source : Union[str, pd.Series]
        source_index of process or the process row
    include_source : bool, optional
        If True include the source process in the results, by default True

    Returns
    -------
    pd.DataFrame
        Child processes

    """
    proc = get_process(procs, source)
    children = procs[procs["parent_key"] == proc.name]
    if include_source:
        return children.append(proc)
    return children


def get_descendents(
    procs: pd.DataFrame,
    source: Union[str, pd.Series],
    include_source: bool = True,
    max_levels: int = -1,
) -> pd.DataFrame:
    """
    Return the descendents of the source process.

    Parameters
    ----------
    procs : pd.DataFrame
        Process events (with process tree metadata)
    source : Union[str, pd.Series]
        source_index of process or the process row
    include_source : bool, optional
        Include the source process in the results, by default True
    max_levels : int, optional
        Maximum number of levels to descend, by default -1 (all levels)

    Returns
    -------
    pd.DataFrame
        Descendent processes

    """
    proc = get_process(procs, source)
    descendents = []
    parent_keys = [proc.name]
    level = 0
    while max_levels == -1 or level < max_levels:
        children = procs[procs["parent_key"].isin(parent_keys)]
        if children.empty:
            break
        descendents.append(children)
        parent_keys = children.index
        level += 1

    if descendents:
        desc_procs = pd.concat(descendents)
    else:
        desc_procs = pd.DataFrame(columns=proc.index, index=None)
        desc_procs.index.name = "proc_key"
    if include_source:
        return desc_procs.append(proc).sort_values("path")
    return desc_procs.sort_values("path")


def get_ancestors(procs: pd.DataFrame, source, include_source=True) -> pd.DataFrame:
    """
    Return the ancestor processes of the source process.

    Parameters
    ----------
    procs : pd.DataFrame
        Process events (with process tree metadata)
    source : Union[str, pd.Series]
        source_index of process or the process row
    include_source : bool, optional
        Include the source process in the results, by default True

    Returns
    -------
    pd.DataFrame
        Ancestor processes

    """
    proc = get_process(procs, source)
    p_path = proc.path.split("/")
    if not include_source:
        p_path.remove(proc.source_index)
    return procs[procs["source_index"].isin(p_path)].sort_values("path")


def get_siblings(
    procs: pd.DataFrame, source: Union[str, pd.Series], include_source: bool = True
) -> pd.DataFrame:
    """
    Return the processes that share the parent of the source process.

    Parameters
    ----------
    procs : pd.DataFrame
        Process events (with process tree metadata)
    source : Union[str, pd.Series]
        source_index of process or the process row
    include_source : bool, optional
        Include the source process in the results, by default True

    Returns
    -------
    pd.DataFrame
        Sibling processes.

    """
    parent = get_parent(procs, source)
    proc = get_process(procs, source)
    siblings = get_children(procs, parent, include_source=False)
    if not include_source:
        return siblings[siblings.index != proc.name]
    return siblings


def get_summary_info(procs: pd.DataFrame) -> Dict[str, int]:
    """
    Return summary information about the process trees.

    Parameters
    ----------
    procs : pd.DataFrame
        Process events (with process tree metadata)

    Returns
    -------
    Dict[str, int]
        Summary statistic about the process tree

    """
    summary: Dict[str, Any] = {}
    summary["Processes"] = len(procs)
    summary["RootProcesses"] = len(procs[procs["IsRoot"]])
    summary["LeafProcesses"] = len(procs[procs["IsLeaf"]])
    summary["BranchProcesses"] = len(procs[procs["IsBranch"]])
    summary["IsolatedProcesses"] = len(procs[(procs["IsRoot"]) & (procs["IsLeaf"])])
    summary["LargestTreeDepth"] = procs["path"].str.count("/").max() + 1
    return summary


# Diagnostic functions
def _check_merge_status(procs, merged_procs, schema):
    """Diagnostic for _merge_parent_by_time."""
    orig_cols = [col for col in merged_procs.columns if not col.endswith("_par")]
    rows_with_dups2 = (
        merged_procs.dropna()
        .groupby(orig_cols)
        .count()
        .reset_index()
        .query(f"{schema.process_id}_par > 1")["source_index"]
    )

    # Check status
    print("Original # procs", len(procs))
    print("Merged # procs", len(merged_procs))
    print("Merged # procs - dropna", len(merged_procs.dropna()))

    print(
        "Unique merged_procs index in merge", len(merged_procs["source_index"].unique())
    )
    print("These two should add up to top line")
    row_dups = len(rows_with_dups2)
    print("Rows with dups", row_dups)
    row_nodups = len(merged_procs[~merged_procs["source_index"].isin(rows_with_dups2)])
    print("Rows with no dups", row_nodups)
    print(row_dups, "+", row_nodups, "=", row_dups + row_nodups)


def _check_inferred_parents(procs, procs_par):
    """Diagnostic for _extract_inferred_parents."""
    print(
        "original:",
        len(procs),
        "inferred_parents",
        len(procs_par) - len(procs),
        "combined",
        len(procs_par),
    )


def _check_proc_keys(merged_procs_par, schema):
    """Diagnostic for _assign_proc_keys."""
    crit1 = merged_procs_par["TimeGenerated_orig_par"].isin(
        merged_procs_par[schema.time_stamp]
    )
    crit2 = merged_procs_par["EffectiveLogonId"].isin(merged_procs_par[schema.logon_id])
    if schema.target_logon_id:
        c2a = merged_procs_par["EffectiveLogonId"].isin(
            merged_procs_par[schema.target_logon_id]
        )
    crit3 = merged_procs_par["parent_proc_lc"].isin(merged_procs_par["new_process_lc"])
    crit4 = merged_procs_par[schema.process_id].isin(merged_procs_par[schema.parent_id])
    crit5 = merged_procs_par["parent_key"].isin(merged_procs_par.index)
    crit6 = merged_procs_par["parent_key"].isna()
    print("has parent time", len(merged_procs_par[crit1]))
    print("effectivelogonId in subjectlogonId", len(merged_procs_par[crit2]))
    if schema.target_logon_id:
        print("effectivelogonId in targetlogonId", len(merged_procs_par[c2a]))
    print("parent_proc_lc in procs", len(merged_procs_par[crit3]))
    print("ProcessId in ParentProcessId", len(merged_procs_par[crit4]))
    print("Parent_key in proc_key", len(merged_procs_par[crit5]))
    print("Parent_key not in proc_key", len(merged_procs_par[~crit5]))
    print("Parent_key is NA", len(merged_procs_par[crit6]))
