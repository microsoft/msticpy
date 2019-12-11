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

import numpy as np
import pandas as pd

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


def build_process_tree(
    procs: pd.DataFrame, schema: ProcSchema = None
) -> Tuple[pd.DataFrame, ProcSchema]:
    # If schema is none, infer schema from columns
    # Use attr to get the keys for the schema.

    procs_cln = _clean_proc_data(procs, schema)
    merged_procs = _merge_parent_by_time(procs_cln, schema)
    _check_merge_status(procs_cln, merged_procs, schema)

    merged_procs_par = _extract_inferred_parents(merged_procs, schema)
    _check_inferred_parents(merged_procs, merged_procs_par)

    merged_procs_keys = _assign_proc_keys(merged_procs_par, schema)
    _check_proc_keys(merged_procs_keys, schema)

    return _build_proc_tree(merged_procs_keys), schema


def _clean_proc_data(procs: pd.DataFrame, schema: ProcSchema) -> pd.DataFrame:
    """Return cleaned process data."""
    procs_cln = (
        procs.drop_duplicates().sort_values(schema.time_stamp, ascending=True).copy()
    )

    # Change Linux int cols to Object/string types
    type_chng_dict = {
        col: c_type for col, c_type in LX_TYPE_DICT.items() if col in procs.columns
    }
    if type_chng_dict:
        procs_cln = procs_cln.astype(type_chng_dict)
    if "EventID" not in procs_cln.columns and "EventType" in procs_cln.columns:
        procs_cln = procs_cln.rename(columns={"EventType": "EventID"})

    procs_cln["EffectiveLogonId"] = procs_cln[schema.logon_id]
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
    if schema.parent_name:
        par_join_cols = [schema.process_id, "new_process_lc"]
        child_join_cols = [schema.parent_id, "parent_proc_lc"]
    else:
        par_join_cols = [schema.process_id]
        child_join_cols = [schema.parent_id]
    return pd.merge_asof(
        left=procs.sort_values(schema.time_stamp, ascending=True),
        right=parent_procs,
        on=schema.time_stamp,
        left_by=child_join_cols,
        right_by=par_join_cols,
        suffixes=("", "_par"),
    )


def _check_merge_status(procs, merged_procs, schema):
    orig_cols = [col for col in merged_procs.columns if not col.endswith("_par")]
    orig_cols
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
        "Unique merged_procs2 index in merge",
        len(merged_procs["source_index"].unique()),
    )
    print("These two should add up to top line")
    row_dups = len(rows_with_dups2)
    print("Rows with dups", row_dups)
    row_nodups = len(merged_procs[~merged_procs["source_index"].isin(rows_with_dups2)])
    print("Rows with no dups", row_nodups)
    print(row_dups, "+", row_nodups, "=", row_dups + row_nodups)


def _extract_inferred_parents(
    merged_procs2: pd.DataFrame, schema: ProcSchema
) -> pd.DataFrame:
    # Fill in missing values for root processes
    root_procs_crit = merged_procs2["source_index_par"].isna()
    merged_procs2.loc[root_procs_crit, "NewProcessId_par"] = merged_procs2[
        schema.parent_id
    ]
    if schema.parent_name:
        merged_procs2.loc[root_procs_crit, "new_process_lc_par"] = merged_procs2[
            "parent_proc_lc"
        ]
    else:
        merged_procs2.loc[root_procs_crit, "new_process_lc_par"] = "unknown"
        merged_procs2.loc[root_procs_crit, f"{schema.process_name}_par"] = "unknown"
        # If the schema doesn't have a ParentProcessName/parent_proc_lc - copy this value
        # from the merged data for ALL processes
        merged_procs2["ParentProcessName"] = merged_procs2[f"{schema.process_name}_par"]
        merged_procs2["parent_proc_lc"] = merged_procs2["new_process_lc_par"]
    merged_procs2.loc[root_procs_crit, "EffectiveLogonId_par"] = merged_procs2[
        schema.logon_id
    ]
    merged_procs2.loc[root_procs_crit, "TimeGenerated_orig_par"] = pd.Timestamp(0)

    # Extract synthentic rows for the parents of root processes
    inferred_parents = (
        merged_procs2[root_procs_crit][
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
            EffectiveLogonId=merged_procs2[schema.logon_id],
        )
        .drop_duplicates()
    )

    plus_parents = pd.concat(
        [merged_procs2, inferred_parents], ignore_index=True, axis=0, sort=False
    )
    return plus_parents


def _check_inferred_parents(procs, procs_par):
    print(
        "original:",
        len(procs),
        "inferred_parents",
        len(procs_par) - len(procs),
        "combined",
        len(procs_par),
    )


def _assign_proc_keys(
    merged_procs_par: pd.DataFrame, schema: ProcSchema
) -> pd.DataFrame:
    fmt_string = "%Y-%m-%d %H:%M:%S.%f"
    # Create Process Key
    merged_procs_par["proc_key"] = (
        merged_procs_par["new_process_lc"]
        + merged_procs_par[schema.process_id].astype(str)
        + merged_procs_par[schema.time_stamp].dt.round("10us").dt.strftime(fmt_string)
    )
    # Create Parent Key
    merged_procs_par["parent_key"] = (
        merged_procs_par["parent_proc_lc"]
        + merged_procs_par[schema.parent_id].astype(str)
        + merged_procs_par["TimeGenerated_orig_par"]
        .dt.round("10us")
        .dt.strftime(fmt_string)
    )
    proc_tree = merged_procs_par.copy()
    # Create labels based on node type
    pids = proc_tree[["proc_key"]].set_index("proc_key")
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
    return proc_tree


def _check_proc_keys(merged_procs_par, schema):
    c1 = merged_procs_par["TimeGenerated_orig_par"].isin(
        merged_procs_par[schema.time_stamp]
    )
    c2 = merged_procs_par["EffectiveLogonId"].isin(merged_procs_par[schema.logon_id])
    if schema.target_logon_id:
        c2a = merged_procs_par["EffectiveLogonId"].isin(
            merged_procs_par[schema.target_logon_id]
        )
    c3 = merged_procs_par["parent_proc_lc"].isin(merged_procs_par["new_process_lc"])
    c4 = merged_procs_par[schema.process_id].isin(merged_procs_par[schema.parent_id])
    c5 = merged_procs_par["parent_key"].isin(merged_procs_par.index)
    c6 = merged_procs_par["parent_key"].isna()
    print("has parent time", len(merged_procs_par[c1]))
    print("effectivelogonId in subjectlogonId", len(merged_procs_par[c2]))
    if schema.target_logon_id:
        print("effectivelogonId in targetlogonId", len(merged_procs_par[c2a]))
    print("parent_proc_lc in procs", len(merged_procs_par[c3]))
    print("ProcessId in ParentProcessId", len(merged_procs_par[c4]))
    print("Parent_key in proc_key", len(merged_procs_par[c5]))
    print("Parent_key not in proc_key", len(merged_procs_par[~c5]))
    print("Parent_key is NA", len(merged_procs_par[c6]))


def _build_proc_tree(input_tree, debug=False, feedback=True, max_depth=-1):
    std_cols = ["parent_key", "source_index", "path"]
    mrg_cols = ["parent_key", "source_index_x", "source_index_y", "path_x", "path_y"]

    # UI elements
    progress = wgt.IntProgress(
        value=0,
        max=len(input_tree),
        step=1,
        description="Events:",
        bar_style="info",  # 'success', 'info', 'warning', 'danger' or ''
        orientation="horizontal",
    )
    done_label = wgt.Label(value="0")
    total_label = wgt.Label(value=f"/{len(input_tree)}")
    display(wgt.HBox([progress, done_label, total_label]))

    if debug:
        print("input_tree")
        display(input_tree.head())
    # set default path == current process ID
    input_tree["path"] = input_tree["source_index"]

    cur_level = input_tree[input_tree["IsRoot"] == True]

    cur_level_num = 0
    l_counts = {}
    visited_procs = set()
    visited_procs.update(cur_level.index)
    progress.value = progress.value + len(cur_level)
    done_label.value = str(progress.value)
    while True:
        l_counts[cur_level_num] = len(cur_level)
        sel_crit = input_tree["parent_key"].isin(cur_level.index)
        next_level = input_tree[sel_crit].copy()

        if next_level.empty:
            print(f"max path depth reached: {cur_level_num}")
            break
        if max_depth != -1 and cur_level_num >= max_depth:
            print(f"max path depth reached: {cur_level_num}")
            print(
                f"processed {progress.value} of {progress.max} for specified depth of {max_depth}"
            )
            progress.value = progress.max
            break
        tmp_df = next_level.merge(
            cur_level[["source_index", "path"]],
            how="inner",
            left_on="parent_key",
            right_index=True,
        )

        if debug:
            print(f"level {cur_level_num}, processes: {l_counts[cur_level_num]}")
            new_procs = set()
            new_procs.update(next_level.index)
            # assert that we're not revisiting a process
            assert not (visited_procs & new_procs)
            visited_procs.update(next_level.index)
            print("cur_level")
            display(cur_level[std_cols].head())
            print("next_level")
            display(next_level[std_cols].head())
            print("tmp_df")
            display(tmp_df[mrg_cols].head())
        next_level.loc[tmp_df.index, "path"] = (
            tmp_df["path_y"] + "/" + tmp_df["source_index_x"]
        )
        input_tree.loc[next_level.index, "path"] = next_level["path"]
        input_tree.loc[tmp_df.index, "parent_index"] = tmp_df["source_index_y"]
        if debug:
            print("input_tree")
            display(input_tree[std_cols].head())
            print(
                f"level: {cur_level_num}: parents: {len(cur_level)}, children: {len(next_level)}"
            )
        cur_level = next_level
        cur_level_num += 1
        progress.value = progress.value + len(cur_level)
        done_label.value = str(progress.value)
        if debug:
            ans = input("continue")
            if ans != "y":
                break

    # Verify path lengths
    if debug:
        for i in l_counts:
            print(
                f"pathlen {i+1}:",
                len(input_tree[input_tree["path"].str.count("/") == i]),
            )
            assert l_counts[i] == len(
                input_tree[input_tree["path"].str.count("/") == i]
            )
    return input_tree


def get_proc_key(procs, source_index) -> str:
    return procs[procs["source_index"] == source_index].iloc[0].name


def get_process(procs, source) -> pd.Series:
    if isinstance(source, str):
        return procs.loc[source]
    elif isinstance(source, pd.Series):
        return source
    else:
        raise ValueError("Unknown type for source parameter.")


def get_parent(procs, source) -> pd.Series:
    proc = get_process(procs, source)
    if proc.parent_key in procs.index:
        return procs.loc[proc.parent_key]
    else:
        print("not found")


def get_root(procs, source) -> pd.Series:
    proc = get_process(procs, source)
    p_path = proc.path.split("/")
    root_proc = procs[procs["source_index"] == p_path[0]]
    return root_proc.iloc[0]


def get_root_tree(procs, source) -> pd.DataFrame:
    proc = get_process(procs, source)
    p_path = proc.path.split("/")
    return procs[procs["path"].str.startswith(p_path[0])]


def get_children(procs, source, include_source=True) -> pd.DataFrame:
    proc = get_process(procs, source)
    children = procs[procs["parent_key"] == proc.name]
    if include_source:
        return children.append(proc)
    return children


def get_descendents(procs, source, include_source=True, max_levels=-1) -> pd.DataFrame:
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


def get_ancestors(procs, source, include_source=True) -> pd.DataFrame:
    proc = get_process(procs, source)
    p_path = proc.path.split("/")
    if not include_source:
        p_path.remove(proc.source_index)
    return procs[procs["source_index"].isin(p_path)].sort_values("path")


def get_siblings(procs, source, include_source=True) -> pd.DataFrame:
    parent = get_parent(procs, source)
    siblings = get_children(procs, parent, include_source=False)
    if not include_source:
        return siblings.loc[~source]
    return siblings
