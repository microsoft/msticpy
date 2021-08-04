# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Process Tree builder for Windows security and Linux auditd events."""
import pandas as pd

from .._version import VERSION


__version__ = VERSION
__author__ = "Ian Hellen"


TS_FMT_STRING = "%Y-%m-%d %H:%M:%S.%f"
LX_INT_TYPES = ["argc", "egid", "euid", "gid", "auid", "ppid", "pid", "ses", "uid"]


def extract_process_tree(
    procs: pd.DataFrame,
    schema: "ProcSchema",  # type: ignore  # noqa: F821
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
    # Clean data
    procs_cln = _clean_proc_data(procs, schema)

    # Merge parent-child
    merged_procs = _merge_parent_by_time(procs_cln, schema)
    if debug:
        _check_merge_status(procs_cln, merged_procs, schema)

    # extract inferred parents
    merged_procs_par = _extract_inferred_parents(merged_procs, schema)
    if debug:
        _check_inferred_parents(merged_procs, merged_procs_par)

    # Create Process and parent Keys
    _assign_proc_key(
        merged_procs_par,
        "proc_key",
        "new_process_lc",
        schema.process_id,
        schema.time_stamp,
    )
    _assign_proc_key(
        merged_procs_par,
        "parent_key",
        "parent_proc_lc",
        schema.parent_id,
        "TimeGenerated_orig_par",
    )
    return merged_procs_par


def _clean_proc_data(
    procs: pd.DataFrame,
    schema: "ProcSchema",  # type: ignore  # noqa: F821
) -> pd.DataFrame:
    """Return cleaned process data."""
    procs_cln = (
        procs.drop_duplicates().sort_values(schema.time_stamp, ascending=True).copy()
    )

    # Filter out any non-process events
    if schema.event_type_col and schema.event_id_identifier:
        event_type_filter = procs_cln[schema.event_type_col] == schema.event_filter
        procs_cln = procs_cln[event_type_filter]

    # Change Linux int cols to force int then to string types
    type_chng_int_dict = {
        col: "int" for col in LX_INT_TYPES if col in procs_cln.columns
    }
    if type_chng_int_dict:
        procs_cln = procs_cln.astype(type_chng_int_dict)
        type_chng_str_dict = {
            col: "str" for col in LX_INT_TYPES if col in procs_cln.columns
        }
        procs_cln = procs_cln.astype(type_chng_str_dict)

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


def _merge_parent_by_time(
    procs: pd.DataFrame,
    schema: "ProcSchema",  # type: ignore  # noqa: F821
) -> pd.DataFrame:
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
    merged_procs: pd.DataFrame, schema: "ProcSchema"  # type: ignore  # noqa: F821
) -> pd.DataFrame:
    """Find any inferred parents and creates rows for them."""
    tz_aware = merged_procs.iloc[0][schema.time_stamp].tz
    time_zero = pd.Timestamp(0) if tz_aware is None else pd.Timestamp(0, tz=0)

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
    merged_procs.loc[root_procs_crit, "TimeGenerated_orig_par"] = time_zero

    # Extract synthentic rows for the parents of root processes
    inferred_parents = (
        merged_procs[root_procs_crit][
            [
                schema.event_id_column,
                schema.host_name_column,
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
        .assign(TimeGenerated=time_zero, EffectiveLogonId=merged_procs[schema.logon_id])
        .drop_duplicates()
    )

    return pd.concat(
        [merged_procs, inferred_parents], ignore_index=True, axis=0, sort=False
    )


def _assign_proc_key(
    proc_data: pd.DataFrame,
    key_name: str,
    proc_name_col: str,
    proc_id_col: str,
    timestamp_col: str,
):
    """Create process and parent keys for unambiguous par-child relation."""
    proc_data[key_name] = (
        proc_data[proc_name_col]
        + "|"
        + proc_data[proc_id_col].astype(str)
        + "|"
        + proc_data[timestamp_col].dt.round("10us").dt.strftime(TS_FMT_STRING)
    )


# Diagnostic/debug functions
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
    c2a = None
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
    if schema.target_logon_id and c2a is not None:
        print("effectivelogonId in targetlogonId", len(merged_procs_par[c2a]))
    print("parent_proc_lc in procs", len(merged_procs_par[crit3]))
    print("ProcessId in ParentProcessId", len(merged_procs_par[crit4]))
    print("Parent_key in proc_key", len(merged_procs_par[crit5]))
    print("Parent_key not in proc_key", len(merged_procs_par[~crit5]))
    print("Parent_key is NA", len(merged_procs_par[crit6]))
