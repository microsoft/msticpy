# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Process tree builder routines for MDE process data."""
from typing import Dict, Tuple

import numpy as np
import pandas as pd

from .._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"


_MDE_NON_STD_COL_MAP = {
    "InitiatingProcessImageCreationTimeUtc": "CreatedProcessFileCreationTime",
    "InitiatingProcessImageMd5": "CreatedProcessFileMd5",
    "InitiatingProcessImageFilePath": "CreatedProcessFilePath",
    "InitiatingProcessImageSha1": "CreatedProcessFileSha1",
    "InitiatingProcessImageSha256": "CreatedProcessFileSha256",
    "InitiatingProcessImageFileSizeInBytes": "CreatedProcessFileSize",
    "InitiatingProcessParentProcessId": "CreatedProcessParentId",
    "InitiatingProcessParentProcessName": "CreatedProcessParentName",
    "InitiatingProcessParentCreationTime": "CreatedProcessParentCreationTimeUtc",
}

TS_FMT_STRING = "%Y-%m-%d %H:%M:%S.%f"
PARENT_KEY = "parent_key"
PROC_KEY = "proc_key"


def extract_process_tree(data: pd.DataFrame, debug: bool = False) -> pd.DataFrame:
    """
    Build a process tree from raw MDE process logs.

    Parameters
    ----------
    data : pd.DataFrame
        DataFrame of process events.

    Returns
    -------
    pd.DataFrame
        Process tree DataFrame with child->parent keys and
        extracted parent processes from child data.

    """
    par_child_col_map = _get_par_child_col_mapping(data)
    inferred_parents = _extract_missing_parents(data, par_child_col_map, debug=debug)
    missing_par_uniq = _get_unique_parents(inferred_parents, debug)
    all_proc_par = pd.concat([data, missing_par_uniq], ignore_index=True)
    missing_gps = _extract_missing_gparents(all_proc_par)
    return pd.concat([all_proc_par, missing_gps], ignore_index=True, axis=0)


def _sort_df_by_time(data, column="CreatedProcessCreationTime"):
    """Return dataframe sorted by specified column and reset index."""
    return data.sort_values(column, ascending=True).reset_index().drop(columns="index")


def _add_proc_key(
    data: pd.DataFrame,
    key_name: str,
    proc_name_col: str,
    proc_id_col: str,
    timestamp_col: str,
):
    """
    Add process/parent key to data.

    Parameters
    ----------
    data : pd.DataFrame
        Input data
    key_name : str
        The name of the key to create
    proc_name_col : str
        Process name column
    proc_id_col : str
        Process ID column
    timestamp_col : str
        Process time stamp column.

    Notes
    -----
    This function adds the key column to the passed `data` dataframe.

    """
    data[key_name] = (
        data[proc_name_col].str.lower()
        + "|"
        + data[proc_id_col].astype(str).str.lower()
        + "|"
        + data[timestamp_col].dt.round("10us").dt.strftime(TS_FMT_STRING)
    )


def _extract_missing_parents(
    data: pd.DataFrame, col_mapping: Dict[str, str], debug: bool = False
) -> pd.DataFrame:
    """Return parent processes that are not in the created process set."""
    # save the source index
    data["src_index"] = data.index
    # add process key
    _add_proc_key(
        data,
        PROC_KEY,
        "CreatedProcessName",
        "CreatedProcessId",
        "CreatedProcessCreationTime",
    )

    # Create full process file path for initiating processes
    data["InitiatingProcessFileName"] = (
        data.InitiatingProcessImageFilePath + "\\" + data.InitiatingProcessName
    )
    # Create parent key
    _add_proc_key(
        data,
        PARENT_KEY,
        "InitiatingProcessName",
        "InitiatingProcessId",
        "InitiatingProcessCreationTime",
    )

    # Separate the InitiatingProcess columns from the DF
    parent_cols = set(data.filter(regex="Initiating.*").columns)
    non_par_cols = set(data.columns) - parent_cols
    non_par_cols.remove("src_index")
    # print(non_par_cols)

    # merge the original data with the parent rows
    merged_parents = data.filter(
        regex="Initiating.*|parent_key|src_index"
    ).merge(  # parents
        data.filter(non_par_cols),  # created_procs
        left_on=PARENT_KEY,
        right_on=PROC_KEY,
        suffixes=("_child", "_par"),
        how="left",
    )

    # Any NA results from the merge indicate that the parent process
    # is not itself in the list of processes
    missing_parents = merged_parents[merged_parents["CreatedProcessParentId"].isna()]
    missing_parents = (
        missing_parents.dropna(axis=1, how="all")
        .rename(columns=col_mapping)
        .rename(columns={"parent_key_child": PROC_KEY})
        .drop(columns=["InitiatingProcessFileName"])
    )
    missing_parents["CreatedProcessFilePath"] = (
        missing_parents.CreatedProcessFilePath
        + "\\"
        + missing_parents.CreatedProcessName
    )
    missing_parents = _sort_df_by_time(missing_parents)
    if debug:
        print("missing parent procs", len(missing_parents))
        # All successful merges indicate that the parent IS in the list of processes
        found_parents = merged_parents[~merged_parents["CreatedProcessParentId"].isna()]
        print("existing parent procs", len(found_parents))
        mpar_uniq_test = (
            missing_parents.drop(columns="src_index").groupby("proc_key").nunique()
        )
        assert mpar_uniq_test[mpar_uniq_test > 1].dropna(how="all").empty
    return missing_parents


# Get unique parents and add the parent key
def _get_unique_parents(data, debug=False):
    """Returns uniq set of processes."""
    if "src_index" in data.columns:
        data = data.drop(columns="src_index")
    missing_par_uniq = data.drop_duplicates().copy()
    missing_par_uniq["InitiatingProcessName"] = missing_par_uniq.apply(
        lambda row: row.CreatedProcessParentName.split("\\")[-1], axis=1
    )
    if debug:
        print("unique:", missing_par_uniq.shape, "original:", data.shape)
    _add_proc_key(
        missing_par_uniq,
        PARENT_KEY,
        "InitiatingProcessName",
        "CreatedProcessParentId",
        "CreatedProcessParentCreationTimeUtc",
    )
    return missing_par_uniq


def _split_file_path(
    input_path: str,
    path_col: str = "CreatedProcessFilePath",
    file_col: str = "CreatedProcessName",
    separator: str = "\\",
):
    """Splits file path in to folder/stem."""
    f_path = f_stem = np.nan
    try:
        f_path, _, f_stem = input_path.rpartition(separator)
    except AttributeError:
        pass
    return {path_col: f_path, file_col: f_stem}


def _extract_missing_gparents(data):
    """Returns grandparent processes for any procs not in Createdprocesses."""
    missing_gps = (
        data[~data.parent_key.isin(data.proc_key)]
        .filter(regex=".*Parent.*")
        .drop_duplicates()
    )
    missing_gps_file_split = missing_gps.apply(
        lambda proc: _split_file_path(proc.CreatedProcessParentName),
        axis=1,
        result_type="expand",
    )
    missing_gps = (
        missing_gps.join(missing_gps_file_split)
        .drop(
            columns=[
                "InitiatingProcessParentCreationTime",
                "InitiatingProcessParentProcessId",
                "InitiatingProcessParentProcessName",
                "CreatedProcessFilePath",
            ]
        )
        .rename(
            columns={
                "CreatedProcessParentCreationTimeUtc": "CreatedProcessCreationTime",
                "CreatedProcessParentName": "CreatedProcessFilePath",
                "CreatedProcessParentId": "CreatedProcessId",
            }
        )
    )
    _add_proc_key(
        missing_gps,
        "proc_key",
        "CreatedProcessName",
        "CreatedProcessId",
        "CreatedProcessCreationTime",
    )
    return missing_gps


def _get_par_child_col_mapping(data: pd.DataFrame) -> Dict[str, str]:
    """Returns a mapping between parent and child column names."""
    created_proc_cols = _remove_col_prefix(data, "Created")
    init_proc_cols = _remove_col_prefix(data, "Initiating")
    init_proc_col_mapping, _ = _map_columns(created_proc_cols, init_proc_cols)
    return {**init_proc_col_mapping, **_MDE_NON_STD_COL_MAP}


def _remove_col_prefix(data: pd.DataFrame, prefix: str) -> Dict[str, str]:
    """Returns a mapping of column stems and columns with `prefix`."""
    return {
        col.replace(prefix, ""): col for col in data.columns if col.startswith(prefix)
    }


def _map_columns(
    created_cols: Dict[str, str], init_cols: Dict[str, str]
) -> Tuple[Dict[str, str], Dict[str, str]]:
    """Returns Initiating -> Created column mapping."""
    col_mapping = {}
    unmapped = {}
    for col_stem, col in init_cols.items():
        if col_stem in created_cols:
            col_mapping[col] = created_cols[col_stem]
        else:
            unmapped[col_stem] = col
    return col_mapping, unmapped
