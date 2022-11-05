# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Process Tree Builder module for Process Tree Visualization."""
from typing import Any, Dict, Optional, Union

import pandas as pd

from .._version import VERSION
from . import proc_tree_build_mde as mde
from . import proc_tree_build_winlx as winlx

# pylint: disable=unused-import
from .proc_tree_schema import ProcSchema  # noqa: F401
from .proc_tree_schema import (  # noqa: F401
    LX_EVENT_SCH,
    MDE_EVENT_SCH,
    MDE_INT_EVENT_SCH,
    SUPPORTED_SCHEMAS,
    SYSMON_PROCESS_CREATE_EVENT_SCH,
    WIN_EVENT_SCH,
)
from .proc_tree_schema import ColNames as Col
from .process_tree_utils import get_summary_info

__version__ = VERSION
__author__ = "Ian Hellen"


def build_process_tree(
    procs: pd.DataFrame,
    schema: Union[ProcSchema, Dict[str, Any]] = None,
    show_summary: bool = False,
    debug: bool = False,
    **kwargs,
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
    if isinstance(schema, dict):
        schema = ProcSchema(**schema)
    # If schema is none, infer schema from columns
    if not schema or schema == MDE_INT_EVENT_SCH:
        # Special case for MDE - since there are two possible schemas
        schema = infer_schema(procs)

    if not schema:
        raise TypeError(
            "No matching schema for input data found.",
            "Please create a schema definition for the process data and",
            "pass it as the 'schema' parameter to this function.",
        )

    if schema == MDE_EVENT_SCH:
        procs = mde.convert_mde_schema_to_internal(
            procs, schema=MDE_EVENT_SCH, plot_args=kwargs.pop("plot_args", {})
        )
        schema = MDE_INT_EVENT_SCH
    if schema == MDE_INT_EVENT_SCH:
        extr_proc_tree = mde.extract_process_tree(procs, debug=debug)
    else:
        extr_proc_tree = winlx.extract_process_tree(procs, schema=schema, debug=debug)
    merged_procs_keys = _add_tree_properties(extr_proc_tree)

    # Build process paths
    proc_tree = build_proc_tree(merged_procs_keys)

    if show_summary:
        print(get_summary_info(proc_tree))
    return proc_tree.sort_values(by=["path", schema.time_stamp], ascending=True)


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
    schema_matches = {
        len(set(src_cols) & set(schema.columns)): schema for schema in SUPPORTED_SCHEMAS
    }

    if max(schema_matches) >= 4:
        return schema_matches[max(schema_matches)]
    return None


def _add_tree_properties(proc_tree):
    """Add root, branch, leaf properties and set proc_key as index."""
    proc_tree = proc_tree.copy()
    # Create labels based on node type
    ppids = proc_tree[[Col.parent_key]].set_index(Col.parent_key)
    proc_tree = proc_tree.assign(IsRoot=False, IsLeaf=False, IsBranch=False)

    is_root = proc_tree[Col.parent_key].isna()
    has_child = proc_tree[Col.proc_key].isin(ppids.index)
    proc_tree.loc[is_root, "IsRoot"] = True
    proc_tree.loc[~has_child, "IsLeaf"] = True
    proc_tree.loc[~is_root & has_child, "IsBranch"] = True

    # Save the current numeric index as "source_index" converting to string
    proc_tree[Col.source_index] = proc_tree.index.astype(str)
    # Set the index of the output frame to be the proc_key
    proc_tree = proc_tree.set_index(Col.proc_key)

    first_unique = proc_tree.index.duplicated()
    proc_tree = proc_tree[~first_unique]
    return proc_tree


def build_proc_tree(input_tree: pd.DataFrame, max_depth: int = -1) -> pd.DataFrame:
    """
    Build process tree paths.

    Parameters
    ----------
    input_tree : pd.DataFrame
        DataFrame containing process and parent key definitions
    max_depth : int, optional
        Maximum depth to process the tree, by default -1 (no limit)

    Returns
    -------
    pd.DataFrame
        DataFrame with ordered paths for each process.

    """
    # set default path == current process ID
    input_tree["path"] = input_tree[Col.source_index]
    # input_tree["parent_index"] = np.nan

    cur_level = input_tree[input_tree["IsRoot"]]
    remaining_procs = input_tree[~input_tree["IsRoot"]]

    cur_level_num = 0
    while True:
        sel_crit = remaining_procs[Col.parent_key].isin(cur_level.index)
        next_level = remaining_procs[sel_crit].copy()
        remaining_procs = remaining_procs[~sel_crit]

        if next_level.empty:
            break
        if max_depth != -1 and cur_level_num >= max_depth:
            print(f"max path depth reached: {cur_level_num}")
            break

        # merge next level with current level
        tmp_df = next_level.merge(
            cur_level[[Col.source_index, "path"]],
            how="inner",
            left_on=Col.parent_key,
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

    return input_tree.copy()
