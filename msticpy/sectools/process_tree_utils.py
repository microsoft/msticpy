# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Process Tree Visualization."""
from typing import Any, Dict, Optional, Union

import pandas as pd

from .._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"


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


# def build_process_key(  # type: ignore  # noqa: F821
#     source_proc: pd.Series,
#     schema: "ProcSchema"
# ) -> str:
#     """
#     Return a process key from a process event.

#     Parameters
#     ----------
#     source_proc : pd.Series, optional
#         Source process
#     schema : ProcSchema, optional
#         The data schema to use, by default None
#         - if None the schema will be inferred

#     Returns
#     -------
#     str
#         Process key of the process

#     """
#     if schema is None:
#         schema = infer_schema(source_proc)
#     proc_path = source_proc[schema.process_name].lower()
#     pid = source_proc[schema.process_id]
#     tstamp = pd.to_datetime(source_proc[schema.time_stamp]).strftime(TS_FMT_STRING)
#     return f"{proc_path}{pid}{tstamp}"


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
    rem_procs: Optional[pd.DataFrame] = None
    while max_levels == -1 or level < max_levels:
        if rem_procs is not None:
            # pylint: disable=unsubscriptable-object
            children = rem_procs[rem_procs["parent_key"].isin(parent_keys)]
            rem_procs = rem_procs[~rem_procs["parent_key"].isin(parent_keys)]
            # pylint: enable=unsubscriptable-object
        else:
            children = procs[procs["parent_key"].isin(parent_keys)]
            rem_procs = procs[~procs["parent_key"].isin(parent_keys)]
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
