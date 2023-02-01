# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Module for converting DataFrame to Networkx graph."""
from typing import Callable, Dict, Iterable, Optional, Union

import networkx as nx
import pandas as pd
from typing_extensions import Literal

from .._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"

GraphType = Literal["graph", "digraph"]
NodeRole = Literal["source", "target"]
GraphItem = Literal["node", "edge"]


def df_to_networkx(
    data: pd.DataFrame,
    source_col: str,
    target_col: str,
    source_attrs: Optional[Iterable[str]] = None,
    target_attrs: Optional[Iterable[str]] = None,
    edge_attrs: Optional[Iterable[str]] = None,
    graph_type: GraphType = "graph",
):
    """
    Create a networkx graph from a DataFrame.

    Parameters
    ----------
    data : pd.DataFrame
       Input data
    source_col : str
        Column for source nodes.
    target_col : str
        Column for target nodes.
    source_attrs : Optional[List[str]], optional
        Optional list of columns to use as source node attributes, by default None
    target_attrs : Optional[List[str]], optional
        Optional list of columns to use as target node attributes, by default None
    edge_attrs : Optional[List[str]], optional
        Optional list of columns to use as edge node attributes, by default None
    graph_type : str
        "graph" or "digraph" (for nx.DiGraph)

    Returns
    -------
    nx.Graph
        The networkx graph object

    """
    create_as = nx.DiGraph if graph_type == "digraph" else nx.Graph
    _verify_columns(
        data, source_col, target_col, source_attrs, target_attrs, edge_attrs
    )
    # remove any source or target rows that are NaN
    data = data.dropna(axis=0, subset=[source_col, target_col])
    nx_graph = nx.from_pandas_edgelist(
        data,
        source=source_col,
        target=target_col,
        edge_attr=edge_attrs,
        create_using=create_as,
    )

    _set_node_attributes(data, nx_graph, source_col, source_attrs, node_role="source")
    _set_node_attributes(data, nx_graph, target_col, target_attrs, node_role="target")
    return nx_graph


def _set_node_attributes(
    data: pd.DataFrame,
    graph: nx.Graph,
    column: str,
    attrib_cols: Optional[Iterable[str]],
    node_role: NodeRole,
):
    """Set node attributes from column values."""
    all_cols = [column, *attrib_cols] if attrib_cols else [column]
    # Create an 'agg' dictionary to apply to DataFrame
    agg_dict: Dict[str, Union[str, Callable]] = (
        {col: _pd_unique_list for col in attrib_cols} if attrib_cols else {}
    )
    # Add these two items as attributes
    agg_dict.update({"node_role": "first", "node_type": "first"})

    # Group by the column value and apply the agg dictionary
    # to_dict(orient="index") produces a dict like
    # { column_val: { attrib_name: attrib_val, attrib2_name: ....}}
    source_attrib_dict = (
        data[all_cols]
        .assign(node_role=node_role, node_type=column)
        .groupby(column)
        .agg(agg_dict)
        .to_dict(orient="index")
    )
    # we can use the dict to directly set the attributes for all graph items.
    nx.set_node_attributes(graph, source_attrib_dict)


def _pd_unique_list(series: pd.Series):
    """Return either a value or a string if item not unique."""
    unique_vals = series.unique()
    if len(unique_vals) == 1:
        return unique_vals[0]
    return ", ".join([str(attrib) for attrib in unique_vals])


def _verify_columns(
    data, source_col, target_col, source_attrs, target_attrs, edge_attrs
):
    """Check specified columns are in data."""
    missing_columns = {
        **_verify_column(data, "source_col", source_col),
        **_verify_column(data, "target_col", target_col),
    }

    for col in source_attrs or []:
        missing_columns.update(_verify_column(data, "source_attrs", col))
    for col in target_attrs or []:
        missing_columns.update(_verify_column(data, "target_attrs", col))
    for col in edge_attrs or []:
        missing_columns.update(_verify_column(data, "edge_attrs", col))
    if missing_columns:
        raise ValueError(
            "The following parameters had columns that are missing from the input data",
            ", ".join(f"{col} ({param})" for col, param in missing_columns.items()),
        )


def _verify_column(data, param, column):
    """Verify individual column."""
    return {column: param} if column not in data.columns else {}
