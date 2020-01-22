# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Python file import analyzer."""
from typing import Dict, Any, Tuple

import networkx as nx
import matplotlib.pyplot as plt

# try:
#     from nxviz.plots import CircosPlot
#     circos = True
# except ImportError:
#     circos = False

from .ast_parser import analyze
from . import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"


def analyze_calls(module: str, all_calls=False) -> nx.DiGraph:
    """
    Analyze and build call graph of simple module.

    Parameters
    ----------
    module : str
        Module path
    all_calls : bool
        Return graph of all calls, default is False

    Returns
    -------
    nx.DiGraph
        Directed graph of functions and call

    """
    file_analysis = analyze(module)

    # create a set of all imports
    return _create_call_graph(file_analysis["calls"], file_analysis["funcs"], all_calls)


def _create_call_graph(
    calls: Dict[str, Any], funcs: Dict[str, Any], all_calls=False
) -> nx.MultiDiGraph:
    # Calculate the span (line numbers) of each function
    func_span = dict()
    last_func = None
    for lineno, name in sorted([(span[0], name) for name, span in funcs.items()]):
        if last_func:
            func_span[last_func] = [func_span[last_func][0], lineno - 1]

        func_span[name] = [lineno, 9999]
        last_func = name

    # create graph and add funcs as nodes
    call_graph = nx.MultiDiGraph()
    call_graph.add_nodes_from(
        [(name, {"start": span[0], "end": span[1]}) for name, span in func_span.items()]
    )

    # add edged to the calls from each function
    for call_name, call_lines in calls.items():
        if call_name in funcs:
            _add_call_edge(call_graph, call_name, func_span, call_lines, "local")
        elif all_calls:
            _add_call_edge(call_graph, call_name, func_span, call_lines, "external")

    for node in call_graph.nodes():
        n_callers = len(list(call_graph.predecessors(node)))
        color = "red" if not n_callers else "blue"
        call_graph.add_node(node, color=color, degree=n_callers)

    return call_graph


def _add_call_edge(
    call_graph: nx.DiGraph,
    call_name: str,
    func_span: Dict[str, Any],
    call_lines,
    call_type="local",
):
    call_graph.add_node(call_name, call_type=call_type)
    for line in call_lines:
        calling_func = [
            func for func, span in func_span.items() if span[0] <= line <= span[1]
        ]
        if calling_func:
            call_graph.add_edge(calling_func[0], call_name, line=line)
        else:
            call_graph.add_edge("ext", call_name, line=line)


def _print_decendents(graph, par_node, indent=0):
    for node in graph.successors(par_node):
        edge_list = []
        for p_node, t_node, attr in graph.edges([par_node, node], data=True):
            if p_node == par_node and t_node == node:
                edge_line = attr["line"]
                edge_list.append((edge_line, par_node, node))
        for e_line, p_node, t_node in sorted(edge_list, key=lambda k: k[0]):
            print(" " * indent, f"+->({e_line}) {t_node}")
            if p_node != t_node:
                _print_decendents(graph, t_node, indent + 4)


def plot_graph(call_graph: nx.Graph, size: Tuple[int, int] = (10, 10)):
    """
    Plot circular graph using matplotlib.

    Parameters
    ----------
    call_graph : nx.Graph
        The graph to plot.
    size : Tuple[int, int]
        size of plot, default is(10,10)

    """
    # if circos:
    #     c = CircosPlot(
    #         call_graph,
    #         node_color='degree',
    #         node_grouping='degree',
    #         node_order="degree",
    #         node_labels=True,
    #         fontsize="large",
    #         node_label_layout="rotation",
    #         figsize=(20,20)
    #     )
    #     c.draw()
    # else:
    pos = nx.circular_layout(call_graph)
    nx.draw_networkx(call_graph, pos=pos)
    plt.gcf().set_size_inches(size)
    plt.show()


def print_call_tree(call_graph: nx.Graph, level="top"):
    """
    Print out the call tree.

    Parameters
    ----------
    call_graph : [type]
        [description]
    level : str, optional
        [description], by default "top"

    """
    for node in call_graph.nodes():
        if level == "top":
            if call_graph.in_degree(node) == 0:
                print(
                    f"\n{node} [{call_graph.nodes()[node]['start']}",
                    f"- {call_graph.nodes()[node]['end']}]",
                )
                print("-" * len(str(node)))
                _print_decendents(call_graph, node, indent=0)
        elif "start" in call_graph.nodes()[node]:
            print(
                f"\n{node} [{call_graph.nodes()[node]['start']}-{call_graph.nodes()[node]['end']}]"
            )
            print("-" * len(str(node)))
            _print_decendents(call_graph, node, indent=0)
