# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Python file import analyzer."""
import argparse
from typing import Dict, Any, Tuple

import networkx as nx
import matplotlib.pyplot as plt

# try:
#     from nxviz.plots import CircosPlot
#     circos = True
# except ImportError:
#     circos = False

from .ast_parser import analyze
from ..msticpy._version import VERSION

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
) -> nx.DiGraph:
    # Calculate the span (line numbers) of each function
    func_span = dict()
    last_func = None
    for lineno, name in sorted([(span[0], name) for name, span in funcs.items()]):
        if last_func:
            func_span[last_func] = [func_span[last_func][0], lineno - 1]

        func_span[name] = [lineno, 9999]
        last_func = name

    # create graph and add funcs as nodes
    call_graph = nx.DiGraph()
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
        call_graph.add_node(node, degree=n_callers)

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
        edge_line = graph.edges[par_node, node]["line"]
        print(" " * indent, f"+->({edge_line}) {node}")
        if par_node != node:
            _print_decendents(graph, node, indent + 4)


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


def _add_script_args():
    parser = argparse.ArgumentParser(description="Module static call tree analyer.")
    parser.add_argument(
        "--module", "-m", default=".", required=True, help="Path to module to analyze."
    )
    parser.add_argument(
        "--all",
        "-a",
        action="store_true",
        default=False,
        help="Show all functions in module. Default shows only top-level functions",
    )
    parser.add_argument(
        "--external",
        "-e",
        action="store_true",
        default=False,
        help="Show all calls including to external functions.",
    )
    return parser


# pylint: disable=invalid-name
if __name__ == "__main__":
    arg_parser = _add_script_args()
    args = arg_parser.parse_args()

    mod_call_graph = analyze_calls(args.module, all_calls=args.external)
    p_level = "all" if args.all else "top"
    print_call_tree(call_graph=mod_call_graph, level=p_level)

    # mod_call_graph = analyze_calls("E:/src/microsoft/msticpy/msticpy/sectools/domain_utils.py", all_calls=True)
    # p_level = "all"
    # print_call_tree(call_graph=mod_call_graph, level=p_level)
