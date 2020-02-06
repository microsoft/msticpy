# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""AST Parser for limited node types."""
import ast
from pprint import pprint
from collections import defaultdict
from typing import List, Dict, Any

from . import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"


def analyze(
    src_file: str, quiet: bool = True, node_types: List[str] = None
) -> Dict[str, Any]:
    """
    Analyze AST of module using Analyser visitor class.

    Parameters
    ----------
    src_file : [type]
        Input file
    quiet : bool, optional
        Hide reported information, by default True
    node_types : List[str], optional
        The node types to be returned, by default None

    Returns
    -------
    Dict[str, Any]
        Dictionary of node results keyed by node type.

    """
    with open(src_file, "r") as source:
        tree = ast.parse(source.read())

    analyzer = Analyzer()
    analyzer.visit(tree)
    if not quiet:
        analyzer.report(node_types=node_types)
    if node_types:
        return {
            n_type: res
            for n_type, res in analyzer.results.items()
            if n_type in node_types
        }
    return analyzer.results


# pylint: disable=invalid-name
class Analyzer(ast.NodeVisitor):
    """
    Module AST visitor class - used by ast.parse.

    Attributes
    ----------
    nodes : Dict[str, Any]
        Dictionary of nodes in AST

    """

    def __init__(self):
        """Instantiate Analyzer."""
        self.supported_types = ["imports", "imports_from", "calls", "funcs"]
        self.nodes: Dict[str, Any] = {}
        self.nodes["imports"] = []
        self.nodes["imports_from"] = defaultdict(list)
        self.nodes["calls"] = defaultdict(list)
        self.nodes["funcs"] = defaultdict(list)

    def visit_Import(self, node: Any):  # noqa: N802
        """
        Collect import statements.

        Parameters
        ----------
        node : Any
            Visited node

        """
        for alias in node.names:
            self.nodes["imports"].append(alias.name)
        self.generic_visit(node)

    def visit_ImportFrom(self, node: Any):  # noqa: N802
        """
        Collect import from statements.

        Parameters
        ----------
        node : Any
            Visited node

        """
        # print("import from:", node, dir(node))
        for alias in node.names:
            self.nodes["imports_from"][node.module].append(alias.name)
        self.generic_visit(node)

    def visit_Call(self, node: Any):  # noqa: N802
        """
        Collect call statements.

        Parameters
        ----------
        node : Any
            Visited node

        """
        if hasattr(node, "func") and hasattr(node.func, "id"):
            self.nodes["calls"][node.func.id].append(node.lineno)
        if hasattr(node, "func") and hasattr(node.func, "attr"):
            self.nodes["calls"][node.func.attr].append(node.lineno)
        self.generic_visit(node)

    def visit_FunctionDef(self, node: Any):  # noqa: N802
        """
        Collect function statements.

        Parameters
        ----------
        node : Any
            Visited node

        """
        # import pdb; pdb.set_trace()
        self.nodes["funcs"][node.name].append(node.lineno)
        self.generic_visit(node)

    def report(self, node_types: List[str] = None):
        """
        Print report of analysis.

        Parameters
        ----------
        node_types : List[str], optional
            Optional list of node types, by default None

        """
        for node_type, results in self.nodes.items():
            if node_types is not None and node_type in node_types:
                print(node_type)
                pprint(results)

    @property
    def results(self) -> Dict[str, Any]:
        """
        Return dictionary of results.

        Returns
        -------
        Dict[str, Any]
            Dictionary of results keyed by node_type.

        """
        return self.nodes
