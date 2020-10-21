# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Entity Graph classes."""
from typing import Any, Dict, Set, Optional

from ..._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"


class Node:
    def __init__(self):
        self.edges: Set["Edge"] = set()

    def add_edge(self, target: "Node", attrs: Optional[Dict[str, Any]] = None):
        edge = Edge(self, target, attrs) if attrs else Edge(self, target)
        if not self.has_edge(target):
            self.edges.add(edge)
        if not target.has_edge(self):
            target.edges.add(edge)

    def has_edge(self, other):
        return any(
            edge for edge in self.edges if edge.target == other or edge.source == other
        )


class Edge:
    def __init__(self, src: Node, tgt: Node, attrs: Dict[str, Any] = None):
        self.source: Node = src
        self.target: Node = tgt

        self.attrs: Dict[str, Any] = attrs or {}

    def add_attr(self, name: str, value: Any):
        self.attrs[name] = value
