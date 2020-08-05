# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Query hierarchy attribute class."""
from functools import partial
from typing import Any

from .._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"


class QueryContainer:
    """Empty class used to create hierarchical attributes."""

    def __len__(self):
        """Return number of items in the attribute collection."""
        return len(self.__dict__)

    def __iter__(self):
        """Return iterator over the attributes."""
        return iter(self.__dict__.items())

    def __getattr__(self, name):
        """Print usable error message if attribute not found."""
        if "." in name:
            try:
                attr = _get_dot_attrib(self, name)
            except KeyError:
                pass
            else:
                return attr
        raise AttributeError(
            f"Query attribute {name} not found.",
            "Use QueryProvider.list_queries() to see available queries.",
        )

    def __repr__(self):
        """Return list of attributes."""
        repr_list = []
        for name, obj in self.__dict__.items():
            if isinstance(obj, QueryContainer):
                repr_list.append(f"{name} (container)")
            elif isinstance(obj, partial):
                repr_list.append(f"{name} (query)")
        return "\n".join(repr_list)

    def __call__(self, *args, **kwargs):
        """Return list of attributes or help."""
        if args or kwargs:
            print(f"{self.__name__} is a container, not a query.")
            print("Items in this container:")
        print(repr(self))


def _get_dot_attrib(obj, elem_path: str) -> Any:
    """Return attribute at dotted path."""
    path_elems = elem_path.split(".")
    cur_node = obj
    for elem in path_elems:
        cur_node = getattr(cur_node, elem, None)
        if cur_node is None:
            raise KeyError(f"{elem} value of {elem_path} is not a valid path")
    return cur_node
