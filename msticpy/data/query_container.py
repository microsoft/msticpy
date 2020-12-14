# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Query hierarchy attribute class."""
from functools import partial
from typing import Any, Dict, Type

from ..common.utility import check_kwarg
from .._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"


class QueryContainer:
    """Empty class used to create hierarchical attributes."""

    _subclasses: Dict[str, Type] = {}

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
        try:
            check_kwarg(name, list(self.__dict__.keys()))
        except NameError:
            pass
        raise AttributeError(
            f"{self.__class__.__name__} object has no attribute {name}"
        )

    def __repr__(self):
        """Return list of attributes."""
        repr_list = []
        for name, obj in self.__dict__.items():
            if isinstance(obj, QueryContainer):
                repr_list.append(f"{name} (container)")
            elif isinstance(obj, partial):
                repr_list.append(f"{name} (query)")
            elif not name.startswith("_"):
                repr_list.append(f"{name} {type(obj).__name__}")
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
