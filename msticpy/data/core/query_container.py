# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Query hierarchy attribute class."""
from functools import partial

from ..._version import VERSION
from ...common.data_types import ObjectContainer

__version__ = VERSION
__author__ = "Ian Hellen"


class QueryContainer(ObjectContainer):
    """Empty class used to create hierarchical attributes."""

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
            print("This attribute is a container, not a query.")
            print("Items in this container:")
        print(repr(self))
