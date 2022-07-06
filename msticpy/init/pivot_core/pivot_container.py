# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Pivot function hierarchy attribute class."""
from ..._version import VERSION
from ...common.data_types import ObjectContainer

__version__ = VERSION
__author__ = "Ian Hellen"


class PivotContainer(ObjectContainer):
    """Empty class used to create hierarchical attributes."""

    def __repr__(self):
        """Return list of attributes."""
        repr_list = []
        for name, obj in sorted(self.__dict__.items()):
            if isinstance(obj, ObjectContainer):
                repr_list.append(f"{name} (container)")
            elif hasattr(obj, "pivot_properties"):
                repr_list.append(f"{name} (pivot function)")
            elif not name.startswith("_"):
                repr_list.append(f"{name} ({type(obj).__name__})")
        return "\n".join(repr_list)

    def __call__(self, *args, **kwargs):
        """Return list of attributes or help."""
        if args or kwargs:
            print("This attribute is a container, not a function.")
            print("Items in this container:")
        print(repr(self))
