# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Object container class."""
from typing import Any, Dict, Optional, Type

from .._version import VERSION
from ..common.utility import check_kwarg

__version__ = VERSION
__author__ = "Ian Hellen"


class ObjectContainer:
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
        nm_err: Optional[Exception] = None
        try:
            # check for similar-named attributes in __dict__
            check_kwarg(name, list(self.__dict__.keys()))
        except NameError as err:
            nm_err = err
        if nm_err:
            raise AttributeError(
                f"{self.__class__.__name__} object has no attribute {name}"
            ) from nm_err
        raise AttributeError(
            f"{self.__class__.__name__} object has no attribute {name}"
        )

    def __repr__(self):
        """Return list of attributes."""
        repr_list = []
        for name, obj in self.__dict__.items():
            if isinstance(obj, ObjectContainer):
                repr_list.append(f"{name} (container)")
            elif not name.startswith("_"):
                repr_list.append(f"{name} {type(obj).__name__}")
        return "\n".join(repr_list)

    def __call__(self, *args, **kwargs):
        """Return list of attributes or help."""
        if args or kwargs:
            print("This attribute is a container, not a function.")
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


# Descriptors


class SharedProperty:
    """Descriptor to share property between instances of a class."""

    def __init__(self, instance_name: str):
        """
        Initialize the descriptor.

        Parameters
        ----------
        instance_name : str
            Name of the instance attribute to use to store the value.

        """
        self.instance_name = instance_name

    def __get__(self, instance, owner):
        """Return the value of the instance attribute."""
        return getattr(instance, self.instance_name, None)

    def __set__(self, instance, value):
        """Set the value of the instance attribute."""
        setattr(instance, self.instance_name, value)


class FallbackProperty:
    """Descriptor for aliased property with fallback property."""

    def __init__(self, instance_name: str, default_name: str):
        """
        Initialize the descriptor.

        Parameters
        ----------
        instance_name : str
            Name of the instance attribute to use to store the value.
        default_name : str
            Name of the instance attribute to use as a fallback value.

        """
        self.instance_name = instance_name
        self.default_name = default_name

    def __get__(self, instance, owner):
        """Return the value of the instance (or default) attribute."""
        return getattr(
            instance, self.instance_name, getattr(instance, self.default_name, None)
        )

    def __set__(self, instance, value):
        """Set the value of the instance attribute."""
        setattr(instance, self.instance_name, value)


class SplitProperty:
    """Descriptor for property that is stored as two delimited attributes."""

    def __init__(self, inst_left_name: str, inst_right_name: str, split_char: str):
        """
        Initialize the descriptor.

        Parameters
        ----------
        inst_left_name : str
            Name of the instance attribute to use to store the left value.
        inst_right_name : str
            Name of the instance attribute to use to store the right value.
        split_char : str
            Character to use to split the value.
        """
        self.inst_left_name = inst_left_name
        self.inst_right_name = inst_right_name
        self.split_char = split_char

    def __get__(self, instance, owner):
        """Return the value of the instance attribute."""
        left_part = getattr(instance, self.inst_left_name, None)
        right_part = getattr(instance, self.inst_right_name, None)
        if left_part and right_part:
            return f"{left_part}{self.split_char}{right_part}"
        return None

    def __set__(self, instance, value):
        """Set the value of the instance attribute."""
        if value is None:
            return
        if self.split_char not in value:
            setattr(instance, self.inst_left_name, value)
            return
        left, right = value.split(self.split_char, maxsplit=1)
        setattr(instance, self.inst_left_name, left)
        setattr(instance, self.inst_right_name, right)
