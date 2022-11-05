# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Utility classes and functions."""
import difflib
import inspect
import sys
from enum import Enum
from functools import wraps
from typing import Any, Callable, Dict, Iterable, List, Optional, Type, TypeVar, Union

from ..._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"


def export(obj: Callable):
    """Decorate function or class to export to __all__."""
    mod = sys.modules[obj.__module__]
    if hasattr(mod, "__all__"):
        all_list = getattr(mod, "__all__")
        all_list.append(obj.__name__)
    else:
        all_list = [obj.__name__]
        setattr(mod, "__all__", all_list)
    return obj


@export
def checked_kwargs(legal_args: Iterable[str]):
    """
    Decorate function to check kwargs names against legal arg names.

    Parameters
    ----------
    legal_args : Iterable[str]
        Iterable of possible arguments.

    Raises
    ------
    NameError
        If any of the arguments are not legal. If the an arg is
        a close match to one or more `legal_args`, these are
        returned in the exception.

    Notes
    -----
    The checking is done against the union of legal_args and
    any named arguments of the wrapped function.

    """

    def arg_check_wrapper(func):
        func_args = inspect.signature(func).parameters.keys() - {"args", "kwargs"}
        valid_arg_names = set(legal_args) | func_args

        @wraps(func)
        def wrapper(*args, **kwargs):
            """Inner argument name checker."""
            name_errs = []
            for name in kwargs:
                try:
                    check_kwarg(name, valid_arg_names)
                except NameError as err:
                    name_errs.append(err)
            if name_errs:
                raise NameError(name_errs)
            return func(*args, **kwargs)

        return wrapper

    return arg_check_wrapper


@export
def check_kwarg(arg_name: str, legal_args: List[str]):
    """
    Check argument names against a list.

    Parameters
    ----------
    arg_name : str
        Argument to check
    legal_args : List[str]
        List of possible arguments.

    Raises
    ------
    NameError
        If the argument is not legal. If the `arg_name` is
        a close match to one or more, `legal_args` these are
        returned in the exception.

    """
    if arg_name not in legal_args:
        closest = difflib.get_close_matches(arg_name, legal_args)
        mssg = f"'{arg_name}' is not a recognized argument or attribute. "
        if len(closest) == 1:
            mssg += f"Closest match is '{closest[0]}'"
        elif closest:
            match_list = [f"'{match}'" for match in closest]
            mssg += f"Closest matches are {', '.join(match_list)}"
        else:
            valid_opts = [f"'{arg}'" for arg in legal_args]
            mssg += f"Valid options are {', '.join(valid_opts)}"
        raise NameError(arg_name, mssg)


@export
def check_kwargs(supplied_args: Dict[str, Any], legal_args: List[str]):
    """
    Check all kwargs names against a list.

    Parameters
    ----------
    supplied_args : Dict[str, Any]
        Arguments to check
    legal_args : List[str]
        List of possible arguments.

    Raises
    ------
    NameError
        If any of the arguments are not legal. If the an arg is
        a close match to one or more `legal_args`, these are
        returned in the exception.

    """
    name_errs = []
    for name in supplied_args:
        try:
            check_kwarg(name, legal_args)
        except NameError as err:
            name_errs.append(err)
    if name_errs:
        raise NameError(name_errs)


# Define generic type so enum_parse returns the same type as
# passed in 'enum_class
EnumType = TypeVar("EnumType")  # pylint: disable=invalid-name


@export
def enum_parse(enum_cls: Type[EnumType], value: str) -> Optional[EnumType]:
    """
    Try to parse a string value to an Enum member.

    Parameters
    ----------
    enum_cls : EnumType
        The Enum type to check against
    value : str
        The enum value to parse

    Returns
    -------
    Optional[EnumType]
        The enumeration value matching `value` or None

    Raises
    ------
    TypeError
        If something other than an Enum subclass is passed.

    """
    if not issubclass(enum_cls, Enum):  # type: ignore
        raise TypeError("Can only be used with classes derived from enum.Enum.")
    if value in enum_cls.__members__:  # type: ignore
        return enum_cls.__members__[value]  # type: ignore
    val_lc = value.casefold()
    val_map = {name.casefold(): name for name in enum_cls.__members__}  # type: ignore
    if val_lc in val_map:
        return enum_cls.__members__[val_map[val_lc]]  # type: ignore
    return None


@export
class ParseableEnum:
    """Mix-in class for parseable Enum sub-classes."""

    def parse(self, value: str):
        """Return enumeration matching (case-insensitive) string value."""
        return enum_parse(enum_cls=self.__class__, value=value)


@export
def arg_to_list(arg: Union[str, List[str]], delims=",; ") -> List[str]:
    """
    Convert an optional list/str/str with delims into a list.

    Parameters
    ----------
    arg : Union[str, List[str]]
        A string, delimited string or list
    delims : str, optional
        The default delimiters to use, by default ",; "

    Returns
    -------
    List[str]
        List of string components

    Raises
    ------
    TypeError
        If `arg` is not a string or list

    """
    if isinstance(arg, list):
        return arg
    if isinstance(arg, str):
        for char in delims:
            if char in arg:
                return [item.strip() for item in arg.split(char)]
        return [arg]
    raise TypeError("`arg` must be a string or a list.")


@export
def collapse_dicts(*dicts: Dict[Any, Any]) -> Dict[Any, Any]:
    """Merge multiple dictionaries - later dicts have higher precedence."""
    if len(dicts) < 2:
        return dicts[0] or {}
    out_dict = dicts[0]
    for p_dict in dicts[1:]:
        out_dict = _merge_dicts(out_dict, p_dict)
    return out_dict


def _merge_dicts(dict1: Dict[Any, Any], dict2: Dict[Any, Any]):
    """Merge dict2 into dict1."""
    if not dict2:
        return dict1 or {}
    if not dict1:
        return dict2 or {}
    out_dict = {}
    for key in set().union(dict1, dict2):  # type: Any
        if (
            key in dict1
            and isinstance(dict1[key], dict)
            and key in dict2
            and isinstance(dict2[key], dict)
        ):
            d_val = _merge_dicts(dict1[key], dict2[key])
        elif key in dict2:
            d_val = dict2[key]
        else:
            d_val = dict1[key]
        out_dict[key] = d_val
    return out_dict


def singleton(cls):
    """Class decorator for singleton classes."""
    instances = {}

    def get_instance(*args, **kwargs):
        nonlocal instances
        if cls not in instances:
            instances[cls] = cls(*args, **kwargs)
        return instances[cls]

    return get_instance


@export
class SingletonClass:
    """
    Singleton decorator class.

    Notes
    -----
    Using this decorator on a class enforces the following
    behavior:

    - First instantiation of class will work as normal
    - Subsequent attempts with the same set/values of kwargs
      will just return the original class
    - The class method `current()` will always return the
      last instance of the class.

    """

    def __init__(self, wrapped_cls):
        """Instantiate the class wrapper."""
        self.wrapped_cls = wrapped_cls
        self.instance = None
        self.__doc__ = wrapped_cls.__doc__

    def __call__(self, *args, **kwargs):
        """Override the __call__ method for the wrapper class."""
        if self.instance is None:
            self.instance = self.wrapped_cls(*args, **kwargs)
        return self.instance

    def current(self):
        """Return the current instance of the wrapped class."""
        return self.instance

    def __getattr__(self, name):
        """Return the attribute `name` from the wrapped class."""
        if hasattr(self.wrapped_cls, name):
            return getattr(self.wrapped_cls, name)
        if self.instance is None:
            return None
        if name == "current":
            return self.instance
        return getattr(self.instance, name)


@export
class SingletonArgsClass(SingletonClass):
    """
    SingletonArgs decorator class.

    Notes
    -----
    Using this decorator on a class enforces the following
    behavior:

    - First instantiation of class will work as normal
    - Subsequent attempts with the same set/values of kwargs
      will just return the original class
    - Instantiation of the class with a different set of kwargs
      will instantiate a new class.
    - The class method `current()` will always return the
      last instance of the class.

    """

    def __call__(self, *args, **kwargs):
        """Override the __call__ method for the wrapper class."""
        if (
            self.instance is None
            or getattr(self.instance, "kwargs", None) != kwargs
            or getattr(self.instance, "args", None) != args
        ):
            self.instance = self.wrapped_cls(*args, **kwargs)
            self.instance.kwargs = kwargs
            self.instance.args = args
        return self.instance


@export
class ImportPlaceholder:
    """Placeholder class for optional imports."""

    def __init__(self, name: str, required_pkgs: List[str]):
        """Initialize class with imported item name and reqd. packages."""
        self.name = name
        self.required_pkgs = required_pkgs
        self.message = (
            f"{self.name} cannot be loaded without the following packages"
            f" installed: {', '.join(self.required_pkgs)}"
        )
        self._mssg_displayed = False

    def _print_req_packages(self):
        if not self._mssg_displayed:
            print(self.message, "\nPlease install and restart the notebook.")
            self._mssg_displayed = True

    def __getattr__(self, name):
        """When any attribute is accessed, print requirements."""
        self._print_req_packages()
        raise ImportError(self.name)

    def __call__(self, *args, **kwargs):
        """If object is called, print requirements."""
        del args, kwargs
        self._print_req_packages()
        raise ImportError(self.name)
