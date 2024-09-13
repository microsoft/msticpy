# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Utility classes and functions."""
from __future__ import annotations

import difflib
import inspect
import sys
from enum import Enum
from functools import wraps
from types import ModuleType
from typing import Any, Callable, Iterable, TypeVar, overload

from typing_extensions import Self

from ..._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"

T = TypeVar("T")


@overload
def export(obj: type[T]) -> type[T]: ...  # noqa: E704


@overload
def export(obj: Callable) -> Callable: ...  # noqa: E704


def export(obj: type | Callable) -> type | Callable:
    """Decorate function or class to export to __all__."""
    mod: ModuleType = sys.modules[obj.__module__]
    if hasattr(mod, "__all__"):
        all_list: list[str] = getattr(mod, "__all__")
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
        func_args: set[str] = inspect.signature(func).parameters.keys() - {
            "args",
            "kwargs",
        }
        valid_arg_names: set[str] = set(legal_args) | func_args

        @wraps(func)
        def wrapper(*args, **kwargs) -> Callable:
            """Inner argument name checker."""
            name_errs: list[Exception] = []
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
def check_kwarg(arg_name: str, legal_args: list[str]) -> None:
    """
    Check argument names against a list.

    Parameters
    ----------
    arg_name : str
        Argument to check
    legal_args : list[str]
        List of possible arguments.

    Raises
    ------
    NameError
        If the argument is not legal. If the `arg_name` is
        a close match to one or more, `legal_args` these are
        returned in the exception.

    """
    if arg_name not in legal_args:
        closest: list[str] = difflib.get_close_matches(arg_name, legal_args)
        msg: str = f"'{arg_name}' is not a recognized argument or attribute. "
        if len(closest) == 1:
            msg += f"Closest match is '{closest[0]}'"
        elif closest:
            match_list: list[str] = [f"'{match}'" for match in closest]
            msg += f"Closest matches are {', '.join(match_list)}"
        else:
            valid_opts: list[str] = [f"'{arg}'" for arg in legal_args]
            msg += f"Valid options are {', '.join(valid_opts)}"
        raise NameError(arg_name, msg)


@export
def check_kwargs(supplied_args: dict[str, Any], legal_args: list[str]) -> None:
    """
    Check all kwargs names against a list.

    Parameters
    ----------
    supplied_args : dict[str, Any]
        Arguments to check
    legal_args : list[str]
        List of possible arguments.

    Raises
    ------
    NameError
        If any of the arguments are not legal. If the an arg is
        a close match to one or more `legal_args`, these are
        returned in the exception.

    """
    name_errs: list[Exception] = []
    for name in supplied_args:
        try:
            check_kwarg(name, legal_args)
        except NameError as err:
            name_errs.append(err)
    if name_errs:
        raise NameError(name_errs)


# Define generic type so enum_parse returns the same type as
# passed in 'enum_class
EnumT = TypeVar("EnumT", bound=Enum)


@export
def enum_parse(enum_cls: type[EnumT], value: str) -> EnumT | None:
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
    if not issubclass(enum_cls, Enum):
        err_msg: str = "Can only be used with classes derived from enum.Enum."
        raise TypeError(err_msg)
    if value in enum_cls.__members__:
        return enum_cls.__members__[value]
    val_lc: str = value.casefold()
    val_map: dict[str, str] = {name.casefold(): name for name in enum_cls.__members__}
    if val_lc in val_map:
        return enum_cls.__members__[val_map[val_lc]]
    return None


@export
class ParseableEnum:
    """Mix-in class for parseable Enum sub-classes."""

    def parse(self: Self, value: str) -> Enum | None:
        """Return enumeration matching (case-insensitive) string value."""
        return enum_parse(enum_cls=self.__class__, value=value)


@export
def arg_to_list(arg: str | list[str], delims: str = ",; ") -> list[str]:
    """
    Convert an optional list/str/str with delims into a list.

    Parameters
    ----------
    arg : Union[str, list[str]]
        A string, delimited string or list
    delims : str, optional
        The default delimiters to use, by default ",; "

    Returns
    -------
    list[str]
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
def collapse_dicts(*dicts: dict) -> dict:
    """Merge multiple dictionaries - later dicts have higher precedence."""
    if len(dicts) == 0:
        return {}
    if len(dicts) == 1:
        return dicts[0]
    out_dict: dict = dicts[0]
    for p_dict in dicts[1:]:
        out_dict = _merge_dicts(out_dict, p_dict)
    return out_dict


def _merge_dicts(dict1: dict[Any, Any], dict2: dict[Any, Any]) -> dict:
    """Merge dict2 into dict1."""
    if not dict2:
        return dict1 or {}
    if not dict1:
        return dict2 or {}
    out_dict: dict = {}
    for key in set().union(dict1, dict2):
        if (
            key in dict1
            and isinstance(dict1[key], dict)
            and key in dict2
            and isinstance(dict2[key], dict)
        ):
            d_val: dict = _merge_dicts(dict1[key], dict2[key])
        elif key in dict2:
            d_val = dict2[key]
        else:
            d_val = dict1[key]
        out_dict[key] = d_val
    return out_dict


def singleton(cls: type) -> Callable:
    """Class decorator for singleton classes."""
    instances: dict[type[object], object] = {}

    def get_instance(*args, **kwargs) -> object:
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

    def __init__(self: SingletonClass, wrapped_cls: type[Any]) -> None:
        """Instantiate the class wrapper."""
        self.wrapped_cls: type[Any] = wrapped_cls
        self.instance: Self | None = None
        self.__doc__ = wrapped_cls.__doc__

    def __call__(self: Self, *args, **kwargs) -> object:
        """Override the __call__ method for the wrapper class."""
        if self.instance is None:
            self.instance = self.wrapped_cls(*args, **kwargs)
        return self.instance

    def current(self: Self) -> object:
        """Return the current instance of the wrapped class."""
        return self.instance

    def __getattr__(self, name: str) -> Any:
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

    def __init__(self: SingletonArgsClass, wrapped_cls: type[Any]) -> None:
        super().__init__(wrapped_cls)
        self.kwargs: dict[str, Any] | None = None
        self.args: tuple[Any] | None = None

    def __call__(self, *args, **kwargs) -> object:
        """Override the __call__ method for the wrapper class."""
        if (
            self.instance is None
            or getattr(self.instance, "kwargs", None) != kwargs
            or getattr(self.instance, "args", None) != args
        ):
            self.instance = self.wrapped_cls(*args, **kwargs)
            if self.instance:
                self.instance.kwargs = kwargs
                self.instance.args = args
        return self.instance


@export
class ImportPlaceholder:
    """Placeholder class for optional imports."""

    def __init__(self, name: str, required_pkgs: list[str]) -> None:
        """Initialize class with imported item name and reqd. packages."""
        self.name: str = name
        self.required_pkgs: list[str] = required_pkgs
        self.message: str = (
            f"{self.name} cannot be loaded without the following packages"
            f" installed: {', '.join(self.required_pkgs)}"
        )
        self._mssg_displayed = False

    def _print_req_packages(self) -> None:
        if not self._mssg_displayed:
            print(self.message, "\nPlease install and restart the notebook.")
            self._mssg_displayed = True

    def __getattr__(self, name) -> None:
        """When any attribute is accessed, print requirements."""
        self._print_req_packages()
        raise ImportError(self.name)

    def __call__(self, *args, **kwargs) -> None:
        """If object is called, print requirements."""
        del args, kwargs
        self._print_req_packages()
        raise ImportError(self.name)
