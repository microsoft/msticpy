# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Module for pre-defined widget layouts."""
from abc import ABC
from enum import IntEnum
from typing import Any, Dict, List, Optional
from weakref import WeakValueDictionary

from IPython.display import display

from .._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"


_WIDGET_REG: WeakValueDictionary = WeakValueDictionary()


# pylint: disable=too-few-public-methods
class RegisteredWidget(ABC):
    """
    Register widget in the widget registry.

    Registered widgets will store their values in the register.
    Each widget has an ID that that is derived from one or more of the
    initialization parameters. If an instance of the same widget class is
    created with the same parameters, its previous value will be repopulated
    from the registry.
    This is especially useful in notebooks where people accidentally re-run
    the same cell after entering values.
    """

    ALLOWED_KWARGS = ["id_vals", "val_attrs", "nb_params", "name_space", "register"]
    _NB_PARAMS: Dict[str, str] = {}

    def __init__(
        self,
        id_vals: Optional[List[Any]] = None,
        val_attrs: Optional[List[str]] = None,
        nb_params: Optional[Dict[str, str]] = None,
        name_space: Dict[str, Any] = globals(),
        register: bool = True,
        **kwargs,
    ):
        """
        Initialize a registered widget.

        Parameters
        ----------
        id_vals : Optional[List[Any]], optional
            The list of parameter values to use to identify this widget instance,
            by default None
        val_attrs : Optional[List[str]], optional
            The names of the attributes to persist in the registry
            and recall, by default ["value"]
        nb_params : Optional[Dict[str, str]], optional
            A dictionary of attribute names and global variables. If the variable
            exists in the global namespace it will be used to populate the
            corresponding widget attribute. This is only done if the widget
            attribute currently has no value (i.e. restoring a value from
            the registry takes priority over this),
            by default None
        name_space : Dict[str, Any], optional
            Namespace to look for global variables, by default None
        register : bool
            Do not register the widget or retrieve values from previously-
            registered instance.

        """
        del kwargs  # allow to be called with kwargs that are ignored
        # Try to retrieve previous values based on ID of this control
        if register and id_vals:
            id_list = [self.__class__.__name__, *[str(val) for val in id_vals]]
            self._id = hash("".join(id_list))

            if not val_attrs:
                val_attrs = ["value"]
            if self._id in _WIDGET_REG:
                for attr in val_attrs:
                    if hasattr(_WIDGET_REG[self._id], attr):
                        setattr(self, attr, getattr(_WIDGET_REG[self._id], attr))
            # register the current instance as the last instance
            _WIDGET_REG[self._id] = self

        # if there are any notebook params relevant to this control
        if nb_params and name_space:
            for attr, nb_param in nb_params.items():
                # if this doesn't have a value set explicitly or
                # one that was recovered from the widget registry
                # set it from the nb_param value
                wgt_internal_name = self._NB_PARAMS.get(attr, attr)
                if nb_param in name_space and not getattr(
                    self, wgt_internal_name, None
                ):
                    setattr(self, wgt_internal_name, name_space[nb_param])


# pylint: enable=too-few-public-methods


class IPyDisplayMixin:
    """IPython display mixin class."""

    def display(self):
        """Display the interactive widgets."""
        display(self.layout)

    def _ipython_display_(self):
        """Display in IPython."""
        self.display()


class TimeUnit(IntEnum):
    """Time unit enumeration and value."""

    SECOND = 1
    MINUTE = 60
    HOUR = 60 * MINUTE
    DAY = 24 * HOUR
    WEEK = 7 * DAY


def parse_time_unit(unit_str: str) -> TimeUnit:
    """Return the TimeUnit enum matching the input string."""
    if unit_str.casefold().startswith("m"):
        return TimeUnit.MINUTE
    if unit_str.casefold().startswith("h"):
        return TimeUnit.HOUR
    if unit_str.casefold().startswith("s"):
        return TimeUnit.SECOND
    if unit_str.casefold().startswith("d"):
        return TimeUnit.DAY
    if unit_str.casefold().startswith("w"):
        return TimeUnit.WEEK
    return TimeUnit.MINUTE


def default_max_buffer(max_default: Optional[int], default: int, unit: TimeUnit) -> int:
    """Return the max time buffer for a give time unit."""
    mag_default = abs(int(default * 4))
    if max_default is not None:
        max_value = abs(max_default)
        return max(max_value, mag_default)
    if unit == TimeUnit.DAY:
        return max(28, mag_default)
    if unit == TimeUnit.HOUR:
        return max(72, mag_default)
    if unit == TimeUnit.WEEK:
        return max(20, mag_default)
    return max(240, mag_default)


def default_before_after(default: Optional[int], unit: TimeUnit) -> int:
    """Return default before and after bounds for a TimeUnit."""
    if default is not None:
        return abs(default)
    if unit in (TimeUnit.DAY, TimeUnit.WEEK):
        return 1
    if unit == TimeUnit.HOUR:
        return 6
    return 60
