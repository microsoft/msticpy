# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Module for pre-defined widget layouts."""
from datetime import datetime, timedelta
from typing import Optional

import ipywidgets as widgets
from ipywidgets import Layout

from .._version import VERSION
from .core import (
    IPyDisplayMixin,
    default_before_after,
    default_max_buffer,
    parse_time_unit,
)

__version__ = VERSION
__author__ = "Ian Hellen"


class Lookback(IPyDisplayMixin):
    """Time lookback slider."""

    # pylint: disable=too-many-arguments
    def __init__(
        self,
        default: Optional[int] = None,
        description: str = "Select time ({units}) to look back",
        origin_time: datetime = None,
        min_value: Optional[int] = None,
        max_value: Optional[int] = None,
        units: str = "hour",
        auto_display: bool = False,
        **kwargs,
    ):
        """
        Create an instance of the lookback slider widget.

        Parameters
        ----------
        default : int, optional
            The default 'lookback' time (the default is 4)
        description : str, optional
            The description to display
            (the default is 'Select time ({units}) to look back')
        origin_time : datetime, optional
            The origin time (the default is `datetime.utcnow()`)
        min_value : int, optional
            Minimum value (the default is 1)
        max_value : int, optional
            Maximum value (the default is 240)
        units : str, optional
            Time unit (the default is 'hour')
            Permissable values are 'day', 'hour', 'minute', 'second',
            'week'
            These can all be abbreviated down to initial characters
            ('d', 'm', etc.)
        auto_display : bool, optional
            Whether to display on instantiation (the default is False)

        """
        # default to now
        self.origin_time = datetime.utcnow() if origin_time is None else origin_time
        description = kwargs.pop("label", description)

        self._time_unit = parse_time_unit(units)
        if "{units}" in description:
            description = description.format(units=self._time_unit.name)
        default = default_before_after(default, self._time_unit)
        min_value = min_value or 1
        max_value = default_max_buffer(max_value, default, self._time_unit)

        self._lookback_wgt = widgets.IntSlider(
            value=default,
            min=min_value,
            max=max_value,
            step=1,
            description=description,
            layout=Layout(width="60%", height="50px"),
            style={"description_width": "initial"},
        )

        self.end = self.origin_time
        self._time_unit = parse_time_unit(units)
        self.start = self.end - timedelta(
            seconds=(self._time_unit.value * self._lookback_wgt.value)
        )

        self._lookback_wgt.observe(self._time_range_change, names="value")

        if auto_display:
            self.display()

    @property
    def layout(self):
        """Return underlying widget."""
        return self._lookback_wgt

    @property
    def lookback(self):
        """Return current widget lookback value."""
        return self._lookback_wgt.value

    @property
    def value(self):
        """Return current widget lookback value."""
        return self._lookback_wgt.value

    def _time_range_change(self, change):
        del change
        self.start = self.origin_time - timedelta(
            0, self._lookback_wgt.value * self._time_unit.value
        )
