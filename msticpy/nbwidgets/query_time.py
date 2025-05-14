# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Module for pre-defined widget layouts."""
from __future__ import annotations

from datetime import datetime, timedelta, timezone
from typing import Any
from typing_extensions import Self

import ipywidgets as widgets
from ipywidgets import Layout

from .._version import VERSION
from ..common.timespan import TimeSpan
from ..common.utility import check_kwargs
from .core import (
    IPyDisplayMixin,
    RegisteredWidget,
    TimeUnit,
    default_before_after,
    default_max_buffer,
    parse_time_unit,
)

__version__ = VERSION
__author__ = "Ian Hellen"


# pylint: disable=too-many-instance-attributes
class QueryTime(RegisteredWidget, IPyDisplayMixin):
    """
    QueryTime.

    Composite widget to capture date and time origin
    and set start and end times for queries.

    See Also
    --------
    RegisteredWidget

    """

    _ALLOWED_KWARGS: list[str] = [
        "origin_time",
        "before",
        "after",
        "start",
        "end",
        "max_before",
        "max_after",
        "label",
        "description",
        "units",
        "auto_display",
        "timespan",
        "register",
        *(RegisteredWidget.ALLOWED_KWARGS),
    ]

    _label_style: dict[str, str] = {"description_width": "initial"}

    IDS_ATTRIBS: list[str] = [
        "before",
        "after",
        "_query_start",
        "_query_end",
        "_label",
    ]

    _NB_PARAMS: dict[str, str] = {
        "start": "_query_start",
        "end": "_query_end",
        "timespan": "timespan",
    }

    def __init__(
        self: QueryTime,
        **kwargs,
    ) -> None:
        """
        Create new instance of QueryTime.

        Parameters
        ----------
        origin_time : datetime, optional
            The origin time (the default is `datetime.utcnow()`)
        description : str, optional
            The description to display
            (the default is 'Select time ({units}) to look back')
            label is an alias for this parameter
        before : int, optional
            The default number of `units` before the `origin_time`
            (the default varies based on the unit)
        after : int, optional
            The default number of `units` after the `origin_time`
            (the default varies based on the unit)
        start : Union[datetime, str]
            Start of query time - alternative to specifying origin,
            before, after
        end : Union[datetime, str]
            End of query time - alternative to specifying origin,
            before, after
        timespan : TimeSpan
            TimeSpan of query time - alternative to specifying origin,
            before, after
        max_before : int, optional
            The largest value for `before` (the default varies based on the unit)
        max_after : int, optional
            The largest value for `after` (the default varies based on the unit)
        units : str, optional
            Time unit (the default is 'hour')
            Permissable values are 'day', 'hour', 'minute', 'second',
            'week'
            These can all be abbreviated down to initial characters
            ('d', 'm', etc.)
        auto_display : bool, optional
            Whether to display on instantiation (the default is False)

        """
        check_kwargs(kwargs, self._ALLOWED_KWARGS)
        self._label = kwargs.pop(
            "description", kwargs.pop("label", "Set query time boundaries")
        )
        self._time_unit = parse_time_unit(kwargs.get("units", "min"))

        self.before = kwargs.pop("before", None)
        self.after = kwargs.pop("after", None)
        self._query_start: Optional[datetime] = None
        self._query_end: Optional[datetime] = None
        self.origin_time = kwargs.pop("origin_time", datetime.now(timezone.utc))
        self._get_time_parameters(**kwargs)

        self.max_before = kwargs.pop("max_before", None)
        self.max_after = kwargs.pop("max_after", None)
        self._adjust_max_before_after(self.max_before, self.max_after)

        # Call superclass to register
        ids_params = [
            self.origin_time,
            self.before,
            self.after,
            self.max_before,
            self.max_after,
            self._label,
            self._time_unit,
        ]

        # Create widgets
        self._w_origin_dt = widgets.DatePicker(
            description="Origin Date", disabled=False, value=self.origin_time.date()
        )
        self._w_origin_tm = widgets.Text(
            description="Time (24hr)",
            disabled=False,
            value=str(self.origin_time.time()),
        )

        range_desc = "Time Range"
        # note self.before and self.after are always set to be not None when we
        # reach here (in _get_time_parameters)
        self._w_tm_range = widgets.IntRangeSlider(
            value=(
                -self.before,  # pylint: disable=invalid-unary-operand-type
                self.after,
            ),
            min=-self.max_before,
            max=self.max_after,
            step=1,
            description=range_desc,
            disabled=False,
            continuous_update=True,
            orientation="horizontal",
            readout=True,
            readout_format="d",
            layout=Layout(width="70%"),
            style=self._label_style,
        )
        # pylint: disable=no-member
        self._w_time_unit = widgets.Dropdown(
            options=[
                unit.capitalize()
                for unit, _ in TimeUnit.__members__.items()
                if unit != "Second"
            ],
            value=self._time_unit.name.capitalize(),
            layout=Layout(width="100px"),
        )
        # pylint: enable=no-member

        self._w_start_time_txt = widgets.Text(
            value=self._query_start.isoformat(sep=" "),
            description="Query start time (UTC):",
            layout=Layout(width="50%"),
            style=self._label_style,
        )
        self._w_end_time_txt = widgets.Text(
            value=self._query_end.isoformat(sep=" "),
            description="Query end time (UTC) :  ",
            layout=Layout(width="50%"),
            style=self._label_style,
        )

        super().__init__(id_vals=ids_params, val_attrs=self.IDS_ATTRIBS, **kwargs)
        self._update_ui_controls()
        self._enable_handlers()

        self.layout: widgets.VBox = self._create_layout()
        if kwargs.pop("auto_display", False):
            self.display()

    def set_time(
        self: Self,
        timespan: TimeSpan | None = None,
        start: datetime | str | None = None,
        end: datetime | str | None = None,
    ) -> None:
        """
        Change the time attributes.

        Parameters
        ----------
        start : Union[datetime, str]
            Start time - (must specify `end`)
        end : Union[datetime, str]
            Start time - (must also specify `end`)
        timespan : TimeSpan
            TimeSpan for query time.

        """
        if timespan or (start and end):
            self.before = None
            self.after = None
            self._get_time_parameters(
                **{"timespan": timespan, "start": start, "end": end}
            )
        self._update_ui_controls()

    def _create_layout(self) -> widgets.VBox:
        return widgets.VBox(
            [
                widgets.HTML(f"<h4>{self._label}</h4>"),
                widgets.HBox([self._w_origin_dt, self._w_origin_tm]),
                widgets.VBox(
                    [
                        widgets.HBox([self._w_tm_range, self._w_time_unit]),
                        self._w_start_time_txt,
                        self._w_end_time_txt,
                    ]
                ),
            ]
        )

    def _get_time_parameters(self: Self, **kwargs) -> None:
        """Process different init time parameters from kwargs."""
        timespan: TimeSpan = kwargs.pop("timespan", None)
        start: datetime | None = kwargs.pop("start", None)
        end: datetime | None = kwargs.pop("end", None)
        if timespan:
            self._query_end = self.origin_time = timespan.end
            self._query_start = timespan.start
        elif start and end:
            timespan = TimeSpan(start=start, end=end)
            self._query_start = timespan.start
            self._query_end = self.origin_time = timespan.end
        else:
            self.before = default_before_after(self.before, self._time_unit)
            self.after = default_before_after(self.after, self._time_unit)
            # Calculate time offsets from origin
            self._query_start = self.origin_time - timedelta(
                0, self.before * self._time_unit.value
            )
            self._query_end = self.origin_time + timedelta(
                0, self.after * self._time_unit.value
            )
        if "units" not in kwargs:
            self._infer_time_units()
        if self.after is None:
            self.after = 0
        if self.before is None:
            self.before = int(
                (self._query_end - self._query_start).total_seconds()
                / self._time_unit.value
            )

    # Utility functions
    def _infer_time_units(self: Self) -> None:
        """Infer reasonable time unit from current timespan."""
        # If time units not set explicitly, set to something sensible,
        # based on start/end times
        if abs(self.timespan.period.days) > 1:
            self._time_unit = TimeUnit.DAY
        elif abs(self.timespan.period.total_seconds()) > 3600:
            self._time_unit = TimeUnit.HOUR
        else:
            self._time_unit = TimeUnit.MINUTE

    def _adjust_max_before_after(
        self: Self,
        max_before: int | None,
        max_after: int | None,
    ) -> None:
        """Adjust the max values so the are always bigger than the defaults."""
        self.max_before = default_max_buffer(
            max_before, self.before or 1, self._time_unit
        )
        self.max_after = default_max_buffer(max_after, self.after or 1, self._time_unit)

    # Widget event handlers
    def _enable_handlers(self: Self) -> None:
        # Add change event handlers
        self._w_tm_range.observe(self._time_range_change, names="value")
        self._w_origin_dt.observe(self._update_origin, names="value")
        self._w_origin_tm.observe(self._update_origin, names="value")
        self._w_time_unit.observe(self._change_time_unit, names="value")

    def _disable_handlers(self: Self) -> None:
        self._w_tm_range.unobserve_all()
        self._w_origin_dt.unobserve_all()
        self._w_origin_tm.unobserve_all()
        self._w_time_unit.unobserve_all()

    def _change_time_unit(self: Self, change: dict[str, Any]) -> None:
        """Handle change event from time unit control."""
        # Reset before/after and max buffers to defaults.
        unit = change["new"]
        self._time_unit = parse_time_unit(unit)
        self.before = default_before_after(default=None, unit=self._time_unit)
        self.after = default_before_after(default=None, unit=self._time_unit)
        self._adjust_max_before_after(max_before=None, max_after=None)
        self._set_time_slider_settings()
        self._update_start_and_end_from_slider()

    def _update_origin(self: Self, _) -> None:
        """Handle change events for origin date and time controls."""
        try:
            tm_value = datetime.strptime(self._w_origin_tm.value, "%H:%M:%S.%f").time()
            self.origin_time = datetime.combine(self._w_origin_dt.value, tm_value)
            self._time_range_change(change=None)
        except (ValueError, TypeError):
            # reset on error
            self._w_origin_dt.value = self.origin_time.date()
            self._w_origin_tm.value = self.origin_time.time().isoformat()

    def _time_range_change(self: Self, _) -> None:
        """Handle change event for time slider control."""
        self._update_start_and_end_from_slider()
        self.before = abs(self._w_tm_range.value[0])
        self.after = abs(self._w_tm_range.value[1])

    def _update_start_and_end_from_slider(self: Self) -> None:
        """Update timespan and start and end text controls."""
        self._query_start = self.origin_time + timedelta(
            0, self._w_tm_range.value[0] * self._time_unit.value
        )
        self._query_end = self.origin_time + timedelta(
            0, self._w_tm_range.value[1] * self._time_unit.value
        )
        self._w_start_time_txt.value = self._query_start.isoformat(sep=" ")
        self._w_end_time_txt.value = self._query_end.isoformat(sep=" ")

    # end - event handlers

    # control updates from attributes
    def _update_ui_controls(self: Self) -> None:
        """Update UI controls from attributes."""
        self._disable_handlers()
        self._w_start_time_txt.value = self._query_start.isoformat(sep=" ")
        self._w_end_time_txt.value = self._query_end.isoformat(sep=" ")
        self._set_time_slider_settings()
        self._w_origin_dt.value = self.origin_time.date()
        self._w_origin_tm.value = self.origin_time.time().isoformat()
        self._w_time_unit.value = self._time_unit.name.capitalize()
        self._enable_handlers()

    def _set_time_slider_settings(self: Self) -> None:
        """Set slider properties based on current before and after values."""
        self._w_tm_range.value = (
            -self.before,  # pylint: disable=invalid-unary-operand-type
            self.after,
        )
        self._w_tm_range.min = -self.max_before
        self._w_tm_range.max = self.max_after

    # end control updates from attributes

    @property
    def start(self: Self) -> datetime | None:
        """Query start time."""
        return self._query_start

    @property
    def end(self: Self) -> datetime | None:
        """Query end time."""
        return self._query_end

    @property
    def units(self: Self) -> str:
        """Time units used by control."""
        return self._time_unit.name.capitalize()

    @property
    def timespan(self: Self) -> TimeSpan:
        """Return the timespan as a TimeSpan object."""
        return TimeSpan(start=self.start, end=self.end)

    @timespan.setter
    def timespan(self: Self, value: TimeSpan) -> None:
        """Set the timespan of the QueryTime widget."""
        self.set_time(timespan=value)

    @property
    def value(self: Self) -> TimeSpan:
        """Return the timespan as a TimeSpan object."""
        return self.timespan

    @value.setter
    def value(self: Self, value: TimeSpan) -> None:
        """Set the timespan of the QueryTime widget."""
        self.set_time(timespan=value)
