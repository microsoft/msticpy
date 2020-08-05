# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Module for pre-defined widget layouts."""
import os
from datetime import datetime, timedelta
from enum import Enum
import json
from json import JSONDecodeError
import random
from typing import Any, Callable, Dict, List, Mapping, Union, Tuple, Optional

from deprecated.sphinx import deprecated
import pandas as pd
from IPython.display import display, HTML
import ipywidgets as widgets
from ipywidgets import Layout

from .._version import VERSION
from ..data.query_defns import QueryParamProvider
from ..common.utility import export

__version__ = VERSION
__author__ = "Ian Hellen"


# pylint: disable=too-many-lines
class TimeUnit(Enum):
    """Time unit enumeration and value."""

    sec = 1
    min = 60
    hour = 60 * 60
    day = 60 * 60 * 24


def _parse_time_unit(unit_str: str) -> TimeUnit:
    """Return the TimeUnit enum matching the input string."""
    if unit_str.startswith("m"):
        return TimeUnit.min
    if unit_str.startswith("h"):
        return TimeUnit.hour
    if unit_str.startswith("s"):
        return TimeUnit.sec
    if unit_str.startswith("d"):
        return TimeUnit.day
    return TimeUnit.min


@export
class Lookback(QueryParamProvider):
    """
    ipwidget wrapper to display integer slider.

    Attributes
    ----------
    before : int
        The default number of `units` before the `origin_time`
        (the default is 60)
    after : int
        The default number of `units` after the `origin_time`
        (the default is 10)
    max_before : int
        The largest value for `before` (the default is 600)
    max_after : int
        The largest value for `after` (the default is 100)
    origin_time : datetime
            The origin time (the default is `datetime.utcnow()`)
    start : datetime
        Query start time.
    end : datetime
        Query end time.

    """

    # pylint: disable=too-many-arguments
    def __init__(
        self,
        default: int = 4,
        label: str = "Select time ({units}) to look back",
        origin_time: datetime = None,
        min_value: int = 1,
        max_value: int = 240,
        units: str = "hour",
        auto_display: bool = False,
    ):
        """
        Create an instance of the lookback slider widget.

        Parameters
        ----------
        default : int, optional
            The default 'lookback' time (the default is 4)
        label : str, optional
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
            Permissable values are 'day', 'hour', 'minute', 'second'
            These can all be abbreviated down to initial characters
            ('d', 'm', etc.)
        auto_display : bool, optional
            Whether to display on instantiation (the default is False)

        """
        # default to now
        self.origin_time = datetime.utcnow() if origin_time is None else origin_time

        self._time_unit = _parse_time_unit(units)
        if "{units}" in label:
            label = label.format(units=self._time_unit.name)
        self._lookback_wgt = widgets.IntSlider(
            value=default,
            min=min_value,
            max=max_value,
            step=1,
            description=label,
            layout=Layout(width="60%", height="50px"),
            style={"description_width": "initial"},
        )

        self.end = self.origin_time
        self._time_unit = _parse_time_unit(units)
        self.start = self.end - timedelta(
            seconds=(self._time_unit.value * self._lookback_wgt.value)
        )

        self._lookback_wgt.observe(self._time_range_change, names="value")

        if auto_display:
            self.display()

    def display(self):
        """Display the interactive widgets."""
        display(self._lookback_wgt)

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

    @property
    def query_params(self):
        """
        Query parameters derived from alert.

        Returns
        -------
            dict(str, str) -- Dictionary of parameter names

        """
        return {"start": self.start, "end": self.end}

    def _ipython_display_(self):
        """Display in IPython."""
        self.display()


# pylint: disable=too-many-instance-attributes
@export
class QueryTime(QueryParamProvider):
    """
    QueryTime.

    Composite widget to capture date and time origin
    and set start and end times for queries.

    Attributes
    ----------
    before : int
        The default number of `units` before the `origin_time`
        (the default is 60)
    after : int
        The default number of `units` after the `origin_time`
        (the default is 10)
    max_before : int
        The largest value for `before` (the default is 600)
    max_after : int
        The largest value for `after` (the default is 100)
    origin_time : datetime
            The origin time (the default is `datetime.utcnow()`)
    start : datetime
        Query start time.
    end : datetime
        Query end time.
    query_params

    """

    _label_style = {"description_width": "initial"}

    # pylint: disable=too-many-arguments
    def __init__(
        self,
        origin_time: datetime = None,
        before: int = 60,
        after: int = 10,
        max_before: int = 600,
        max_after: int = 100,
        label: str = None,
        units: str = "min",
        auto_display: bool = False,
    ):
        """
        Create new instance of QueryTime.

        Parameters
        ----------
        origin_time : datetime, optional
            The origin time (the default is `datetime.utcnow()`)
        label : str, optional
            The description to display
            (the default is 'Select time ({units}) to look back')
        before : int, optional
            The default number of `units` before the `origin_time`
            (the default is 60)
        after : int, optional
            The default number of `units` after the `origin_time`
            (the default is 10)
        max_before : int, optional
            The largest value for `before` (the default is 600)
        max_after : int, optional
            The largest value for `after` (the default is 100)
        units : str, optional
            Time unit (the default is 'min')
            Permissable values are 'day', 'hour', 'minute', 'second'
            These can all be abbreviated down to initial characters
            ('d', 'm', etc.)
        auto_display : bool, optional
            Whether to display on instantiation (the default is False)

        """
        self._label = "Set query time boundaries" if label is None else label
        self._time_unit = _parse_time_unit(units)

        max_before = abs(max_before)
        max_after = abs(max_after)
        before = abs(before)
        after = abs(after)
        if max_before < before:
            before = max_before
        if max_after < after:
            after = max_after

        # default to now
        self.origin_time = datetime.utcnow() if origin_time is None else origin_time
        # Calculate time offsets from origin
        self._query_start = self.origin_time - timedelta(
            0, before * self._time_unit.value
        )
        self._query_end = self.origin_time + timedelta(0, after * self._time_unit.value)

        # Create widgets
        self._w_origin_dt = widgets.DatePicker(
            description="Origin Date", disabled=False, value=self.origin_time.date()
        )
        self._w_origin_tm = widgets.Text(
            description="Time (24hr)",
            disabled=False,
            value=str(self.origin_time.time()),
        )

        range_desc = "Time Range ({}):".format(self._time_unit.name)
        self._w_tm_range = widgets.IntRangeSlider(
            value=[-before, after],
            min=-max_before,
            max=max_after,
            step=1,
            description=range_desc,
            disabled=False,
            continuous_update=True,
            orientation="horizontal",
            readout=True,
            readout_format="d",
            layout=Layout(width="80%"),
            style=self._label_style,
        )

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

        self._w_tm_range.observe(self._time_range_change, names="value")
        self._w_origin_dt.observe(self._update_origin, names="value")
        self._w_origin_tm.observe(self._update_origin, names="value")

        if auto_display:
            self.display()

    def display(self):
        """Display the interactive widgets."""
        display(widgets.HTML("<h4>{}</h4>".format(self._label)))
        display(widgets.HBox([self._w_origin_dt, self._w_origin_tm]))
        display(
            widgets.VBox(
                [self._w_tm_range, self._w_start_time_txt, self._w_end_time_txt]
            )
        )

    def _update_origin(self, change):
        del change
        try:
            tm_value = datetime.strptime(self._w_origin_tm.value, "%H:%M:%S.%f").time()
            self.origin_time = datetime.combine(self._w_origin_dt.value, tm_value)
            self._time_range_change(change=None)
        except ValueError:
            pass

    def _time_range_change(self, change):
        del change
        self._query_start = self.origin_time + timedelta(
            0, self._w_tm_range.value[0] * self._time_unit.value
        )
        self._query_end = self.origin_time + timedelta(
            0, self._w_tm_range.value[1] * self._time_unit.value
        )
        self._w_start_time_txt.value = self._query_start.isoformat(sep=" ")
        self._w_end_time_txt.value = self._query_end.isoformat(sep=" ")

    @property
    def start(self):
        """Query start time."""
        return self._query_start

    @property
    def end(self):
        """Query end time."""
        return self._query_end

    @property
    def query_params(self):
        """
        Query parameters derived from alert.

        Returns
        -------
            dict(str, str) -- Dictionary of parameter names

        """
        return {"start": self.start, "end": self.end}

    def _ipython_display_(self):
        """Display in IPython."""
        self.display()


# pylint: disable=too-many-instance-attributes
@export
class SelectAlert:
    """
    Alert Selector.

    View list of alerts and select one for investigation.
    Optionally provide and action to call with the selected alert as a parameter
    (typically used to display the alert.)

    Attributes
    ----------
    selected_alert : SecurityAlert
        The selected alert
    alert_id : str
        The SystemAlertId of the selected alert
    alerts : List[SecurityAlert]
        The current alert list (DataFrame)
    action : Callable[..., Optional[Tuple[...]]]
        The callback action to execute on selection
        of an alert.

    """

    _ALERTID_REGEX = r"\[id:(?P<alert_id>.*)\]$"

    def __init__(
        self,
        alerts: pd.DataFrame,
        action: Callable[..., Optional[Tuple]] = None,
        columns: List[str] = None,
        auto_display: bool = False,
    ):
        """
        Create a new instance of AlertSelector.

        Parameters
        ----------
        alerts : pd.DataFrame
            DataFrame of alerts.
        action : Callable[..., Optional[Tuple]], optional
            Optional function to execute for each selected alert.
            If the function returns one or a tuple of displayable objects
            these will be displayed.
        columns : List[str], optional
            Override the default column names to use from `alerts`
            (the default is ['StartTimeUtc', 'AlertName',
            'CompromisedEntity', 'SystemAlertId'])
        auto_display : bool, optional
            Whether to display on instantiation (the default is False)

        """
        self.alerts = alerts
        self.alert_action = action

        if not columns:
            columns = [
                "StartTimeUtc",
                "AlertName",
                "CompromisedEntity",
                "SystemAlertId",
            ]

        items = alerts[columns]
        items = items.sort_values("StartTimeUtc", ascending=True)
        self._select_items = items.apply(self._alert_summary, axis=1).values.tolist()

        self.selected_alert = None
        self.alert_id = None

        self._w_select_alert = widgets.Select(
            options=self._select_items,
            description="Select alert :",
            layout=Layout(width="95%", height="300px"),
            style={"description_width": "initial"},
        )

        self._w_filter_alerts = widgets.Text(
            value="",
            description="Filter alerts by title:",
            style={"description_width": "initial"},
        )

        # setup to use updatable display objects
        rand_id = random.randint(0, 999999)  # nosec
        self._output_id = f"{self.__class__.__name__}_{rand_id}"
        self._disp_elems: List[Any] = []

        # set up observer callbacks
        self._w_filter_alerts.observe(self._update_options, names="value")
        self._w_select_alert.observe(self._select_alert, names="value")

        if auto_display:
            self.display()

    def display(self):
        """Display the interactive widgets."""
        display(widgets.VBox([self._w_filter_alerts, self._w_select_alert]))
        display(HTML("<hr>"))
        self._select_top_alert()

    @staticmethod
    def _alert_summary(alert_row):
        """Return summarized string of alert properties."""
        if "TI Risk" in alert_row:
            return (
                f"{alert_row.StartTimeUtc} - {alert_row.AlertName}"
                + f" - ({alert_row.CompromisedEntity}) "
                + f" - TI Risk: {alert_row['TI Risk']}"
                + f" - [id:{alert_row.SystemAlertId}]",
                alert_row.SystemAlertId,
            )

        return (
            f"{alert_row.StartTimeUtc} - {alert_row.AlertName}"
            + f" - ({alert_row.CompromisedEntity}) "
            + f" - [id:{alert_row.SystemAlertId}]",
            alert_row.SystemAlertId,
        )

    def _update_options(self, change):
        """Filter the alert list by substring."""
        if change is not None and "new" in change:
            self._w_select_alert.options = [
                alert_dtl
                for alert_dtl in self._select_items
                if change["new"].lower() in alert_dtl[0].lower()
            ]

    def _select_alert(self, selection=None):
        """Select action triggered by picking item from list."""
        if (
            selection is None
            or "new" not in selection
            or not isinstance(selection["new"], str)
        ):
            self.selected_alert = None
        else:
            self.alert_id = selection["new"]
            self.selected_alert = self._get_alert(self.alert_id)
            if self.alert_action is not None:
                self._run_action()

    def _get_alert(self, alert_id):
        """Get the alert by alert_id."""
        self.alert_id = alert_id
        selected_alerts = self.alerts[self.alerts["SystemAlertId"] == alert_id]

        if selected_alerts.shape[0] > 0:
            alert = pd.Series(selected_alerts.iloc[0])
            if isinstance(alert["ExtendedProperties"], str):
                try:
                    alert["ExtendedProperties"] = json.loads(
                        (alert["ExtendedProperties"])
                    )
                except JSONDecodeError:
                    pass
            if isinstance(alert["Entities"], str):
                try:
                    alert["Entities"] = json.loads((alert["Entities"]))
                except JSONDecodeError:
                    pass
            return alert
        return None

    def _select_top_alert(self):
        """Select the first alert by default."""
        top_alert = self.alerts.iloc[0]
        if not top_alert.empty:
            self.alert_id = top_alert.SystemAlertId
            self.selected_alert = self._get_alert(self.alert_id)
            if self.alert_action is not None:
                self._run_action()

    def _run_action(self):
        """Run any action function and display details, if any."""
        output_objs = self.alert_action(self.selected_alert)
        if output_objs is None:
            return
        if not isinstance(output_objs, (tuple, list)):
            output_objs = [output_objs]
        display_objs = bool(self._disp_elems)
        for idx, out_obj in enumerate(output_objs):
            if not display_objs:
                self._disp_elems.append(
                    display(out_obj, display_id=f"{self._output_id}_{idx}")
                )
            else:
                if idx == len(self._disp_elems):
                    break
                self._disp_elems[idx].update(out_obj)

    def _ipython_display_(self):
        """Display in IPython."""
        self.display()


# pylint: disable=too-many-instance-attributes
@deprecated(
    reason="Superceded by SelectAlert. Please use that version", version="0.5.2"
)
@export
class AlertSelector(SelectAlert):
    """
    AlertSelector.

    View list of alerts and select one for investigation.
    Optionally provide and action to call with the selected alert as a parameter
    (typically used to display the alert.)

    Attributes
    ----------
    selected_alert : SecurityAlert
        The selected alert
    alert_id : str
        The SystemAlertId of the selected alert
    alerts : List[SecurityAlert]
        The current alert list (DataFrame)
    action : Callable[..., None]
        The callback action to execute on selection
        of an alert.

    """

    def __init__(
        self,
        alerts: pd.DataFrame,
        action: Callable[..., None] = None,
        columns: List[str] = None,
        auto_display: bool = False,
    ):
        """
        Create a new instance of AlertSelector.

        Parameters
        ----------
        alerts : pd.DataFrame
            DataFrame of alerts.
        action : Callable[..., None], optional
            Optional function to execute for each selected alert.
            (the default is None)
        columns : List[str], optional
            Override the default column names to use from `alerts`
            (the default is ['StartTimeUtc', 'AlertName',
            'CompromisedEntity', 'SystemAlertId'])
        auto_display : bool, optional
            Whether to display on instantiation (the default is False)

        """
        self._w_output = widgets.Output(layout={"border": "1px solid black"})
        super().__init__(alerts, action, columns, auto_display)

    def display(self):
        """Display the interactive widgets."""
        self._select_top_alert()
        display(
            widgets.VBox([self._w_filter_alerts, self._w_select_alert, self._w_output])
        )

    def _run_action(self):
        self._w_output.clear_output()
        with self._w_output:
            self.alert_action(self.selected_alert)

    @property
    def query_params(self):
        """
        Query parameters derived from alert.

        Returns
        -------
            dict(str, str) -- Dictionary of parameter names

        """
        return {"provider_alert_id": self.alert_id}


@export
class GetEnvironmentKey:
    """
    GetEnvironmentKey.

    Tries to retrieve an environment variable value. The value
    can be changed/set and optionally saved back to the system
    environment.
    """

    def __init__(
        self,
        env_var: str,
        help_str: str = None,
        prompt: str = "Enter the value: ",
        auto_display: bool = False,
    ):
        """
        Create a new instance of GetEnvironmentKey.

        Parameters
        ----------
        env_var : str
            Name of the environment variable.
        help_str : str, optional
            Help to display if the environment variable is not set. (the default is None)
        prompt : str, optional
            Prompt to display with the text box.
            (the default is "Enter the value: ")
        auto_display : bool, optional
            Whether to display on instantiation (the default is False)

        """
        self._value = os.environ.get(env_var)
        self._name = env_var

        if not self._value:
            display(widgets.HTML(value=help_str))

        self._w_text = widgets.Text(
            value=self._value,
            description=prompt,
            layout=Layout(width="50%"),
            style={"description_width": "initial"},
        )

        self._w_save_button = widgets.Button(
            description="Set",
            layout=Layout(width="10%", disabled=False),
            style={"description_width": "initial"},
        )
        self._w_check_save = widgets.Checkbox(
            value=True, description="Save as environment var", disabled=False
        )
        self._w_save_button.on_click(self._on_save_button_clicked)
        self._hbox = widgets.HBox(
            [self._w_text, self._w_save_button, self._w_check_save]
        )

        if auto_display:
            self.display()

    @property
    def value(self):
        """Get the current value of the key."""
        return self._w_text.value.strip()

    @property
    def name(self):
        """Get the current name of the key."""
        return self._name

    def display(self):
        """Display the interactive widgets."""
        display(self._hbox)

    def _on_save_button_clicked(self, button):
        del button
        if self._w_check_save.value:
            os.environ[self._name] = self._w_text.value.strip()

    def _ipython_display_(self):
        """Display in IPython."""
        self.display()


@export
class SelectItem:
    """
    Selection list from list or dict.

    Attributes
    ----------
    value : Any
        The selected value.
    item_action : Callable[..., Optional[Tuple]]
        Action to call for each selection.

    """

    # pylint: disable=too-many-arguments, too-few-public-methods
    def __init__(
        self,
        description: str = "Select an item",
        item_list: List[str] = None,
        action: Callable[..., Optional[Tuple]] = None,
        item_dict: Mapping[str, str] = None,
        auto_display: bool = False,
        height: str = "100px",
        width: str = "50%",
        display_filter: bool = True,
    ):
        """
        Select an item from a list or dict.

        Parameters
        ----------
        description : str, optional
            The widget label to display.
            (the default is 'Select an item')
        item_list : List[str], optional
            A `list` of items to select from (the default is None)
        item_dict : Mapping[str, str], optional
            A `dict` of items to select from. When using `item_dict`
            the keys are displayed as the selectable items and value
            corresponding to the selected key is set as the `value`
            property.
            (the default is None)
        action : Callable[..., Optional[Tuple[...]]], optional
            function to call when item selected (passed a single
            parameter - the value of the currently selected item)
            (the default is None).
            If the function returns one or a tuple of displayable objects
            these will be displayed.
        auto_display : bool, optional
            Whether to display on instantiation (the default is False)
        height : str, optional
            Selection list height (the default is '100px')
        width : str, optional
            Selection list width (the default is '50%')
        display_filter : bool, optional
            Whether to display item filter (the default is True)

        """
        if item_list:
            self._item_list = item_list
            self._item_dict = None
            self.value = item_list[0]
        elif item_dict:
            self._item_list = list(item_dict.keys())
            self._item_dict = item_dict
            self.value = list(self._item_dict.values())[0]
        else:
            raise ValueError("One of item_list or item_dict must be supplied.")

        self._wgt_select = widgets.Select(
            options=self._item_list,
            description=description,
            layout=Layout(width=width, height=height),
            style={"description_width": "initial"},
        )
        self._display_filter = display_filter
        if display_filter:
            self._w_filter = widgets.Text(
                value="", description="Filter:", style={"description_width": "initial"}
            )

            # set up observer callbacks
            self._w_filter.observe(self._update_options, names="value")
        self._wgt_select.observe(self._select_item, names="value")

        self.item_action = action

        # setup to use updatable display objects
        rand_id = random.randint(0, 999999)  # nosec
        self._output_id = f"{self.__class__.__name__}_{rand_id}"
        self._disp_elems: List[Any] = []

        if auto_display:
            self.display()

    def _select_item(self, selection):
        if (
            selection is None
            or "new" not in selection
            or not isinstance(selection["new"], str)
        ):
            return
        value = selection["new"]
        self.value = self._item_dict.get(value, None) if self._item_dict else value
        if self.item_action is not None:
            self._run_action()

    def _update_options(self, change):
        """Filter the alert list by substring."""
        if change is not None and "new" in change:
            self._wgt_select.options = [
                i for i in self._item_list if change["new"].lower() in i.lower()
            ]

    def _run_action(self):
        """Run any action function and display details, if any."""
        output_objs = self.item_action(self.value)
        if output_objs is None:
            return
        if not isinstance(output_objs, (tuple, list)):
            output_objs = [output_objs]
        display_objs = bool(self._disp_elems)
        for idx, out_obj in enumerate(output_objs):
            if not display_objs:
                self._disp_elems.append(
                    display(out_obj, display_id=f"{self._output_id}_{idx}")
                )
            else:
                if idx == len(self._disp_elems):
                    break
                self._disp_elems[idx].update(out_obj)

    def display(self):
        """Display the interactive widget."""
        wgt_list = []
        if self._display_filter:
            wgt_list.append(self._w_filter)
        wgt_list.append(self._wgt_select)
        display(widgets.VBox(wgt_list))
        display(HTML("<hr>"))
        self._show_top_item()

    def _show_top_item(self):
        """Run action on the first item by default."""
        if self.item_action is not None and self.value is not None:
            self._run_action()

    def _ipython_display_(self):
        """Display in IPython."""
        self.display()


@deprecated(reason="Superceded by SelectItem. Please use that version", version="0.5.2")
@export
class SelectString(SelectItem):
    """
    Selection list from list or dict.

    Attributes
    ----------
    value : Any
        The selected value.
    item_action : Callable[..., None]
        Action to call for each selection.

    """

    # pylint: disable=too-many-arguments, too-few-public-methods
    def __init__(
        self,
        description: str = "Select an item",
        item_list: List[str] = None,
        action: Callable[..., None] = None,
        item_dict: Mapping[str, str] = None,
        auto_display: bool = False,
        height: str = "100px",
        width: str = "50%",
        display_filter: bool = True,
    ):
        """
        Select an item from a list or dict.

        Parameters
        ----------
        description : str, optional
            The widget label to display.
            (the default is 'Select an item')
        item_list : List[str], optional
            A `list` of items to select from (the default is None)
        item_dict : Mapping[str, str], optional
            A `dict` of items to select from. When using `item_dict`
            the keys are displayed as the selectable items and value
            corresponding to the selected key is set as the `value`
            property.
            (the default is None)
        action : Callable[..., None], optional
            function to call when item selected (passed a single
            parameter - the value of the currently selected item)
            (the default is None)
        auto_display : bool, optional
            Whether to display on instantiation (the default is False)
        height : str, optional
            Selection list height (the default is '100px')
        width : str, optional
            Selection list width (the default is '50%')
        display_filter : bool, optional
            Whether to display item filter (the default is True)

        """
        self._w_output = widgets.Output(layout={"border": "1px solid black"})

        super().__init__(
            description=description,
            item_list=item_list,
            item_dict=item_dict,
            action=action,
            auto_display=auto_display,
            height=height,
            width=width,
            display_filter=display_filter,
        )

    def _run_action(self):
        self._w_output.clear_output()
        with self._w_output:
            self.item_action(self.value)

    def display(self):
        """Display the interactive widget."""
        self._show_top_item()
        wgt_list = []
        if self._display_filter:
            wgt_list.append(self._w_filter)
        wgt_list.append(self._wgt_select)
        if self.item_action:
            wgt_list.append(self._w_output)
        display(widgets.VBox(wgt_list))


@export
class SelectSubset:
    """
    Class to select a subset from an input list.

    Attributes
    ----------
    selected_values : List[Any]
        The selected item values.
    selected_items : List[Any]
        The selected items label and value

    """

    def __init__(
        self,
        source_items: Union[Dict[str, str], List[Any]],
        default_selected: Union[Dict[str, str], List[Any]] = None,
        display_filter: bool = True,
    ):
        """
        Create instance of SelectSubset widget.

        Parameters
        ----------
        source_items : Union[Dict[str, str], List[Any]]
            List of source items - either a dictionary(label, value),
            a simple list or
            a list of (label, value) tuples.
        default_selected : Union[Dict[str, str], List[Any]]
            Populate the selected list with values - either
            a dictionary(label, value),
            a simple list or
            a list of (label, value) tuples.
        display_filter : bool, optional
            Whether to display item filter (the default is True)

        """
        if isinstance(source_items, dict):
            source_items = list(source_items.items())

        self.src_items = sorted(set(source_items))
        if isinstance(self.src_items[0], tuple):
            self._src_dict = {val: (label, val) for label, val in self.src_items}
        else:
            self._src_dict = {}

        layout = widgets.Layout(width="40%", height="200px")
        self._source_list = widgets.SelectMultiple(
            options=sorted(set(self.src_items)), layout=layout, description="Source: "
        )

        if isinstance(default_selected, dict):
            default_selected = list(default_selected.items())
        if default_selected:
            set_selected = set(default_selected)
            selected_items = sorted(set_selected.intersection(source_items))
        else:
            selected_items = []

        self._select_list = widgets.SelectMultiple(
            options=selected_items, layout=layout, description="Selected: "
        )

        self._display_filter = display_filter
        if display_filter:
            self._w_filter = widgets.Text(
                value="", description="Filter:", style={"description_width": "initial"}
            )

            # set up observer callbacks
            self._w_filter.observe(self._update_options, names="value")

        self._b_add_all = widgets.Button(description="Add All \u21fe")
        self._b_add = widgets.Button(description="Add \u21fe")
        self._b_del = widgets.Button(description="\u21fd Remove")
        self._b_del_all = widgets.Button(description="\u21fd Remove All")

        self._b_add.on_click(self._on_btn_add)
        self._b_del.on_click(self._on_btn_del)
        self._b_del_all.on_click(self._on_btn_del_all)
        self._b_add_all.on_click(self._on_btn_add_all)

        v_box = widgets.VBox(
            [self._b_add_all, self._b_add, self._b_del, self._b_del_all]
        )
        self.layout = widgets.HBox([self._source_list, v_box, self._select_list])
        if self._display_filter:
            self.layout = widgets.VBox([self._w_filter, self.layout])
        display(self.layout)

    @property
    def selected_items(self) -> List[Any]:
        """
        Return a list of the selected items.

        If the input list is a list of tuples, this returns
        a list of the selected tuples.

        Returns
        -------
        List[Any]
            List of items in the selected list.

        """
        return list(self._select_list.options)

    @property
    def selected_values(self) -> List[Any]:
        """
        Return list of selected values.

        If the input list is a list of tuples, this returns
        a list of values of the items.

        Returns
        -------
        List[Any]
            List of selected item values.

        """
        if self._select_list.options and isinstance(
            self._select_list.options[0], tuple
        ):
            return [item[1] for item in self._select_list.options]
        return self.selected_items

    def _update_options(self, change):
        """Filter the alert list by substring."""
        if change is not None and "new" in change:
            self._source_list.options = sorted(
                {
                    i
                    for i in self.src_items
                    if str(change["new"]).lower() in str(i).lower()
                }
            )

    # pylint: disable=not-an-iterable
    def _on_btn_add(self, button):
        del button
        selected_set = set(self._select_list.options)
        for selected in self._source_list.value:
            if self._src_dict:
                selected_set.add(self._src_dict[selected])
            else:
                selected_set.add(selected)
        self._select_list.options = sorted(list(selected_set))

    def _on_btn_add_all(self, button):
        del button
        self._select_list.options = sorted(list(set(self._source_list.options)))

    def _on_btn_del(self, button):
        del button
        selected_set = set(self._select_list.options)
        # save the current index
        cur_index = max(self._select_list.index)
        if selected_set:
            if self._src_dict:
                # if we're working with tuples, we need to specify the tuple to remove
                for selected in self._select_list.value:
                    selected_set.remove(self._src_dict[selected])
            else:
                # else just delete the value
                for selected in self._select_list.value:
                    selected_set.remove(selected)
            self._select_list.options = sorted(list(selected_set))
        if not self._select_list.options:
            return
        # try to set the index to the next item in the list
        if cur_index < len(self._select_list.options):
            next_item = cur_index if cur_index else 0
            self._select_list.index = tuple([next_item])
        else:
            last_item = max(len(self._select_list.options) - 1, 0)
            self._select_list.index = tuple([last_item])

    # pylint: enable=not-an-iterable

    def _on_btn_del_all(self, button):
        del button
        self._select_list.options = []

    def display(self):
        """Display the control."""
        display(self.layout)

    def _ipython_display_(self):
        """Display in IPython."""
        self.display()


@export
class Progress:
    """UI Progress bar."""

    def __init__(self, completed_len: int, visible: bool = True):
        """
        Instantiate new _Progress UI.

        Parameters
        ----------
        completed_len : int
            The expected value that indicates 100% done.
        visible : bool
            If True start the progress UI visible, by default True.

        """
        self._completed = 0
        self._total = completed_len
        self._progress = widgets.IntProgress(
            value=0,
            max=100,
            step=1,
            description="Progress:",
            bar_style="info",
            orientation="horizontal",
        )
        self._done_label = widgets.Label(value="0%")
        self._progress.visible = visible
        self._done_label.visible = visible
        self.layout = widgets.HBox([self._progress, self._done_label])
        self.display()

    @property
    def value(self) -> int:
        """
        Return the current progress value.

        Returns
        -------
        int
            Progess value

        """
        return self._completed

    @property
    def max(self) -> int:
        """
        Return the current progress maximum value.

        Returns
        -------
        int
            Max value

        """
        return self._total

    def update_progress(self, new_total: int = 0, delta: int = 0):
        """
        Update progress UI by increment or new total.

        Parameters
        ----------
        new_total : int, optional
            New total, by default 0
        delta : int, optional
            Increment to update current total, by default 0

        """
        if new_total:
            self._completed = new_total
        else:
            self._completed += delta
        perc_total = int(100 * self._completed / self._total)
        self._progress.value = perc_total
        self._done_label.value = f"{perc_total}%"

    def show(self):
        """Make the controls visible."""
        self._progress.visible = True
        self._done_label.visible = True

    def hide(self):
        """Hide the controls."""
        self._progress.visible = True
        self._done_label.visible = True

    def display(self):
        """Display the control."""
        display(self.layout)

    def _ipython_display_(self):
        """Display in IPython."""
        self.display()
