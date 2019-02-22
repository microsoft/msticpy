# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Module for pre-defined widget layouts."""

import json
from json import JSONDecodeError
import os
import re
from datetime import datetime, timedelta
from enum import Enum
from typing import Callable

import pandas as pd
from IPython.display import display
import ipywidgets as widgets
from ipywidgets import Layout

from . import kql as qry
from .query_defns import QueryParamProvider
from .utility import export
from .. _version import VERSION

__version__ = VERSION
__author__ = 'Ian Hellen'


class TimeUnit(Enum):
    """Time unit enumeration and value."""

    sec = 1
    min = 60
    hour = 60 * 60
    day = 60 * 60 * 24


def _parse_time_unit(unit_str: str) -> TimeUnit:
    """Return the TimeUnit enum matching the input string."""
    if unit_str.startswith('m'):
        return TimeUnit.min
    if unit_str.startswith('h'):
        return TimeUnit.hour
    if unit_str.startswith('s'):
        return TimeUnit.sec
    if unit_str.startswith('d'):
        return TimeUnit.day
    return TimeUnit.min


@export
class Lookback(QueryParamProvider):
    """
    ipwidget wrapper to display integer slider.

    Attributes:
        lookback: the time value of the slider
        value: synonym for lookback

    """

    def __init__(self, default: int = 4, label: str = 'Select time ({units}) to look back',
                 origin_time: datetime = None, min_value: int = 1, max_value: int = 240,
                 units: str = 'hour', auto_display: bool = False):
        """
        Create an instance of the lookback slider widget.

            :param default=4: Default value.
            :param label='Select time (hrs) to look back': prompt string
            :param min_value=1: Minimum value of range.
            :param max_value=240: Maximum value of range.
            :param origin_time: The time from which to calculate the lookback offset
        """
        # default to now
        self.origin_time = datetime.utcnow() if origin_time is None else origin_time

        self._time_unit = _parse_time_unit(units)
        if '{units}' in label:
            label = label.format(units=self._time_unit)
        self._lookback_wgt = widgets.IntSlider(value=default,
                                               min=min_value,
                                               max=max_value,
                                               step=1,
                                               description=label,
                                               layout=Layout(
                                                   width='60%', height='50px'),
                                               style={'description_width': 'initial'})

        self.end = datetime.utcnow
        self._time_unit = _parse_time_unit(units)
        self.start = self.end - timedelta(self._time_unit * self._lookback_wgt.value)

        self._lookback_wgt.observe(self._time_range_change, names='value')

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
        self.start = (self.origin_time +
                      timedelta(0, self._lookback_wgt.value[0] * self._time_unit.value))
        self.end = (self.origin_time +
                    timedelta(0, self._lookback_wgt.value[1] * self._time_unit.value))

    @property
    def query_params(self):
        """
        Query parameters derived from alert.

        Returns:
            dict(str, str) -- Dictionary of parameter names

        """
        return {
            'start': self.start,
            'end': self.end
        }


@export
class QueryTime(QueryParamProvider):
    """
    QueryTime.

    Composite widget to capture date and time origin
    and set start and end times for queries.

    Atrributes:
        start: the selected query start time
        end: the selected query end time

    """

    _label_style = {'description_width': 'initial'}

    def __init__(self, origin_time: datetime = None, before: int = 60, after: int = 10,
                 max_before: int = 600, max_after: int = 100, label: str = None,
                 units: str = 'min', auto_display: bool = False):
        """
        Create new instance of QueryTime.

            :param origin_time:datetime=None: The starting time for the time range widget
            defaults to datetime.utcnow()
            :param before=60: The default start time offset from origin_time
            :param after=10: The default end time offset from origin_time
            :param max_before=600: The maximum offset value for the start time range
            :param max_after=100: The maximum offset value for the end time range
            :param label=None: override the default label 'Set query time boundaries'
            :param units='min: time unit to use ('min', 'hour', 'sec', 'day')
        """
        self._label = 'Set query time boundaries' if label is None else label
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
        self._query_start = self.origin_time - \
            timedelta(0, before * self._time_unit.value)
        self._query_end = self.origin_time + \
            timedelta(0, after * self._time_unit.value)

        # Create widgets
        self._w_origin_dt = widgets.DatePicker(description='Origin Date',
                                               disabled=False,
                                               value=self.origin_time.date())
        self._w_origin_tm = widgets.Text(description='Time (24hr)',
                                         disabled=False,
                                         value=str(self.origin_time.time()))

        range_desc = 'Time Range ({}):'.format(self._time_unit.name)
        self._w_tm_range = widgets.IntRangeSlider(value=[-before, after],
                                                  min=-max_before,
                                                  max=max_after,
                                                  step=1,
                                                  description=range_desc,
                                                  disabled=False,
                                                  continuous_update=True,
                                                  orientation='horizontal',
                                                  readout=True, readout_format='d',
                                                  layout=Layout(width='80%'),
                                                  style=self._label_style)

        self._w_start_time_txt = widgets.Text(value=self._query_start.isoformat(sep=' '),
                                              description='Query start time (UTC):',
                                              layout=Layout(width='50%'),
                                              style=self._label_style)
        self._w_end_time_txt = widgets.Text(value=self._query_end.isoformat(sep=' '),
                                            description='Query end time (UTC) :  ',
                                            layout=Layout(width='50%'),
                                            style=self._label_style)

        self._w_tm_range.observe(self._time_range_change, names='value')
        self._w_origin_dt.observe(self._update_origin, names='value')
        self._w_origin_tm.observe(self._update_origin, names='value')

        if auto_display:
            self.display()

    def display(self):
        """Display the interactive widgets."""
        display(widgets.HTML('<h4>{}</h4>'.format(self._label)))
        display(widgets.HBox([self._w_origin_dt, self._w_origin_tm]))
        display(widgets.VBox([self._w_tm_range,
                              self._w_start_time_txt,
                              self._w_end_time_txt]))

    # pylint: disable=locally-disabled, W0613
    def _update_origin(self, change):
        try:
            tm_value = datetime.strptime(
                self._w_origin_tm.value, '%H:%M:%S.%f').time()
            self.origin_time = datetime.combine(
                self._w_origin_dt.value, tm_value)
            self._time_range_change(change=None)
        except ValueError:
            pass

    def _time_range_change(self, change):
        self._query_start = (self.origin_time +
                             timedelta(0, self._w_tm_range.value[0] * self._time_unit.value))
        self._query_end = (self.origin_time +
                           timedelta(0, self._w_tm_range.value[1] * self._time_unit.value))
        self._w_start_time_txt.value = self._query_start.isoformat(sep=' ')
        self._w_end_time_txt.value = self._query_end.isoformat(sep=' ')

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

        Returns:
            dict(str, str) -- Dictionary of parameter names

        """
        return {
            'start': self.start,
            'end': self.end
        }


@export
class AlertSelector(QueryParamProvider):
    """
    AlertSelector.

    View list of alerts and select one for investigation.
    Optionally provide and action to call with the selected alert as a parameter
    (typically used to display the alert.)

    Attributes:
        selected_alert: the selected alert
        alert_id: the ID of the selected alert
        alerts: the current alert list (DataFrame)

    """

    _ALERTID_REGEX = r'\[id:(?P<alert_id>.*)\]$'

    def __init__(self, alerts: pd.DataFrame, action: Callable[..., None] = None,
                 columns: list({str})=None, auto_display: bool = False):
        """
        Create a new instance of AlertSelector.

            :param alerts: DataFrame of alerts.
            :param action=None: Optional function to execute for each selected alert.
        """
        self.alerts = alerts
        self.alert_action = action

        if not columns:
            columns = ['StartTimeUtc', 'AlertName',
                       'CompromisedEntity', 'SystemAlertId']
        items = alerts[columns]
        items = items.sort_values('StartTimeUtc', ascending=True)
        self._select_items = \
            items.apply(self._alert_summary, axis=1).values.tolist()

        self.selected_alert = None
        self.alert_id = None

        self._w_select_alert = widgets.Select(options=self._select_items,
                                              description='Select alert :',
                                              layout=Layout(
                                                  width='95%', height='300px'),
                                              style={'description_width': 'initial'})

        self._w_filter_alerts = widgets.Text(value='', description='Filter alerts by title:',
                                             style={'description_width': 'initial'})
        self._w_output = widgets.Output(layout={'border': '1px solid black'})

        # set up observer callbacks
        self._w_filter_alerts.observe(self._update_options, names='value')
        self._w_select_alert.observe(self._select_alert, names='value')

        if auto_display:
            self.display()

    def display(self):
        """Display the interactive widgets."""
        self._select_top_alert()
        display(widgets.VBox([self._w_filter_alerts,
                              self._w_select_alert,
                              self._w_output]))

    def _alert_summary(self, alert_row):
        """Return summarized string of alert properties."""
        return '{time}  {alert} ({host}) [id:{id}]'.format(time=alert_row.StartTimeUtc,
                                                           alert=alert_row.AlertName,
                                                           host=alert_row.CompromisedEntity,
                                                           id=alert_row.SystemAlertId)

    def _update_options(self, change):
        """Filter the alert list by substring."""
        if change is not None and 'new' in change:
            self._w_select_alert.options = [
                i for i in self._select_items if change['new'].lower() in i.lower()]

    def _select_alert(self, selection=None):
        """Select action triggered by picking item from list."""
        if (selection is None or 'new' not in selection or
                not isinstance(selection['new'], str)):
            self.selected_alert = None
        else:
            match = re.search(self._ALERTID_REGEX, selection['new'])
            if match is not None:
                self.alert_id = match.groupdict()['alert_id']
                self.selected_alert = self._get_alert(self.alert_id)
                if self.alert_action is not None:
                    self._w_output.clear_output()
                    with self._w_output:
                        self.alert_action(self.selected_alert)

    def _get_alert(self, alert_id):
        """Get the alert by alert_id."""
        self.alert_id = alert_id
        selected_alerts = self.alerts[self.alerts['SystemAlertId'] == alert_id]

        if selected_alerts.shape[0] > 0:
            alert = pd.Series(selected_alerts.iloc[0])
            if isinstance(alert['ExtendedProperties'], str):
                alert['ExtendedProperties'] = json.loads(
                    (alert['ExtendedProperties']))
            if isinstance(alert['Entities'], str):
                try:
                    alert['Entities'] = json.loads((alert['Entities']))
                except JSONDecodeError:
                    pass
            return alert

    def _select_top_alert(self):
        """Select the first alert by default."""
        top_alert = self.alerts.iloc[0]
        if len(top_alert) == 1:
            self.alert_id = top_alert.SystemAlertId
            self.selected_alert = self._get_alert(self.alert_id)
            if self.alert_action is not None:
                self._w_output.clear_output()
                with self._w_output:
                    self.alert_action(self.selected_alert)

    @property
    def query_params(self):
        """
        Query parameters derived from alert.

        Returns:
            dict(str, str) -- Dictionary of parameter names

        """
        return {
            'provider_alert_id': self.alert_id
        }


@export
class GetSingleAlert(QueryParamProvider):
    """
    GetSingleAlert.

    Try to fetch a single alert by SystemAlertId.

    Attributes:
        selected_alert: the selected alert
        alert_id: the ID of the selected alert
        alerts: the current alert list (DataFrame)

    """

    def __init__(self, action: Callable[..., None] = None, max_lookback: int = 28,
                 query_time_provider=None, auto_display: bool = False):
        """
        Create a new instance of GetSingleAlert.

            :param action=None: Optional function to execute for retrieved alert.
            :param max_lookback: days
            :param query_time_provider - an object with 'start' and 'end' properties
                or a QueryParamProvider with start and end defined in its
                query_params property
        """
        self.alert_action = action
        self.selected_alert = None
        self.alert_id = None
        self.alerts = None

        self._start = None
        self._end = None
        if query_time_provider is not None:
            if ('start' in query_time_provider.__dir__ and
                    'end' in query_time_provider.__dir__):
                self._start = query_time_provider.start
                self._end = query_time_provider.end
            elif isinstance(query_time_provider, QueryParamProvider):
                self._start = query_time_provider.query_params.get('start', None)
                self._end = query_time_provider.query_params.get('end', None)

        if self._end is None:
            self._end = datetime.now()
            self._start = self._end - timedelta(max_lookback)

        self._w_target_alert = widgets.Text(
            value=self.alert_id,
            placeholder='SystemAlertId',
            description='SystemAlertId for alert :',
            layout=Layout(width='50%'),
            style={'description_width': 'initial'})

        self._w_fetch_button = widgets.Button(description="Get alert..")
        self._w_fetch_button.on_click(self._click_get_alert)

        self._w_output = widgets.Output(layout={'border': '1px solid black'})

        if auto_display:
            self.display()

    @property
    def query_params(self):
        """
        Query parameters derived from alert.

        Returns:
            dict(str, str) -- Dictionary of parameter names

        """
        return {
            'provider_alert_id': self.alert_id
        }

    def display(self):
        """Display the interactive widgets."""
        display(widgets.VBox([self._w_target_alert,
                              self._w_fetch_button,
                              self._w_output]))

    def _click_get_alert(self, button):
        del button
        self.alert_id = self._w_target_alert.value
        if not self.alert_id or not self.alert_id.strip():
            print('Error: AlertID was not entered')

        self.alerts = qry.exec_query(query_name='get_alert',
                                     start=self._start,
                                     end=self._end,
                                     system_alert_id=self.alert_id)
        if self.alerts is not None:
            self.selected_alert = self._get_alert(self.alert_id)
            if self.alert_action is not None:
                self._w_output.clear_output()
                with self._w_output:
                    self.alert_action(self.selected_alert)
        else:
            print('Alert not found.')

    def _get_alert(self, alert_id):
        self.alert_id = alert_id
        selected_alerts = self.alerts[self.alerts['SystemAlertId'] == alert_id]

        if selected_alerts.shape[0] > 0:
            alert = pd.Series(selected_alerts.iloc[0])
            if isinstance(alert['ExtendedProperties'], str):
                alert['ExtendedProperties'] = json.loads(
                    (alert['ExtendedProperties']))
            if isinstance(alert['Entities'], str):
                try:
                    alert['Entities'] = json.loads((alert['Entities']))
                except JSONDecodeError:
                    pass
            return alert


@export
class GetEnvironmentKey(object):
    """
    GetEnvironmentKey.

    Tries to retrieve an environment variable value. The value
    can be changed/set and optionally saved back to the system
    environment.

    Attributes:
        name: the name of the environment variable
        value: the value of the variable

    """

    def __init__(self, env_var: str, help_str: str = None, prompt: str = "Enter the value: ",
                 auto_display: bool = False):
        """
        Create a new instance of GetEnvironmentKey.

            :param env_var: Name of the environment variable.
            :param help_str=None: Help to display if the environment variable is not set.
            :param prompt="Enter the value:": Prompt to display with the text box.
        """
        self._value = os.environ.get(env_var)
        self._name = env_var

        if not self._value:
            display(widgets.HTML(value=help_str))

        self._w_text = widgets.Text(value=self._value,
                                    description=prompt, layout=Layout(
                                        width='50%'),
                                    style={'description_width': 'initial'})

        self._w_save_button = widgets.Button(description='Set',
                                             layout=Layout(
                                                 width='10%', disabled=False),
                                             style={'description_width': 'initial'})
        self._w_check_save = widgets.Checkbox(value=True,
                                              description='Save as environment var',
                                              disabled=False)
        self._w_save_button.on_click(self._on_save_button_clicked)
        self._hbox = widgets.HBox(
            [self._w_text, self._w_save_button, self._w_check_save])

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

    # pylint: disable=locally-disabled, W0613
    def _on_save_button_clicked(self, button):
        if self._w_check_save.value:
            os.environ[self._name] = self._w_text.value.strip()


@export
class SelectString(object):
    """
    Selection list from list or dict.

    Attributes:
        value : The selected value.

    """

    def __init__(self, description: str = None,
                 item_list: list({str})=None,
                 action: Callable[..., None] = None,
                 item_dict: dict({str: str})=None,
                 auto_display: bool = False,
                 height: str = '100px',
                 width: str = '50%'):
        """
        Initialize and display list picker.

            :param description=None: List label
            :param item_list=None: Item List
            :param item_dict=None: Item dictionary { display_string: value }
            :param action=None: function to call when item selected
            :param height='100px': height of list box
            :param width='50%': width of list box
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
            raise ValueError(
                "One of item_list or item_dict must be supplied.")

        self.item_action = action
        self._wgt_select = widgets.Select(options=self._item_list,
                                          description=description,
                                          layout=Layout(
                                              width=width, height=height),
                                          style={'description_width': 'initial'})
        self._wgt_select.observe(self._select_item, names='value')

        if auto_display:
            self.display()

    def _select_item(self, selection):
        if (selection is None or 'new' not in selection or
                not isinstance(selection['new'], str)):
            return
        value = selection['new']

        if self._item_dict:
            self.value = self._item_dict.get(value, None)
        else:
            self.value = value

        if self.item_action is not None:
            self.item_action(self.value)

    def display(self):
        """Display the interactive widget."""
        display(self._wgt_select)
