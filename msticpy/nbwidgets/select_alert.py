# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Module for pre-defined widget layouts."""

import contextlib
import json
import random
from json import JSONDecodeError
from typing import Any, Callable, List, Optional, Tuple

import ipywidgets as widgets
import pandas as pd
from deprecated.sphinx import deprecated
from IPython.display import HTML, display
from ipywidgets import Layout

from .._version import VERSION
from .core import IPyDisplayMixin

__version__ = VERSION
__author__ = "Ian Hellen"


# pylint: disable=too-many-instance-attributes
class SelectAlert(IPyDisplayMixin):
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
        id_col: str = "SystemAlertId",
        **kwargs,
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
            Override the default column names to use from `alerts` to
            generate the select list item summary
            (the default is ['AlertName', 'ProductName'])
        time_col : str, optional
            The column in your alerts that determines when it was created
            Default is 'StartTimeUtc' with a fallback to 'TimeGenerated'.
        id_col : str, optional
            The column in your data that determines the alert id
            Default is 'SystemAlertId'.
        auto_display : bool, optional
            Whether to display on instantiation (the default is False)
        default_alert: str, optional
            If you want to select a default value provide the ID.

        """
        self.alerts = alerts
        self.alert_action = action
        self.id_col = id_col
        time_col = kwargs.get("time_col", "TimeGenerated")
        self.time_col = time_col if time_col in alerts.columns else "TimeGenerated"
        self.default_alert = kwargs.get("default_alert", None)

        columns = columns or ["AlertName", "ProductName"]
        self.disp_columns = list({col for col in columns if col in alerts.columns})
        if not self.disp_columns:
            raise ValueError(
                f"Display columns {','.join(columns)} not found in alerts."
            )
        self._select_items = self._get_select_options(
            alerts, self.time_col, self.id_col, self.disp_columns
        )

        self.selected_alert = None
        self.alert_id = None

        self._w_select_alert = widgets.Select(
            options=self._select_items,
            description="Select alert :",
            layout=Layout(width="95%", height="300px"),
            style={"description_width": "initial"},
            value=self.default_alert,
        )
        self._w_filter_alerts = widgets.Text(
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
        wgt_list = [self._w_filter_alerts, self._w_select_alert]
        self._w_display_details = widgets.Checkbox(
            value=True,
            description="Display details",
        )
        if action:
            self._w_display_details.observe(self._run_action, names="value")
            wgt_list.append(self._w_display_details)
        self.layout = widgets.VBox(wgt_list)

        if auto_display:
            self.display()

    @property
    def value(self):
        """Return selected alert."""
        return self.selected_alert

    def display(self):
        """Display the interactive widgets."""
        super().display()
        self._select_top_alert()

    @staticmethod
    def _alert_summary(alert_row, time_col, id_col, columns):
        """Return summarized string of alert properties."""
        items = [f"{alert_row[time_col]}"]
        items.extend(alert_row[col] for col in columns)
        items.append(alert_row[id_col])
        return " || ".join(items)

    @staticmethod
    def _get_select_options(data, time_col, id_col, columns):
        """Return dictionary of displayable options."""
        data_cols = list({time_col, id_col, *columns})
        return {
            val: key
            for key, val in data[data_cols]
            .sort_values(time_col, ascending=True)
            .set_index(id_col)
            .apply(lambda x: " - ".join(str(c) for c in x), axis=1)
            .to_dict()
            .items()
        }

    def _update_options(self, change):
        """Filter the alert list by substring."""
        if change is not None and "new" in change:
            self._w_select_alert.options = {
                alert_desc: alert_id
                for alert_desc, alert_id in self._select_items.items()
                if change["new"].casefold() in alert_desc.casefold()
            }

    def _select_alert(self, selection=None):
        """Select action triggered by picking item from list."""
        if (
            selection is None
            or "new" not in selection
            or not isinstance(selection["new"], str)
        ):
            self.selected_alert = None
        else:
            self.alert_id = selection.new

            self.selected_alert = self._get_alert(self.alert_id)
            if self.alert_action is not None:
                self._run_action()

    def _get_alert(self, alert_id):
        """Get the alert by alert_id."""
        self.alert_id = alert_id
        selected_alerts = self.alerts[self.alerts[self.id_col] == alert_id]
        if selected_alerts.shape[0] > 0:
            alert = pd.Series(selected_alerts.iloc[0])
            if "ExtendedProperties" in alert.index and isinstance(
                alert["ExtendedProperties"], str
            ):
                with contextlib.suppress(JSONDecodeError):
                    alert["ExtendedProperties"] = json.loads(
                        (alert["ExtendedProperties"])
                    )
            if "Entities" in alert.index and isinstance(alert["Entities"], str):
                with contextlib.suppress(JSONDecodeError):
                    alert["Entities"] = json.loads((alert["Entities"]))
            return alert
        return None

    def _select_top_alert(self):
        """Select the first alert by default."""
        top_alert = self.alerts.iloc[0]
        if self.default_alert:
            top_alert = self.alerts[
                self.alerts[self.id_col] == self.default_alert
            ].iloc[0]
        if not top_alert.empty:
            self._w_select_alert.index = 0
            self.alert_id = top_alert[self.id_col]
            self.selected_alert = self._get_alert(self.alert_id)
            if self.alert_action is not None:
                self._run_action()

    def _run_action(self, change=None):
        """Run any action function and display details, if any."""
        del change
        output_objs = None
        if self._w_display_details.value:
            output_objs = self.alert_action(self.selected_alert)
        if output_objs is None:
            self._clear_display()
            return
        if not isinstance(output_objs, (tuple, list)):
            output_objs = [output_objs]
        display_objs = bool(self._disp_elems)
        for idx, out_obj in enumerate(output_objs):
            if not display_objs:
                self._disp_elems.append(
                    display(out_obj, display_id=f"{self._output_id}_{idx}")
                )
            elif idx == len(self._disp_elems):
                break
            else:
                self._disp_elems[idx].update(out_obj)

    def _clear_display(self):
        """Clear any current details."""
        if not self._disp_elems:
            return
        for disp_obj in self._disp_elems:
            disp_obj.update(HTML(""))


# pylint: disable=too-many-instance-attributes
@deprecated(
    reason="Superceded by SelectAlert. Will be removed in v2.0.0.", version="0.5.2"
)
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
        super().__init__(
            alerts=alerts, action=action, columns=columns, auto_display=auto_display
        )

    def display(self):
        """Display the interactive widgets."""
        self._select_top_alert()
        display(
            widgets.VBox([self._w_filter_alerts, self._w_select_alert, self._w_output])
        )

    def _run_action(self, change=None):
        del change
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
