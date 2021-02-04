# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Module docstring."""
import ipywidgets as widgets

from .._version import VERSION
from ..data import DataEnvironment
from .ce_common import LIST_LAYOUT
from .comp_edit import CEItemsBase
from .compound_ctrls import UserDefLoadComponent, UserDefQryProvCtrl
from .mp_config_control import MpConfigControls, get_or_create_mpc_section

__version__ = VERSION
__author__ = "Ian Hellen"


# pylint: disable=too-many-ancestors
class CEAutoLoadQProvs(CEItemsBase):
    """User Default query providers edit component."""

    _COMP_PATH = "UserDefaults.QueryProviders"
    _DESCRIPTION = "User Defaults - Auto-loaded query providers"
    _HELP_TEXT = """
    Choose the query providers to load when you run nbinit.init_notebook().<br>

    There are two classes of providers - AzureSentinel workspaces and other
    providers.
    In the former case you must specify a workspace to load (by default it is
    the "Default" workspace).
    Other providers typically only have a single profile so there is no analgous
    setting<br><br>

    Query providers have two optional parameters:
    <ul>
        <li><b>alias</b> - upon loading, msticpy will create a variable based on
        this alias, prefixed with "qry_". E.g. if you set alias="local" a variable
        named "qry_local" is created in the Python/Jupyter namespace. You can use
        this variable to execute queries for this provider.
        </li>
        <li><b>connect</b> - the default behavior is to connect/authenticate
        this provider after loading it. Set this to False if you do not want
        to authenticate immediately.
        </li>
    </ul>
    """
    _HELP_URI = {
        "User Defaults": (
            "https://msticpy.readthedocs.io/en/latest/getting_started/"
            + "msticpyconfig.html#user-defaults"
        )
    }

    def __init__(self, mp_controls: MpConfigControls):
        """
        Initialize an instance of CEAutoLoad class.

        Parameters
        ----------
        mp_controls : MpConfigControls
            The config/controls/settings database

        """
        super().__init__(mp_controls)

        get_or_create_mpc_section(self.mp_controls, self._COMP_PATH)
        self.select_item.options = self._get_select_opts()
        self.select_item.layout = LIST_LAYOUT["layout"]
        self.select_item.style = LIST_LAYOUT["style"]
        self.select_item.description = "Providers"
        self.select_item.observe(self._select_provider, names="label")

        self.prov_options = widgets.Dropdown(
            options=self._get_available_options(),
            description="New prov",
            value=self.select_item.label,
            style=LIST_LAYOUT["style"],
        )
        self.items_frame.children = [*(self.items_frame.children), self.prov_options]

        prov_name = self.select_item.label
        self.edit_ctrls = self._get_settings_ctrls(prov_name, self._COMP_PATH).layout
        self.edit_frame.children = [self.edit_ctrls]

        self.edit_buttons.btn_del.on_click(self._del_component)
        self.edit_buttons.btn_add.on_click(self._add_component)
        self.edit_buttons.btn_save.on_click(self._save_component)

    def _select_provider(self, change):
        prov_name = change.get("new")
        self.edit_ctrls = self._get_settings_ctrls(prov_name, self._COMP_PATH).layout
        self.edit_frame.children = [self.edit_ctrls]
        self.mp_controls.populate_ctrl_values(f"{self._COMP_PATH}.{prov_name}")

    def _add_component(self, btn):
        del btn
        comp_name = self.prov_options.value
        if comp_name in self.select_item.options:
            self.set_status(f"This provider already exists: {comp_name}")
            return

        def_value = self._get_default_values(comp_name, self._COMP_PATH)
        if def_value is None:
            self.set_status(f"No definition for this provider was found: {comp_name}")
            return

        self.select_item.options = sorted([comp_name, *(self.select_item.options)])
        self.select_item.label = comp_name

    def _del_component(self, btn):
        del btn
        curr_provider = self.select_item.label
        prov_path = f"{self._COMP_PATH}.{curr_provider}"
        self.mp_controls.del_value(prov_path)
        remaining_opts = self._get_select_opts()
        self.select_item.options = remaining_opts
        if remaining_opts:
            self.select_item.label = remaining_opts[-1][0]

    def _save_component(self, btn):
        del btn
        comp_path = f"{self._COMP_PATH}.{self.select_item.label}"
        self.mp_controls.save_ctrl_values(comp_path)

    def _get_available_options(self):
        opt_list = [
            f"AzureSentinel.{ws}"
            for ws in self.mp_controls.mp_config.get("AzureSentinel", {}).get(
                "Workspaces"
            )
        ]
        opt_list.extend(
            {
                DataEnvironment.parse(prov).name
                for prov in DataEnvironment.__members__
                if prov not in ("Unknown", "Kusto", "AzureSentinel", "LogAnalytics")
            }
        )
        return opt_list

    def _get_select_opts(self):
        return list(self._get_query_providers())

    def _get_query_providers(self):
        for name, settings in self.mp_controls.get_value(self._COMP_PATH).items():
            if name == "AzureSentinel":
                yield from (f"{name}.{wkspace}" for wkspace in settings)
            else:
                yield name

    def _get_settings_ctrls(self, prov_name, conf_path):
        if not prov_name:
            return widgets.VBox([], layout=self.no_border_layout("95%"))

        setting_path = f"{conf_path}.{prov_name}"
        prov_ctrl = self.mp_controls.get_control(setting_path)
        if not isinstance(prov_ctrl, UserDefQryProvCtrl):
            prov_ctrl = UserDefQryProvCtrl(prov_name)
            self.mp_controls.set_control(setting_path, prov_ctrl)

        curr_val = self.mp_controls.get_value(setting_path)
        if curr_val is None:
            curr_val = self._get_default_values(prov_name, conf_path)
        else:
            if "." in prov_name:
                prov, child = prov_name.split(".", maxsplit=1)
                curr_val = {prov: {child: curr_val}}
            else:
                curr_val = {prov_name: curr_val}

        prov_ctrl.value = curr_val

        return prov_ctrl

    def _get_default_values(self, prov_name, conf_path):
        wkspace = None
        if "." in prov_name:
            defn_name, wkspace = prov_name.split(".", maxsplit=1)
        else:
            defn_name = "provider"
        defn_path = f"{conf_path}.{defn_name}"
        prov_defn = self.mp_controls.get_defn(defn_path)
        if prov_defn is None:
            self.set_status(f"No definition for this provider was found: {prov_name}")
            return {}
        def_settings = {}
        for setting, defn in prov_defn.items():
            st_type, st_opts = defn if isinstance(defn, tuple) else "str", {}
            if st_type == "bool":
                def_settings[setting] = st_opts.get("default", False)
            elif st_type == "str":
                def_settings[setting] = st_opts.get("default")
        if wkspace:
            return {defn_name: {wkspace: def_settings}}
        return {defn_name: def_settings}


# pylint: disable=too-many-ancestors
class CEAutoLoadComps(CEAutoLoadQProvs):
    """User Default load components edit component."""

    _COMP_PATH = "UserDefaults.LoadComponents"
    _DESCRIPTION = "User Defaults - Auto-loaded components"
    _HELP_TEXT = """
    Choose the other components to load when you run nbinit.init_notebook().<br>

    This is related to the QueryProvider auto-load section but the parameters
    here are different.<br>
    Available components include the following:<br>
    (the names in parantheses are the names of the global variables used
    when creating these components - use theses to perform any operations
    required on the component.)
    <ul>
        <li>TILookup (ti_lookup) - Threat Intelligence lookups</li>
        <li>GeoIpLookup (geoip) - Geolocation of IP address lookups
        Note: you must specify which GeoIP Provider you wish to use.
        </li>
        <li>Notebooklets (nb) - Load the MSTIC notebooklets package. This must be
        installed in your notebook environment. You must specificy a default
        query provider for this component.
        </li>
        <li>Pivot (pivot) - Load Pivot functions into entities.</li>
        <li>AzureData (az_data) - Load the AzureData component. Optionally specify
        the authentication options you want to use.</li>
        <li>AzureSentinelAPI (azs_api) - Load the AzureSentinel API component. Optionally
        specify the authentication options you want to use.</li>
    </ul>

    The last two components also support a "connect" parameter. If this is not
    set the default is to initiate authentication to the service as soon as
    the component is loaded. set connect=False to prevent this.

    """
    _HELP_URI = {
        "User Defaults": (
            "https://msticpy.readthedocs.io/en/latest/getting_started/"
            + "msticpyconfig.html#user-defaults"
        )
    }

    def _get_available_options(self):
        return list(self.mp_controls.get_defn(self._COMP_PATH).keys())

    def _get_select_opts(self):
        return list(self.mp_controls.get_value(self._COMP_PATH).keys())

    def _get_settings_ctrls(self, prov_name, conf_path):
        if not prov_name:
            return widgets.VBox([], layout=self.no_border_layout("95%"))

        setting_path = f"{conf_path}.{prov_name}"
        prov_ctrl = self.mp_controls.get_control(setting_path)
        if not isinstance(prov_ctrl, UserDefLoadComponent):
            prov_ctrl = UserDefLoadComponent(
                self.mp_controls, prov_name, self._COMP_PATH
            )
            self.mp_controls.set_control(setting_path, prov_ctrl)

        curr_val = self.mp_controls.get_value(setting_path)
        prov_ctrl.value = curr_val
        return prov_ctrl

    def _get_default_values(self, prov_name, conf_path):
        defn_path = f"{conf_path}.{prov_name}"
        prov_defn = self.mp_controls.get_defn(defn_path)
        def_settings = {}
        if prov_defn is None:
            return {"alias": None}
        for setting, defn in prov_defn.items():
            st_type, st_opts = defn if isinstance(defn, tuple) else "str", {}
            if st_type == "str":
                def_settings[setting] = st_opts.get("default")
            elif st_type == "bool":
                def_settings[setting] = st_opts.get("default", False)
        return def_settings
