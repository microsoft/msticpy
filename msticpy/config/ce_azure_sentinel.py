# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Module docstring."""
from typing import Any, Dict

import ipywidgets as widgets

from .._version import VERSION
from .ce_common import LIST_LAYOUT
from .ce_provider_base import get_wgt_ctrl
from .comp_edit import CEItemsBase, CompEditDisplayMixin
from .mp_config_control import MpConfigControls, get_or_create_mpc_section

__version__ = VERSION
__author__ = "Ian Hellen"


# pylint: disable=too-many-ancestors
class CEAzureSentinel(CEItemsBase):
    """Azure Sentinel Workspaces editor component."""

    _DESCRIPTION = "Azure Sentinel workspace settings"
    _COMP_PATH = "AzureSentinel.Workspaces"
    _HELP_TEXT = """
    Supply the parameters for your Azure Sentinel workspaces here.<br>

    You can get all of these (apart from 'TenantID') from your workspace portal.
    Navigate to "Settings" (on the left side of the screen), then click the
    "Workspace Settings" tab near the top of the page.

    The Tenant ID is the only value missing from this settings page. If you cannot
    find this you can use the following code to try to look it up:
    <pre>
    from msticpy.common.config.ce_common import get_def_tenant_id
    get_def_tenant_id("{workspace-subscription-id}")
    </pre>

    The name that you use for workspace (the "Name" text box) does not have to
    be the same as the official name. You can use any helpful name that you
    like. You can even have the same workspace included multiple times with
    different names.

    There is a special case of the workspace name "Default". Normally, when you
    connect to a workspace you need to supply the name corresponding to the
    workspace entry in your settings. If you give a workspace the name "Default"
    it will be picked if you do not specify a name. Note, the "Default" entry
    can be a duplicate of another named entry.
    """
    _HELP_URI = {
        "Connecting to an Azure Sentinel Workspace": (
            "https://msticpy.readthedocs.io/en/latest/data_acquisition/"
            + "DataProviders.html#connecting-to-an-azure-sentinel-workspace"
        )
    }

    def __init__(self, mp_controls: MpConfigControls):
        """
        Initialize an instance of CEAzureSentinel.

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
        self.select_item.description = "Workspaces"
        self.select_item.observe(self._select_provider, names="label")

        prov_name = self.select_item.label
        self.edit_ctrls = _get_ws_ctrls(prov_name, self.mp_controls, self._COMP_PATH)
        self.txt_workspace = self.edit_ctrls.children[0]
        self.txt_workspace.value = prov_name
        self.btn_set_default = widgets.Button(description="Set as default")
        self.edit_frame.children = [self.edit_ctrls, self.btn_set_default]

        self.edit_buttons.btn_del.on_click(self._del_item)
        self.edit_buttons.btn_add.on_click(self._add_item)
        self.edit_buttons.btn_save.on_click(self._save_item)
        self.btn_set_default.on_click(self._set_default)

        self.current_workspace = prov_name

    def _get_select_opts(self):
        """Get provider options to populate select list."""
        provs = self.mp_controls.get_value(self._COMP_PATH)
        return [(val, idx) for idx, val in enumerate(provs.keys())]

    def _select_provider(self, change):
        """Select a provider from the list."""
        prov_name = change.get("new")
        self.edit_ctrls = _get_ws_ctrls(prov_name, self.mp_controls, self._COMP_PATH)
        self.txt_workspace.value = prov_name
        self.current_workspace = prov_name
        self.edit_frame.children = [self.edit_ctrls, self.btn_set_default]
        self.mp_controls.populate_ctrl_values(f"{self._COMP_PATH}.{prov_name}")

    def _add_item(self, btn):
        """Add an item."""
        del btn
        current_options = self._get_select_opts()
        new_provider = f"NewWorkspace_{len(current_options)}"
        if new_provider in dict(self.select_item.options):
            self.set_status(f"Warning this workspace already exists: {new_provider}")
            return

        # Get the definition for a default workspace
        prov_defn = self.mp_controls.get_defn(f"{self._COMP_PATH}.Default")
        # create an entry in the settings
        self.mp_controls.set_value(
            f"{self._COMP_PATH}.{new_provider}", _enum_template(prov_defn)
        )
        # add new workspace to the workspace selection list
        current_options.append((new_provider, len(current_options)))
        self.select_item.options = current_options
        self.select_item.label = new_provider

    def _del_item(self, btn):
        """Delete an item."""
        del btn
        curr_provider = self.txt_workspace.value
        prov_path = f"{self._COMP_PATH}.{curr_provider}"
        self.mp_controls.del_value(prov_path)
        remaining_opts = self._get_select_opts()
        self.select_item.options = remaining_opts
        if remaining_opts:
            self.select_item.label = remaining_opts[-1][0]

    def _save_item(self, btn):
        """Save the current item."""
        del btn
        curr_provider = self.txt_workspace.value
        if curr_provider != self.current_workspace:
            self.mp_controls.rename_path(
                f"{self._COMP_PATH}.{self.current_workspace}",
                f"{self._COMP_PATH}.{curr_provider}",
            )
            self.select_item.options = self._get_select_opts()
        prov_path = f"{self._COMP_PATH}.{curr_provider}"
        self.mp_controls.save_ctrl_values(prov_path)

    def _set_default(self, btn):
        """Set selected item to be the Default entry."""
        del btn
        src_wkspace = self.current_workspace
        if src_wkspace == "Default":
            return
        src_path = f"{self._COMP_PATH}.{src_wkspace}"
        src_settings = self.mp_controls.get_value(src_path)
        def_path = f"{self._COMP_PATH}.Default"

        self.mp_controls.set_value(def_path, src_settings)
        self.select_item.label = "Default"

    def _select_labels(self):
        return [label for label, _ in self.select_item.options]


_TEXT_PARAMS = {
    "layout": widgets.Layout(width="70%"),
    "style": {"description_width": "150px"},
}


def _get_ws_ctrls(workspace, mp_controls, conf_path):

    if not workspace:
        return widgets.VBox([], layout=CompEditDisplayMixin.no_border_layout("95%"))
    defn_path = f"{conf_path}.Default"
    prov_path = f"{conf_path}.{workspace}"
    prov_defn = mp_controls.get_defn(defn_path)

    ctrls = [widgets.Text(description="Name", **_TEXT_PARAMS)]
    for setting in prov_defn:
        wgt = get_wgt_ctrl(prov_path, setting, mp_controls)
        wgt.layout = _TEXT_PARAMS["layout"]
        wgt.style = _TEXT_PARAMS["style"]
        ctrls.append(wgt)

    return widgets.VBox(ctrls)


_KNOWN_SEC_NAMES = ("AuthKey", "ApiID", "clientSecret", "password")


def _enum_template(template: Dict[str, Any]):
    output: Dict[str, Any] = {}
    for key, value in template.items():
        if key in _KNOWN_SEC_NAMES:
            output[key] = ""
        elif isinstance(value, dict):
            output[key] = _enum_template(value)
        elif isinstance(value, tuple):
            val_type, val_opt = value
            if val_type == "str":
                output[key] = val_opt.get("default", "")
            elif val_type == "bool":
                bool_str = val_opt.get("default", False)
                if isinstance(bool_str, str):
                    output[key] = bool_str.casefold() == "true"
                else:
                    output[key] = bool_str
            elif val_type == "int":
                output[key] = int(val_opt.get("default", 0))
            elif val_type == "enum":
                output[key] = val_opt.get("default", val_opt.get("options")[0])
        else:
            output[key] = value
    return output
