# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Module docstring."""
from datetime import datetime

import ipywidgets as widgets

from .._version import VERSION
from ..context.azure.sentinel_core import MicrosoftSentinel
from .ce_common import (
    ITEM_LIST_LAYOUT,
    get_or_create_mpc_section,
    get_wgt_ctrl,
    print_debug,
)
from .comp_edit import CEItemsBase, CompEditDisplayMixin
from .mp_config_control import MpConfigControls

__version__ = VERSION
__author__ = "Ian Hellen"


# pylint: disable=too-many-ancestors
class CEAzureSentinel(CEItemsBase):
    """Microsoft Sentinel Workspaces editor component."""

    _DESCRIPTION = "Microsoft Sentinel workspace settings"
    _COMP_PATH = "AzureSentinel.Workspaces"
    _HELP_TEXT = """
    Supply the parameters for your Microsoft Sentinel workspaces here.<br>

    You can use the URL import to quickly fetch Microsoft Sentinel Workspace
    settings. In your browser, navigate to your Sentinel workspace in the
    Azure portal (e.g. the Overview page). Copy the URL and paste into the
    <b>Portal URL</b> text box, then click on <b>Import from URL</b>.

    You can also add partial information (such as a Workspace ID) and use
    the <b>Resolve Settings</b> button to fetch the other settings from
    the Azure Resource Graph.

    Note you must be authenticated to Azure for either of these to work.

    You can also get all of these settings (apart from 'TenantID') from your
    workspace portal manually.
    Navigate to "Settings" (on the left side of the screen), then click the
    "Workspace Settings" tab near the top of the page.

    The Tenant ID is the only value missing from this settings page. If you cannot
    find this you can use the following code to try to look it up:
    <pre>
    from msticpy.common.config.ce_common import get_def_tenant_id
    get_def_tenant_id("{workspace-subscription-id}")
    </pre>

    The name that you use for workspace (the "Name" text box) does not have to
    be the same as the Workspace name. You can use any helpful name that you
    like. You can even have the same workspace included multiple times with
    different names.

    There is a special case of the workspace name "Default". Normally, when you
    connect to a workspace you need to supply the name corresponding to the
    workspace entry in your settings. If you give a workspace the name "Default"
    it will be picked if you do not specify a name. Note, the "Default" entry
    can be a duplicate of another named entry.
    """
    _HELP_URI = {
        "Connecting to a Microsoft Sentinel Workspace": (
            "https://msticpy.readthedocs.io/en/latest/data_acquisition/"
            + "DataProviders.html#connecting-to-an-azure-sentinel-workspace"
        ),
        "Help on this tab": (
            "https://msticpy.readthedocs.io/en/latest/getting_started/"
            + "SettingsEditor.html#azure-sentinel-workspaces"
        ),
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
        self.select_item.layout = ITEM_LIST_LAYOUT["layout"]
        self.select_item.style = ITEM_LIST_LAYOUT["style"]
        self.select_item.description = "Workspaces"
        self.select_item.observe(self._select_provider, names="label")

        prov_name = self.select_item.label
        self.edit_ctrls = _get_ws_ctrls(prov_name, self.mp_controls, self._COMP_PATH)
        self.edit_ctrls.children[0].value = prov_name or ""
        self.btn_set_default = widgets.Button(
            description="Set as default",
            tooltip="Set the current workspace as your default.",
        )
        btn_resolve = widgets.Button(
            description="Resolve settings",
            tooltip="Try to resolve any missing settings using the Azure resource graph.",
        )
        self.btn_imp_url = widgets.Button(
            description="Import from URL",
            tooltip="Try to lookup workspace settings from the Sentinel portal URL.",
        )
        self.txt_imp_url = widgets.Text(
            description="Portal URL",
            # Tooltip not yet supported in widgets.Text
            # tooltip="Paste in the URL from the Sentinel Azure portal to fetch settings.",
        )
        self.ws_btns = widgets.HBox([self.btn_set_default, btn_resolve])
        self.url_imp_ctrls = widgets.HBox([self.txt_imp_url, self.btn_imp_url])
        if prov_name:
            self.edit_frame.children = [
                self.edit_ctrls,
                self.ws_btns,
                self.url_imp_ctrls,
            ]
        else:
            self.edit_frame.children = [self.edit_ctrls]
        self.edit_buttons.btn_del.on_click(self._del_item)
        self.edit_buttons.btn_add.on_click(self._add_item)
        self.edit_buttons.btn_save.on_click(self._save_item)
        self.btn_set_default.on_click(self._set_default)
        btn_resolve.on_click(self._resolve_settings)
        self.btn_imp_url.on_click(self._imp_ws_from_url)
        self.current_workspace = prov_name

    @property
    def _current_path(self):
        return f"{self._COMP_PATH}.{self.select_item.label}"

    def _get_select_opts(self):
        """Get provider options to populate select list."""
        provs = self.mp_controls.get_value(self._COMP_PATH)
        return [(val, idx) for idx, val in enumerate(provs.keys())]

    def _select_provider(self, change):
        """Select a provider from the list."""
        prov_name = change.get("new")
        self.edit_ctrls = _get_ws_ctrls(prov_name, self.mp_controls, self._COMP_PATH)
        self.edit_ctrls.children[0].value = prov_name
        self.current_workspace = prov_name
        self.edit_frame.children = [self.edit_ctrls, self.ws_btns, self.url_imp_ctrls]
        self.mp_controls.populate_ctrl_values(self.current_workspace)

    def _add_item(self, btn):
        """Add an item."""
        del btn
        # Generate a "unique" temp name
        new_provider = f"NewWorkspace_{datetime.now().strftime('%f')[:4]}"
        while new_provider in dict(self.select_item.options):
            new_provider = f"NewWorkspace_{datetime.now().strftime('%f')[:4]}"
        _get_ws_ctrls(new_provider, self.mp_controls, self._COMP_PATH)
        self.mp_controls.save_ctrl_values(f"{self._COMP_PATH}.{new_provider}")
        current_options = self._get_select_opts()
        self.select_item.options = current_options
        self.select_item.label = new_provider

    def _del_item(self, btn):
        """Delete an item."""
        del btn
        self.mp_controls.del_value(self._current_path)
        remaining_opts = self._get_select_opts()
        self.select_item.options = remaining_opts
        if remaining_opts:
            self.select_item.label = remaining_opts[-1][0]

    def _save_item(self, btn):
        """Save the current item."""
        del btn
        # Use may have edited the workspace name.
        # If so save the existing one and rename the paths to match new name
        edited_provider_name = self.edit_ctrls.children[0].value
        self.mp_controls.save_ctrl_values(self._current_path)
        if edited_provider_name != self.current_workspace and self.current_workspace:
            self.mp_controls.rename_path(
                f"{self._COMP_PATH}.{self.current_workspace}",
                f"{self._COMP_PATH}.{edited_provider_name}",
            )
            self.select_item.options = self._get_select_opts()
            self.select_item.label = edited_provider_name
        valid, status = _validate_ws(
            edited_provider_name, self.mp_controls, self._COMP_PATH
        )
        if not valid:
            self.set_status(status)

    def _set_default(self, btn):
        """Set selected item to be the Default entry."""
        del btn
        src_wkspace = self.current_workspace
        if src_wkspace == "Default" or not src_wkspace:
            return
        src_path = f"{self._COMP_PATH}.{src_wkspace}"
        src_settings = self.mp_controls.get_value(src_path)
        def_path = f"{self._COMP_PATH}.Default"
        # save the src values to the Default key and refresh the select_item list
        self.mp_controls.set_value(def_path, src_settings)
        print_debug("Current options", self._get_select_opts())
        self.select_item.options = self._get_select_opts()
        self.select_item.label = "Default"

    def _imp_ws_from_url(self, btn):
        """Import workspace settings from a portal URL."""
        del btn
        url = self.txt_imp_url.value
        if not url:
            self.set_status("Please paste portal URL into ")
            return
        self._update_settings(MicrosoftSentinel.get_workspace_details_from_url(url))

    def _update_settings(self, ws_settings):
        """Update current controls with workspace settings."""
        if not ws_settings:
            self.set_status("Could not resolve workspace from URL")
            return
        ws_name = next(iter(ws_settings))
        _get_named_control(self.edit_ctrls, "Name").value = ws_name
        for setting, value in ws_settings[ws_name].items():
            ctrl = _get_named_control(self.edit_ctrls, setting)
            if ctrl is None or ctrl.value:
                # don't overwrite existing settings
                continue
            ctrl.value = value

    def _resolve_settings(self, btn):
        """Resolve missing settings for workspace."""
        del btn
        subscription_id = _get_named_control(self.edit_ctrls, "SubscriptionId").value
        workspace_id = _get_named_control(self.edit_ctrls, "WorkspaceId").value
        workspace_name = _get_named_control(self.edit_ctrls, "WorkspaceName").value
        resource_group = _get_named_control(self.edit_ctrls, "ResourceGroup").value
        if not (workspace_id or workspace_name):
            self.set_status(
                "Need at least WorkspaceId or WorkspaceName to lookup settings."
            )
            return
        if workspace_id:
            self._update_settings(
                MicrosoftSentinel.get_workspace_settings(workspace_id=workspace_id)
            )
        else:
            self._update_settings(
                MicrosoftSentinel.get_workspace_settings_by_name(
                    workspace_name=workspace_name,
                    subscription_id=subscription_id,
                    resource_group=resource_group,
                )
            )

    def _select_labels(self):
        return [label for label, _ in self.select_item.options]


_TEXT_PARAMS = {
    "layout": widgets.Layout(width="70%"),
    "style": {"description_width": "150px"},
}


def _get_ws_ctrls(workspace, mp_controls, conf_path):
    """Return or create the control set for a workspace."""
    defn_path = f"{conf_path}.Default"
    prov_defn = mp_controls.get_defn(defn_path)

    ctrls = [widgets.Text(description="Name", **_TEXT_PARAMS)]
    if not workspace:
        return widgets.VBox(ctrls, layout=CompEditDisplayMixin.no_border_layout("95%"))
    prov_path = f"{conf_path}.{workspace}"
    for setting in prov_defn:
        wgt = get_wgt_ctrl(prov_path, setting, mp_controls)
        wgt.layout = _TEXT_PARAMS["layout"]
        wgt.style = _TEXT_PARAMS["style"]
        ctrls.append(wgt)

    return widgets.VBox(ctrls)


def _get_named_control(edit_ctrls, name):
    """Get the control with matching name."""
    try:
        return next(ctrl for ctrl in edit_ctrls.children if ctrl.description == name)
    except StopIteration:
        return None


def _validate_ws(workspace, mp_controls, conf_path):
    """Validate the settings for a workspace."""
    defn_path = f"{conf_path}.Default"

    if not workspace:
        return False, "Workspace name must be supplied."
    prov_path = f"{conf_path}.{workspace}"
    results = mp_controls.validate_setting(path=prov_path, defn_path=defn_path)
    status = []
    validated = True
    for valid, mssg in results:
        if not valid:
            validated = False
            status.append(mssg)
    return validated, "  ".join(status)
