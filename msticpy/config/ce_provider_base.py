# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Module docstring."""
from abc import ABC
from typing import List, Optional

import ipywidgets as widgets

from .._version import VERSION
from .ce_common import (
    ITEM_LIST_LAYOUT,
    get_defn_or_default,
    get_or_create_mpc_section,
    get_wgt_ctrl,
)
from .comp_edit import CEItemsBase, CompEditDisplayMixin
from .compound_ctrls import get_arg_ctrl
from .mp_config_control import MpConfigControls

__version__ = VERSION
__author__ = "Ian Hellen"


_PROV_GENERIC_HELP = """
Select a provider to edit its settings.<br>
To add a new provider, select the name from the "Add prov" drop-down and click "Add"<br>

The Storage option tells the settings module to look for the
value in one of three places:
<ul>
    <li>Text - this is the usual way to store values that are not sensitive</li>
    <li>Environment Var - Type the name of variable in the text box</li>
    <li>Key Vault - use this for sensitive data like passwords and API keys<br>
    The KeyVault value here can be left empty (the secret name is generated
    from the settings path), can contain a secret name or can contain a
    path {vaultname}/{secretname}
    </li>
</ul>

<b>Note:</b> Storing values in KeyVault requires some work on your part<br>
You must do the following:
<ol>
    <li>Create the Key Vault</li>
    <li>Add the settings for the Vault in the KeyVault section of the configuration</li>
    <li>Add the values that you want to use to the Vault</li>
</ol>
The Key Vault Configuration link below describes this setup and how you can
configure your Key Vault settings and transfer secrets settings from your
configuration file to a vault.
"""

HELP_URIS = {
    "Key Vault Configuration": (
        "https://msticpy.readthedocs.io/en/latest/getting_started/"
        + "msticpyconfig.html#specifying-secrets-as-key-vault-secrets"
    ),
    "MSTICPy Configuration": (
        "https://msticpy.readthedocs.io/en/latest/"
        + "getting_started/msticpyconfig.html"
    ),
    "Help on this tab": (
        "https://msticpy.readthedocs.io/en/latest/getting_started/"
        + "SettingsEditor.html#adding-threat-intelligence-ti-providers"
    ),
}


# pylint: disable=too-many-ancestors
class CEProviders(CEItemsBase, ABC):
    """Abstract base class for Provider edit components."""

    _HELP_TEXT = _PROV_GENERIC_HELP

    def __init__(self, mp_controls: MpConfigControls):
        """
        Initialize an instance of the component.

        Parameters
        ----------
        mp_controls : MpConfigControls
            The config/controls/settings database

        """
        super().__init__(mp_controls)

        get_or_create_mpc_section(self.mp_controls, self._COMP_PATH)
        self.prov_settings_map = _get_map(mp_controls.get_value(self._COMP_PATH))
        self.select_item.options = self._get_select_opts()
        self.select_item.layout = ITEM_LIST_LAYOUT["layout"]
        self.select_item.style = ITEM_LIST_LAYOUT["style"]
        self.select_item.description = "Providers"
        self.select_item.observe(self._select_provider, names="label")

        self.prov_options = widgets.Dropdown(
            options=self.mp_controls.get_defn(path=self._COMP_PATH).keys(),
            description="Add prov",
            # value=self.select_item.label,
            style=ITEM_LIST_LAYOUT["style"],
        )
        self.items_frame.children = [*(self.items_frame.children), self.prov_options]

        self._populate_edit_ctrls()

        self.edit_buttons.btn_del.on_click(self._del_provider)
        self.edit_buttons.btn_add.on_click(self._add_provider)
        self.edit_buttons.btn_save.on_click(self._save_provider)

    @property
    def _current_path(self) -> str:
        """Return current settings path."""
        return f"{self._COMP_PATH}.{self.select_item.label}"

    @property
    def _prov_name(self) -> str:
        """Return current provider instance name."""
        prov_name = self.select_item.label or ""
        return prov_name.strip()

    @property
    def _prov_ctrl_name(self) -> str:
        """Return current provider control name."""
        return self._prov_name

    @property
    def _select_prov_instance_name(self):
        """Return the provider instance name (minus instance suffix)."""
        return ""

    def _get_select_opts(self):
        """Get provider options to populate select list."""
        provs = self.mp_controls.get_value(self._COMP_PATH)
        self.prov_settings_map = _get_map(provs)
        return [(val, idx) for idx, val in enumerate(sorted(provs.keys()))]

    def _populate_edit_ctrls(
        self,
        control_name: Optional[str] = None,
        new_provider: bool = False,
    ):
        """Retrieve and populate form controls for the provider to display."""
        self.edit_ctrls = _get_prov_ctrls(
            prov_name=control_name or self._prov_ctrl_name,
            mp_controls=self.mp_controls,
            conf_path=self._COMP_PATH,
            prov_instance_name=self._select_prov_instance_name
            if not new_provider
            else "",
        )
        self.edit_frame.children = [self.edit_ctrls]

    def _select_provider(self, change):
        """Update based on new selection in current providers."""
        del change
        self._populate_edit_ctrls()
        self.mp_controls.populate_ctrl_values(
            f"{self._COMP_PATH}.{self.select_item.label}"
        )

    def _add_provider(self, btn):
        """Add a new provider from prov_options."""
        del btn
        if self.prov_options.label in dict(self.select_item.options):
            self.set_status(f"This provider already exists: {self.prov_options.label}")
            return
        if not self.prov_options.label:
            self.set_status("Error: please select a provider name to add.")
            return
        self._populate_edit_ctrls(
            control_name=self.prov_options.label, new_provider=True
        )
        self.mp_controls.save_ctrl_values(
            f"{self._COMP_PATH}.{self.prov_options.label}"
        )
        self.select_item.options = self._get_select_opts()
        self.select_item.label = self.prov_options.label

    def _del_provider(self, btn):
        """Delete the currently selected provider."""
        del btn
        if not self.select_item.label:
            return
        self.mp_controls.del_value(self._current_path)
        remaining_opts = self._get_select_opts()
        self.select_item.options = remaining_opts
        if remaining_opts:
            self.select_item.label = remaining_opts[-1][0]

    def _save_provider(self, btn):
        """Save/update the currently edited provider settings."""
        del btn
        if not self.select_item.label:
            return
        self.mp_controls.save_ctrl_values(self._current_path)
        val_results = self.mp_controls.validate_setting(self._current_path)
        status = "  ".join(res[1] for res in val_results if not res[0])
        if status:
            self.set_status(status)


def _get_prov_ctrls(prov_name, mp_controls, conf_path, prov_instance_name: str = None):
    ctrls: List[widgets.Widget] = []
    if not prov_name:
        return widgets.VBox(ctrls, layout=CompEditDisplayMixin.no_border_layout("95%"))
    # prov_path = f"{conf_path}.{prov_name}"
    if prov_instance_name:
        instance_path = f"{conf_path}.{prov_name}-{prov_instance_name}"
    else:
        instance_path = f"{conf_path}.{prov_name}"
    defn_path = f"{conf_path}.{prov_name}"
    prov_defn = mp_controls.get_defn(defn_path)

    for setting in prov_defn:
        if setting != "Args":
            wgt = get_wgt_ctrl(
                setting_path=defn_path,
                var_name=setting,
                mp_controls=mp_controls,
                instance_name=prov_instance_name,
            )
            if setting == "Provider":
                wgt.disabled = True
            ctrls.append(wgt)
            continue

        setting_path = f"{instance_path}.{setting}"
        setting_defn_path = f"{defn_path}.{setting}"
        for var_name in prov_defn.get(setting):
            comp_defn = mp_controls.get_defn(f"{setting_defn_path}.{var_name}")
            if get_defn_or_default(comp_defn)[0] == "cred_key":
                arg_ctrl = get_arg_ctrl(setting_path, var_name, mp_controls)
                ctrls.append(arg_ctrl.hbox)
            else:
                ctrls.append(
                    get_wgt_ctrl(
                        setting_path=setting_defn_path,
                        var_name=var_name,
                        mp_controls=mp_controls,
                        instance_name=prov_instance_name,
                    )
                )

    return widgets.VBox(ctrls)


def _get_map(providers_stgs):
    return dict(enumerate(providers_stgs.keys()))
