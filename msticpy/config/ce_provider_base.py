# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Module docstring."""
from abc import ABC

import ipywidgets as widgets

from .._version import VERSION
from .ce_common import LIST_LAYOUT, py_to_widget, dict_to_txt
from .comp_edit import CEItemsBase, CompEditDisplayMixin
from .compound_ctrls import get_arg_ctrl
from .mp_config_control import (
    MpConfigControls,
    get_defn_or_default,
    get_or_create_mpc_section,
)

__version__ = VERSION
__author__ = "Ian Hellen"


_PROV_GENERIC_HELP = """
Select a provider to edit its settings.<br>
To add a new provider, select the name from the "New prov" drop-down and click "Add"<br>

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
        self.select_item.layout = LIST_LAYOUT["layout"]
        self.select_item.style = LIST_LAYOUT["style"]
        self.select_item.description = "Providers"
        self.select_item.observe(self._select_provider, names="label")

        self.prov_options = widgets.Dropdown(
            options=self.mp_controls.get_defn(path=self._COMP_PATH).keys(),
            description="New prov",
            value=self.select_item.label,
            style=LIST_LAYOUT["style"],
        )
        self.items_frame.children = [*(self.items_frame.children), self.prov_options]

        prov_name = self.select_item.label
        self.edit_ctrls = _get_prov_ctrls(prov_name, self.mp_controls, self._COMP_PATH)
        self.edit_frame.children = [self.edit_ctrls]

        self.edit_buttons.btn_del.on_click(self._del_provider)
        self.edit_buttons.btn_add.on_click(self._add_provider)
        self.edit_buttons.btn_save.on_click(self._save_provider)

    def _get_select_opts(self, new_provider=None):
        """Get provider options to populate select list."""
        provs = self.mp_controls.get_value(self._COMP_PATH)
        self.prov_settings_map = _get_map(provs)
        existing_provs = list(provs.keys())
        if new_provider:
            existing_provs.append(new_provider)
        return [(val, idx) for idx, val in enumerate(sorted(existing_provs))]

    def _select_provider(self, change):
        prov_name = change.get("new")
        self.edit_ctrls = _get_prov_ctrls(prov_name, self.mp_controls, self._COMP_PATH)
        self.edit_frame.children = [self.edit_ctrls]
        self.mp_controls.populate_ctrl_values(f"{self._COMP_PATH}.{prov_name}")

    def _add_provider(self, btn):
        del btn
        new_provider = self.prov_options.label
        if new_provider in dict(self.select_item.options):
            self.set_status(f"This provider already exists: {new_provider}")
            return
        if not new_provider:
            self.set_status("Error: please select a provider name to add.")
            return

        self.select_item.options = self._get_select_opts(new_provider)
        self.select_item.label = new_provider

    def _del_provider(self, btn):
        del btn
        curr_provider = self.select_item.label
        prov_path = f"{self._COMP_PATH}.{curr_provider}"
        self.mp_controls.del_value(prov_path)
        remaining_opts = self._get_select_opts()
        self.select_item.options = remaining_opts
        if remaining_opts:
            self.select_item.label = remaining_opts[-1][0]

    def _save_provider(self, btn):
        del btn
        prov_path = f"{self._COMP_PATH}.{self.select_item.label}"
        self.mp_controls.save_ctrl_values(prov_path)


def get_wgt_ctrl(
    setting_path: str, var_name: str, mp_controls: "MpConfigControls"  # type: ignore
) -> widgets.Widget:
    """
    Return widget appropriate to value type of `var_name`.

    Parameters
    ----------
    setting_path : str
        The setting path (parent path) as dotted string.
    var_name : str
        The key name for the setting below `setting_path`.
    mp_controls : MpConfigControls
        Instance of MpConfigControls data

    Returns
    -------
    widgets.Widget
        The widget.

    """
    var_path = f"{setting_path}.{var_name}"
    ctrl = mp_controls.get_control(var_path)
    comp_defn = mp_controls.get_defn(var_path)
    st_type, st_opts = get_defn_or_default(comp_defn)
    def_value = st_opts["default"] if st_opts and "default" in st_opts else ""
    curr_val = mp_controls.get_value(var_path) or def_value

    if ctrl is None:
        st_type, st_opts = get_defn_or_default(comp_defn)
        if st_type == "bool":
            ctrl = widgets.Checkbox(
                description=var_name, value=py_to_widget(curr_val, val_type=st_type)
            )
        elif st_type in ("enum", "m_enum"):
            ctrl = widgets.SelectMultiple(
                description=var_name,
                options=st_opts.get("options"),
                value=curr_val or [],
            )
        elif st_type == "txt_dict":
            ctrl = widgets.Textarea(
                description=var_name,
                value=dict_to_txt(curr_val) or "",
            )
            setattr(ctrl, "tag", "txt_dict")
        else:
            ctrl = widgets.Text(
                description=var_name, value=py_to_widget(curr_val, val_type=st_type)
            )
        mp_controls.set_control(var_path, ctrl)
    else:
        ctrl.value = py_to_widget(curr_val, ctrl=ctrl)
    return ctrl


def _get_prov_ctrls(prov_name, mp_controls, conf_path):
    ctrls = []
    if not prov_name:
        return widgets.VBox(ctrls, layout=CompEditDisplayMixin.no_border_layout("95%"))
    prov_path = f"{conf_path}.{prov_name}"
    prov_defn = mp_controls.get_defn(prov_path)

    for setting in prov_defn:
        if setting != "Args":
            wgt = get_wgt_ctrl(prov_path, setting, mp_controls)
            if setting == "Provider":
                wgt.disabled = True
            ctrls.append(wgt)
            continue

        setting_path = f"{prov_path}.{setting}"
        for var_name in prov_defn.get(setting):
            comp_defn = mp_controls.get_defn(f"{setting_path}.{var_name}")
            if get_defn_or_default(comp_defn)[0] == "cred_key":
                arg_ctrl = get_arg_ctrl(setting_path, var_name, mp_controls)
                ctrls.append(arg_ctrl.hbox)
            else:
                ctrls.append(get_wgt_ctrl(setting_path, var_name, mp_controls))

    return widgets.VBox(ctrls)


def _get_map(providers_stgs):
    return dict(enumerate(providers_stgs.keys()))
