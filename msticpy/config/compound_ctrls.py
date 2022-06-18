# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Compound control classes."""
import os
from copy import deepcopy
from typing import Any, Dict, Optional, Tuple, Union

import ipywidgets as widgets

from .._version import VERSION

try:
    from ..auth.keyvault_client import BHKeyVaultClient, MsticpyKeyVaultConfigError
    from ..auth.secret_settings import SecretsClient

    _KEYVAULT = True
except ImportError:
    _KEYVAULT = False
from .ce_common import print_debug, py_to_widget, widget_to_py
from .comp_edit import CompEditDisplayMixin, CompEditStatusMixin, SettingsControl
from .mp_config_control import MpConfigControls

__version__ = VERSION
__author__ = "Ian Hellen"


STORE_TEXT = "Text"
STORE_ENV_VAR = "EnvironmentVar"
STORE_KEYVAULT = "KeyVault"


class ArgControl(SettingsControl, CompEditStatusMixin):
    """Args setting element edit component."""

    def __init__(
        self,
        setting_path: Optional[str],
        name: str,
        store_type: str = STORE_TEXT,
        item_value: Any = None,
    ):
        """
        Initialize and ArgControl object.

        Parameters
        ----------
        setting_path : Optional[str], optional
            The full path to the setting (minus the name)
        name : str
            The name of the setting
        store_type : str, optional
            The storage type for the setting value, by default "Text"
            Other options are "EnvironmentVar" and "KeyVault"
        item_value : Any, optional
            The value of the setting, by default None
            Note None is a legitimate value for store_type "KeyVault"

        """
        self.setting_path = setting_path
        self.name = name
        self.kv_client = None

        self.lbl_setting = widgets.Label(
            value=self.name, layout=widgets.Layout(width="130px")
        )
        self.rb_store_type = widgets.RadioButtons(
            options=[STORE_TEXT, STORE_ENV_VAR, STORE_KEYVAULT],
            description="Storage:",
            layout=widgets.Layout(width="35%"),
            style={"description_width": "100px"},
        )
        self.rb_store_type.value = store_type
        if isinstance(item_value, bool):
            self.txt_val = widgets.Checkbox(description="Value")
        else:
            if item_value is None:
                item_value = ""
            self.txt_val = widgets.Text(
                description="Value" if store_type == STORE_TEXT else store_type,
                layout=widgets.Layout(width="99%"),
                style={"description_width": "100px"},
                value=str(item_value) or "",
            )
        # KeyVault settings
        self.cb_kv_def = widgets.Checkbox(description="Def KV Path")
        self.btn_add_kv_secret = widgets.Button(description="Upload to KV")
        self.btn_add_kv_secret.on_click(self._set_kv_secret)
        self.cb_kv_def.value = not bool(item_value)
        self._set_kv_visibility()
        contents_hbox = widgets.HBox(
            [
                self.rb_store_type,
                widgets.VBox(
                    [self.txt_val, self.cb_kv_def, self.btn_add_kv_secret],
                    layout=widgets.Layout(width="64%"),
                ),
            ]
        )
        self.hbox = widgets.VBox(
            [self.lbl_setting, contents_hbox, self.status],
            layout=CompEditDisplayMixin.no_border_layout("99%"),
        )

        self.cb_kv_def.observe(self._disable_txt, names="value")
        self.rb_store_type.observe(self._change_store, names="value")

    @property
    def value(self) -> Union[str, Dict[str, Optional[str]]]:
        """
        Return the value of the control.

        Returns
        -------
        Union[str, Dict[str, Optional[str]]]
            Either a string value or a dict of:
            {"EnvironmentVar": value} or
            {"KeyVault": value or None}

        """
        if self.rb_store_type.value == STORE_TEXT:
            return widget_to_py(self.txt_val)
        return {self.rb_store_type.value: widget_to_py(self.txt_val)}

    @value.setter
    def value(self, value: Union[str, Dict[str, Optional[str]]]):
        """
        Set control to value.

        Parameters
        ----------
        value : Union[str, Dict[str, Optional[str]]]
            Either a str or a dict of:
            .. parsed_literal::

                {"Text": value} or
                {"EnvironmentVar": value} or
                {"KeyVault": value or None}

        """
        print_debug("_ArgControl setter val:", value)
        if not isinstance(value, dict):
            self.txt_val.value = py_to_widget(value, val_type="str")
            self.rb_store_type.value = STORE_TEXT
            return
        store_type, store_val = next(iter(value.items()))
        self.txt_val.value = py_to_widget(store_val, val_type="str")
        self.rb_store_type.value = store_type
        self.cb_kv_def.value = not bool(store_val)
        self._set_kv_visibility()

    def _change_store(self, change):
        """Handle event for store type radio button."""
        st_type = change.get("new")
        self.txt_val.description = "Value" if st_type == STORE_TEXT else st_type
        self._set_kv_visibility()
        self.lbl_setting.value = st_type
        if st_type == STORE_KEYVAULT and not self.txt_val.value:
            self.cb_kv_def.value = True

    def _disable_txt(self, change):
        """Disable the text field if KeyVault and kv_def_enabled."""
        if self.rb_store_type.value != STORE_KEYVAULT:
            return
        kv_def_enabled = change.get("new")
        if kv_def_enabled:
            self.txt_val.value = ""
        self.txt_val.disabled = kv_def_enabled

    def _set_kv_visibility(self):
        """Set the visibility of the keyvault-related controls."""
        if self.rb_store_type.value == STORE_KEYVAULT:
            self.cb_kv_def.layout.visibility = "visible"
            self.btn_add_kv_secret.layout.visibility = "hidden"
        else:
            self.cb_kv_def.layout.visibility = "hidden"
            self.btn_add_kv_secret.layout.visibility = "visible"

    def _set_kv_secret(self, btn):
        """Try to store the current value to key vault."""
        del btn
        if not self.setting_path:
            self.set_status("No setting path to create KV secret name.")
            return
        sec_value = None
        if self.rb_store_type.value == STORE_TEXT:
            sec_value = self.txt_val.value
        elif self.rb_store_type.value == STORE_ENV_VAR:
            sec_value = os.environ.get(self.txt_val.value.strip())
        else:
            return
        if not sec_value:
            self.set_status("No secret value to store.")
            return
        kv_client = self.kv_client
        if _KEYVAULT:
            result, status, kv_client = _set_kv_secret_value(
                setting_path=self.setting_path,
                item_name=self.name,
                value=sec_value,
                kv_client=kv_client,
            )
        else:
            self.set_status("Azure keyvault support is not installed.")
            return
        if result:
            self.set_status(status)
            self.kv_client = kv_client
            self.rb_store_type.value = STORE_KEYVAULT
            self.txt_val.value = ""
            self.cb_kv_def.value = True
        else:
            self.set_status("Error saving secret: status")


def _get_args_val(arg_setting):
    """Return a dict whether the value is a str or a dict."""
    _, arg_val = next(iter(arg_setting.items()))
    if isinstance(arg_val, (str, int, bool)):
        return STORE_TEXT, arg_val
    return next(iter(arg_val.items()))


def get_arg_ctrl(setting_path, var_name, mp_controls):
    """Create the ArgControl based on the current value of the setting."""
    var_path = f"{setting_path}.{var_name}"
    arg_ctrl = mp_controls.get_control(var_path)
    curr_val = mp_controls.get_value(var_path)

    print_debug(var_name)
    if arg_ctrl is None:
        if curr_val is not None:
            store_type, store_val = _get_args_val({var_name: curr_val})
        else:
            store_type, store_val = STORE_TEXT, ""
        arg_ctrl = ArgControl(
            setting_path=setting_path,
            name=var_name,
            store_type=store_type,
            item_value=store_val,
        )
        mp_controls.set_control(var_path, arg_ctrl)
    else:
        arg_ctrl.value = curr_val
    return arg_ctrl


if _KEYVAULT:

    def _set_kv_secret_value(
        setting_path: str,
        item_name: str,
        value: str,
        kv_client: Any = None,
    ) -> Tuple[bool, str, Any]:
        """
        Set the Key Vault secret to `value`.

        Parameters
        ----------
        setting_path : str
            The setting path (parent path)
        item_name : str
            The setting name
        value : str
            The value to store
        kv_client : Optional[BHKeyVaultClient], optional
            Cached Key Vault secrets client if one is already
            attached to the ArgControl, by default None

        Returns
        -------
        Tuple[bool, str, Optional[BHKeyVaultClient]]
            True - if success
            Status - status string
            KeyVault secrets client (used to cache the client)

        """
        try:
            secret_name = SecretsClient.format_kv_name(f"{setting_path}.{item_name}")
            kv_client = kv_client or BHKeyVaultClient()
            kv_client.set_secret(secret_name, value)
            return True, f"Saved as {secret_name}", kv_client
        except MsticpyKeyVaultConfigError:
            return False, "Missing or invalid Key Vault configuration", None
        except Exception as err:  # pylint: disable=broad-except
            return False, str(err), None

else:

    def _set_kv_secret_value(
        setting_path: str,
        item_name: str,
        value: str,
        kv_client: Any = None,
    ) -> Tuple[bool, str, Any]:
        """Return empty response function if Key Vault cannot be initialized."""
        del setting_path, item_name, value, kv_client
        return False, "Azure keyvault libraries are not installed", None


class UserDefQryProvCtrl(SettingsControl):
    """User Defaults Query Provider edit component."""

    def __init__(self, prov_name: str):
        """
        Initialize the control.

        Parameters
        ----------
        prov_name : str
            The query provider name

        """
        self.prov_name = prov_name
        self.prov_type = "Workspace"
        self._set_prov_name(prov_name)
        w_style = {"description_width": "100px"}

        self.txt_alias = widgets.Text(description="Alias", style=w_style)
        self.cb_connect = widgets.Checkbox(description="Auto-connect", style=w_style)
        self.layout = widgets.VBox(
            [self.lbl_type, widgets.VBox([self.txt_alias, self.cb_connect])]
        )

    def _set_prov_name(self, prov_name):
        """
        Set the provider name.

        Notes
        -----
        The provider name can be a simple string or a compound,
        dotted string (in the case of AzureSentinel it will be of the form
        AzureSentinel.WorkspaceName)

        """
        self.prov_name = prov_name
        self.prov_type = "Workspace" if "." in prov_name else "Provider"
        self.lbl_type = widgets.Label(
            value=f"{self.prov_name} ({self.prov_type})",
            layout=widgets.Layout(width="300px"),
        )

    @property
    def value(self) -> Union[str, Dict[str, Optional[str]]]:
        """
        Return the current value of the control.

        Returns
        -------
        Union[str, Dict[str, Optional[str]]]
            The value dict.
            In cases where optional 'alias' and 'connect' settings
            are not used this will be an empty dictionary.

        """
        alias = {"alias": self.txt_alias.value} if self.txt_alias.value else {}
        connect = (
            {"connect": self.cb_connect.value} if not self.cb_connect.value else {}
        )
        return {**alias, **connect}

    @value.setter
    def value(self, value: Union[str, Dict[str, Optional[str]]]):
        """
        Set the value of the component from settings.

        Parameters
        ----------
        value : Optional[Dict[str, str]]
            The current value to set in the control.

        """
        if not value or isinstance(value, str):
            value = {}
        self.txt_alias.value = value.get("alias", "")
        self.cb_connect.value = value.get("connect", True)


class UserDefLoadComponent(SettingsControl):
    """User Defaults Load component edit component."""

    _W_STYLE = {"description_width": "100px"}

    # pylint: disable=line-too-long
    def __init__(
        self, mp_controls: MpConfigControls, comp_name: str, setting_path: str
    ):
        """
        Initialize the control.

        Parameters
        ----------
        mp_controls : MpConfigControls
            Msticpy configu controls data store.
        comp_name : str
            Component name
        setting_path : str
            Path to setting (minus comp_name)

        """
        # pylint: enable=line-too-long
        self.comp_name = comp_name

        self.comp_path = f"{setting_path}.{comp_name}"
        self.control_map = deepcopy(mp_controls.get_defn(self.comp_path))
        self.controls = {}
        self.controls["label"] = widgets.Label(value=self.comp_name)

        # build controls from dict
        self._create_controls(self.comp_path, mp_controls)

    def _create_controls(self, path, mp_controls):
        """Create controls from settings."""
        comp_defn = mp_controls.get_defn(path)
        # If the setting is a simple string - no configurable elements
        if comp_defn is None:
            self.controls["no_settings"] = widgets.Label(
                value="No settings for this component."
            )
            return

        if not isinstance(comp_defn, dict):
            raise TypeError(f"component definition type is invalid {type(comp_defn)}")

        for name, settings in comp_defn.items():
            curr_value = mp_controls.get_value(f"{path}.{name}")
            if settings is None:
                self.controls["no_settings"] = widgets.Label(
                    value="No settings for this component."
                )
                return
            ctrl_path = f"{path}.{name}"
            if isinstance(settings, str):
                # Simple case of a string value
                self.controls[name] = widgets.Text(
                    description="Value", value=curr_value or ""
                )
                self._add_control_to_map(ctrl_path, self.controls[name])
            if isinstance(settings, tuple):
                # if tuple then the second elem of the tuple is the type defn
                self.controls[name] = self._create_select_ctrl(
                    settings, name, curr_value
                )
                self._add_control_to_map(ctrl_path, self.controls[name])
            elif isinstance(settings, dict):
                self.controls[name] = widgets.Text(value=name, disabled=True)
                self._create_controls(ctrl_path, mp_controls)

    def _add_control_to_map(self, path, ctrl):
        """Set a value at dotted path location in self.control_map dict."""
        ctrl_path = path.replace(f"{self.comp_path}.", "")
        ctrl_map = self.control_map
        for elem in ctrl_path.split("."):
            if not isinstance(ctrl_map.get(elem), dict):
                ctrl_map[elem] = ctrl
                break
            ctrl_map = ctrl_map.get(elem)

    @property
    def value(self) -> Union[str, Dict[str, Optional[str]]]:
        """
        Return the current value of the control.

        Returns
        -------
        Optional[Dict[str, Any]]
            Control value dictionary.

        """
        return self._get_val_from_ctrl(self.control_map)

    @value.setter
    def value(self, value: Union[str, Dict[str, Optional[str]]]):
        """Set value of controls from dict."""
        if isinstance(value, dict):
            self._set_ctrl_from_val(path="", value=value)

    def _get_val_from_ctrl(self, val_dict):
        """Recursive get values from control dictionary."""
        ctrl_val = {}
        if not val_dict:
            return val_dict
        for name, value in val_dict.items():
            if isinstance(value, widgets.Label):
                continue
            if isinstance(value, dict):
                ctrl_val[name] = self._get_val_from_ctrl(value)
            elif isinstance(value, widgets.Widget):
                ctrl_val[name] = widget_to_py(ctrl=value)
            else:
                ctrl_val[name] = value
        return ctrl_val

    def _set_ctrl_from_val(self, path, value):
        if value is None:
            return
        for key, val in value.items():
            sub_path = f"{path}.{key}" if path else key
            if isinstance(val, dict):
                if isinstance(self.controls[key], widgets.Textarea):
                    self.controls[key].value = py_to_widget(
                        val, ctrl=self.controls[key]
                    )
                else:
                    self._set_ctrl_from_val(sub_path, val)
            elif key in self.controls:
                self.controls[key].value = py_to_widget(val, ctrl=self.controls[key])

    @property
    def layout(self):
        """Return the widget layout for the control."""
        return widgets.VBox(list(self.controls.values()))

    def _create_select_ctrl(self, ctrl_defn, name, curr_value):
        val_type, val_opts = ctrl_defn
        if curr_value is None:
            curr_value = val_opts.get("default")

        if val_type == "str":
            return widgets.Text(
                description=name,
                value=py_to_widget(curr_value, val_type=val_type),
                style=self._W_STYLE,
            )
        if val_type == "bool":
            if isinstance(curr_value, str):
                curr_value = curr_value.casefold() == "true"
            return widgets.Checkbox(
                description=name,
                value=py_to_widget(curr_value, val_type=val_type),
                style=self._W_STYLE,
            )
        if val_type in ("enum", "m_enum"):
            return widgets.SelectMultiple(
                description=name,
                options=val_opts.get("options"),
                value=curr_value or [],
                style=self._W_STYLE,
            )
        if val_type == "txt_dict":
            wgt = widgets.Textarea(
                description=name,
                style=self._W_STYLE,
            )
            setattr(wgt, "tag", "txt_dict")
            wgt.value = py_to_widget(curr_value, ctrl=wgt) or ""
            return wgt
        raise TypeError(f"Unknown definition type {val_type} for {name}/{ctrl_defn}")
