# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Module docstring."""
from typing import Any, Dict, Optional, Union

import ipywidgets as widgets
from IPython.display import display

from .._version import VERSION
from .ce_azure import CEAzure
from .ce_azure_sentinel import CEAzureSentinel
from .ce_data_providers import CEDataProviders
from .ce_keyvault import CEKeyVault
from .ce_other_providers import CEOtherProviders
from .ce_ti_providers import CETIProviders
from .ce_user_defaults import CEAutoLoadComps, CEAutoLoadQProvs
from .comp_edit import CETabControlDef, CompEditDisplayMixin, CompEditTabs
from .mp_config_control import MpConfigControls, get_mpconfig_definitions
from .mp_config_file import MpConfigFile

__version__ = VERSION
__author__ = "Ian Hellen"


# pylint: disable=too-many-instance-attributes
class MpConfigEdit(CompEditDisplayMixin):
    """Msticpy Configuration helper class."""

    _TAB_DEFINITIONS = {
        "MicrosoftSentinel": CEAzureSentinel,
        "TI Providers": CETIProviders,
        "Data Providers": CEDataProviders,
        "GeoIP Providers": CEOtherProviders,
        "Key Vault": CEKeyVault,
        "Azure": CEAzure,
        "Autoload QueryProvs": CEAutoLoadQProvs,
        "Autoload Components": CEAutoLoadComps,
    }

    def __init__(
        self,
        settings: Optional[Union[Dict[str, Any], MpConfigFile, str]] = None,
        conf_filepath: str = None,
    ):
        """
        Initialize instance of MpConfigEdit.

        Parameters
        ----------
        settings : Optional[Union[Dict[str, Any], MpConfigFile, str]], optional
            settings can be one of the following:

            - A dict of msticpyconfig settings
            - An instance of MpConfigFile with loaded settings
            - A file path to a msticpyconfig.yaml

            If None, the settings will be read from the
            default (via MSTICPYCONFIG variable)

        conf_filepath : str
            If settings are passed as MPConfigFile instance or a dict,
            this parameter will override the file path used to save the
            settings.
            If settings is a file path, this parameter is ignored.

        """
        self._lbl_loading = widgets.Label(value="Loading. Please wait.")
        display(self._lbl_loading)
        if isinstance(settings, MpConfigFile):
            self.mp_conf_file = MpConfigFile(
                settings=settings.settings, file=conf_filepath
            )
        elif isinstance(settings, dict):
            self.mp_conf_file = MpConfigFile(settings=settings, file=conf_filepath)
        elif isinstance(settings, str):
            self.mp_conf_file = MpConfigFile(file=settings)
        else:
            # This is the default if neither settings nor conf_filepath are passed.
            self.mp_conf_file = MpConfigFile(file=conf_filepath)
            self.mp_conf_file.load_default()
        self.tool_buttons: Dict[str, widgets.Widget] = {}
        self._inc_loading_label()

        # Get the settings definitions and Config controls object
        mp_def_dict = get_mpconfig_definitions()
        self.mp_controls = MpConfigControls(mp_def_dict, self.mp_conf_file.settings)
        self._inc_loading_label()

        # Set up the tabs
        self.tab_ctrl = CompEditTabs(self._get_tab_definitions())
        self._inc_loading_label()

        self.txt_current_file = widgets.Text(
            description="Conf File",
            value=self.current_config_file,
            layout=widgets.Layout(width="75%"),
        )
        self.btn_save = widgets.Button(
            description="Save Settings",
            tooltip="Save current settings to your config file.",
        )
        self.btn_save.on_click(self._save_file)
        self.btn_validate = widgets.Button(
            description="Validate Settings",
            tooltip="Run basic sanity checks on current settings.",
        )
        self.btn_validate.on_click(self._validate_config)
        self.cb_backup = widgets.Checkbox(description="Create backup", value=False)
        self.cb_refresh = widgets.Checkbox(description="Refresh on save", value=True)
        vbox = widgets.VBox(
            [
                self.txt_current_file,
                widgets.HBox(
                    [self.btn_save, self.cb_refresh, self.cb_backup, self.btn_validate]
                ),
                self.mp_conf_file.viewer,
            ]
        )
        self.layout = widgets.VBox([self.tab_ctrl.layout, vbox])
        self._lbl_loading.layout.visibility = "hidden"

    def _inc_loading_label(self):
        self._lbl_loading.value = f"{self._lbl_loading.value}."

    @property
    def tab_names(self):
        """Return a list of current tabs."""
        return self.tab_ctrl.tab_names

    @property
    def controls(self):
        """Return a list of current tab names and controls."""
        return self.tab_ctrl.tab_controls

    def set_tab(self, tab_name: Optional[str], index: int = 0):
        """Programatically set the tab by name or index."""
        self.tab_ctrl.set_tab(tab_name, index)

    def _save_file(self, btn):
        del btn
        if self.txt_current_file.value:
            self.mp_conf_file.save_to_file(
                self.txt_current_file.value, backup=self.cb_backup.value
            )
        if self.cb_refresh.value:
            self.mp_conf_file.refresh_mp_config()

    def _validate_config(self, btn):
        del btn
        self.mp_conf_file.validate_settings()
        mpc_validation = self.mp_controls.validate_all_settings()
        log_val_txt = self.mp_conf_file.txt_viewer.value
        self.mp_conf_file.txt_viewer.value = "\n".join(
            [
                "Logical validation results",
                "--------------------------",
                log_val_txt,
                "",
                "Type validation results",
                "-----------------------",
                *(item.status for item in mpc_validation),
            ]
        )

    def _create_data_tabs(self):
        """Create all tab contents."""
        self.tab_ctrl.tab.children = []
        for name, (ctrl_cls, args) in self._get_tab_definitions().items():
            ctrl = ctrl_cls(*args)
            # add to tabs
            self.tab_ctrl.add_tab(name, control=ctrl)
            # Set these controls as named attributes on the object
            setattr(self, name.replace(" ", "_"), ctrl)

    def _get_tab_definitions(self) -> Dict[str, CETabControlDef]:
        """Return tab definitions and arguments."""
        return {
            name: (cls, [self.mp_controls])
            for name, cls in self._TAB_DEFINITIONS.items()
        }

    @property
    def current_config_file(self):
        """Return the currently loaded configuration file path."""
        return self.mp_conf_file.current_file
