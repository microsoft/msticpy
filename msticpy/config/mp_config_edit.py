# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Module docstring."""
from typing import Any, Dict, Optional, Union

import ipywidgets as widgets

from .._version import VERSION
from .ce_azure_sentinel import CEAzureSentinel
from .ce_data_providers import CEDataProviders
from .ce_keyvault import CEKeyVault
from .ce_other_providers import CEOtherProviders
from .ce_ti_providers import CETIProviders
from .ce_user_defaults import CEAutoLoadComps, CEAutoLoadQProvs
from .comp_edit import CompEditDisplayMixin, CompEditTabs
from .mp_config_file import MpConfigFile
from .mp_config_control import MpConfigControls, get_mpconfig_definitions

__version__ = VERSION
__author__ = "Ian Hellen"


class MpConfigEdit(CompEditDisplayMixin):
    """Msticpy Configuration helper class."""

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
        if isinstance(settings, MpConfigFile):
            self.mp_conf_file = MpConfigFile(settings=settings.settings)
            if not self.mp_conf_file.current_file and conf_filepath:
                self.mp_conf_file.current_file = conf_filepath
        elif isinstance(settings, dict):
            self.mp_conf_file = MpConfigFile(settings=settings)
            if not self.mp_conf_file.current_file and conf_filepath:
                self.mp_conf_file.current_file = conf_filepath
        elif isinstance(settings, str):
            self.mp_conf_file = MpConfigFile()
            self.mp_conf_file.load_from_file(file=settings)
        else:
            self.mp_conf_file = MpConfigFile()
            self.mp_conf_file.load_default()
        self.tool_buttons: Dict[str, widgets.Widget] = {}

        mp_def_dict = get_mpconfig_definitions()
        self.mp_controls = MpConfigControls(mp_def_dict, self.mp_conf_file.settings)
        self.tab_ctrl = CompEditTabs()
        self.controls: Dict[str, Any] = {}
        self._create_data_tabs(self.mp_controls)

        self.txt_current_file = widgets.Text(
            description="Conf File",
            value=self.current_config_file,
            layout=widgets.Layout(width="75%"),
        )
        self.btn_save = widgets.Button(description="Save Settings")
        self.btn_save.on_click(self._save_file)
        self.btn_validate = widgets.Button(description="Validate Settings")
        self.btn_validate.on_click(self._validate_config)
        self.cb_backup = widgets.Checkbox(description="Create backup", value=False)
        vbox = widgets.VBox(
            [
                self.txt_current_file,
                widgets.HBox([self.btn_save, self.btn_validate]),
                self.mp_conf_file.viewer,
            ]
        )
        self.layout = widgets.VBox([self.tab_ctrl.layout, vbox])

    @property
    def tab_names(self):
        """Return a list of current tabs."""
        return self.tab_ctrl.tab_names

    def set_tab(self, tab_name: Optional[str], index: int = 0):
        """Programatically set the tab by name or index."""
        self.tab_ctrl.set_tab(tab_name, index)

    def _save_file(self, btn):
        del btn
        if self.txt_current_file.value:
            self.mp_conf_file.save_to_file(
                self.txt_current_file.value, backup=self.cb_backup.value
            )

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

    def _create_data_tabs(self, mp_controls):
        self.controls = {
            "AzureSentinel": CEAzureSentinel(mp_controls),
            "TI Providers": CETIProviders(mp_controls),
            "Data Providers": CEDataProviders(mp_controls),
            "GeoIP Providers": CEOtherProviders(mp_controls),
            "Key Vault": CEKeyVault(mp_controls),
            "Autoload QueryProvs": CEAutoLoadQProvs(mp_controls),
            "Autoload Components": CEAutoLoadComps(mp_controls),
        }
        self.tab_ctrl.tab.children = []
        for name, ctrl in self.controls.items():
            # add to tabs
            self.tab_ctrl.add_tab(name, control=ctrl)
            # Set these controls as named attributes on the object
            setattr(self, name.replace(" ", "_"), ctrl)

    @property
    def current_config_file(self):
        """Return the currently loaded configuration file path."""
        return self.mp_conf_file.current_file
