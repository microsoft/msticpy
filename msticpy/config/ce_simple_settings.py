# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Simple settings component edit base class."""
import ipywidgets as widgets

from .._version import VERSION
from .ce_common import get_or_create_mpc_section, get_wgt_ctrl
from .comp_edit import CompEditSimple
from .mp_config_control import MpConfigControls

__version__ = VERSION
__author__ = "Ian Hellen"


class CESimpleSettings(CompEditSimple):
    """Base class for components containing no item list."""

    _DESCRIPTION = ""
    _COMP_PATH = ""
    _HELP_TEXT = ""
    _HELP_URI = {
        "MSTICPy Configuration": (
            "https://msticpy.readthedocs.io/en/latest/"
            + "getting_started/msticpyconfig.html"
        )
    }

    def __init__(self, mp_controls: MpConfigControls):
        """Initialize the class. Set the controls and retrieve settings."""
        super().__init__(description=self._DESCRIPTION)

        self.mp_controls = mp_controls
        self.comp_defn = self._get_settings_path(
            mp_controls.config_defn, self._COMP_PATH
        )
        self.settings = self._get_settings_path(mp_controls.mp_config, self._COMP_PATH)

        self.help.set_help(self._HELP_TEXT, self._HELP_URI)

        self.controls = {}

        get_or_create_mpc_section(self.mp_controls, self._COMP_PATH)
        prov_defn = mp_controls.get_defn(self._COMP_PATH)

        w_style = {
            "style": {"description_width": "100px"},
            "layout": widgets.Layout(width="80%"),
        }
        self.controls = {
            setting: get_wgt_ctrl(self._COMP_PATH, setting, mp_controls, w_style)
            for setting in prov_defn
        }
        self.edit_frame.children = list(self.controls.values())
        self.btn_save.on_click(self._save_settings)

    def _save_settings(self, btn):
        del btn
        prov_path = f"{self._COMP_PATH}"
        self.mp_controls.save_ctrl_values(prov_path)
        val_results = self.mp_controls.validate_setting(prov_path)
        status = "  ".join(res[1] for res in val_results if not res[0])
        if status:
            self.set_status(status)

    def _check_instance_settings(self):
        if not self._DESCRIPTION:
            raise NotImplementedError("You must set a description for this class.")
        if not self._COMP_PATH:
            raise NotImplementedError("You must set the Section name for this class.")
        if not self._HELP_TEXT:
            raise NotImplementedError("You supply help text for this class.")
