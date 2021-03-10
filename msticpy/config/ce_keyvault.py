# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Key Vault component edit."""
import ipywidgets as widgets

from .._version import VERSION
from .comp_edit import CESimpleBase
from .ce_common import get_wgt_ctrl, get_or_create_mpc_section
from .mp_config_control import MpConfigControls

__version__ = VERSION
__author__ = "Ian Hellen"


class CEKeyVault(CESimpleBase):
    """Key Vault settings edit component."""

    _DESCRIPTION = "Key Vault Setup"
    _COMP_PATH = "KeyVault"
    _HELP_TEXT = """
    Supply the parameters for your Key Vault here to store secret
    values such as API Keys.<br>

    Check "UseKeyring" if you have Keyring installed and want to be
    able to cache the secrets locally.<br>

    The first five items are mandatory. If you are not using a regional
    or private cloud the value for "Authority" should be global.
    """
    _HELP_URI = {
        "Key Vault Settings": (
            "https://msticpy.readthedocs.io/en/latest/getting_started/"
            + "msticpyconfig.html#specifying-secrets-as-key-vault-secrets"
        )
    }

    def __init__(self, mp_controls: MpConfigControls):
        """
        Instantiate the CEKeyVault class.

        Parameters
        ----------
        mp_controls : MpConfigControls
            The config/controls/settings database

        """
        super().__init__(mp_controls)

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
