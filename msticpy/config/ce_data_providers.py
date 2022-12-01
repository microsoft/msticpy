# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Data Providers Component Edit."""
import re
from typing import Optional

import ipywidgets as widgets

from .._version import VERSION
from .ce_common import TEXT_LAYOUT
from .ce_provider_base import HELP_URIS, CEProviders
from .mp_config_control import MpConfigControls

__version__ = VERSION
__author__ = "Ian Hellen"


# pylint: disable=too-many-ancestors, duplicate-code
class CEDataProviders(CEProviders):
    """Data providers edit component."""

    _DESCRIPTION = "Settings for Data Providers"
    _COMP_PATH = "DataProviders"
    # _HELP_TEXT inherited from base
    _HELP_URI = {
        "Data Providers": (
            "https://msticpy.readthedocs.io/en/latest/" + "DataAcquisition.html"
        ),
        "Spunk": (
            "https://msticpy.readthedocs.io/en/latest/data_acquisition/SplunkProvider.html"
        ),
        "Sumologic": (
            "https://github.com/microsoft/msticpy/blob/main/docs/notebooks/"
            "Sumologic-DataConnector.ipynb"
        ),
        "Security Datasets (aka Mordor)": (
            "https://msticpy.readthedocs.io/en/latest/data_acquisition/MordorData.html"
        ),
        **HELP_URIS,
    }

    _COMPONENT_HELP = """
    <p><b>LocalData provider <i>data_paths</i></b>
    Enter one or more data paths, separated by new lines
    </p>

    """

    def __init__(self, mp_controls: MpConfigControls):
        """
        Initialize an instance of the component.

        Parameters
        ----------
        mp_controls : MpConfigControls
            The config/controls/settings database

        """
        self.text_prov_instance = widgets.Text(
            description="Provider instance",
            placeholder="(optional) instance name for provider",
            **TEXT_LAYOUT,
        )
        super().__init__(mp_controls)
        self._last_instance_path: Optional[str] = None

    @property
    def _current_path(self):
        if self._form_current_instance_name:
            return f"{self._COMP_PATH}.{self._prov_ctrl_name}-{self._form_current_instance_name}"
        return f"{self._COMP_PATH}.{self._prov_ctrl_name}"

    @property
    def _prov_name(self) -> str:
        if self.text_prov_instance.value:
            return f"{super()._prov_name}-{self.text_prov_instance.value}"
        return super()._prov_name

    @property
    def _prov_ctrl_name(self):
        """Return the provider generic name (minus instance suffix)."""
        if "-" in super()._prov_name:
            return super()._prov_name.split("-", maxsplit=1)[0]
        return super()._prov_name

    @property
    def _select_prov_instance_name(self):
        """Return the provider instance name (minus instance suffix)."""
        if "-" in super()._prov_name:
            return super()._prov_name.split("-", maxsplit=1)[1]
        return ""

    @property
    def _form_current_instance_name(self):
        """Return the current instance name."""
        return self.text_prov_instance.value.strip()

    def _populate_edit_ctrls(
        self,
        control_name: Optional[str] = None,
        new_provider: bool = False,
    ):
        """Retrieve and populate form controls for the provider to display."""
        super()._populate_edit_ctrls(
            control_name=control_name, new_provider=new_provider
        )
        # add the instance text box
        self.edit_ctrls.children = [
            self.text_prov_instance,
            *(self.edit_ctrls.children),
        ]
        self.edit_frame.children = [self.edit_ctrls]

    def _select_provider(self, change):
        self.text_prov_instance.value = self._select_prov_instance_name
        super()._select_provider(change)
        self._last_instance_path = self._current_path

    def _save_provider(self, btn):
        if self._form_current_instance_name:
            if not re.match(r"^[\w._:]+$", self._form_current_instance_name):
                self.set_status(
                    "Error: instance name can only contain alphanumeric and '._:'"
                )
                return
            # The instance name may have changed, which alters the path
            if self._last_instance_path != self._current_path:
                self.mp_controls.rename_path(
                    self._last_instance_path, self._current_path
                )
        super()._save_provider(btn)
        # refresh the item list and re-select the current item
        edited_provider = self._prov_name
        self.select_item.options = self._get_select_opts()
        self.select_item.label = edited_provider
