# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Msticpy Config class."""
import io
import os
import pprint
from contextlib import redirect_stdout
from datetime import datetime
from pathlib import Path
from typing import Any, Dict, Optional

import ipywidgets as widgets
import yaml
from IPython.display import display

from .._version import VERSION

try:
    from ..common.keyvault_client import BHKeyVaultClient, MsticpyKeyVaultConfigError

    _KEYVAULT = True
except ImportError:
    _KEYVAULT = False
from ..common.pkg_config import refresh_config, validate_config
from .comp_edit import CompEditStatusMixin, CompEditDisplayMixin
from .file_browser import FileBrowser

__version__ = VERSION
__author__ = "Ian Hellen"


_CONFIG_MAP = {
    "resource_group": "ResourceGroup",
    "subscription_id": "SubscriptionId",
    "tenant_id": "TenantId",
    "workspace_id": "WorkspaceId",
}

_TXT_STYLE = {
    "style": {"description_width": "150px"},
    "layout": widgets.Layout(width="99%"),
}


# pylint: disable=too-many-instance-attributes
class MpConfigFile(CompEditStatusMixin, CompEditDisplayMixin):
    """
    MSTICPy Configuration management class.

    Use the functions from the commandline or display
    in a Jupter notebook to use interactive version.

    """

    def __init__(
        self, file: Optional[str] = None, settings: Optional[Dict[str, Any]] = None
    ):
        """
        Create an instance of the MSTICPy Configuration helper class.

        Parameters
        ----------
        file : Optional[str], optional
            config file to load, by default None
        settings : Optional[Dict[str, Any]], optional
            setting dict to load, by default None

        """
        self.settings = settings or {}

        self.kv_client: Any = None
        self.mp_config_def_path = os.environ.get("MSTICPYCONFIG", "")
        self._current_file = None

        # Set up controls
        self.file_browser = FileBrowser(select_cb=self.load_from_file)
        self.txt_viewer = widgets.Textarea(
            layout=widgets.Layout(width="99%", height="300px")
        )
        self.btn_close = widgets.Button(description="Close")
        self.btn_close.on_click(self._close_view)

        self.html_title = widgets.HTML("<h3>MSTICPy settings</h3>")
        self.txt_current_file = widgets.Text(description="Current file", **_TXT_STYLE)
        self.txt_current_file.value = self.current_file or ""
        self.txt_current_file.observe(self._update_curr_file, "value")
        self.txt_curr_mpconfig = widgets.Text(
            description="Value of MSTICPCONFIG", **_TXT_STYLE
        )
        self.txt_curr_mpconfig.value = self.mp_config_def_path
        self.txt_curr_mpconfig.disabled = True

        self.buttons: Dict[str, widgets.Button] = {}
        self.btn_pane = self._setup_buttons()
        self.info_pane = widgets.VBox(
            [
                self.txt_current_file,
                self.txt_curr_mpconfig,
            ],
            layout=self.border_layout("60%"),
        )
        self.viewer = widgets.VBox([])
        self.layout = widgets.VBox(
            [
                self.html_title,
                widgets.HBox([self.info_pane, self.btn_pane]),
                self.viewer,
            ],
            layout=self.border_layout("99%"),
        )

        self.current_file = file
        if file and Path(file).is_file():
            self.load_from_file(file)

    @property
    def current_file(self):
        """Return currently loaded file path."""
        return self._current_file

    @current_file.setter
    def current_file(self, file_name: str):
        self._current_file = str(file_name) if file_name else None
        self.txt_current_file.value = self._current_file or ""

    def _update_curr_file(self, change):
        del change
        self.current_file = self.txt_current_file.value

    def load_default(self):
        """Load default settings specified by MSTICPYCONFIG env var."""
        if self.mp_config_def_path:
            self.load_from_file(self.mp_config_def_path)
            self.current_file = self.mp_config_def_path

    def browse_for_file(self, show: bool = True):
        """Open the browser to browser/search fr a file."""
        self.viewer.children = [self.file_browser.layout, self.btn_close]
        if show:
            display(self.viewer)

    def load_from_file(self, file: str):
        """Load settings from `file`."""
        self.settings = self._read_mp_config(file)
        self.current_file = file

    def view_settings(self, show: bool = True):
        """View the current settings as text."""
        self.txt_viewer.value = pprint.pformat(self.settings, compact=True)
        self.viewer.children = [self.txt_viewer, self.btn_close]
        if show:
            display(self.viewer)

    def _close_view(self, btn):
        del btn
        self.viewer.children = []

    def validate_settings(self, show: bool = True):
        """Run the validator against currently loaded settings."""
        results = io.StringIO()
        with redirect_stdout(results):
            validate_config(mp_config=self.settings)
        self.txt_viewer.value = results.getvalue()
        self.viewer.children = [self.txt_viewer, self.btn_close]
        if show:
            display(self.viewer)

    def save_to_file(self, file: str, backup: bool = True):
        """
        Save current configuration to `file`.

        Parameters
        ----------
        file : str
            The file path to save to.
        backup : bool, optional
            Create a backup file, if overwriting existing file,
            by default True

        """
        # remove empty settings sections before saving
        empty_items = [
            section for section, settings in self.settings.items() if not settings
        ]
        for empty_section in empty_items:
            del self.settings[empty_section]
        # create a backup, if required
        if backup and Path(file).is_file():
            Path(file).replace(f"{file}.save_{datetime.now().strftime('%H%M%S')}")
        with open(file, "w") as mp_hdl:
            yaml.safe_dump(self.settings, mp_hdl)

    def show_kv_secrets(self, show: bool = True):
        """Show secrets from currently configured Key Vault."""
        view_text = []
        if not _KEYVAULT:
            self.txt_viewer.value = "\n".join(
                [
                    "Azure keyvault libraries not found.",
                    "Please install 'azure_keyvault_secrets' and",
                    "'azure_mgmt_keyvault'",
                ]
            )
            self.viewer.children = [self.txt_viewer, self.btn_close]
            if show:
                display(self.viewer)
            return
        if self.kv_client is None:
            try:
                self.kv_client = BHKeyVaultClient()
            except MsticpyKeyVaultConfigError:
                view_text = ["Key Vault settings have not been configured correctly."]
        if self.kv_client is not None:
            try:
                secrets = self.kv_client.secrets
            except Exception as err:  # pylint: disable=broad-except
                view_text.extend(
                    [
                        "Could not get secrets list from Key Vault.",
                        "It is likely that the authentication failed."
                        f"The exception is show here: {err}",
                    ]
                )
                secrets = []

            for sec_path in secrets:
                sec_name = sec_path.rsplit("/", maxsplit=1)[-1]
                try:
                    sec_result = f"Value: '{self.kv_client.get_secret(sec_name)}'"
                except Exception:  # pylint: disable=broad-except
                    sec_result = "Value: Could not display secret"
                view_text.append(f"Secret '{sec_name}'  {sec_result}")
        self.txt_viewer.value = "\n".join(view_text)
        self.viewer.children = [self.txt_viewer, self.btn_close]
        if show:
            display(self.viewer)

    def _read_mp_config(self, file):
        if Path(file).is_file():
            with open(file, "r") as mp_hdl:
                try:
                    return yaml.safe_load(mp_hdl)
                except yaml.scanner.ScannerError as err:
                    self.set_status(str(err))
        raise FileNotFoundError(f"Cannot read file {file}")

    def map_json_to_mp_ws(self):
        """Map config.json settings to MSTICPy settings."""
        if "resource_group" in self.settings:
            ws_settings = {
                _CONFIG_MAP[entry]: self.settings[entry] for entry in _CONFIG_MAP
            }
            workspace = self.settings.get("workspace_name", "Default")
            self.settings = {"AzureSentinel": {"Workspaces": {workspace: ws_settings}}}
            return self.settings
        return {}

    @staticmethod
    def refresh_mp_config():
        """Refresh global MSTICPy settings from config file."""
        refresh_config()

    def _convert_to_mp(self, btn):
        del btn
        self.map_json_to_mp_ws()
        self.view_settings()

    def _save_file(self, btn):
        del btn
        if self.current_file:
            self.save_to_file(file=self.current_file, backup=True)

    def _btn_func(self, func_name: str):
        """Wrap methods to be called from button events."""

        def _btn_exec(*args):
            del args
            func = getattr(self, func_name)
            func()

        return _btn_exec

    def _btn_func_no_disp(self, func_name: str):
        """Wrap methods to be called from button events."""

        def _btn_exec(*args):
            del args
            func = getattr(self, func_name)
            func(show=False)

        return _btn_exec

    def _setup_buttons(self):
        btn_style = {"layout": widgets.Layout(width="200px")}
        self.buttons["load"] = widgets.Button(description="Load file", **btn_style)
        self.buttons["load_def"] = widgets.Button(
            description="Load default", **btn_style
        )
        self.buttons["reload"] = widgets.Button(
            description="Reload settings", **btn_style
        )
        self.buttons["view"] = widgets.Button(description="View Settings", **btn_style)
        self.buttons["validate"] = widgets.Button(
            description="Validate Settings", **btn_style
        )
        self.buttons["convert"] = widgets.Button(
            description="Convert to MP", **btn_style
        )
        self.buttons["save"] = widgets.Button(description="Save file", **btn_style)
        self.buttons["showkv"] = widgets.Button(
            description="Show Key Vault secrets", **btn_style
        )

        self.buttons["load"].on_click(self._btn_func_no_disp("browse_for_file"))
        self.buttons["load_def"].on_click(self._btn_func("load_default"))
        self.buttons["view"].on_click(self._btn_func_no_disp("view_settings"))
        self.buttons["validate"].on_click(self._btn_func_no_disp("validate_settings"))
        self.buttons["convert"].on_click(self._convert_to_mp)
        self.buttons["save"].on_click(self._save_file)
        self.buttons["reload"].on_click(self._btn_func("refresh_mp_config"))
        self.buttons["showkv"].on_click(self._btn_func_no_disp("show_kv_secrets"))

        btns1 = widgets.VBox(list(self.buttons.values())[: int(len(self.buttons) / 2)])
        # flake8: noqa: E203
        # conflicts with Black formatting
        btns2 = widgets.VBox(list(self.buttons.values())[int(len(self.buttons) / 2) :])
        btns_all = widgets.HBox([btns1, btns2])
        return widgets.VBox(
            [widgets.Label(value="Operations"), btns_all],
            layout=self.border_layout("39%"),
        )
