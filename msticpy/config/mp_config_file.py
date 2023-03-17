# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Msticpy Config class."""

import io
import pprint
from contextlib import redirect_stdout, suppress
from datetime import datetime
from pathlib import Path
from typing import Any, Dict, Optional, Union

import ipywidgets as widgets
import yaml
from IPython.display import display

from .._version import VERSION

try:
    from ..auth.keyvault_client import BHKeyVaultClient, MsticpyKeyVaultConfigError

    _KEYVAULT = True
except ImportError:
    _KEYVAULT = False

try:
    from ..context.azure.sentinel_core import MicrosoftSentinel

    _SENTINEL = True
except ImportError:
    _SENTINEL = False
from ..common.pkg_config import current_config_path, refresh_config, validate_config
from .comp_edit import CompEditDisplayMixin, CompEditStatusMixin
from .file_browser import FileBrowser

__version__ = VERSION
__author__ = "Ian Hellen"


_CONFIG_MAP = {
    "resource_group": "ResourceGroup",
    "subscription_id": "SubscriptionId",
    "tenant_id": "TenantId",
    "workspace_id": "WorkspaceId",
    "workspace_name": "WorkspaceName",
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
    in a Jupyter notebook to use interactive version.

    """

    _DEF_FILENAME = "./msticpyconfig.yaml"

    def __init__(
        self,
        file: Union[str, Path, None] = None,
        settings: Optional[Dict[str, Any]] = None,
    ):
        """
        Create an instance of the MSTICPy Configuration helper class.

        Parameters
        ----------
        file : Optional[str], optional
            config file path to use.
            If `file` is not supplied, the msticpyconfig path defaults to the system
            default location for msticpyconfig.yaml (MSTICPYCONFIG env var,
            current directory, home directory). This path will also be used as
            the location to save any changed settings.
            If `file` is supplied and exists, the configuration will be loaded from here,
            unless `settings` parameter is also is supplied. If `settings` is supplied
            the `file` path is used as the file to save configuration but
            the configuration settings are taken from the `settings` parameter
            If `file` is supplied but does not exist, no configuration
            is loaded but this path will be used as the file name to
            save any configuration by default None
        settings : Optional[Dict[str, Any]], optional
            setting dict to load, by default None
            If a settings dictionary is supplied it will override any settings
            read from the `file` parameter.

        """
        self.kv_client: Any = None

        # Set up controls
        self.file_browser = FileBrowser(select_cb=self.load_from_file)
        self.txt_viewer = widgets.Textarea(
            layout=widgets.Layout(width="99%", height="300px")
        )
        self.btn_close = widgets.Button(description="Close viewer")
        self.btn_close.on_click(self._close_view)

        self.html_title = widgets.HTML("<h3>MSTICPy settings</h3>")
        self.txt_current_config_path = widgets.Text(
            description="Current file", **_TXT_STYLE
        )
        self.txt_default_config_path = widgets.Text(
            description="Default Config path", **_TXT_STYLE
        )
        self._txt_import_url = widgets.Text(
            description="MS Sentinel Portal URL", **_TXT_STYLE
        )
        self._last_workspace: Dict[str, Dict[str, str]]
        self.buttons: Dict[str, widgets.Button] = {}
        self.btn_pane = self._setup_buttons()
        self.info_pane = widgets.VBox(
            [
                self.txt_current_config_path,
                self.txt_default_config_path,
                self._txt_import_url,
            ],
            layout=self.border_layout("80%"),
        )
        self.viewer = widgets.VBox([])
        self.edit_controls = widgets.VBox([self.btn_pane, self.info_pane])
        self.layout = widgets.VBox(
            [
                self.html_title,
                self.edit_controls,
                self.viewer,
            ],
            layout=self.border_layout("99%"),
        )

        if file is None:
            self.current_file = current_config_path()
            if self.current_file is None:
                self.current_file = self._DEF_FILENAME
        else:
            self.current_file = file

        # set the default location even if user supplied file parameter
        self.mp_config_def_path = current_config_path() or self.current_file

        if settings is not None:
            # If caller has supplied settings, we don't want to load
            # anything from a file
            self.settings = settings
            return
        # otherwise load settings from the current config file.
        if self.current_file and Path(self.current_file).is_file():
            self.load_from_file(self.current_file)
        else:
            # no file so set default empty settings
            self.settings = {}
            self.set_status(f"Filename does not exist: '{self.current_file}'.")

    @property
    def current_file(self):
        """Return currently loaded file path."""
        return self.txt_current_config_path.value

    @current_file.setter
    def current_file(self, file_name: Union[str, Path]):
        """Set currently loaded file path."""
        self.txt_current_config_path.value = str(file_name)

    @property
    def default_config_file(self):
        """Return default msticpyconfig path."""
        return self.txt_default_config_path.value

    @default_config_file.setter
    def default_config_file(self, file_name: Union[str, Path]):
        """Set default msticpyconfig path."""
        self.txt_default_config_path.value = file_name

    def load_default(self):
        """Load default settings specified by MSTICPYCONFIG env var."""
        if self.mp_config_def_path:
            self.current_file = self.mp_config_def_path
            with suppress(FileNotFoundError):
                self.load_from_file(self.mp_config_def_path)

    def browse_for_file(self, show: bool = True):
        """Open the browser to browser/search for a file."""
        self.viewer.children = [self.file_browser.layout, self.btn_close]
        if show:
            display(self.viewer)

    def load_from_file(self, file: Union[str, Path]):
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
        with open(file, "w", encoding="utf-8") as mp_hdl:
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

    @staticmethod
    def get_workspace_from_url(url: str) -> Dict[str, Dict[str, str]]:
        """
        Return workspace settings from Sentinel portal URL.

        Parameters
        ----------
        url : str
            The URL of the Azure portal page for the Sentinel workspace.

        Returns
        -------
        Dict[str, Dict[str, str]]
            Dictionary with a single element keyed by workspace name
            The value is the workspace settings dictionary for the
            workspace.

        """
        return MicrosoftSentinel.get_workspace_details_from_url(url)

    def _show_sentinel_workspace(self, show: bool = True):
        """Fetch settings from Sentinel Portal URL."""
        if not self._txt_import_url.value:
            return
        self._last_workspace = self.get_workspace_from_url(self._txt_import_url.value)
        workspace_settings = pprint.pformat(self._last_workspace, compact=True)
        self.txt_viewer.value = "\n".join(
            [
                workspace_settings,
                "\n"
                "Use 'Import into settings' button to import into current settings.",
            ]
        )
        self.viewer.children = [self.txt_viewer, self.btn_close]
        if self._last_workspace:
            self.buttons["import_workspace"].disabled = False
        if show:
            display(self.viewer)

    def _import_sentinel_settings(self):
        """Import sentinel settings from portal URL."""
        if not self._last_workspace:
            return
        curr_workspaces = self.settings.get("AzureSentinel", {}).get("Workspaces")
        curr_workspaces.update(self._last_workspace)
        self.view_settings()

    def _read_mp_config(self, file):
        if Path(file).is_file():
            with open(file, "r", encoding="utf-8") as mp_hdl:
                try:
                    return yaml.safe_load(mp_hdl)
                except yaml.scanner.ScannerError as err:
                    self.set_status(str(err))
        raise FileNotFoundError(f"Cannot read file {file}")

    def map_json_to_mp_ws(self):
        """Map config.json settings to MSTICPy settings."""
        if "resource_group" in self.settings:
            ws_settings = {
                config: self.settings[entry] for entry, config in _CONFIG_MAP.items()
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

    _BUTTON_DEFS = {
        "load": {
            "description": "Load File",
            "tooltip": "Load a settings file from a path.",
        },
        "load_def": {
            "description": "Load default",
            "tooltip": "Load default settings from configured msticpyconfig.yaml.",
        },
        "reload": {
            "description": "Reload",
            "tooltip": "Reload current saved settings to MSTICPy.",
        },
        "view": {
            "description": "View Settings",
            "tooltip": "Display settings in viewer.",
        },
        "convert": {
            "description": "Convert to MP",
            "tooltip": "Convert config.json settings to MSTICPy format.",
        },
        "save": {
            "description": "Save File",
            "tooltip": "Save current settings to current file.",
        },
        "validate": {
            "description": "Validate Settings",
            "tooltip": "Run validation checks on current settings.",
        },
        "showkv": {
            "description": "Show Key Vault secrets",
            "tooltip": "Show Key Vault secrets from current config.",
        },
        "get_workspace": {
            "description": "Get Workspace from URL",
            "tooltip": "Import Sentinel workspace settings from portal URL.",
        },
        "import_workspace": {
            "description": "Import into settings",
            "tooltip": "Import retrieved Workspace settings into current settings.",
            "disabled": True,
        },
    }

    def _setup_buttons(self):
        btn_style = {"layout": widgets.Layout(width="200px")}
        self.buttons["load"] = widgets.Button(
            **(self._BUTTON_DEFS["load"]), **btn_style
        )
        self.buttons["load_def"] = widgets.Button(
            **(self._BUTTON_DEFS["load_def"]), **btn_style
        )
        self.buttons["reload"] = widgets.Button(
            **(self._BUTTON_DEFS["reload"]), **btn_style
        )
        self.buttons["view"] = widgets.Button(
            **(self._BUTTON_DEFS["view"]), **btn_style
        )
        self.buttons["validate"] = widgets.Button(
            **(self._BUTTON_DEFS["validate"]), **btn_style
        )
        self.buttons["convert"] = widgets.Button(
            **(self._BUTTON_DEFS["convert"]), **btn_style
        )
        self.buttons["save"] = widgets.Button(
            **(self._BUTTON_DEFS["save"]), **btn_style
        )
        self.buttons["showkv"] = widgets.Button(
            **(self._BUTTON_DEFS["showkv"]), **btn_style
        )
        self.buttons["get_workspace"] = widgets.Button(
            **(self._BUTTON_DEFS["get_workspace"]), **btn_style
        )
        self.buttons["import_workspace"] = widgets.Button(
            **(self._BUTTON_DEFS["import_workspace"]), **btn_style
        )
        self._btn_view_setting = widgets.Button(
            description="Get Workspace", **btn_style
        )
        self._btn_import_settings = widgets.Button(
            description="Import into settings", disabled=True, **btn_style
        )

        self.buttons["load"].on_click(self._btn_func_no_disp("browse_for_file"))
        self.buttons["load_def"].on_click(self._btn_func("load_default"))
        self.buttons["view"].on_click(self._btn_func_no_disp("view_settings"))
        self.buttons["validate"].on_click(self._btn_func_no_disp("validate_settings"))
        self.buttons["convert"].on_click(self._convert_to_mp)
        self.buttons["save"].on_click(self._save_file)
        self.buttons["reload"].on_click(self._btn_func("refresh_mp_config"))
        self.buttons["showkv"].on_click(self._btn_func_no_disp("show_kv_secrets"))
        self.buttons["get_workspace"].on_click(
            self._btn_func("_show_sentinel_workspace")
        )
        self.buttons["import_workspace"].on_click(
            self._btn_func("_import_sentinel_settings")
        )

        btns1 = widgets.VBox(list(self.buttons.values())[: len(self.buttons) // 2])
        # flake8: noqa: E203
        # conflicts with Black formatting
        btns2 = widgets.VBox(list(self.buttons.values())[len(self.buttons) // 2 :])
        btns_all = widgets.HBox([btns1, btns2])
        return widgets.VBox(
            [widgets.Label(value="Operations"), btns_all],
            # layout=self.border_layout("50%"),
        )
