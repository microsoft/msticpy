# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Module for Log Analytics-related configuration."""
import contextlib
import json
import os
import re
from pathlib import Path
from typing import Any, Dict, Optional

import ipywidgets as widgets
from IPython.display import display

from .._version import VERSION
from .exceptions import MsticpyUserConfigError
from .pkg_config import get_config, refresh_config
from .utility import export, is_valid_uuid, md, md_warn

__version__ = VERSION
__author__ = "Ian Hellen"


_RESOURCES = [
    (
        "https://github.com/Azure/Azure-Sentinel-Notebooks/blob/"
        "master/ConfiguringNotebookEnvironment.ipynb"
    ),
    (
        "https://github.com/Azure/Azure-Sentinel-Notebooks/blob/"
        "master/A%20Getting%20Started%20Guide%20For%20Azure%20"
        "Sentinel%20ML%20Notebooks.ipynb"
    ),
]

_NO_CONFIG_WARN = [
    "Could not find Microsoft Sentinel settings in msticpyconfig.yaml or a config.json ",
    "(in the current directory or via a MSTICPYCONFIG variable.)",
    "We found the file '{config_file}' and will use this.",
    "We recommend using an explicit msticpyconfig.yaml specified using the",
    "MSTICPYCONFIG environment variable. See:",
    (
        "https://msticpy.readthedocs.io/en/latest/data_acquisition/DataProviders.html#"
        "connecting-to-an-azure-sentinel-workspace"
    ),
    "and",
    "https://msticpy.readthedocs.io/en/latest/getting_started/msticpyconfig.html",
    "for more details.",
    "Also see the notebooks:",
    *_RESOURCES,
]

_NO_CONFIG_ERR = [
    "Could not find msticpyconfig.yaml or config.json.",
    "The 'config.json' file is created when you launch notebooks from "
    "Microsoft Sentinel. If you have copied the notebook to another location "
    "or folder you will need to copy this configuration file.",
    "Alternatively, we recommend using an explicit msticpyconfig.yaml"
    "and adding your Workspace and Tenant IDs to that file.",
    "",
    "You can create a settings file using the following commands:",
    ">>> from msticpy.config import MpConfigEdit",
    ">>> MpConfigEdit()",
]

WIDGET_DEFAULTS = {
    "layout": widgets.Layout(width="95%"),
    "style": {"description_width": "150px"},
}


@export
class WorkspaceConfig:
    """Workspace configuration class."""

    # Constants
    TENANT_ID = "{{cookiecutter.tenant_id}}"
    SUBSCRIPTION_ID = "{{cookiecutter.subscription_id}}"
    RESOURCE_GROUP = "{{cookiecutter.resource_group}}"
    WORKSPACE_ID = "{{cookiecutter.workspace_id}}"
    WORKSPACE_NAME = "{{cookiecutter.workspace_name}}"

    CONF_TENANT_ID = "TenantId"
    CONF_WS_ID = "WorkspaceId"
    CONF_SUB_ID = "SubscriptionId"
    CONF_RES_GROUP = "ResourceGroup"
    CONF_WS_NAME = "WorkspaceName"
    CONF_ARGS = "Args"

    # Legacy key names / constants
    PKG_CONF_TENANT_KEY = "TenantId"
    PKG_CONF_WS_KEY = "WorkspaceId"
    PKG_CONF_SUB_KEY = "SubscriptionId"
    PKG_CONF_RES_GROUP_KEY = "ResourceGroup"
    PKG_CONF_NAME_KEY = "WorkspaceName"
    PKG_CONF_ARGS_KEY = "Args"

    CONF_WS_ID_KEY = "workspace_id"
    CONF_TENANT_ID_KEY = "tenant_id"
    CONF_SUB_ID_KEY = "subscription_id"
    CONF_RES_GROUP_KEY = "resource_group"
    CONF_WS_NAME_KEY = "workspace_name"
    CONF_ARGS_KEY = "args"

    _SETTINGS_TO_CONFIG_NAME_MAP = {
        CONF_TENANT_ID: CONF_TENANT_ID_KEY,
        CONF_WS_ID: CONF_WS_ID_KEY,
        CONF_SUB_ID: CONF_SUB_ID_KEY,
        CONF_RES_GROUP: CONF_RES_GROUP_KEY,
        CONF_WS_NAME: CONF_WS_NAME_KEY,
        CONF_ARGS: CONF_ARGS_KEY,
    }
    _CONFIG_TO_SETTINGS_NAME_MAP = {
        val: key for key, val in _SETTINGS_TO_CONFIG_NAME_MAP.items()
    }

    def __init__(
        self,
        workspace: Optional[str] = None,
        config_file: Optional[str] = None,
        interactive: bool = True,
        config: Optional[Dict[str, str]] = None,
    ):
        """
        Load current Azure Notebooks configuration for Log Analytics.

        Parameters
        ----------
        config_file : Optional[str], optional
            path to a configuration file,
            If not specified, the defaults is to use a configured msticpyconfig.yaml
            If this isn't configured, it will search for (first) a config.json
            and (second) a msticpyconfig.yaml in (first) the current directory
            and (second) the parent directory and subfolders.
        workspace : Optional[str]
            Workspace name (where multiple workspaces are configured),
            by default the Default workspace will be used.
        interactive : bool, optional
            If this is False, initializing the class will not raise an
            exception if no configuration is found. By default, True.
        config : Optional[Dict[str, str]],
            Workspace configuration as dictionary.

        """
        self._config: Dict[str, Any] = {}
        self._interactive = interactive
        self._config_file = config_file
        self.workspace_key = workspace or "Default"
        self.settings_key: Optional[str] = None

        # If config file specified, use that
        if config:
            self._config.update(config)
        elif config_file:
            self._config.update(self._read_config_values(config_file))
        else:
            self._determine_config_source(workspace)

    def __getattr__(self, attribute: str):
        """Return attribute from configuration."""
        with contextlib.suppress(KeyError):
            return self[attribute]
        raise AttributeError(
            f"{self.__class__.__name__} has no attribute '{attribute}'"
        )

    def __getitem__(self, key: str):
        """Allow property get using dictionary key syntax."""
        if key in self._SETTINGS_TO_CONFIG_NAME_MAP:
            return self._config.get(key)
        if key in self._CONFIG_TO_SETTINGS_NAME_MAP:
            return self._config.get(self._CONFIG_TO_SETTINGS_NAME_MAP[key])
        raise KeyError(f"{self.__class__.__name__} has no attribute '{key}'")

    def __setitem__(self, key: str, value: Any):
        """Allow property set using dictionary key syntax."""
        self._config[key] = value

    def __contains__(self, key: str):
        """Allow property in test."""
        # In operator overload
        return (
            key == "Type"
            or key in self._config
            or self._CONFIG_TO_SETTINGS_NAME_MAP.get(key) in self._config
            or key in self.__dict__
        )

    def __repr__(self):
        """Return contents of current config."""
        return self._config.__repr__()

    def get(self, key, default: Any = None):
        """Return key value or default."""
        try:
            return self[key]
        except KeyError:
            return default

    @property
    def config_loaded(self) -> bool:
        """
        Return True if workspace id and tenant id have values.

        Returns
        -------
        bool
            True if configuration loaded.

        """
        ws_value = self._config.get(self.CONF_WS_ID, None)
        ten_value = self._config.get(self.CONF_TENANT_ID, None)
        return is_valid_uuid(ws_value) and is_valid_uuid(ten_value)  # type: ignore

    @property
    def code_connect_str(self) -> str:
        """
        Return the Log Analytics connection string for dev code auth.

        Returns
        -------
        str
            Connection string

        """
        ten_id = self[self.CONF_TENANT_ID]
        ws_id = self[self.CONF_WS_ID]
        if not ten_id:
            raise KeyError(
                f"Configuration setting for {self.CONF_TENANT_ID} "
                + "could not be found."
            )
        if not ws_id:
            raise KeyError(
                f"Configuration setting for {self.CONF_WS_ID} " + "could not be found."
            )
        return f"loganalytics://code().tenant('{ten_id}').workspace('{ws_id}')"

    @property
    def mp_settings(self):
        """Return the equivalent MSTICPY settings dictionary."""
        return {
            self.CONF_WS_NAME: self._config.get(self.CONF_WS_NAME),
            self.CONF_SUB_ID: self._config.get(self.CONF_SUB_ID),
            self.CONF_WS_ID: self._config.get(self.CONF_WS_ID),
            self.CONF_TENANT_ID: self._config.get(self.CONF_TENANT_ID),
            self.CONF_RES_GROUP: self._config.get(self.CONF_RES_GROUP),
            self.CONF_ARGS: self._config.get(self.CONF_ARGS),
        }

    @property
    def args(self) -> Dict[str, str]:
        """Return any additional arguments."""
        return self._config.get(self.CONF_ARGS, {})

    @property
    def settings_path(self) -> Optional[str]:
        """Return the path to the settings in the MSTICPY config."""
        if self.settings_key:
            return f"AzureSentinel.Workspaces.{self.settings_key}"
        return None

    @property
    def settings(self) -> Dict[str, Any]:
        """Return the current settings dictionary."""
        return get_config(self.settings_path, {})

    @classmethod
    def from_settings(cls, settings: Dict[str, Any]) -> "WorkspaceConfig":
        """Create a WorkstationConfig from MSTICPY Workspace settings."""
        return cls(
            config={  # type: ignore
                cls.CONF_WS_NAME: settings.get(cls.CONF_WS_NAME),  # type: ignore
                cls.CONF_SUB_ID: settings.get(cls.CONF_SUB_ID),  # type: ignore
                cls.CONF_WS_ID: settings.get(cls.CONF_WS_ID),  # type: ignore
                cls.CONF_TENANT_ID: settings.get(cls.CONF_TENANT_ID),  # type: ignore
                cls.CONF_RES_GROUP: settings.get(cls.CONF_RES_GROUP),  # type: ignore
            }
        )

    @classmethod
    def from_connection_string(cls, connection_str: str) -> "WorkspaceConfig":
        """Create a WorkstationConfig from a connection string."""
        tenant_regex = r"""
        .*tenant\s?[=\(]\s?['\"]\{?
        (?P<tenant_id>[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12})
        \}?['\"].*"""
        workspace_regex = r"""
        .*workspace\s?[=\(]\s?['\"]\{?
        (?P<workspace_id>[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12})
        \}?['\"].*"""
        ws_name_regex = r".*alias\s?[=\(]\s?['\"]\{?(?P<workspace_name>\w+)['\"].*"

        tenant_id = workspace_id = workspace_name = None
        if match := re.match(tenant_regex, connection_str, re.IGNORECASE | re.VERBOSE):
            tenant_id = match.groupdict()["tenant_id"]
        else:
            raise ValueError("Could not find tenant ID in connection string.")
        if match := re.match(
            workspace_regex, connection_str, re.IGNORECASE | re.VERBOSE
        ):
            workspace_id = match.groupdict()["workspace_id"]
        else:
            raise ValueError("Could not find workspace ID in connection string.")
        if match := re.match(ws_name_regex, connection_str, re.IGNORECASE | re.VERBOSE):
            workspace_name = match.groupdict()["workspace_name"]
        return cls(
            config={
                cls.CONF_WS_ID: workspace_id,  # type: ignore[dict-item]
                cls.CONF_TENANT_ID: tenant_id,  # type: ignore[dict-item]
                cls.CONF_WS_NAME: workspace_name,  # type: ignore[dict-item]
            }
        )

    @classmethod
    def _read_config_values(cls, file_path: str) -> Dict[str, str]:
        """Read configuration file."""
        if not file_path:
            return {}
        with contextlib.suppress(json.JSONDecodeError):
            with open(file_path, "r", encoding="utf-8") as json_file:
                if json_file:
                    config_ws = json.load(json_file)
                    return {
                        cls._CONFIG_TO_SETTINGS_NAME_MAP[key]: value
                        for key, value in config_ws.items()
                        if key in cls._CONFIG_TO_SETTINGS_NAME_MAP
                    }
        return {}

    @classmethod
    def list_workspaces(cls) -> Dict:
        """
        Return list of available workspaces.

        Returns
        -------
        Dict
            Dictionary of workspaces with workspace and tenantIds.

        """
        ws_settings = get_config("AzureSentinel", {}).get("Workspaces")
        return (
            {
                ws_name: {
                    cls.CONF_WS_ID: ws.get(cls.CONF_WS_ID),
                    cls.CONF_TENANT_ID: ws.get(cls.CONF_TENANT_ID),
                }
                for ws_name, ws in ws_settings.items()
            }
            if ws_settings
            else {}
        )

    def prompt_for_ws(self):
        """Display an interactive prompt for Workspace details."""
        md_warn("No Microsoft Sentinel configuration found.")
        md(
            "Please enter the workspace ID and tenant Id"
            + " to allow connection to the Microsoft Sentinel workspace."
        )
        ws_id_wgt = widgets.Text(description="Workspace Id:", **WIDGET_DEFAULTS)
        ten_id_wgt = widgets.Text(description="Tenant Id:", **WIDGET_DEFAULTS)

        def update_ws(chg):
            self["workspace_id"] = chg.get("new")

        def update_tnt(chg):
            self["tenant_id"] = chg.get("new")

        ws_id_wgt.observe(update_ws, names="value")
        ten_id_wgt.observe(update_tnt, names="value")
        display(widgets.VBox([ws_id_wgt, ten_id_wgt]))
        md(
            (
                "You can avoid this prompt in future by following the"
                " guidance in the"
                " <a href='https://msticpy.readthedocs.io/en/latest/"
                "data_acquisition/DataProviders.html#"
                "connecting-to-an-azure-sentinel-workspace' "
                "target='_blank' rel='noopener noreferrer'>"
                "Connecting to a Microsoft Sentinel Workspace</a>"
            ),
        )

    def _determine_config_source(self, workspace):
        # First, try default MSTICPy config
        self._read_pkg_config_values(workspace_name=workspace)
        if self.config_loaded:
            return
        # Next, search for a config.json in the current directory
        if Path("./config.json").exists():
            self._config_file = "./config.json"
        else:
            self._config_file = self._search_for_file("**/config.json")
        if self._config_file:
            self._config.update(self._read_config_values(self._config_file))
            return

        # Finally, search for a msticpyconfig.yaml
        self._config_file = self._search_for_file("**/msticpyconfig.yaml")
        if self._config_file:
            os.environ["MSTICPYCONFIG"] = self._config_file
            refresh_config()
            self._read_pkg_config_values(workspace_name=workspace)
            return
        # Finally, throw an exception (unless non-interactive)
        if self._interactive:
            # If we've arrived here after searching current folder and parent
            # then we give up. (We create but don't raise an actual exception)
            display(
                MsticpyUserConfigError(
                    *_NO_CONFIG_ERR,
                    title="Workspace configuration missing.",
                    **{f"nb_{idx}_uri": res for idx, res in enumerate(_RESOURCES)},
                )
            )

    def _read_pkg_config_values(self, workspace_name: Optional[str] = None):
        """Try to find a usable config from the MSTICPy config file."""
        ws_settings = get_config("AzureSentinel", {}).get("Workspaces")  # type: ignore
        if not ws_settings:
            return
        selected_workspace: Dict[str, str] = {}
        if workspace_name:
            selected_workspace, self.settings_key = self._lookup_ws_name_and_id(
                workspace_name, ws_settings
            )
        elif "Default" in ws_settings:
            selected_workspace = ws_settings["Default"]
            self.settings_key = "Default"
        elif len(ws_settings) == 1:
            # If only one workspace, use that
            self.settings_key, selected_workspace = next(iter(ws_settings.items()))

        if selected_workspace:
            self._config = {}
            self._config.update(selected_workspace)

    def _lookup_ws_name_and_id(self, ws_name: str, ws_configs: dict):
        for name, ws_config in ws_configs.items():
            if ws_name.casefold() == name.casefold():
                return ws_config, name
            if ws_config.get(self.CONF_WS_ID, "").casefold() == ws_name.casefold():
                return ws_config, name
            if ws_config.get(self.CONF_WS_NAME, "").casefold() == ws_name.casefold():
                return ws_config, name
        return {}, None

    def _search_for_file(self, pattern: str) -> Optional[str]:
        config_file = None
        for start_path in (".", ".."):
            searched_configs = list(Path(start_path).glob(pattern))
            for found_file in searched_configs:
                if found_file.name == "msticpyconfig.yaml":
                    config_file = str(found_file)
                    break
                test_content = self._read_config_values(str(found_file))
                if "workspace_id" in test_content:
                    config_file = str(found_file)
                    break
            if config_file:
                break
        if config_file:
            # Warn that we're using a "found" file, not one in the current directory
            print("Warning")
            print("\n".join(_NO_CONFIG_WARN).format(config_file=config_file))
        return config_file
