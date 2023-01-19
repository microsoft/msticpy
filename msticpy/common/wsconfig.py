# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Module for Log Analytics-related configuration."""


import contextlib
import json
import os
from pathlib import Path
from typing import Any, Dict, Optional

import ipywidgets as widgets
from IPython.display import display

from .._version import VERSION
from . import pkg_config
from .exceptions import MsticpyUserConfigError
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

    PKG_CONF_TENANT_KEY = "TenantId"
    PKG_CONF_WS_KEY = "WorkspaceId"
    PKG_CONF_SUB_KEY = "SubscriptionId"
    PKG_CONF_RES_GROUP_KEY = "ResourceGroup"
    PKG_CONF_NAME_KEY = "WorkspaceName"

    CONF_WS_ID_KEY = "workspace_id"
    CONF_TENANT_ID_KEY = "tenant_id"
    CONF_SUB_ID_KEY = "subscription_id"
    CONF_RES_GROUP_KEY = "resource_group"
    CONF_WS_NAME_KEY = "workspace_name"

    def __init__(
        self,
        workspace: Optional[str] = None,
        config_file: Optional[str] = None,
        interactive: bool = True,
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
        workspace : str, Optional[str]
            Workspace name (where multiple workspaces are configured),
            by default the Default workspace will be used.
        interactive : bool, optional
            If this is False, initializing the class will not raise an
            exception if no configuration is found. By default, True.

        """
        self._config: Dict[str, str] = {}
        self._interactive = interactive
        self._config_file = config_file
        self.workspace_key = workspace or "Default"
        # If config file specified, use that
        if config_file:
            self._config.update(self._read_config_values(config_file))
        else:
            self._determine_config_source(workspace)

    def __getitem__(self, key: str):
        """Allow property get using dictionary key syntax."""
        if key in self._config:
            return self._config[key]
        raise KeyError

    def __setitem__(self, key: str, value: Any):
        """Allow property set using dictionary key syntax."""
        self._config[key] = value

    def __contains__(self, key: str):
        """Allow property in test."""
        # In operator overload
        return key == "Type" or key in self._config or key in self.__dict__

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
        ws_value = self._config.get(self.CONF_WS_ID_KEY, None)
        ten_value = self._config.get(self.CONF_TENANT_ID_KEY, None)
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
        ten_id = self._config.get(self.CONF_TENANT_ID_KEY, None)
        ws_id = self._config.get(self.CONF_WS_ID_KEY, None)
        if not ten_id:
            raise KeyError(
                f"Configuration setting for {self.CONF_TENANT_ID_KEY} "
                + "could not be found."
            )
        if not ws_id:
            raise KeyError(
                f"Configuration setting for {self.CONF_WS_ID_KEY} "
                + "could not be found."
            )
        return f"loganalytics://code().tenant('{ten_id}').workspace('{ws_id}')"

    @property
    def mp_settings(self):
        """Return the equivalent MSTICPY settings dictionary."""
        return {
            self.PKG_CONF_NAME_KEY: self._config.get(self.CONF_WS_NAME_KEY),
            self.PKG_CONF_SUB_KEY: self._config.get(self.CONF_SUB_ID_KEY),
            self.PKG_CONF_WS_KEY: self._config.get(self.CONF_WS_ID_KEY),
            self.PKG_CONF_TENANT_KEY: self._config.get(self.CONF_TENANT_ID_KEY),
            self.PKG_CONF_RES_GROUP_KEY: self._config.get(self.CONF_RES_GROUP_KEY),
        }

    @classmethod
    def from_settings(cls, settings: Dict[str, Any]):
        """Create a WorkstationConfig from MSTICPY Workspace settings."""
        ws_config = cls(workspace="**DUMMY_WORKSPACE**")  # type: ignore
        ws_config._config = {  # type: ignore
            cls.CONF_WS_NAME_KEY: settings.get(cls.PKG_CONF_NAME_KEY),  # type: ignore
            cls.CONF_SUB_ID_KEY: settings.get(cls.PKG_CONF_SUB_KEY),  # type: ignore
            cls.CONF_WS_ID_KEY: settings.get(cls.PKG_CONF_WS_KEY),  # type: ignore
            cls.CONF_TENANT_ID_KEY: settings.get(cls.PKG_CONF_TENANT_KEY),  # type: ignore
            cls.CONF_RES_GROUP_KEY: settings.get(cls.PKG_CONF_RES_GROUP_KEY),  # type: ignore
        }
        return ws_config

    @staticmethod
    def _read_config_values(file_path: str) -> Dict[str, str]:
        """Read configuration file."""
        if not file_path:
            return {}
        with contextlib.suppress(json.JSONDecodeError):
            with open(file_path, "r", encoding="utf-8") as json_file:
                if json_file:
                    return json.load(json_file)
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
        ws_settings = pkg_config.settings.get("AzureSentinel", {}).get("Workspaces")
        return (
            {
                ws_name: {
                    cls.PKG_CONF_WS_KEY: ws.get(cls.PKG_CONF_WS_KEY),
                    cls.PKG_CONF_TENANT_KEY: ws.get(cls.PKG_CONF_TENANT_KEY),
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
        # Next, search for a config.json in the current director
        if Path("./config.json").exists():
            self._config_file = "./config.json"
        else:
            self._config_file = self._search_for_file("**/config.json")
        if self._config_file:
            self._config.update(self._read_config_values(self._config_file))
            return

        # Finally, search for a msticpyconfig.yaml
        if (
            os.environ.get("MSTICPYCONFIG")
            and Path(os.environ.get("MSTICPYCONFIG")).exists()
        ):
            self._config_file = os.environ.get("MSTICPYCONFIG")
        elif Path("./msticpyconfig.yaml").exists():
            self._config_file = "./msticpyconfig.yaml"
        else:
            self._config_file = self._search_for_file("**/msticpyconfig.yaml")
        if self._config_file:
            os.environ["MSTICPYCONFIG"] = self._config_file
            pkg_config.refresh_config()
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

    def _read_pkg_config_values(self, workspace_name: str = None):
        as_settings = pkg_config.settings.get("AzureSentinel")
        if not as_settings:
            return {}
        ws_settings = as_settings.get("Workspaces")  # type: ignore
        if not ws_settings:
            return {}
        if workspace_name and workspace_name in ws_settings:
            selected_workspace = ws_settings[workspace_name]
        elif "Default" in ws_settings:
            selected_workspace = ws_settings["Default"]
        elif len(ws_settings) == 1:
            selected_workspace = next(iter(ws_settings.values()))
        else:
            return {}
        if (
            selected_workspace
            and self.PKG_CONF_WS_KEY in selected_workspace
            and self.PKG_CONF_TENANT_KEY in selected_workspace
        ):
            self._config[self.CONF_WS_ID_KEY] = selected_workspace.get(
                self.PKG_CONF_WS_KEY
            )
            self._config[self.CONF_TENANT_ID_KEY] = selected_workspace.get(
                self.PKG_CONF_TENANT_KEY
            )
        if self.PKG_CONF_SUB_KEY in selected_workspace:
            self._config[self.CONF_SUB_ID_KEY] = selected_workspace.get(
                self.PKG_CONF_SUB_KEY
            )
        if self.PKG_CONF_RES_GROUP_KEY in selected_workspace:
            self._config[self.CONF_RES_GROUP_KEY] = selected_workspace.get(
                self.PKG_CONF_RES_GROUP_KEY
            )
        if self.PKG_CONF_NAME_KEY in selected_workspace:
            self._config[self.CONF_WS_NAME_KEY] = selected_workspace.get(
                self.PKG_CONF_NAME_KEY
            )
        return {}

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
