# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Module for Log Analytics-related configuration."""

import os
import json
from typing import Dict, Any, Optional
from pathlib import Path
import warnings

from .exceptions import MsticpyUserConfigError
from .utility import export, is_valid_uuid
from . import pkg_config
from .._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"


_NO_CONFIG_WARN = [
    "Could not find msticpyconfig.yaml or a config.json in the current directory.",
    "Using '{config_file}'.",
    "We recommend using an explicit msticpyconfig.yaml specified using the",
    "MSTICPYCONFIG environment variable. See",
    "https://msticpy.readthedocs.io/en/latest/getting_started/msticpyconfig.html",
    "for more details.",
]

_NO_CONFIG_ERR = [
    "Could not find msticpyconfig.yaml or config.json."
    "The config.json file is created when you launch notebooks from",
    "Azure Sentinel. If you have copied the notebook to another location",
    "or folder you will need to copy this file."
    "Alternatively, we recommend using an explicit msticpyconfig.yaml",
    "and adding your Workspace and Tenant IDs to that file.",
]


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

    CONF_WS_ID_KEY = "workspace_id"
    CONF_TENANT_ID_KEY = "tenant_id"
    CONF_SUB_ID_KEY = "subscription_id"
    CONF_RES_GROUP_KEY = "resource_group"
    CONF_WS_NAME_KEY = "workspace_name"

    def __init__(self, config_file: Optional[str] = None, workspace: str = None):
        """
        Load current Azure Notebooks configuration for Log Analytics.

        Parameters
        ----------
        config_file : Optional[str], optional
            path to a configuration file,
            Defaults to msticpyconfig.yaml if settings are configured there.
            If not, looks for a config.json in current folder
        workspace : str, optional
            Workspace name (where multiple workspaces are configured),
            by default the Default workspace will be used.

        """
        self._config: Dict[str, str] = {}
        if not config_file:
            self._read_pkg_config_values(workspace_name=workspace)
            if self.config_loaded:
                return
            if Path("./config.json").exists():
                config_file = "./config.json"
            else:
                config_file = self._search_for_file("**/config.json")
        self._config_file = config_file
        #
        config = self._read_config_values(config_file)
        if config:
            self._config.update(config)
        else:
            os.environ["MSTICPYCONFIG"] = config_file
            pkg_config.refresh_config()
            self._read_pkg_config_values(workspace_name=workspace)

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

    @classmethod
    def _read_config_values(cls, file_path: str) -> Dict[str, str]:
        """Read configuration file."""
        try:
            with open(file_path) as json_file:
                if json_file:
                    return json.load(json_file)
        except json.JSONDecodeError:
            pass
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
        if not ws_settings:
            return {}
        return {
            ws_name: {
                cls.PKG_CONF_WS_KEY: ws.get(cls.PKG_CONF_WS_KEY),
                cls.PKG_CONF_TENANT_KEY: ws.get(cls.PKG_CONF_TENANT_KEY),
            }
            for ws_name, ws in ws_settings.items()
        }

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
        return {}

    def _search_for_file(self, pattern: str) -> str:
        config_file = None
        searched_configs = list(Path("..").glob(pattern))
        if not searched_configs:
            raise MsticpyUserConfigError(
                *_NO_CONFIG_ERR, title="Workspace configuration missing."
            )
        for found_file in searched_configs:
            test_content = self._read_config_values(str(found_file))
            if "workspace_id" in test_content:
                config_file = found_file
                break
        if config_file is None:
            raise MsticpyUserConfigError(
                *_NO_CONFIG_ERR, title="Workspace configuration missing."
            )
        # Warn that we're using a "found" file
        warnings.warn("\n".join(_NO_CONFIG_WARN).format(config_file=config_file))
        return str(config_file)
