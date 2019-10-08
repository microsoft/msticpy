# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Module for Log Analytics-related configuration."""

import json
from typing import Dict, Any, Optional
from pathlib import Path

from .utility import export
from . import pkg_config
from .._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"


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
                raise RuntimeError(
                    "Could not find workspace configuration in "
                    + "msticpyconfig.yaml or in config.json."
                )
        self._config_file = config_file
        self._config.update(self._read_config_values(config_file))

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

    @property
    def config_loaded(self) -> bool:
        """
        Return True if workspace id and tenant id have values.

        Returns
        -------
        bool
            True if configuration loaded.

        """
        ws_ok = self._config.get(self.CONF_WS_ID_KEY, None)
        ten_ok = self._config.get(self.CONF_TENANT_ID_KEY, None)
        return ws_ok and ten_ok  # type: ignore

    @property
    def code_connect_str(self) -> str:
        """
        Return the Log Analytics connection string for dev code auth.

        Returns
        -------
        str
            Connection string

        """
        con_str = 'loganalytics://code().tenant("{ten_id}").workspace("{ws_id}")'
        return con_str.format(
            ten_id=self[self.CONF_TENANT_ID_KEY], ws_id=self[self.CONF_WS_ID_KEY]
        )

    @classmethod
    def _read_config_values(cls, file_path: str) -> Dict[str, str]:
        """Read configuration file."""
        with open(file_path) as json_file:
            if json_file:
                json_config = json.load(json_file)
                return json_config
        return {}

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
