# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Module for Log Analytics-related configuration."""

import json
from typing import Dict, Any
from .utility import export

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

    def __init__(self, config_file: str):
        """
        Load current Azure Notebooks configuration for Log Analytics.

        Arguments
        ---------
            config_file : str
                path to the configuration file.

        """
        self._config_file = config_file
        self._config = self._read_config_values(config_file)

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

    @classmethod
    def _read_config_values(cls, file_path: str) -> Dict[str, str]:
        """Read configuration file."""
        with open(file_path) as json_file:
            if json_file:
                json_config = json.load(json_file)
                return json_config
        return {}
