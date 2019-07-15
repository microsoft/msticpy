# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
Package configuration reader.

Reads default configuration from package file `msticpyconfig.yaml`.
Optionally reads custom configuration from file specified in environment
variable `MSTICPYCONFIG`. If this is not defined the package will look for
a file `msticpyconfig.yaml` in the current directory.

Default settings are accessible as an attribute `default_settings`.
Custom settings are accessible as an attribute `custom_settings`.
Consolidated settings are accessible as an attribute `settings`.

"""
from typing import Dict, Any
import os
from pathlib import Path
import pkg_resources
import yaml

from .._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"

_CONFIG_FILE: str = "msticpyconfig.yaml"
_CONFIG_ENV_VAR: str = "MSTICPYCONFIG"


def _read_config_file(config_file: str) -> Dict[str, Any]:
    """
    Read a yaml config definition file.

    Parameters
    ----------
    config_file : str
        Path to yaml config file

    Returns
    -------
    Dict
        Configuration settings

    """
    with open(config_file) as f_handle:
        # use safe_load instead of load
        return yaml.safe_load(f_handle)
    return {}


def _consolidate_configs(def_config: Dict[str, Any], cust_config: Dict[str, Any]) -> Dict[str, Any]:
    resultant_config = {}
    resultant_config.update(def_config)

    _override_config(resultant_config, cust_config)
    return resultant_config


def _override_config(base_config: Dict[str, Any], new_config: Dict[str, Any]):
    for c_key, c_item in new_config:
        if isinstance(c_item, dict):
            _override_config(base_config[c_key], new_config[c_key])
        else:
            base_config[c_key] = new_config[c_key]


def _get_default_config():
    conf_file = pkg_resources.resource_filename(__name__, _CONFIG_FILE)
    return _read_config_file(conf_file)


def _get_custom_config():
    config_path = os.environ.get(_CONFIG_ENV_VAR, None)
    if config_path and Path(config_path).is_file():
        return _read_config_file(config_path)

    if Path(_CONFIG_FILE).is_file():
        return _read_config_file(_CONFIG_FILE)
    return {}


# pylint: disable=invalid-name
default_settings: Dict[str, Any] = _get_default_config()
custom_settings: Dict[str, Any] = _get_custom_config()
settings: Dict[str, Any] = _consolidate_configs(default_settings, custom_settings)
