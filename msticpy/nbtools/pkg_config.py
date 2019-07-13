# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Package configuration provider."""
from typing import Tuple, Dict, Iterable, Any
from pathlib import Path
import yaml

from .._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"

settings: Dict[str, Any] = {}



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


def _consolidate_configs(configs: List[Dict[str, Any]]) -> Dict[str, Any]:
    resultant_config = {}
    if not configs:
        return {}
    
    default_config = configs.pop[0]
    resultant_config.update(default_config)

    for config in configs:
        _override_config(default_config, config)

def _override_config(base_config: Dict[str, Any], new_config: Dict[str, Any]):
    for c_key, c_item in new_config:
        if isinstance(c_item, dict):
            _override_config(base_config[c_key], new_config[c_key])
        else:
            base_config[c_key] = new_config[c_key]
