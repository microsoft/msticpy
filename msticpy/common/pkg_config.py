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
import os
import sys
from pathlib import Path
from typing import Any, Dict, Optional, Callable

import pkg_resources
import yaml
from yaml.error import YAMLError

from . import exceptions
from .exceptions import MsticpyUserConfigError
from .utility import is_valid_uuid
from .._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"

_CONFIG_FILE: str = "msticpyconfig.yaml"
_CONFIG_ENV_VAR: str = "MSTICPYCONFIG"
_DP_KEY = "DataProviders"
_AZ_SENTINEL = "AzureSentinel"
_AZ_CLI = "AzureCLI"

# pylint: disable=invalid-name
default_settings: Dict[str, Any] = {}
custom_settings: Dict[str, Any] = {}
settings: Dict[str, Any] = {}


def _get_current_config() -> Callable[[Any], Optional[str]]:
    """Closure for holding path of config file."""
    _current_conf_file: Optional[str] = None

    def _current_config(file_path: Optional[str] = None) -> Optional[str]:
        nonlocal _current_conf_file  # noqa
        if file_path is not None:
            _current_conf_file = file_path
        return _current_conf_file

    return _current_config


_CURRENT_CONF_FILE = _get_current_config()


def current_config_path() -> Optional[str]:
    """
    Return the path of the current config file, if any.

    Returns
    -------
    Optional[str]
        path of the current config file

    """
    return _CURRENT_CONF_FILE(None)


def refresh_config():
    """Re-read the config settings."""
    # pylint: disable=global-statement
    global default_settings, custom_settings, settings
    default_settings = _get_default_config()
    custom_settings = _get_custom_config()
    custom_settings = _create_data_providers(custom_settings)
    settings = _consolidate_configs(default_settings, custom_settings)


def get_config(setting_path: str) -> Any:
    """
    Return setting item for path.

    Parameters
    ----------
    setting_path : str
        Path to setting item expressed as dot-separated
        string

    Returns
    -------
    Any
        The item at the path location.

    """
    path_elems = setting_path.split(".")
    cur_node = settings
    for elem in path_elems:
        cur_node = cur_node.get(elem, None)
        if cur_node is None:
            raise KeyError(f"{elem} value of {setting_path} is not a valid path")
    return cur_node


def set_config(setting_path: str, value: Any):
    """
    Set setting value for path.

    Parameters
    ----------
    setting_path : str
        Path to setting item expressed as dot-separated
        string
    value : Any
        The value to set.

    """
    path_elems = setting_path.split(".")
    cur_node = settings
    for elem in path_elems:
        if elem in cur_node:
            cur_node[elem] = value
            break
        cur_node = cur_node.get(elem, None)
        if cur_node is None:
            raise KeyError(f"{elem} value of {setting_path} is not a valid path")
    return cur_node


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
    if Path(config_file).is_file():
        with open(config_file) as f_handle:
            # use safe_load instead of load
            try:
                return yaml.safe_load(f_handle)
            except YAMLError as yml_err:
                raise MsticpyUserConfigError(
                    f"Check that your {config_file} is valid YAML.",
                    "The following error was encountered",
                    str(yml_err),
                    title="config file could not be read",
                )
    return {}


def _consolidate_configs(
    def_config: Dict[str, Any], cust_config: Dict[str, Any]
) -> Dict[str, Any]:
    resultant_config = {}
    resultant_config.update(def_config)

    _override_config(resultant_config, cust_config)
    return resultant_config


def _override_config(base_config: Dict[str, Any], new_config: Dict[str, Any]):
    for c_key, c_item in new_config.items():
        if c_item is None:
            continue
        if isinstance(base_config.get(c_key), dict):
            _override_config(base_config[c_key], new_config[c_key])
        else:
            base_config[c_key] = new_config[c_key]


def _get_default_config():
    # When called from a unit test msticpy is a level above the package root
    # so the first call produces an invalid path
    # return the actual path - pkgpath/msticpy/filename.yaml or just
    # pkgpath/filename.yaml. So we test it as we go
    conf_file = None
    top_module = _get_top_module()
    try:
        conf_file = pkg_resources.resource_filename(top_module, _CONFIG_FILE)
        if not Path(conf_file).is_file():
            conf_file = pkg_resources.resource_filename(
                top_module, "msticpy/" + _CONFIG_FILE
            )
    except ModuleNotFoundError:
        pass
    if not (conf_file and Path(conf_file).is_file()):
        # if all else fails we try to find the package default config somewhere
        # in the package tree - we use the first one we find
        pkg_paths = sys.modules[top_module]
        if pkg_paths:
            conf_file = next(Path(pkg_paths.__path__[0]).glob("**/" + _CONFIG_FILE))
    if conf_file:
        return _read_config_file(conf_file)
    return {}


def _get_custom_config():
    config_path = os.environ.get(_CONFIG_ENV_VAR, None)
    if config_path and Path(config_path).is_file():
        _CURRENT_CONF_FILE(str(Path(config_path).resolve()))
        return _read_config_file(config_path)

    if Path(_CONFIG_FILE).is_file():
        _CURRENT_CONF_FILE(str(Path(".").joinpath(_CONFIG_FILE).resolve()))
        return _read_config_file(_CONFIG_FILE)
    return {}


def _get_top_module():
    module_path = __name__.split(".")
    top_module = __name__
    for idx in range(1, len(module_path)):
        test_module = ".".join(module_path[:-idx])
        if test_module in sys.modules:
            top_module = test_module
        else:
            break
    return top_module


def _create_data_providers(mp_config: Dict[str, Any]) -> Dict[str, Any]:
    if _DP_KEY not in mp_config:
        mp_config[_DP_KEY] = {}
    data_providers = mp_config[_DP_KEY]

    az_sent_config = mp_config.get(_AZ_SENTINEL)
    if az_sent_config and az_sent_config.get("Workspaces"):
        for section, prov_settings in mp_config[_AZ_SENTINEL]["Workspaces"].items():
            sec_name = f"{_AZ_SENTINEL}_{section}"
            if sec_name in data_providers:
                continue
            data_providers[sec_name] = {"Args": prov_settings}
    az_cli_config = mp_config.get(_AZ_CLI)
    if az_cli_config and _AZ_CLI not in data_providers:
        data_providers[_AZ_CLI] = mp_config[_AZ_CLI]
    return mp_config


# read initial config when first imported.
refresh_config()


def validate_config(mp_config: Dict[str, Any] = None, config_file: str = None):
    """
    Validate msticpy config settings.

    Parameters
    ----------
    mp_config : Dict[str, Any], optional
        The settings dictionary, by default it will
        check the currently loaded settings.
    config_file : str
        path to config file to check, by default None

    """
    if config_file:
        mp_config = _read_config_file(config_file)
    if not (mp_config or config_file):
        mp_config = settings

    if not isinstance(mp_config, dict):
        raise TypeError("Unknown format for configuration settings.")

    mp_errors, mp_warn = _validate_azure_sentinel(mp_config=mp_config)

    auth_key_providers = [
        "OTX",
        "VirusTotal",
        "XForce",
        "OpenPageRank",
        "GeoIPLite",
        "IPStack",
    ]
    for conf_section in ["TIProviders", "OtherProviders", _DP_KEY]:
        prov_errors, prov_warn = _check_provider_settings(
            mp_config=mp_config.get(conf_section, {}),
            section=conf_section,
            key_provs=auth_key_providers,
        )
        if conf_section == _DP_KEY and mp_config.get(conf_section) is None:
            continue
        mp_errors.extend(prov_errors)
        mp_warn.extend(prov_warn)

    # Special handling for AzureCLI if it is not in DataProviders
    # but specified at the top level of the config file
    if _AZ_CLI not in mp_config.get(_DP_KEY, {}):
        if _AZ_CLI not in mp_config:
            mp_warn.append("No AzureCLI section in settings.")
        else:
            az_cli_settings = {"DataProviders": mp_config.get(_AZ_CLI)}
            prov_errors, prov_warn = _check_provider_settings(
                mp_config=az_cli_settings, section=_AZ_CLI, key_provs=None
            )
            mp_errors.extend(prov_errors)
            mp_warn.extend(prov_warn)

    _print_validation_report(mp_errors, mp_warn)
    if mp_errors or mp_warn:
        return mp_errors, mp_warn
    return None


def _print_validation_report(mp_errors, mp_warn):
    if mp_errors:
        title = "\nThe following configuration errors were found:"
        print(title, "\n", "-" * len(title))
        for err in mp_errors:
            print(err)
    else:
        print("No errors found.")
    if mp_warn:
        title = "\nThe following configuration warnings were found:"
        print(title, "\n", "-" * len(title))
        for err in mp_warn:
            print(err)
    else:
        print("No warnings found.")


def _validate_azure_sentinel(mp_config):
    mp_errors = []
    mp_warnings = []
    as_settings = mp_config.get(_AZ_SENTINEL, {})
    if not as_settings:
        mp_errors.append("Missing or empty 'AzureSentinel' section")
        return mp_errors, mp_warnings
    ws_settings = as_settings.get("Workspaces", {})
    if not ws_settings:
        mp_errors.append("Missing or empty 'Workspaces' key in 'AzureSentinel' section")
        return mp_errors, mp_warnings
    no_default = True
    for ws, ws_settings in ws_settings.items():
        if ws == "Default":
            no_default = False
        ws_id = ws_settings.get("WorkspaceId")
        if not (ws_id and is_valid_uuid(ws_id)):
            mp_errors.append(f"Invalid GUID for WorkspaceId in {ws} section")
        ten_id = ws_settings.get("TenantId")
        if not (ten_id and is_valid_uuid(ten_id)):
            mp_errors.append(f"Invalid GUID for TenantId in {ws} section")
    mp_warnings = ["No default workspace set"] if no_default else []
    return mp_errors, mp_warnings


def _check_provider_settings(mp_config, section, key_provs):
    mp_errors = []
    mp_warnings = []
    if not mp_config:
        mp_warnings.append(f"'{section}' section has no settings.")
        return mp_errors, mp_warnings
    for p_name, p_setting in mp_config.items():
        sec_args = p_setting.get("Args")
        if not sec_args:
            continue
        sec_path = f"{section}/{p_name}" if section else f"{p_name}"
        if key_provs and p_name in key_provs:
            _check_required_key(mp_errors, sec_args, "AuthKey", sec_path)
        if p_name == "XForce":
            _check_required_key(mp_errors, sec_args, "ApiID", sec_path)
        if p_name == _AZ_SENTINEL:
            _check_is_uuid(mp_errors, sec_args, "WorkspaceID", sec_path)
            _check_is_uuid(mp_errors, sec_args, "TenantID", sec_path)
        if p_name.startswith("AzureSentinel_"):
            _check_is_uuid(mp_errors, sec_args, "WorkspaceId", sec_path)
            _check_is_uuid(mp_errors, sec_args, "TenantId", sec_path)
        if p_name == _AZ_CLI:
            _check_required_key(mp_errors, sec_args, "clientId", sec_path)
            _check_required_key(mp_errors, sec_args, "tenantId", sec_path)
            _check_required_key(mp_errors, sec_args, "clientSecret", sec_path)

        mp_errors.extend(
            _check_env_vars(args_key=p_setting.get("Args"), section=sec_path)
        )
    return mp_errors, mp_warnings


def _check_required_key(mp_errors, conf_section, key, sec_path):
    if key not in conf_section or not conf_section[key]:
        mp_errors.append(f"{sec_path}: Missing or invalid {key}.")


def _check_is_uuid(mp_errors, conf_section, key, sec_path):
    if (
        key not in conf_section
        or not conf_section[key]
        or not is_valid_uuid(conf_section[key])
    ):
        mp_errors.append(f"{sec_path}: Missing or invalid {key}.")


def _check_env_vars(args_key, section):
    mp_errs = []
    if not args_key:
        return mp_errs
    for val in args_key.values():
        if "EnvironmentVar" in val:
            env_name = val.get("EnvironmentVar")
            if not env_name:
                mp_errs.append(f"{section}: No environment variable name specified.")
            elif env_name not in os.environ:
                mp_errs.append(f"{section}: Env variable {env_name} not set.")
            elif not os.environ[env_name]:
                mp_errs.append(f"{section}: Env variable {env_name} value is not set.")
    return mp_errs


# Set get_config function in exceptions module
# so that it can be called without having a circular import
# pylint: disable=protected-access
exceptions._get_config = get_config
