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
import numbers
import os
from importlib.util import find_spec
from pathlib import Path
from typing import Any, Callable, Dict, Optional, Union

import httpx
import pkg_resources
import yaml
from yaml.error import YAMLError

from .._version import VERSION
from . import exceptions
from .exceptions import MsticpyUserConfigError
from .utility import is_valid_uuid

__version__ = VERSION
__author__ = "Ian Hellen"

_CONFIG_FILE: str = "msticpyconfig.yaml"
_CONFIG_ENV_VAR: str = "MSTICPYCONFIG"
_DP_KEY = "DataProviders"
_AZ_SENTINEL = "AzureSentinel"
_AZ_CLI = "AzureCLI"
_HOME_PATH = "~/.msticpy/"

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
    return _get_config(setting_path, settings)


def _get_config(setting_path: str, settings_dict: Dict[str, Any]) -> Any:
    """Return value from setting_path."""
    path_elems = setting_path.split(".")
    cur_node = settings_dict
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
    return _set_config(setting_path, settings, value)


def _set_config(setting_path: str, settings_dict, value: Any) -> Any:
    """Set `setting_path` in `settings_dict` to `value`."""
    path_elems = setting_path.split(".")
    cur_node = settings_dict
    for elem in path_elems:
        if elem in cur_node:
            cur_node[elem] = value
            break
        cur_node = cur_node.get(elem, None)
        if cur_node is None:
            raise KeyError(f"{elem} value of {setting_path} is not a valid path")
    return cur_node


def _del_config(setting_path: str, settings_dict) -> Any:
    """Delete `setting_path` from `settings_dict`."""
    path_elems = setting_path.split(".")
    cur_node = settings_dict
    current_value = None
    for elem in path_elems:
        if elem in cur_node:
            current_value = cur_node[elem]
            del cur_node[elem]
            break
        cur_node = cur_node.get(elem, None)
        if cur_node is None:
            raise KeyError(f"{elem} value of {setting_path} is not a valid path")
    return current_value


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
        with open(config_file, "r", encoding="utf-8") as f_handle:
            # use safe_load instead of load
            try:
                return yaml.safe_load(f_handle)
            except YAMLError as yml_err:
                raise MsticpyUserConfigError(
                    f"Check that your {config_file} is valid YAML.",
                    "The following error was encountered",
                    str(yml_err),
                    title="config file could not be read",
                ) from yml_err
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
    """Return the package default config file."""
    conf_file = None
    package = "msticpy"
    try:
        conf_file = pkg_resources.resource_filename(package, _CONFIG_FILE)
    except ModuleNotFoundError as mod_err:
        # if all else fails we try to find the package default config somewhere
        # in the package tree - we use the first one we find
        pkg_root = _get_pkg_path("msticpy")
        if not pkg_root:
            raise MsticpyUserConfigError(
                f"Unable to locate the package default {_CONFIG_FILE}",
                "msticpy package may be corrupted.",
                title=f"Package {_CONFIG_FILE} missing.",
            ) from mod_err
        conf_file = next(iter(pkg_root.glob(f"**/{_CONFIG_FILE}")))
    if conf_file:
        return _read_config_file(conf_file)
    return {}


def _get_custom_config():
    config_path = os.environ.get(_CONFIG_ENV_VAR, None)
    if config_path and Path(config_path).is_file():
        _CURRENT_CONF_FILE(str(Path(config_path).resolve()))
        return _read_config_file(current_config_path())

    if Path(_CONFIG_FILE).is_file():
        _CURRENT_CONF_FILE(str(Path(".").joinpath(_CONFIG_FILE).resolve()))
        return _read_config_file(current_config_path())

    home_config = Path(os.path.join(_HOME_PATH, _CONFIG_FILE)).expanduser().resolve()
    if home_config.is_file():
        _CURRENT_CONF_FILE(str(home_config))
        return _read_config_file(current_config_path())
    return {}


def _get_pkg_path(pkg_name):
    current_path = Path(__file__)
    while current_path.name != pkg_name:
        if current_path == current_path.parent:
            return None
        current_path = current_path.parent
    return current_path


def _create_data_providers(mp_config: Dict[str, Any]) -> Dict[str, Any]:
    if mp_config.get(_DP_KEY) is None:
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


def _translate_legacy_settings(
    mp_config: Dict[str, Any], translate: Dict[str, str]
) -> Dict[str, Any]:
    """Map legacy settings to new location."""
    for src, target in translate.items():
        src_value = _get_config(src, mp_config)
        _set_config(target, mp_config, src_value)
        _del_config(src, mp_config)
    return mp_config


def get_http_timeout(
    **kwargs,
) -> httpx.Timeout:
    """Return timeout from settings or overridden in `kwargs`."""
    timeout_params = kwargs.get(
        "timeout", kwargs.get("def_timeout", settings.get("http_timeout", None))  # type: ignore
    )  # type: ignore
    if isinstance(timeout_params, dict):
        timeout_params = {
            name: _valid_timeout(val) for name, val in timeout_params.items()
        }
        return httpx.Timeout(**timeout_params)
    if isinstance(timeout_params, httpx.Timeout):
        return timeout_params
    if isinstance(timeout_params, numbers.Real):
        return httpx.Timeout(_valid_timeout(timeout_params))
    if isinstance(timeout_params, (list, tuple)):
        timeout_params = [_valid_timeout(val) for val in timeout_params]
        if len(timeout_params) >= 2:
            return httpx.Timeout(timeout=timeout_params[0], connect=timeout_params[1])
        if timeout_params:
            return httpx.Timeout(timeout_params[0])
    return httpx.Timeout(None)


def _valid_timeout(timeout_val) -> Union[float, None]:
    """Return float in valid range or None."""
    if isinstance(timeout_val, numbers.Real) and float(timeout_val) >= 0.0:
        return float(timeout_val)
    return None


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
        "RiskIQ",
        "IntSights",
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

    _print_validation_report(mp_errors, mp_warn)
    if mp_errors or mp_warn:
        return mp_errors, mp_warn
    return [], []


def _print_validation_report(mp_errors, mp_warn):
    if mp_errors:
        _print_validation_item(
            "\nThe following configuration errors were found:", mp_errors
        )

    else:
        print("No errors found.")
    if mp_warn:
        _print_validation_item(
            "\nThe following configuration warnings were found:", mp_warn
        )

    else:
        print("No warnings found.")


def _print_validation_item(arg0, arg1):
    title = arg0
    print(title, "\n", "-" * len(title))
    for err in arg1:
        print(err)


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
        if not p_setting:
            mp_warnings.append(f"'{section}/{p_name}' sub-section has no settings.")
            continue
        if "Args" not in p_setting:
            continue
        sec_args = p_setting.get("Args")
        if not sec_args or not isinstance(sec_args, dict):
            mp_errors.append(
                f"'{section}/{p_name}/{sec_args}' key has no settings or "
                + "is not a valid format."
            )
            continue
        sec_path = f"{section}/{p_name}" if section else f"{p_name}"
        mp_errors.extend(
            _check_required_provider_settings(sec_args, sec_path, p_name, key_provs)
        )

        mp_errors.extend(
            _check_env_vars(args_key=p_setting.get("Args"), section=sec_path)
        )
    return mp_errors, mp_warnings


def _check_required_provider_settings(sec_args, sec_path, p_name, key_provs):
    errs = []
    if key_provs and p_name in key_provs:
        errs.append(_check_required_key(sec_args, "AuthKey", sec_path))
    if p_name == "XForce":
        errs.append(_check_required_key(sec_args, "ApiID", sec_path))
    if p_name == _AZ_SENTINEL:
        errs.extend(
            (
                _check_is_uuid(sec_args, "WorkspaceID", sec_path),
                _check_is_uuid(sec_args, "TenantID", sec_path),
            )
        )

    if p_name.startswith("AzureSentinel_"):
        errs.extend(
            (
                _check_is_uuid(sec_args, "WorkspaceId", sec_path),
                _check_is_uuid(sec_args, "TenantId", sec_path),
            )
        )

    if (
        p_name == _AZ_CLI
        and "clientId" in sec_args
        and sec_args["clientId"] is not None
    ):
        # only warn if partially filled - since these are optional
        errs.extend(
            (
                _check_required_key(sec_args, "clientId", sec_path),
                _check_required_key(sec_args, "tenantId", sec_path),
                _check_required_key(sec_args, "clientSecret", sec_path),
            )
        )
    if p_name == "RiskIQ":
        errs.extend(
            (
                _check_required_key(sec_args, "ApiID", sec_path),
                _check_required_package("passivetotal", sec_path),
            )
        )

    return [err for err in errs if err]


def _check_required_key(conf_section, key, sec_path):
    if key not in conf_section or not conf_section.get(key):
        return f"{sec_path}: Missing or invalid {key}."
    return None


def _check_required_package(package, sec_path):
    if find_spec(package) is None:
        return f"{sec_path}: Required package '{package}' is not installed."
    return None


def _check_is_uuid(conf_section, key, sec_path):
    if (
        key not in conf_section
        or not conf_section[key]
        or not is_valid_uuid(conf_section[key])
    ):
        return f"{sec_path}: Missing or invalid {key}."
    return None


def _check_env_vars(args_key, section):
    mp_errs = []
    if not args_key:
        return mp_errs
    for val in args_key.values():
        if not val:
            continue
        if isinstance(val, dict) and "EnvironmentVar" in val:
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
