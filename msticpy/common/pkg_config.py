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

"""
import contextlib
import numbers
import os
from collections import UserDict
from contextlib import AbstractContextManager
from importlib.util import find_spec
from pathlib import Path
from typing import Any, Callable, Dict, List, Optional, Tuple, Union

import httpx
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
_MS_SENTINEL = "MSSentinel"
_AZ_CLI = "AzureCLI"
_HOME_PATH = "~/.msticpy/"


class SettingsDict(UserDict):
    """Dictionary class for settings with aliasing."""

    _ALIASES = {_AZ_SENTINEL: _MS_SENTINEL}

    def __getitem__(self, key):
        """Get item with aliasing."""
        key = self._ALIASES.get(key, key)
        return super().__getitem__(key)

    def __setitem__(self, key, value):
        """Set item with aliasing."""
        key = self._ALIASES.get(key, key)
        super().__setitem__(key, value)

    def get(self, key, default=None):
        """Get item with aliasing."""
        key = self._ALIASES.get(key, key)
        return super().get(key, default)


# pylint: disable=invalid-name
_default_settings = SettingsDict()
_custom_settings = SettingsDict()
_settings = SettingsDict()


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


def get_settings():
    """Return the current settings."""
    return _settings


def refresh_config():
    """Re-read the config settings."""
    # pylint: disable=global-statement
    global _default_settings, _custom_settings, _settings
    _default_settings = _get_default_config()
    _custom_settings = _get_custom_config()
    _custom_settings = _create_data_providers(_custom_settings)
    _settings = _consolidate_configs(_default_settings, _custom_settings)


def has_config(setting_path: str) -> bool:
    """Return True if a settings path exists."""
    try:
        _get_config(setting_path=setting_path, settings_dict=_settings)
        return True
    except KeyError:
        return False


_DEFAULT_SENTINEL = "@@@NO-DEFAULT-VALUE@@@"


def get_config(
    setting_path: Optional[str] = None, default: Any = _DEFAULT_SENTINEL
) -> Any:
    """
    Return setting item for path.

    Parameters
    ----------
    setting_path : str
        Path to setting item expressed as dot-separated
        string
    default : Any
        Default value to return if setting does not exist.

    Returns
    -------
    Any
        The item at the path location.

    Exceptions
    ----------
    KeyError : if path not found and no default provided.

    """
    if setting_path is None:
        return _settings
    try:
        return _get_config(setting_path, _settings)
    except KeyError:
        if default != _DEFAULT_SENTINEL:
            return default
        raise


def _get_config(setting_path: str, settings_dict: SettingsDict) -> Any:
    """Return value from setting_path."""
    path_elems = setting_path.split(".")
    cur_node = settings_dict
    for elem in path_elems:
        cur_node = cur_node.get(elem, None)
        if cur_node is None:
            raise KeyError(f"{elem} value of {setting_path} is not a valid path")
    return cur_node


def set_config(setting_path: str, value: Any, create_path: bool = False):
    """
    Set setting value for path.

    Parameters
    ----------
    setting_path : str
        Path to setting item expressed as dot-separated
        string
    value : Any
        The value to set.
    create_path : bool
        If True create any missing elements in the settings
        path.

    Returns
    -------
    Any :
        The current settings node with new value.

    Exceptions
    ----------
    KeyError : if not found and no default provided.

    """
    return _set_config(setting_path, _settings, value, create_path)


def _set_config(
    setting_path: str, settings_dict: SettingsDict, value: Any, create_path: bool
) -> Any:
    """Set `setting_path` in `settings_dict` to `value`."""
    path_elems = setting_path.split(".")
    parent_path = ".".join(path_elems[:-1])
    key_name = path_elems[-1]
    with contextlib.suppress(KeyError):
        parent_node = _get_config(parent_path, settings_dict)
        parent_node[key_name] = value
        return parent_node[key_name]
    cur_node = settings_dict
    # if there are any intermediate missing paths
    # and create_path is true, add these.
    for elem in path_elems[:-1]:
        next_node = cur_node.get(elem)
        if next_node is None:
            if create_path:
                cur_node[elem] = {}
                cur_node = cur_node[elem]
                continue
            raise KeyError(f"{elem} value of {setting_path} is not a valid path")
        cur_node = next_node
    # set the current node's value
    cur_node[key_name] = value
    return cur_node[key_name]


def _del_config(setting_path: str, settings_dict: SettingsDict) -> Any:
    """Delete `setting_path` from `settings_dict`."""
    path_elems = setting_path.split(".")
    cur_node = settings_dict
    current_value = None
    for idx, elem in enumerate(path_elems):
        if idx == len(path_elems) - 1:
            current_value = cur_node[elem]
            del cur_node[elem]
            break
        cur_node = cur_node.get(elem, None)
        if cur_node is None:
            raise KeyError(f"{elem} value of {setting_path} is not a valid path")
    return current_value


def _read_config_file(config_file: Union[str, Path]) -> SettingsDict:
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
                return SettingsDict(yaml.safe_load(f_handle))
            except YAMLError as yml_err:
                raise MsticpyUserConfigError(
                    f"Check that your {config_file} is valid YAML.",
                    "The following error was encountered",
                    str(yml_err),
                    title="config file could not be read",
                ) from yml_err
    return SettingsDict()


def _consolidate_configs(
    def_config: SettingsDict, cust_config: SettingsDict
) -> SettingsDict:
    resultant_config = SettingsDict()
    resultant_config.update(def_config)

    _override_config(resultant_config, cust_config)
    return resultant_config


def _override_config(base_config: SettingsDict, new_config: SettingsDict):
    for c_key, c_item in new_config.items():
        if c_item is None:
            continue
        if isinstance(base_config.get(c_key), (dict, SettingsDict)):
            _override_config(base_config[c_key], new_config[c_key])
        else:
            base_config[c_key] = new_config[c_key]


def _get_default_config():
    """Return the package default config file."""
    package = "msticpy"
    try:
        from importlib.resources import (  # pylint: disable=import-outside-toplevel
            as_file,
            files,
        )

        package_path: AbstractContextManager = as_file(
            files(package).joinpath(_CONFIG_FILE)
        )
    except ImportError:
        # If importlib.resources is not available we fall back to
        # older Python method
        from importlib.resources import path  # pylint: disable=import-outside-toplevel

        # pylint: disable=deprecated-method
        package_path = path(package, _CONFIG_FILE)  # noqa: W4902

    try:
        with package_path as config_path:
            return _read_config_file(config_path) if config_path.exists() else {}
    except ModuleNotFoundError as mod_err:
        # if all else fails we try to find the package default config somewhere
        # in the package tree - we use the first one we find
        pkg_root: Optional[Path] = _get_pkg_path("msticpy")
        if not pkg_root:
            raise MsticpyUserConfigError(
                f"Unable to locate the package default {_CONFIG_FILE}",
                "msticpy package may be corrupted.",
                title=f"Package {_CONFIG_FILE} missing.",
            ) from mod_err
        config_path = next(iter(pkg_root.glob(f"**/{_CONFIG_FILE}")))
    return _read_config_file(config_path) if config_path else {}


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


#############################
# Specialized settings


def get_http_timeout(
    *,
    timeout: Optional[int] = None,
    def_timeout: Optional[int] = None,
    **kwargs,
) -> httpx.Timeout:
    """Return timeout from settings or overridden in `kwargs`."""
    del kwargs
    config_timeout: Union[int, Dict, httpx.Timeout, List, Tuple] = get_config(
        "msticpy.http_timeout", get_config("http_timeout", None)
    )
    timeout_params: Union[int, Dict, httpx.Timeout, List[Union[float, None]], Tuple] = (
        timeout or def_timeout or config_timeout
    )
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


def _valid_timeout(
    timeout_val: Optional[Union[float, numbers.Real]]
) -> Union[float, None]:
    """Return float in valid range or None."""
    if isinstance(timeout_val, numbers.Real) and float(timeout_val) >= 0.0:
        return float(timeout_val)
    return None


# End specialized settings
############################

# read initial config when first imported.
refresh_config()


def validate_config(
    mp_config: Union[SettingsDict, Dict[str, Any], None] = None, config_file: str = None
):
    """
    Validate msticpy config settings.

    Parameters
    ----------
    mp_config : Union[SettingsDict, Dict[str, Any], None], optional
        The settings dictionary, by default it will
        check the currently loaded settings.
    config_file : str
        path to config file to check, by default None

    """
    if config_file:
        mp_config = _read_config_file(config_file)
    if not mp_config and not config_file:
        mp_config = _settings

    if not isinstance(mp_config, (dict, SettingsDict)):
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
    return (mp_errors, mp_warn) if mp_errors or mp_warn else ([], [])


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
