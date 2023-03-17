# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""MP Config Control Class."""
import pkgutil
import re
from collections import namedtuple
from typing import Any, Dict, List, Optional, Tuple, Union

import ipywidgets as widgets
import yaml

from .._version import VERSION
from ..common.utility import is_valid_uuid
from .ce_common import print_debug, py_to_widget, widget_to_py
from .comp_edit import SettingsControl

__version__ = VERSION
__author__ = "Ian Hellen"


STORE_TEXT = "Text"
STORE_ENV_VAR = "EnvironmentVar"
STORE_KEYVAULT = "KeyVault"
STORE_OPT = "StoreType"

ValidationResult = namedtuple("ValidationResult", "result, status")
_VALID_SUCCESS = "Validation succeeded"


class MpConfigControls:
    """Msticpy configuration and settings database."""

    def __init__(self, mp_config_def: Dict[str, Any], mp_config: Dict[str, Any]):
        """
        Return an instance of MpConfigControls.

        Parameters
        ----------
        mp_config_def : Dict[str, Any]
            Msticpy config setting definitions.
        mp_config : Dict[str, Any]
            Msticpy Settings dictionary

        """
        self.mp_config = mp_config
        self._raw_config_defn = mp_config_def
        self.config_defn = self._convert_mp_config(mp_config_def)
        self.controls = self._create_ctrl_dict(self.mp_config)

        if "AzureCLI" in self.mp_config:
            if "DataProviders" not in self.mp_config:
                self.mp_config["DataProviders"] = {}
            self.mp_config["DataProviders"]["AzureCLI"] = self.mp_config["AzureCLI"]

    @staticmethod
    def _get_elem_from_path(path, member_dict: Dict[str, Any]):
        """Return an item at the path from `member_dict`."""
        paths = path.split(".")
        current_elem = member_dict
        for elem in paths:
            next_elem = current_elem.get(elem)
            if next_elem is None:
                return next_elem
            current_elem = next_elem
        return current_elem

    def _set_elem_at_path(
        self, path: str, member_dict: Dict[str, Any], value: Any, create: bool = True
    ):
        """Set item at the path from `member_dict` to `value`."""
        path_elems = path.rsplit(".", maxsplit=1)
        parent_path = path_elems[0]
        parent = self._get_elem_from_path(parent_path, member_dict)
        if parent is not None:
            if len(path_elems) > 1:
                parent[path_elems[1]] = value
            else:
                member_dict[parent_path] = value
        elif create:
            # if an element in the path doesn't exist, create any needed
            if len(path_elems) == 1:
                member_dict[parent_path] = value
                return
            tgt_key = path_elems[1]
            current_elem = member_dict
            for elem in parent_path.split("."):
                next_elem = current_elem.get(elem)
                if next_elem is None:
                    next_elem = {}
                    current_elem[elem] = next_elem
                current_elem = next_elem
            current_elem[tgt_key] = value
            print_debug("set", parent_path, tgt_key, value)

    def _del_elem_at_path(self, path: str, member_dict: Dict[str, Any]):
        """Delete an item at `path`."""
        parent_path, tgt_key = path.rsplit(".", maxsplit=1)
        parent = self._get_elem_from_path(parent_path, member_dict)
        if parent is not None and tgt_key in parent:
            del parent[tgt_key]

    def get_value(self, path: str) -> Any:
        """Return setting value at `path`."""
        return self._get_elem_from_path(path, self.mp_config)

    def set_value(self, path: str, value: Any):
        """Set setting value at path to `value`."""
        self._set_elem_at_path(path, self.mp_config, value)

    def del_value(self, path: str, keep_ctrl: bool = False):
        """Delete setting item at `path`."""
        self._del_elem_at_path(path, self.mp_config)
        if not keep_ctrl:
            self.del_control(path)

    def get_control(self, path: str) -> Any:
        """Return the control stored at `path`."""
        return self._get_elem_from_path(path, self.controls)

    def set_control(self, path: str, control):
        """Set the control stored at `path`."""
        print_debug("set_control", path)
        self._set_elem_at_path(path, self.controls, value=control)

    def del_control(self, path: str):
        """Delete the control stored at `path`."""
        self._del_elem_at_path(path, self.controls)

    def get_defn(self, path: str) -> Union[Dict[str, Any], Tuple[str, Any]]:
        """Return the setting definition at `path`."""
        defn = self._get_elem_from_path(path, self.config_defn)
        if defn is not None:
            return defn
        if path.startswith("AzureSentinel.Workspaces"):
            path = re.sub(
                r"(?P<root>AzureSentinel\.Workspaces\.)([^.]+)", r"\1Default", path
            )
        return self._get_elem_from_path(path, self.config_defn)

    def rename_path(self, old_path: str, new_path: str):
        """Rename a setting from `old_path` to `new_path`."""
        old_path_elems = old_path.rsplit(".", maxsplit=1)
        new_path_elems = new_path.rsplit(".", maxsplit=1)
        if (
            old_path_elems[0] != new_path_elems[0]
            or len(old_path_elems) == 1
            or len(new_path_elems) == 1
        ):
            raise ValueError(
                "Can only rename the bottom element of paths", old_path, new_path
            )
        path_root = old_path_elems[0]
        src_key = old_path_elems[1]
        tgt_key = new_path_elems[1]
        for setting_dict in (self.mp_config, self.controls):
            parent_elem = self._get_elem_from_path(path_root, setting_dict)
            parent_elem[tgt_key] = parent_elem.get(src_key)
            if src_key in parent_elem:
                del parent_elem[src_key]

    def populate_ctrl_values(self, path: str):
        """Populate control at `path` from settings at `path`."""
        print_debug("populate_ctrl_values", path)
        self._set_ctrl_values(path)

    def _set_ctrl_values(self, path: str):
        """Recursively set settings from control values."""
        ctrl_tree = self.get_control(path)
        if isinstance(ctrl_tree, widgets.Widget):
            print_debug("_set_ctrl_values - widget", ctrl_tree)
            curr_value = py_to_widget(self.get_value(path), ctrl=ctrl_tree)
            if curr_value:
                ctrl_tree.value = curr_value
        elif isinstance(ctrl_tree, SettingsControl):
            print_debug(
                "_set_ctrl_values - _SettingsControl",
                ctrl_tree,
                ctrl_tree.value,
                path,
                self.get_value(path),
            )
            curr_value = self.get_value(path)
            if curr_value:
                ctrl_tree.value = curr_value
        elif isinstance(ctrl_tree, dict):
            for key in ctrl_tree:
                self._set_ctrl_values(f"{path}.{key}")

    def save_ctrl_values(self, path: str):
        """Save the values in the control at `path` to settings."""
        self.set_value(path, self._get_ctrl_values(path))

    def _get_ctrl_values(self, path: str):
        """Recursively save values in the control at `path` to settings."""
        ctrl_tree = self.get_control(path)
        print_debug(
            type(ctrl_tree),
            "instance check",
            isinstance(ctrl_tree, (widgets.Widget, SettingsControl)),
        )
        if isinstance(ctrl_tree, (widgets.Widget, SettingsControl)):
            return widget_to_py(ctrl_tree)
        if isinstance(ctrl_tree, dict):
            return {key: self._get_ctrl_values(f"{path}.{key}") for key in ctrl_tree}
        raise ValueError(f"No control found at path {path}")

    def _create_ctrl_dict(self, config_dict):
        """Create a blank control dictionary from settings."""
        ctrl_dict = config_dict.copy()
        for name, val in ctrl_dict.items():
            if (
                isinstance(val, dict)
                and STORE_KEYVAULT not in val
                and STORE_ENV_VAR not in val
            ):
                ctrl_dict[name] = self._create_ctrl_dict(val)
            else:
                ctrl_dict[name] = None
        return ctrl_dict

    def validate_all_settings(self, show_all: bool = False) -> List[ValidationResult]:
        """
        Validate settings against definitions.

        Parameters
        ----------
        show_all : bool, optional
            Show success validations as well as failures, by default False

        Returns
        -------
        List[ValidtnResult]
            List of validation results:
            bool - True == valid
            status - validation result description

        """
        results = []
        for key in self.mp_config:
            results.extend(self.validate_setting(path=key, show_all=show_all))
        return results

    def validate_setting(
        self, path: str, defn_path: Optional[str] = None, show_all: bool = False
    ) -> List[ValidationResult]:
        """
        Validate settings against definitions for a specific path.

        Parameters
        ----------
        path : str
            The setting path
        defn_path : Optional[str], optional
            The definition path, by default None
            Unless, specified this is the same as the setting path
        show_all : bool, optional
            Return successful as well as failed validation results
            , by default False

        Returns
        -------
        List[ValidtnResult]
            List of validation results:
            bool - True == valid
            status - validation result description

        """
        results = self._validate_setting_at_path(path=path, defn_path=defn_path)
        if isinstance(results, tuple):
            return [results] if show_all or not results[0] else []
        if isinstance(results, list):
            up_results = self._unpack_lists(results)
            return [res for res in up_results if not res[0] or show_all]
        return [ValidationResult(True, "No validation results found")]

    def _unpack_lists(self, res_list: List[Any]) -> List[ValidationResult]:
        """Unpack nested lists into a single list."""
        results = []
        for item in res_list:
            if isinstance(item, list):
                results.extend(self._unpack_lists(item))
            else:
                results.append(item)
        return results

    # pylint: disable=too-many-return-statements
    def _validate_setting_at_path(
        self, path: str, defn_path: Optional[str] = None, index: Optional[int] = None
    ) -> Union[ValidationResult, List[Union[ValidationResult, List[Any]]]]:
        """Recursively validate settings at path."""
        defn_path = defn_path or path
        conf_defn = self.get_defn(defn_path)
        setting = self.get_value(path)
        # to support recursive calls on members of lists (in the
        # if isinstance(conf_defn, list) section) we need to pull out
        # which item we're testing here
        if index is not None and setting is not None:
            setting = setting[index]

        # If we don't have a definition, there's nothing to validate against.
        if not conf_defn:
            return ValidationResult(True, f"No definition for path '{path}'")

        # If this is None and we're not at a leaf setting it means the
        # settings path doesn't exist, so bail here.
        if not setting and not isinstance(conf_defn, tuple):
            return ValidationResult(True, f"No setting at path '{path}'")

        # if the current defn node is a dict, then we need to recurse
        # into it.
        if isinstance(conf_defn, dict):
            val_res = [
                self._validate_setting_at_path(
                    path=f"{path}.{key}", defn_path=f"{defn_path}.{key}"
                )
                for key in conf_defn
            ]
            return val_res[0] if len(val_res) == 1 else val_res

        # if the defn is a list, we need to iterate through the members
        if isinstance(conf_defn, list):
            if isinstance(setting, list):
                # if the setting value is a list - check each one
                val_res = [
                    self._validate_setting_at_path(path, defn_path, index=idx)
                    for idx, _ in enumerate(setting)
                ]
                return val_res[0] if len(val_res) == 1 else val_res
            # Otherwise assume that we are already checking an item in
            # the list (recusing) - so we need to pull the defn from the
            # first item in the definition list
            conf_defn = conf_defn[0]

        # If the defn is a tuple we've reached a leaf node with a true
        # definition
        if isinstance(conf_defn, tuple):
            val_type, val_opts = conf_defn
            validator = _VALIDATORS.get(val_type)
            if validator:
                return validator(setting, path, val_type, val_opts)
            return ValidationResult(True, "No validator for type '{val_type}'")

        # If the conf definition is a literal value - compare directly
        if conf_defn == setting:
            return ValidationResult(True, f"Value is valid at path '{path}'")

        # Otherwise, we assume failure.
        return ValidationResult(False, f"Validation failed for path '{path}'")

    def _yml_extract_type(self, conf_val):
        """Extract type and options from definition."""
        if not conf_val or "(" not in conf_val or ")" not in conf_val:
            return "unknown", {}
        val_type_match = re.match(
            r"(?P<type>[^()]+)\((?P<params>.*)\)$", conf_val.strip()
        )
        val_type = val_type_match.groupdict().get("type")
        val_param_str = val_type_match.groupdict().get("params", "")

        if val_param_str:
            val_params = {
                param.split("=")[0].strip(): param.split("=")[1].strip()
                for param in val_param_str.split(",")
                if "=" in param
            }
            val_params = {
                key: True if val == "True" else False if val == "False" else val
                for key, val in val_params.items()
            }
        else:
            val_params = {}
        if "options" in val_params:
            val_params["options"] = [
                val.strip("'\"")
                for val in val_params["options"].strip()[1:-1].split("; ")
            ]
        if "mp_defn_path" in val_params:
            defn_path = val_params.pop("mp_defn_path").strip(" /\"'").replace("/", ".")
            defn = self._get_elem_from_path(defn_path, self._raw_config_defn)
            val_params["defn"] = defn
        default_val = val_params.get("default", "")
        if isinstance(default_val, str):
            if default_val.strip().startswith("["):
                # This is a default list - so we need to parse it
                val_params["default"] = [
                    val.strip("'\"")
                    for val in re.split(r"\s*;\s*|\s*,\s*", default_val.strip()[1:-1])
                ]
            else:
                val_params["default"] = default_val.strip("'\"")
        return val_type, val_params

    def _convert_mp_config(self, mp_conf_item):
        """Convert definition dictionary to extract definitions."""
        if isinstance(mp_conf_item, list):
            return self._convert_mp_config_list(mp_conf_item)
        if isinstance(mp_conf_item, dict):
            return self._convert_mp_config_dict(mp_conf_item)
        return mp_conf_item

    def _convert_mp_config_dict(self, mp_conf_dict):
        """Recursively convert definition dictionary to extract definitions."""
        out_dict = {}
        for key, val in mp_conf_dict.items():
            if isinstance(val, dict):
                out_dict[key] = self._convert_mp_config_dict(val)
            elif isinstance(val, list):
                out_dict[key] = self._convert_mp_config_list(val)
            else:
                key_type, opts = self._yml_extract_type(val)
                out_dict[key] = val if key_type == "unknown" else (key_type, opts)
        return out_dict

    def _convert_mp_config_list(self, mp_conf_list):
        """Recursively convert definition list to extract definitions."""
        out_list = []
        for val in mp_conf_list:
            if isinstance(val, dict):
                out_list.append(self._convert_mp_config_dict(val))
            elif isinstance(val, list):
                out_list.append(self._convert_mp_config_list(val))
            else:
                key_type, opts = self._yml_extract_type(val)
                out_list.append(val if key_type == "unknown" else (key_type, opts))
        return out_list


def get_mpconfig_definitions() -> Dict[str, Any]:
    """
    Return the current msticpyconfig definition dictionary.

    Returns
    -------
    Dict[str, Any]
        msticpyconfig definition dictionary

    Raises
    ------
    ValueError:
        Could not load definitions from resources/mpconfig_defaults.yaml

    """
    pkg_root = __package__.split(".", maxsplit=1)[0]
    file_bytes = pkgutil.get_data(pkg_root, "resources/mpconfig_defaults.yaml")
    if file_bytes:
        return yaml.safe_load(file_bytes)
    raise ValueError("Could not load definitions from resources/mpconfig_defaults.yaml")


# Supporting functions for MpConfigControls
def _get_mssg(value, path):
    return f"value '{value}', for setting at path '{path}'"


def _is_none_and_not_required(value, val_type, val_opts):
    if value is None or (val_type == "str" and not value):
        return not val_opts.get("required", True)
    return False


def _validate_string(value, path, val_type, val_opts):
    mssg = _get_mssg(value, path)
    if _is_none_and_not_required(value, val_type, val_opts):
        return ValidationResult(True, f"{_VALID_SUCCESS} {mssg}")
    if not isinstance(value, str):
        return ValidationResult(
            False,
            f"Value '{value}' of type {type(value)} should be type {val_type} - {mssg}",
        )
    if "options" in val_opts and value not in val_opts["options"]:
        return ValidationResult(
            False,
            f"Value {value} must be one of {', '.join(val_opts['options'])} - {mssg}",
        )
    if val_opts.get("format") == "uuid" and not is_valid_uuid(value):
        return ValidationResult(
            False,
            f"Value {value} should be a UUID - {mssg}",
        )
    return ValidationResult(True, f"{_VALID_SUCCESS} {mssg}")


def _validate_bool(value, path, val_type, val_opts):
    mssg = _get_mssg(value, path)
    if _is_none_and_not_required(value, val_type, val_opts):
        return ValidationResult(True, f"{_VALID_SUCCESS} {mssg}")
    if not isinstance(value, bool):
        return ValidationResult(
            False,
            f"Value '{value}' of type {type(value)} should be type {val_type} - {mssg}",
        )
    return ValidationResult(True, f"{_VALID_SUCCESS} {mssg}")


def _validate_m_enum(value, path, val_type, val_opts):
    mssg = _get_mssg(value, path)
    if _is_none_and_not_required(value, val_type, val_opts):
        return ValidationResult(True, f"{_VALID_SUCCESS} {mssg}")
    if not isinstance(value, (str, list)):
        return ValidationResult(
            False,
            f"Value '{value}' of type {type(value)} should be type {val_type} - {mssg}",
        )
    if "options" in val_opts:
        if isinstance(value, str) and value not in val_opts["options"]:
            return ValidationResult(
                False,
                f"Value {value} must be one of {', '.join(val_opts['options'])} - {mssg}",
            )
        if not isinstance(value, list):
            return ValidationResult(
                False,
                f"Value '{value}' should be a string or list. "
                + f"Must be one of {', '.join(val_opts['options'])} - {mssg}",
            )
        invalid_opts = [val for val in value if val not in val_opts["options"]]
        if invalid_opts:
            return ValidationResult(
                False,
                f"Invalid values '{invalid_opts}' found. "
                + f"Must be one of {', '.join(val_opts['options'])} - {mssg}",
            )
    return ValidationResult(True, f"{_VALID_SUCCESS} {mssg}")


def _validate_txt_dict(value, path, val_type, val_opts):
    mssg = _get_mssg(value, path)
    if _is_none_and_not_required(value, val_type, val_opts):
        return ValidationResult(True, f"{_VALID_SUCCESS} {mssg}")
    if isinstance(value, dict):
        for d_key, d_val in value.items():
            if not isinstance(d_key, str):
                return ValidationResult(
                    False,
                    f"Key {d_key} of {value} must be a string - {mssg}",
                )
            if not isinstance(d_val, (str, int, bool)):
                return ValidationResult(
                    False,
                    f"Value {d_val} of key {d_key} in {value} must be a"
                    + f" string, int or bool - {mssg}",
                )
        return ValidationResult(True, f"{_VALID_SUCCESS} {mssg}")
    return ValidationResult(False, f"Value {value} should be a dictionary - {mssg}")


def _validate_list(value, path, val_type, val_opts):
    mssg = _get_mssg(value, path)
    if _is_none_and_not_required(value, val_type, val_opts):
        return ValidationResult(True, f"{_VALID_SUCCESS} {mssg}")
    if isinstance(value, list):
        for item_val in value:
            expected_type = val_opts.get("elem_type", "str")
            val_type = type(item_val).__name__
            if expected_type != val_type:
                return ValidationResult(
                    False,
                    f"Item {item_val} of {value} expected to be a {expected_type} - {mssg}",
                )
        return ValidationResult(True, f"{_VALID_SUCCESS} {mssg}")
    return ValidationResult(False, f"Value {value} should be a list - {mssg}")


def _validate_defn(value, path, val_type, val_opts):
    """
    Validate a compound type definition.

    We want to validate a value against a definition like:
    ::
        cred_key:
            one_of:
                - str: str()
                - EnvironmentVar: str()
                - KeyVault: str(required=False)

    This only handles this type of definition and only looks at
    the "required" flag in the options. We'll extend this checker if we
    need to for other options.

    """
    mssg = _get_mssg(value, path)
    if _is_none_and_not_required(value, val_type, val_opts):
        return ValidationResult(True, f"{_VALID_SUCCESS} {mssg}")

    # This only handles "one_of" lists of alternatives
    opt_list = val_opts.get("defn", {}).get("one_of")
    if not opt_list:
        return ValidationResult(True, f"{_VALID_SUCCESS} {mssg}")
    # pull the definitions (val_type and val_opts) for each item into a dict
    opt_dict = {next(iter(val.keys())): next(iter(val.values())) for val in opt_list}

    # if the checked value is a string, validate the string
    if isinstance(value, str) and "str" in opt_dict:
        opt_v_opts = {"required": "required=False" not in opt_dict["str"]}
        return _validate_string(value, path, "str", opt_v_opts)

    # If the value is a dict
    if isinstance(value, dict):
        # We assume that the value is a dict with a single key
        v_key, v_val = next(iter(value.items()))
        # If the name is in the one_of dictionary names
        if v_key in opt_dict:
            # get the element type
            opt_v_type = re.sub(r"\(.*\)", "", opt_dict[v_key])
            # add "required=False" if that is the value options for this item
            opt_v_opts = {"required": "required=False" not in opt_dict[v_key]}
            # get the validator function for the option type and use that
            # to check the actual value
            _validator = _VALIDATORS.get(opt_v_type, _validate_string)
            return _validator(v_val, f"{path}.{v_key}", opt_v_type, opt_v_opts)
    # Otherwise the validation failed
    return ValidationResult(
        False,
        f"Value type {type(value)} does not match definition {val_opts['defn']} - {mssg}",
    )


_VALIDATORS = {
    "str": _validate_string,
    "enum": _validate_string,
    "m_enum": _validate_m_enum,
    "bool": _validate_bool,
    "cred_key": _validate_defn,
    "txt_dict": _validate_txt_dict,
    "list": _validate_list,
}
