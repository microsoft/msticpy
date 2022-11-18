# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Component edit utility functions."""
from typing import Any, Dict, Optional, Tuple, Union

import httpx
import ipywidgets as widgets

from .._version import VERSION
from ..auth.azure_auth_core import AzureCloudConfig
from ..common.utility import mp_ua_header
from .comp_edit import SettingsControl

__version__ = VERSION
__author__ = "Ian Hellen"

_DEBUG = False

ITEM_LIST_LAYOUT = {
    "layout": widgets.Layout(height="150px", width="300px"),
    "style": {"description_width": "70px"},
}

SETTING_LIST_LAYOUT = {
    "layout": widgets.Layout(height="150px", width="300px"),
    "style": {"description_width": "100px"},
}

TEXT_LAYOUT = {
    "layout": widgets.Layout(width="70%"),
    "style": {"description_width": "100px"},
}

TEXT_AREA_LAYOUT = {
    "layout": widgets.Layout(width="70%"),
    "style": {"description_width": "100px"},
}


if _DEBUG:

    def print_debug(*args):
        """Print args to std_out (Debug version)."""
        if not _DEBUG:
            return
        print(*args)

else:

    def print_debug(*args):
        """Print nothing std_out (Prod version)."""
        del args


_TEXT_WIDGETS = (widgets.Text, widgets.Textarea, widgets.Label, widgets.Select)


# pylint: disable=too-many-return-statements
def py_to_widget(
    value: Any, ctrl: Optional[widgets.Widget] = None, val_type: Optional[str] = None
) -> Any:
    """
    Adjust type and format to suit target widget.

    Parameters
    ----------
    value : Any
        The value to process
    ctrl : Optional[widgets.Widget], optional
        The target widget type, by default None
    val_type : Optional[str], optional
        The target value type ("str", "bool"), by default None

    Returns
    -------
    Any
        The converted value

    Raises
    ------
    ValueError
        If neither a target control or expected val_type
        are specified.

    Notes
    -----
    This function handles conversion of None to an empty string
    or bools expressed as text strings into actual bools.

    """
    if ctrl is None and val_type is None:
        raise ValueError("Must specify either a target control or expected val_type.")
    if (
        isinstance(ctrl, widgets.Checkbox)
        or val_type == "bool"
        or isinstance(value, bool)
    ):
        if isinstance(value, str):
            return value.casefold() == "true"
        return bool(value)
    if val_type == "txt_dict" or (
        isinstance(ctrl, _TEXT_WIDGETS) and getattr(ctrl, "tag", None) == "txt_dict"
    ):
        return dict_to_txt(value)
    if val_type == "list" or (
        isinstance(ctrl, _TEXT_WIDGETS) and getattr(ctrl, "tag", None) == "list"
    ):
        return "\n".join(value)
    if val_type == "str" or isinstance(ctrl, _TEXT_WIDGETS) or isinstance(value, str):
        if value is None:
            return ""
        return str(value)
    return value


def widget_to_py(ctrl: Union[widgets.Widget, SettingsControl]) -> Any:
    """
    Adjust type and format of value returned from `ctrl.value`.

    Parameters
    ----------
    ctrl : Union[widgets.Widget, SettingsControl]
        The source widget

    Returns
    -------
    Any
        Converted value.

    Notes
    -----
    This function handles conversion of widget values to
    configuration (Python) values.

    """
    if isinstance(ctrl, widgets.Checkbox):
        return ctrl.value
    if isinstance(ctrl, widgets.Textarea) and getattr(ctrl, "tag", None) == "txt_dict":
        return txt_to_dict(ctrl.value) or None
    if isinstance(ctrl, widgets.Textarea) and getattr(ctrl, "tag", None) == "list":
        return ctrl.value.split("\n") if ctrl.value else []
    if isinstance(ctrl, _TEXT_WIDGETS):
        if ctrl.value == "":
            return None
        return str(ctrl.value)
    if isinstance(ctrl, widgets.SelectMultiple):
        return list(ctrl.value)
    return None if not ctrl.value else ctrl.value


# pylint: enable=too-many-return-statements


def get_def_tenant_id(sub_id: str) -> Optional[str]:
    """
    Get the tenant ID for a subscription.

    Parameters
    ----------
    sub_id : str
        Subscription ID

    Returns
    -------
    Optional[str]
        TenantID or None if it could not be found.

    Notes
    -----
    This function returns the tenant ID that owns the subscription.
    This may not be the correct ID to use if you are using delegated
    authorization via Azure Lighthouse.

    """
    res_mgmt_uri = AzureCloudConfig().endpoints.resource_manager
    get_tenant_url = (
        f"{res_mgmt_uri}/subscriptions/{{subscriptionid}}" + "?api-version=2015-01-01"
    )
    resp = httpx.get(
        get_tenant_url.format(subscriptionid=sub_id), headers=mp_ua_header()
    )
    # Tenant ID is returned in the WWW-Authenticate header/Bearer authorization_uri
    www_header = resp.headers.get("WWW-Authenticate")
    if not www_header:
        return None
    hdr_dict = {
        item.split("=")[0]: item.split("=")[1].strip('"')
        for item in www_header.split(", ")
    }
    tenant_path = hdr_dict.get("Bearer authorization_uri", "").split("/")
    return tenant_path[-1] if tenant_path else None


def txt_to_dict(txt_val: str) -> Dict[str, Any]:
    """
    Return dict from string of "key:val; key2:val2" pairs.

    Parameters
    ----------
    txt_val : str
        The key/value string (items separated by ";",
        key/value separated by ":")

    Returns
    -------
    Dict[str, Any]
        Dictionary of key/values

    """
    if not txt_val:
        return {}
    kvpairs = [
        kv_pair.strip().split(":", maxsplit=1)
        for kv_pair in txt_val.split("\n")
        if kv_pair.strip()
    ]
    return {
        kval[0].strip(): kval[1].strip() if len(kval) > 1 else None for kval in kvpairs
    }


def dict_to_txt(dict_val: Union[str, Dict[str, Any]]) -> str:
    """
    Return string as "key:val; key2:val2" pairs from `dict_val`.

    Parameters
    ----------
    dict_val : Union[str, Dict[str, Any]]
        Dict of key/val pairs
        or string of single key/value

    Returns
    -------
    str
        str formatted as "key:val; key2:val2"

    """
    if isinstance(dict_val, str):
        if not dict_val:
            return ""
        if ":" in dict_val:
            key, val = dict_val.split(":", maxsplit=1)
        else:
            key, val = dict_val, ""
        return f"{key}:{val}"
    if isinstance(dict_val, dict):
        return "\n".join(f"{key}:{val}" for key, val in dict_val.items())
    return ""


# pylint: disable=too-many-branches
# flake8: noqa: F821
def get_wgt_ctrl(
    setting_path: str,
    var_name: str,
    mp_controls: "MpConfigControls",  # type: ignore
    wgt_style: Optional[Dict[str, Any]] = None,
    instance_name: str = None,
) -> widgets.Widget:
    """
    Return widget appropriate to value type of `var_name`.

    Parameters
    ----------
    setting_path : str
        The setting path (parent path) as dotted string.
    var_name : str
        The key name for the setting below `setting_path`.
    mp_controls : MpConfigControls
        Instance of MpConfigControls data
    wgt_style : Optional[Dict[str, Any]]
        Dict of style and layout items:
        .. parsed-literal::

            {
                "style": {"description_width": "100px"},
                "layout": widgets.Layout(width="50%")
            }
    instance_name : Optional[str]
        An optional sub-path to an instance of this provider
        (e.g. 'Cluster1' of path.Kusto-Cluster1)

    Returns
    -------
    widgets.Widget
        The widget.

    """
    if wgt_style is None:
        wgt_style = {}
    if instance_name:
        # insert instance name into path
        path_elems = setting_path.split(".")
        var_root = ".".join(
            [path_elems[0], f"{path_elems[1]}-{instance_name}", *path_elems[2:]]
        )
    else:
        var_root = setting_path
    var_path = f"{var_root}.{var_name}"
    ctrl = mp_controls.get_control(var_path)
    defn_path = f"{setting_path}.{var_name}"
    comp_defn = mp_controls.get_defn(defn_path)
    if comp_defn and not isinstance(comp_defn, tuple):
        # definition is a literal
        def_value = comp_defn
    else:
        st_type, st_opts = get_defn_or_default(comp_defn)
        def_value = st_opts["default"] if st_opts and "default" in st_opts else ""
    curr_val = mp_controls.get_value(var_path) or def_value

    if ctrl is None:
        st_type, st_opts = get_defn_or_default(comp_defn)
        if st_type == "bool":
            ctrl = widgets.Checkbox(
                description=var_name,
                value=py_to_widget(curr_val, val_type=st_type),
                **(wgt_style or TEXT_LAYOUT),
            )
        elif st_type == "enum":
            ctrl = widgets.Select(
                description=var_name,
                options=st_opts.get("options"),
                value=curr_val or "",
                **(wgt_style or SETTING_LIST_LAYOUT),
            )
        elif st_type == "m_enum":
            ctrl = widgets.SelectMultiple(
                description=var_name,
                options=st_opts.get("options"),
                value=curr_val or [],
                **(wgt_style or SETTING_LIST_LAYOUT),
            )
        elif st_type == "txt_dict":
            ctrl = widgets.Textarea(
                description=var_name, value=dict_to_txt(curr_val) or "", **wgt_style
            )
            setattr(ctrl, "tag", "txt_dict")
        elif st_type == "list":
            ctrl = widgets.Textarea(
                description=var_name,
                value=py_to_widget(curr_val, val_type="list") or "",
                **(wgt_style or TEXT_AREA_LAYOUT),
                # tooltip="Enter each item as 'key:value'. Separate items with new lines.",
            )
            setattr(ctrl, "tag", "list")
        else:
            ctrl = widgets.Text(
                description=var_name,
                value=py_to_widget(curr_val, val_type=st_type),
                **(wgt_style or TEXT_AREA_LAYOUT),
                # tooltip="Enter items (no quotes) on separate lines.",
            )
        mp_controls.set_control(var_path, ctrl)
    else:
        ctrl.value = py_to_widget(curr_val, ctrl=ctrl)
    return ctrl


def get_defn_or_default(defn: Union[Tuple[str, Any], Any]) -> Tuple[str, Dict]:
    """
    Return the type and options (or a default) for the setting definition.

    Parameters
    ----------
    defn : Optional[Tuple[str, dict]]
        Setting definition. Returns a default of "str", {}
        if no definition is passed.

    Returns
    -------
    Tuple[str, Dict]
        Tuple of setting type and options.

    """
    if isinstance(defn, tuple):
        return defn[0], defn[1]
    return "str", {}


# flake8: noqa: F821
def get_or_create_mpc_section(
    mp_controls: "MpConfigControls", section: str, subkey: Optional[str] = None  # type: ignore
) -> Any:
    """
    Return (and create if it doesn't exist) a settings section.

    Parameters
    ----------
    mp_controls : MpConfigControls
        The MP Config database.
    section : str
        The section name (top level settings item)
    subkey : Optional[str], optional
        Optional subkey to create, by default None

    Returns
    -------
    Any
        The settings at that section[subkey] location.

    """
    curr_section = mp_controls.get_value(section)
    if curr_section is None:
        mp_controls.set_value(section, {})
        curr_section = mp_controls.get_value(section)
    if subkey and subkey not in curr_section:
        mp_controls.set_value(f"{section}.{subkey}", {})
        return mp_controls.get_value(f"{section}.{subkey}")
    return mp_controls.get_value(section)
