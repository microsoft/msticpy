# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Component edit utility functions."""
from typing import Any, Dict, Optional, Union

import ipywidgets as widgets
import requests

from .._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"

_DEBUG = True

LIST_LAYOUT = {
    "layout": widgets.Layout(height="150px", width="300px"),
    "style": {"description_width": "70px"},
}


if _DEBUG:

    def print_debug(*args):
        """Print args to std_out (Debug version)."""
        if not _DEBUG:
            return
        print(*args)


else:

    def print_debug(*args):
        """Print args to std_out (Prod version)."""
        del args


_TEXT_WIDGETS = (widgets.Text, widgets.Textarea, widgets.Label, widgets.Select)


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
    if val_type == "str" or isinstance(ctrl, _TEXT_WIDGETS) or isinstance(value, str):
        if value is None:
            return ""
        return str(value)
    return value


def widget_to_py(
    value: Any, ctrl: Optional[widgets.Widget] = None, val_type: Optional[str] = None
) -> Any:
    """
    Adjust type and format of value returned from `ctrl.value`.

    Parameters
    ----------
    value : Any
        The control value.
    ctrl : Optional[widgets.Widget], optional
        The source widget type, by default None
    val_type : Optional[str], optional
        The target value type, by default None

    Returns
    -------
    Any
        Converted value.

    Notes
    -----
    This function handles conversion of "" in text widgets
    to None.

    """
    if (
        isinstance(ctrl, widgets.Checkbox)
        or val_type == "bool"
        or isinstance(value, bool)
    ):
        return value
    if val_type == "txt_dict" or (
        isinstance(ctrl, _TEXT_WIDGETS) and getattr(ctrl, "tag", None) == "txt_dict"
    ):
        return txt_to_dict(value)
    if isinstance(value, str) or val_type == "str" or isinstance(ctrl, _TEXT_WIDGETS):
        if value == "":
            return None
        return str(value)
    return None if not value else value


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
    get_tenant_url = (
        "https://management.azure.com/subscriptions/{subscriptionid}"
        + "?api-version=2015-01-01"
    )
    resp = requests.get(get_tenant_url.format(subscriptionid=sub_id))
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
        for kv_pair in txt_val.split(";")
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
        if ":" in dict_val:
            key, val = dict_val.split(":", maxsplit=1)
        else:
            key, val = dict_val, ""
        return f"{key}={val};"
    return "; ".join(f"{key}:{val}" for key, val in dict_val.items())
