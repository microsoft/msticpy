# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Notebook utility functions."""
# pickle only used here for storing data.
import pickle  # nosec
from base64 import b64encode
from typing import Any, Iterable, Optional, Union

from IPython import get_ipython
from IPython.display import HTML, DisplayHandle, display

from ..._version import VERSION
from .types import export

__version__ = VERSION
__author__ = "Ian Hellen"


# pylint: disable=invalid-name
@export
def md(
    string: str,
    styles: Union[str, Iterable[str]] = None,
    disp_id: Optional[Union[bool, DisplayHandle]] = None,
) -> Optional[DisplayHandle]:
    """
    Display a string as Markdown with optional style.

    Parameters
    ----------
    string : str
        The string to display
    styles : Union[str, Iterable[str]], optional
        A style mnemonic or collection of styles. If multiple styles,
        these can be supplied as an interable of strings or a comma-separated
        string, by default None
    disp_id : Optional[Union[bool, DisplayHandle]], optional
        If True, the function will return a display handle that can be re-used
        in subsequent calls to update the display object.
        If this is previously-created display handle, this is used as the
        target display object to update it with the content of this call,
        by default None

    Returns
    -------
    DisplayHandle
        A handle to the display object that can be used to update the
        contents.

    """
    style_str = ""
    if isinstance(styles, str):
        if "," in styles:
            styles = [style.strip() for style in styles.split(",")]
        else:
            style_str = _F_STYLES.get(styles, "")
    if isinstance(styles, list):
        style_str = ";".join(_F_STYLES.get(style, "") for style in styles)
    content = HTML(f"<p style='{style_str}'>{string}</p>")

    if isinstance(disp_id, bool) and disp_id:
        return display(content, display_id=True)
    if isinstance(disp_id, DisplayHandle):
        return disp_id.update(content)
    display(content)
    return None


# pylint: enable=invalid-name


@export
def md_warn(string: str, disp_id: Optional[DisplayHandle] = None):
    """
    Return string as a warning - orange text prefixed by "Warning".

    Parameters
    ----------
    string : str
        The warning message.
    disp_id : Optional[DisplayHandle], optional
        If True, the function will return a display handle that can be re-used
        in subsequent calls to update the display object.
        If this is previously-created display handle, this is used as the
        target display object to update it with the content of this call,
        by default None

    Returns
    -------
    DisplayHandle
        A handle to the display object that can be used to update the
        contents.

    """
    return md(f"Warning: {string}", "bold, orange, large", disp_id)


@export
def md_error(string: str, disp_id: Optional[DisplayHandle] = None):
    """
    Return string as an error - red text prefixed by "Error".

    Parameters
    ----------
    string : str
        The error message.
    disp_id : Optional[Union[bool, DisplayHandle]], optional
        If True, the function will return a display handle that can be re-used
        in subsequent calls to update the display object.
        If this is previously-created display handle, this is used as the
        target display object to update it with the content of this call,
        by default None

    """
    return md(f"Error: {string}", "bold, orange, large", disp_id)


# Styles available to use in the above Markdown tools.
_F_STYLES = {
    "bold": "font-weight: bold",
    "italic": "font-style: italic",
    "red": "color: red",
    "green": "color: green",
    "blue": "color: blue",
    "large": "font-size: 130%",
    "heading": "font-size: 200%",
    "outline": "border: solid; padding: 5pt",
}


@export
def is_ipython(notebook: bool = False) -> bool:
    """
    Return True if running in IPython environment.

    Parameters
    ----------
    notebook : bool, optional
        If notebook is true this will only return true if running
        in a Jupyter notebook.

    Returns
    -------
    bool
        True if running in IPython environment,
        otherwise False

    """
    return (
        get_ipython() and type(get_ipython()).__name__.startswith("ZMQ")
        if notebook
        else bool(get_ipython())
    )


_CODE_CELL_TEMPLATE = """#########################################
# Run this cell to restore cached data to
# the object "{var_name}"
#########################################

from base64 import b64decode
import pickle

## Store dynamic summaries as base64 byte string
summary_data = {encoded_bytes}

# decode and unpickle the summaries
{var_name} = pickle.loads(b64decode(summary_data))
{var_name}
"""


@export
def save_obj_to_cell(obj: Any, var_name: str):
    """
    Save a pickle-able object to a new cell.

    Parameters
    ----------
    obj : Any
        The object to be stored. Must be pickle-able.
    var_name : str
        The variable name use to restore the object
        in the new cell.

    Raises
    ------
    TypeError
        If the object does not support pickling.

    Notes
    -----
    Saves `obj` as picked, base64-encoded blob to
    a new notebook cell.

    """
    try:
        picked_obj = pickle.dumps(obj)
    except pickle.PickleError as err:
        raise TypeError(f"Object of type {type(obj)} cannot be picked.") from err
    encoded_bytes = b64encode(picked_obj)
    cell_text = _CODE_CELL_TEMPLATE.format(
        encoded_bytes=encoded_bytes,
        var_name=var_name,
    )
    shell = get_ipython()
    # create a new cell using `cell_code` as the code contents.
    shell.set_next_input(cell_text)
