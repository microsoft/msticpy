# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Notebook utility functions."""

from typing import Iterable, Optional, Union

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
