# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Miscellaneous helper methods for Jupyter Notebooks."""
import re
import sys
from pathlib import Path
from typing import Callable, Optional, Any, Tuple, Union, Iterable
import warnings

from IPython.core.display import display, HTML, Markdown
import pandas as pd

from .._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"


def export(func: Callable):
    """Decorate function or class to export to __all__."""
    mod = sys.modules[func.__module__]
    if hasattr(mod, "__all__"):
        all_list = getattr(mod, "__all__")
        all_list.append(func.__name__)
    else:
        all_list = [func.__name__]
        setattr(mod, "__all__", all_list)
    return func


@export
def string_empty(string: str) -> bool:
    """Return True if the input string is None or whitespace."""
    return (string is None) or not (string and string.strip())


@export
def is_not_empty(test_object: Any) -> bool:
    """Return True if the test_object is not None or empty."""
    if test_object:
        if isinstance(test_object, str):
            if test_object.strip():
                # test_object is not None AND myString is not empty or blank
                return True
            return False
        return True
    return False


# Toggle Code Cell Contents
_TOGGLE_CODE_STR = """
<form action="javascript:code_toggle()">
    <input type="submit" id="toggleButton" value="Show/Hide Code">
</form>
"""

_TOGGLE_CODE_PREPARE_STR = """
    <script>
    function code_toggle() {
        if ($('div.cell.code_cell.rendered.selected div.input').css('display')!='none'){
            $('div.cell.code_cell.rendered.selected div.input').hide();
        } else {
            $('div.cell.code_cell.rendered.selected div.input').show();
        }
    }
    </script>

"""


@export
def enable_toggle_code():
    """Load JS Function to enable code toggle button."""
    display(HTML(_TOGGLE_CODE_PREPARE_STR))


@export
def toggle_code():
    """Display a toggle button to hide/reveal code cell."""
    display(HTML(_TOGGLE_CODE_STR))


# String escapes
@export
def escape_windows_path(str_path: str) -> str:
    """Escape backslash characters in a string."""
    if is_not_empty(str_path):
        return str_path.replace("\\", "\\\\")
    return str_path


@export
def unescape_windows_path(str_path: str) -> str:
    """Remove escaping from backslash characters in a string."""
    if is_not_empty(str_path):
        return str_path.replace("\\\\", "\\")
    return str_path


_PD_INSTALLED_VERSION = tuple(pd.__version__.split("."))
_PD_VER_23 = ("0", "23", "0")


@export
def pd_version_23() -> bool:
    """Return True if pandas version 0.23.0 or later is installed."""
    return _PD_INSTALLED_VERSION >= _PD_VER_23


@export
def get_nb_query_param(nb_url_search: str, param: str) -> Optional[str]:
    """
    Get a url query parameter from the search string.

    Parameters
    ----------
        nb_url_search {str} -- The URL search string
        param {str} -- The parameter name to search for

    Returns
    -------
        value of the query string parameter or None if not found.

    """
    qs_regex = r"[\\?&]{param}=(?P<val>[^&#]*)".format(param=param)
    query_string_match = re.search(qs_regex, nb_url_search)
    if query_string_match:
        return query_string_match["val"]
    return None


@export
def get_nb_query_params(nb_url_search: str) -> dict:
    """
    Get the url query parameters from the search string.

    Parameters
    ----------
    nb_url_search : str
        The URL search string

    Returns
    -------
    dict
        dictionary of the query string parameters.

    """
    nb_params = {}
    query_string_match = re.search(r"\?(?P<qs>[^#]+)#?", nb_url_search)
    if query_string_match:
        for param in query_string_match["qs"].split("&"):
            if "=" in param:
                nb_params[param.split("=")[0]] = param.split("=")[1]
    return nb_params


@export
def get_notebook_query_string():
    """Execute javascript to publish notebook query string as python variable."""
    HTML(
        """
    <script type="text/javascript">
        IPython.notebook.kernel.execute(
            "nb_query_string='".concat(window.location.search).concat("'"));
    </script>
    """
    )


@export
def check_py_version(min_ver: Tuple = (3, 6)):
    """
    Check that the current python version is not less than `min_ver`.

    Parameters
    ----------
    min_ver : Tuple, optional
        Minimum required version, by default (3,6)

    """
    if isinstance(min_ver, (float, str)):
        min_ver_list = str(min_ver).split(".")
        min_ver = (min_ver_list[0], min_ver_list[1])
    if sys.version_info < min_ver:
        print("Check the Kernel->Change Kernel menu and ensure that Python 3.6")
        print("or later is selected as the active kernel.")
        raise SystemExit(
            "Python %s.%s or later is required.\n" % min_ver[0], min_ver[1]
        )


@export
def resolve_pkg_path(part_path: str):
    """
    Resolve a path relative to the package.

    Parameters
    ----------
    part_path : str
        Absolute or relative path to resolve.

    """
    if Path(part_path).is_absolute():
        return part_path

    resolved_path = str(Path(__file__).resolve().parent.joinpath(part_path))
    if Path(resolved_path).exists():
        return str(resolved_path)

    searched_paths = list(
        Path(__file__).resolve().parent.glob(str(Path("**").joinpath(part_path)))
    )
    if not searched_paths or len(searched_paths) > 1:
        warnings.warn(f"No path or ambiguous match for {part_path} not found")
        return None
    return str(searched_paths[0])


# pylint: disable=invalid-name
def md(string: str, styles: Union[str, Iterable[str]] = None):
    """
    Return string as Markdown with optional style.

    Parameters
    ----------
    string : str
        The string to display
    styles : Union[str, Iterable[str]], optional
        A style mnemonic or collection of styles. If multiple styles,
        these can be supplied as an interable of strings or a comma-separated
        string, by default None

    """
    style_str = ""
    if isinstance(styles, str):
        if "," in styles:
            styles = [style.strip() for style in styles.split(",")]
        else:
            style_str = _F_STYLES.get(styles, "")
    if isinstance(styles, list):
        style_str = ";".join([_F_STYLES.get(style, "") for style in styles])
    display(Markdown(f"<p style='{style_str}'>{string}</p>"))


def md_warn(string: str):
    """
    Return string as a warning - red text prefixed by "Warning".

    Parameters
    ----------
    string : str
        The warning message.

    """
    md(f"Warning: {string}", "bold, red, large")


# Styles available to use in the above Markdown tools.
_F_STYLES = {
    "bold": "font-weight: bold",
    "italic": "font-style: italic",
    "red": "color: red",
    "large": "font-size: 130%",
    "heading": "font-size: 200%",
}
