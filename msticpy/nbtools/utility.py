# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Miscellaneous helper methods for Jupyter Notebooks."""
import re
import sys
from typing import Callable, Optional, Any

from IPython.core.display import display, HTML
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
