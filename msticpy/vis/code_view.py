# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Display code with with highlighting."""
from typing import List, Optional

from IPython.display import HTML, DisplayHandle, display
from pygments import formatters, highlight, lexers, styles

from .._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"


def to_html(code: str, language: str, style: str = "default", full: bool = True) -> str:
    """
    Return Pygments-highlighted code for specified language.

    Parameters
    ----------
    code : str
        Input code as string.
    language : str
        The language name or alias.
    style : str, optional
        The pygments style to use, by default "default"
    full : bool, optional
        By default this function will return a complete HTML document,
        to only return the formatted code snippet (minus styles), set
        full=False.

    Returns
    -------
    str
       HTML document with pygments formatting.

    See Also
    --------
    list_pygments_styles - list the available styles use

    """
    pygments_lexer = lexers.get_lexer_by_name(language)
    # pylint: disable=no-member
    return highlight(
        code,
        pygments_lexer,
        formatters.HtmlFormatter(style=style, full=full, nobackground=True),
    )
    # pylint: enable=no-member


def list_pygments_styles() -> List[str]:
    """
    Return list of pygments styles available.

    Returns
    -------
    List[str]
        The list of pygments style names.

    """
    return list(styles.STYLE_MAP)


def display_html(
    code: str,
    language: str,
    style: str = "stata-dark",
    full: bool = True,
    display_handle: bool = False,
) -> Optional[DisplayHandle]:
    """
    Display pygments-formatted code.

    Parameters
    ----------
    code : str
        Input code as string.
    language : str
        The language name or alias.
    style : str, optional
        The pygments style to use, by default "stata-dark"
    full : bool, optional
        By default this function will return a complete HTML document,
        to only return the formatted code snippet (minus styles), set
        full=False.
    display_handle : bool, optional
        If True, will return an IPython DisplayHandle for the display
        object created.

    Returns
    -------
    Optional[DisplayHandle] :
        Handle to the IPython display object.

    See Also
    --------
    to_html

    """
    html = to_html(code, language, style, full)
    if display_handle:
        return display(HTML(html), display_id=True)
    display(HTML(html))
    return None
