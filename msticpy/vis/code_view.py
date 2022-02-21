# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Display code with with highlighting."""
from typing import List

from IPython.display import HTML, display, DisplayHandle
from pygments import highlight, lexers, styles, formatters

from .._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"


def to_html(
    code: str, language: str, style: str = "stata-dark", full: bool = True
) -> str:
    """
    Return Pygments-highlighted code for specified language.

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

    Returns
    -------
    str
       HTML document with pygments formatting.

    """
    pygments_lexer = lexers.get_lexer_by_name(language)
    return highlight(
        code,
        pygments_lexer,
        formatters.HtmlFormatter(style=style, full=full, nobackground=True),
    )


def list_pygments_styles() -> List[str]:
    """
    Return list of pygments styles available.

    Returns
    -------
    List[str]
        _description_
    """
    return list(styles.STYLE_MAP)


def display_html(
    code: str, language: str, style: str = "stata-dark", full: bool = True
) -> DisplayHandle:
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

    Returns
    -------
    DisplayHandle:
        Handle to the IPython display object.

    See Also
    --------
    to_html

    """
    html = to_html(code, language, style, full)
    return display(HTML(html), display_id=True)
