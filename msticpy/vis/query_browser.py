# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""QueryProvider Query Browser."""
import textwrap
from typing import Any, Generator

from IPython.display import HTML

from .._version import VERSION
from ..nbwidgets import SelectItem

__version__ = VERSION
__author__ = "Ian Hellen"


def browse_queries(query_provider: Any, **kwargs) -> SelectItem:
    """
    Return QueryProvider query browser.

    Parameters
    ----------
    query_provider : QueryProvider
        Initialized query provider.

    Other Parameters
    ----------------
    kwargs :
        passed to SelectItem constuctor.

    Returns
    -------
    SelectItem
        SelectItem browser for TI Data.

    """
    if "height" not in kwargs:
        kwargs["height"] = "300px"
    disp_func = _query_display_func(query_provider)
    opts = query_provider.list_queries()
    return SelectItem(item_list=opts, action=disp_func, **kwargs)


def _format_query_doc(query_doc) -> Generator[str, None, None]:
    """Format query docstring as HTML."""
    qdoc_lines = query_doc.split("\n")
    yield f"<h3>{qdoc_lines[0]}</h3>"
    for line in qdoc_lines[1:]:
        if line.strip() == "Parameters":
            yield f"<p><b>{line}</b></p>"
        elif line.strip().startswith("---"):
            continue
        else:
            indent = len(line) - len(line.strip())
            if indent:
                indent *= 10
                yield f"<div style='margin-left: {indent}px'>{line}</div>"
            else:
                yield f"<div>{line}</div>"


def _format_query(query_text):
    """Format query for display."""
    q_lines = []
    for line in query_text.split("|"):
        if len(line) > 80:
            line = "\n  ".join(textwrap.wrap(line))
        q_lines.append(line)
    q_text = "\n|".join(q_lines)
    return f"<p><b>Query</b></p><pre>{q_text}</pre>"


def _get_query_sample(query):
    """Format query sample."""
    return f"""
        <p><b>Example</b></p>
        <p>{{QueryProvider}}[.QueryPath].QueryName(params...)</p>
        <pre>qry_prov.{query}(start=start, end=end, hostname=host)</pre>
    """


def _query_display_func(query_provider):
    """Closure returning function to display query."""

    def get_query_doc(query):
        """Return doc string and query as HTML."""
        query_func = getattr(query_provider, query)
        q_help = "".join(_format_query_doc(query_func.__doc__))
        q_text = query_provider.get_query(query).strip()
        q_sample = _get_query_sample(query)
        q_text = _format_query(q_text)
        return HTML(f"{q_help}<br>{q_text}<br>{q_sample}")

    return get_query_doc
