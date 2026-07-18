# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
Pivot dispatch - resolve and invoke a pivot function.

Given a catalog entry, an input value and optional parameters, invokes the
underlying pivot function and normalizes the result to a pandas DataFrame.
"""

from __future__ import annotations

import logging
from typing import Any

import pandas as pd

from .._version import VERSION
from .catalog import PivotEntry

__version__ = VERSION
__author__ = "Ian Hellen"

logger = logging.getLogger(__name__)


class PivotDispatchError(Exception):
    """Raised when a pivot cannot be invoked or fails during execution."""


def _seq_to_dataframe(result: list | tuple) -> pd.DataFrame:
    """Normalize a list/tuple result to a DataFrame."""
    if not result:
        return pd.DataFrame()
    if all(isinstance(item, dict) for item in result):
        return pd.DataFrame(list(result))
    return pd.DataFrame({"result": list(result)})


def _to_dataframe(result: Any) -> pd.DataFrame:
    """Normalize an arbitrary pivot result to a pandas DataFrame."""
    if isinstance(result, pd.DataFrame):
        return result
    if isinstance(result, pd.Series):
        return result.to_frame()
    if isinstance(result, dict):
        return pd.DataFrame([result])
    if isinstance(result, (list, tuple)):
        return _seq_to_dataframe(result)
    # scalar / string / other
    return pd.DataFrame({"result": [result]})


def invoke_pivot(
    entry: PivotEntry,
    value: str | list[str],
    params: dict[str, Any] | None = None,
) -> pd.DataFrame:
    """
    Invoke a pivot function and return its result as a DataFrame.

    Parameters
    ----------
    entry : PivotEntry
        The catalog entry describing the pivot.
    value : str | list[str]
        The entity value(s) to pass as the pivot's primary input.
    params : dict[str, Any] | None, optional
        Additional keyword arguments to pass to the pivot function.

    Returns
    -------
    pd.DataFrame
        The normalized pivot result.

    Raises
    ------
    PivotDispatchError
        If the pivot invocation fails.

    """
    params = dict(params or {})
    call_kwargs = dict(params)
    call_args: tuple[Any, ...] = ()

    if entry.kind == "query":
        if entry.primary_param:
            # The MCP 'value' argument is authoritative - it overrides any
            # same-named entry the caller may have put in params.
            call_kwargs[entry.primary_param] = value
        elif not call_kwargs:
            raise PivotDispatchError(
                f"Cannot determine the primary parameter for query pivot "
                f"'{entry.entity}.{entry.pivot_path}'. Use describe_pivot to see its "
                f"parameters and pass them explicitly via 'params'."
            )
        # else: caller supplied explicit params for a query with no inferable
        # primary parameter - pass them through as-is.
    else:
        # Enrichment pivots take the value as their first positional argument.
        call_args = (value,)

    logger.info(
        "Invoking pivot %s.%s args=%s kwargs=%s",
        entry.entity,
        entry.pivot_path,
        call_args,
        list(call_kwargs),
    )
    try:
        result = entry.func(*call_args, **call_kwargs)
    except TypeError as err:
        raise PivotDispatchError(
            f"Invalid arguments for pivot '{entry.entity}.{entry.pivot_path}': {err}. "
            f"Use describe_pivot to inspect its parameters."
        ) from err
    except Exception as err:  # pylint: disable=broad-except
        raise PivotDispatchError(
            f"Pivot '{entry.entity}.{entry.pivot_path}' failed: {err}"
        ) from err

    return _to_dataframe(result)
