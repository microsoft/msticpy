# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Data utility functions."""

from __future__ import annotations

import pandas as pd
from azure.kusto.data.helpers import parse_timedelta as parse_timespan

from .._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"

# Re-export parse_timespan (from azure.kusto.data.helpers.parse_timedelta)
__all__ = ["parse_timespan", "ensure_df_datetimes", "ensure_df_timedeltas", "df_has_data"]


def ensure_df_datetimes(
    data: pd.DataFrame,
    columns: str | list[str] | None = None,
    add_utc_tz: bool = True,
) -> pd.DataFrame:
    """
    Return dataframe with converted TZ-aware timestamps.

    Parameters
    ----------
    data : pd.DataFrame
        Input dataframe
    columns : str | list[str] | None, optional
        column (str) or list of columns to convert, by default None.
        If this parameter is not supplied then any column containing
        the substring "time" is used as a candidate for conversion.
    add_utc_tz: bool, optional
        If True any datetime columns in the `columns` parameter (
        (or default `'.*time.*'` columns) that are timezone-naive,
        will be converted to Timezone-aware timestamps marked as UTC.

    Returns
    -------
    pd.DataFrame
        Converted DataFrame.

    """
    if not columns:
        columns = list(data.filter(regex=".*[Tt]ime.*").columns)
    if isinstance(columns, str):
        columns = [columns]
    col_map = {
        col: "datetime64[ns, UTC]"
        for col in set(columns)
        if col in data.columns and not pd.api.types.is_datetime64_any_dtype(data[col])
    }
    converted_data = data.astype(col_map, errors="ignore")

    # Look for any TZ-naive columns in the list
    if add_utc_tz:
        localize_cols = {col for col in columns if col in data.select_dtypes("datetime")}
        for col in localize_cols:
            converted_data[col] = converted_data[col].dt.tz_localize(
                "UTC", ambiguous="infer", nonexistent="shift_forward"
            )
    return converted_data


def df_has_data(data) -> bool:
    """Return true if data is a pd.DataFrame and is not empty."""
    return isinstance(data, pd.DataFrame) and not data.empty


def ensure_df_timedeltas(
    data: pd.DataFrame,
    columns: str | list[str],
) -> pd.DataFrame:
    """
    Return dataframe with KQL timespan columns converted to timedelta64[ns].

    This function converts string columns containing KQL timespan values to
    pandas timedelta64[ns] dtype. It handles both small timespans (< 1 day)
    and large timespans (>= 1 day) which use the "d.hh:mm:ss.fffffff" format.

    Parameters
    ----------
    data : pd.DataFrame
        Input dataframe
    columns : str | list[str]
        Column name (str) or list of column names to convert.

    Returns
    -------
    pd.DataFrame
        Converted DataFrame with timespan columns as timedelta64[ns].

    Raises
    ------
    ValueError
        If any timespan string in the specified columns cannot be parsed.

    Examples
    --------
    >>> df = pd.DataFrame({"duration": ["1.00:00:00", "00:00:00.001"]})
    >>> df_converted = ensure_df_timedeltas(df, columns="duration")
    >>> df_converted["duration"].dtype
    dtype('timedelta64[ns]')

    >>> # Specify multiple columns
    >>> df_converted = ensure_df_timedeltas(df, columns=["duration", "elapsed"])

    Notes
    -----
    Uses azure.kusto.data.helpers.parse_timedelta for parsing.

    See Also
    --------
    parse_timespan : Parse individual timespan strings
    ensure_df_datetimes : Similar function for datetime conversion

    """
    converted_data = data.copy()

    if isinstance(columns, str):
        columns = [columns]

    # Convert each timespan column
    for col in columns:
        if col not in data.columns:
            continue
        # Skip if already timedelta dtype
        if pd.api.types.is_timedelta64_dtype(data[col]):
            continue

        converted_data[col] = data[col].apply(parse_timespan)

    return converted_data
