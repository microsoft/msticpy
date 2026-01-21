# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Data utility functions."""

import re

import pandas as pd

from .._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"


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
    columns : Union[str, List[str], None], optional
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


def parse_timespan(timespan: str | pd.Timedelta | None) -> pd.Timedelta | None:
    """
    Parse a KQL/Kusto timespan-like formatted string into a pandas Timedelta.

    This function handles the timespan format returned by KQL queries, which
    uses "d.hh:mm:ss.fffffff" format for timespans >= 1 day. This format is
    not compatible with pandas' to_timedelta function, which expects only
    "hh:mm:ss" format.

    Parameters
    ----------
    timespan : Union[str, pd.Timedelta, None]
        The timespan to parse. Can be:
        - A string in KQL timespan format (e.g., "1.00:00:00", "00:00:00.001")
        - An existing pd.Timedelta object (returned as-is)
        - None (returned as None)

    Returns
    -------
    Optional[pd.Timedelta]
        Parsed timedelta with nanosecond resolution, or None if input is None.

    Raises
    ------
    ValueError
        If the timespan string does not match the expected format.

    Examples
    --------
    >>> parse_timespan("00:00:00.001")
    Timedelta('0 days 00:00:00.001000')
    >>> parse_timespan("1.00:00:00")
    Timedelta('1 days 00:00:00')
    >>> parse_timespan("3.23:59:59.9999999")
    Timedelta('3 days 23:59:59.999999900')
    >>> parse_timespan("-1.00:00:00")
    Timedelta('-2 days +00:00:00')

    Notes
    -----
    - Requires at minimum "hh:mm:ss" format to be present in the string
    - Returns a nanosecond resolution Timedelta object (pandas default)
    - Handles negative timespans with a leading "-" sign
    - Fractional seconds are rounded to nanosecond precision

    """
    if timespan is None:
        return None
    if isinstance(timespan, pd.Timedelta):
        return timespan

    # Pattern matches: [-][d.]hh:mm:ss[.fffffff]
    # Groups: sign, days, hours, minutes, seconds, frac_seconds
    # Note: We don't validate that hours/minutes/seconds are in valid ranges
    # (e.g., hours 0-23, minutes/seconds 0-59) because:
    # 1. KQL may return these values as emitted from the database
    # 2. pandas.Timedelta handles normalization (e.g., 90 minutes -> 1h 30m)
    # 3. It's more robust to accept the format and let pandas normalize
    pattern = re.compile(
        r"(?P<sign>-?)(?:(?P<days>\d+)\.)?"
        r"(?P<hours>\d+):(?P<minutes>\d+):(?P<seconds>\d+)"
        r"(?P<frac_seconds>\.\d+)?"
    )
    match = pattern.match(str(timespan))
    if not match:
        msg = f"Invalid timespan format: {timespan}"
        raise ValueError(msg)

    match_groups = match.groupdict()
    sign = -1 if match_groups["sign"] == "-" else 1
    days = int(match_groups["days"]) if match_groups["days"] else 0
    hours = int(match_groups["hours"])
    minutes = int(match_groups["minutes"])
    seconds = int(match_groups["seconds"])
    
    # Parse fractional seconds as string to avoid floating-point precision errors
    # KQL returns up to 7 decimal places (e.g., .1697513 = 169751300 nanoseconds)
    frac_str = match_groups["frac_seconds"]
    if frac_str:
        # Remove leading '.' and pad/truncate to 9 digits (nanoseconds)
        frac_digits = frac_str[1:]  # Remove the '.'
        # Pad with zeros if less than 9 digits, truncate if more
        frac_digits = frac_digits.ljust(9, "0")[:9]
        nano_seconds = int(frac_digits)
    else:
        nano_seconds = 0

    return sign * pd.Timedelta(
        days=days,
        hours=hours,
        minutes=minutes,
        seconds=seconds,
        nanoseconds=nano_seconds,
    )


def ensure_df_timedeltas(
    data: pd.DataFrame,
    columns: str | list[str] | None = None,
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
    columns : Union[str, List[str], None], optional
        column (str) or list of columns to convert, by default None.
        If this parameter is not supplied, the function will attempt to
        auto-detect timespan columns by checking for string columns that
        match the timespan pattern.

    Returns
    -------
    pd.DataFrame
        Converted DataFrame with timespan columns as timedelta64[ns].

    Examples
    --------
    >>> df = pd.DataFrame({"duration": ["1.00:00:00", "00:00:00.001"]})
    >>> df_converted = ensure_df_timedeltas(df)
    >>> df_converted["duration"].dtype
    dtype('timedelta64[ns]')

    >>> # Specify columns explicitly
    >>> df_converted = ensure_df_timedeltas(df, columns="duration")

    Notes
    -----
    - This function uses vectorized operations where possible for efficiency
    - Columns that are already timedelta dtype are skipped
    - Invalid timespan strings will raise a ValueError during conversion

    See Also
    --------
    parse_timespan : Parse individual timespan strings
    ensure_df_datetimes : Similar function for datetime conversion

    """
    converted_data = data.copy()

    # Auto-detect timespan columns if not specified
    if columns is None:
        columns = _detect_timespan_columns(data)
    elif isinstance(columns, str):
        columns = [columns]

    # Convert each timespan column
    for col in columns:
        if col not in data.columns:
            continue
        # Skip if already timedelta dtype
        if pd.api.types.is_timedelta64_dtype(data[col]):
            continue

        # Try vectorized conversion first (for small timespans)
        try:
            converted_data[col] = pd.to_timedelta(data[col])
        except (ValueError, TypeError):
            # Fall back to element-wise parsing for large timespans
            converted_data[col] = data[col].apply(parse_timespan)

    return converted_data


def _detect_timespan_columns(data: pd.DataFrame) -> list[str]:
    """
    Detect columns that likely contain KQL timespan values.

    Parameters
    ----------
    data : pd.DataFrame
        Input dataframe to analyze

    Returns
    -------
    list[str]
        List of column names that likely contain timespan values

    """
    timespan_pattern = re.compile(
        r"^-?(?:\d+\.)?(?:\d{1,2}):(?:\d{1,2}):(?:\d{1,2})(?:\.\d+)?$"
    )

    timespan_columns = []
    # Only check object/string columns
    for col in data.select_dtypes(include=["object", "string"]).columns:
        # Sample first non-null value
        sample_val = data[col].dropna().head(1)
        if not sample_val.empty and timespan_pattern.match(str(sample_val.iloc[0])):
            timespan_columns.append(col)

    return timespan_columns
