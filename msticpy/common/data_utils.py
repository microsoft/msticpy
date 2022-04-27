# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Data utility functions."""
from typing import List, Union

import pandas as pd

from .._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"


def ensure_df_datetimes(
    data: pd.DataFrame,
    columns: Union[str, List[str], None] = None,
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
        localize_cols = {
            col for col in columns if col in data.select_dtypes("datetime")
        }
        for col in localize_cols:
            converted_data[col] = converted_data[col].dt.tz_localize(
                "UTC", ambiguous="infer", nonexistent="shift_forward"
            )
    return converted_data


def df_has_data(data) -> bool:
    """Return true if data is a pd.DataFrame and is not empty."""
    return isinstance(data, pd.DataFrame) and not data.empty
