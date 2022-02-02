# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Query helper definitions."""
from abc import ABC, abstractmethod
from enum import Enum
from typing import Union, List

import pandas as pd

from ..common.utility import export
from .._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"


# pylint: disable=invalid-name
@export
class DataFamily(Enum):
    """
    Enumeration of data families.

    Used to identify which queries are relevant for which
    data sources.
    """

    Unknown = 0
    WindowsSecurity = 1
    LinuxSecurity = 2
    SecurityAlert = 3
    SecurityGraphAlert = 4
    LinuxSyslog = 5
    AzureNetwork = 6
    MDATP = 7
    Splunk = 8
    ResourceGraph = 9
    Sumologic = 10
    Cybereason = 11

    @classmethod
    def parse(cls, value: Union[str, int]) -> "DataFamily":
        """
        Convert string or int to enum.

        Parameters
        ----------
        value : Union[str, int]
            value to parse

        """
        if isinstance(value, cls):
            return value

        parsed_enum = cls.Unknown
        if isinstance(value, str):
            try:
                parsed_enum = cls[value]
            except KeyError:
                # match to value if case is incorrect
                # pylint: disable=no-member
                for e_name, e_val in cls.__members__.items():
                    if e_name.upper() == value.upper():
                        return e_val
                return cls.Unknown
                # pylint: enable=no-member
        if isinstance(value, int):
            try:
                parsed_enum = cls(value)
            except ValueError:
                parsed_enum = cls.Unknown
        return parsed_enum


@export
class DataEnvironment(Enum):
    """
    Enumeration of data environments.

    Used to identify which queries are relevant for which
    data sources.
    """

    Unknown = 0
    AzureSentinel = 1  # alias of LogAnalytics
    LogAnalytics = 1
    MSSentinel = 1
    Kusto = 2
    AzureSecurityCenter = 3
    SecurityGraph = 4
    MDE = 5  # alias of MDATP
    MDATP = 5
    LocalData = 6
    Splunk = 7
    Mordor = 8
    ResourceGraph = 9
    Sumologic = 10
    M365D = 11
    Cybereason = 12

    @classmethod
    def parse(cls, value: Union[str, int]) -> "DataEnvironment":
        """
        Convert string or int to enum.

        Parameters
        ----------
        value : Union[str, int]
            value to parse

        """
        if isinstance(value, cls):
            return value

        parsed_enum = cls.Unknown
        if isinstance(value, str):
            try:
                parsed_enum = cls[value]
            except KeyError:
                pass
        if isinstance(value, int):
            parsed_enum = cls(value)
        return parsed_enum


# pylint: disable=too-few-public-methods
@export
class QueryParamProvider(ABC):
    """
    Abstract type for QueryParamProvider.

    Method query_params must be overridden by derived classes.

    """

    @property
    @abstractmethod
    def query_params(self):
        """
        Return dict of query parameters.

        These parameters are sourced in the object
        implementing this method.

        Returns
        -------
            dict -- dictionary of query parameter values.

        """
        return {}


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
