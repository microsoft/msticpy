# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Query helper definitions."""
from abc import ABC, abstractmethod
from enum import Enum
from typing import Union

from ..._version import VERSION
from ...common.utility import export

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
    Elastic = 14

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
                return next(
                    (
                        e_val
                        for e_name, e_val in cls.__members__.items()
                        if e_name.upper() == value.upper()
                    ),
                    cls.Unknown,
                )
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
    MSSentinel = 1
    AzureSentinel = 1  # alias of LogAnalytics
    LogAnalytics = 1
    Kusto = 2
    AzureSecurityCenter = 3
    MSGraph = 4
    SecurityGraph = 4
    MDE = 5
    MDATP = 5  # alias of MDE
    LocalData = 6
    Splunk = 7
    OTRF = 8
    Mordor = 8
    ResourceGraph = 9
    Sumologic = 10
    M365D = 11
    Cybereason = 12
    Elastic = 14

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


class Formatters:
    """Names of custom format handlers specified by driver."""

    DATETIME = "datetime"
    LIST = "list"
    PARAM_HANDLER = "custom_param_handler"
