# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Query helper definitions."""
from abc import ABC, abstractmethod
from enum import Enum
from typing import List, Union

import attr
from attr import Factory

from .utility import export
from .._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"

__all__ = ["KqlQuery"]


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
                for e_name, e_val in cls.__members__.items():
                    if e_name.upper() == value.upper():
                        return e_val
                return cls.Unknown
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
    LogAnalytics = 1
    Kusto = 2
    AzureSecurityCenter = 3
    SecurityGraph = 4

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


# Query definition
# pylint: disable=too-few-public-methods
@attr.s(auto_attribs=True)
class KqlQuery:
    """KqlQuery definition."""

    name: str = ""
    query: str = ""
    description: str = ""
    data_source: str = ""
    data_families: List[DataFamily] = Factory(list)
    data_environments: List[DataEnvironment] = Factory(list)
    optional_params: List[str] = Factory(list)
