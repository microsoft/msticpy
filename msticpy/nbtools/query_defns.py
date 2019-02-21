# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Query helper definitions."""
from abc import ABC, abstractmethod
from enum import Enum

from attr import attrs, attrib, Factory

from . utility import export

__all__ = ['KqlQuery']


# Query definition
@attrs
class KqlQuery(object):
    """KqlQuery definition."""

    name = attrib(default=None)
    query = attrib(default=None)
    description = attrib(default=None)
    data_source = attrib(default=None)
    data_families = attrib(default=Factory(list))
    data_environments = attrib(default=Factory(list))
    optional_params = attrib(default=Factory(list))


@export
class DataFamily(Enum):
    """
    Enumeration of data families.

    Used to identify which queries are relevant for which
    data sources.
    """

    WindowsSecurity = 1
    LinuxSecurity = 2
    SecurityAlert = 3

    @classmethod
    def parse(cls, value):
        """
        Conver string or int to enum.

            :param value: value to parse
        """
        if isinstance(value, cls):
            return value

        parsed_enum = None
        if isinstance(value, str):
            try:
                parsed_enum = cls[value]
            except KeyError:
                pass
        if isinstance(value, int):
            parsed_enum = cls(value)
        return parsed_enum


@export
class DataEnvironment(Enum):
    """
    Enumeration of data environments.

    Used to identify which queries are relevant for which
    data sources.
    """

    LogAnalytics = 1
    Kusto = 2

    @classmethod
    def parse(cls, value):
        """
        Conver string or int to enum.

            :param value: value to parse
        """
        if isinstance(value, cls):
            return value

        parsed_enum = None
        if isinstance(value, str):
            try:
                parsed_enum = cls[value]
            except KeyError:
                pass
        if isinstance(value, int):
            parsed_enum = cls(value)
        return parsed_enum


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

        Returns:
            dict -- dictionary of query parameter values.

        """
        return {}
