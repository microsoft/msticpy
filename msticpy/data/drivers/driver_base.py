# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Data driver base class."""
import abc
from abc import ABC
from collections import defaultdict
from typing import Any, Dict, Iterable, Optional, Set, Tuple, Union

import pandas as pd

from ..._version import VERSION
from ...common.exceptions import MsticpyNotConnectedError
from ...common.pkg_config import get_http_timeout
from ...common.provider_settings import ProviderSettings, get_provider_settings
from ..core.query_defns import DataEnvironment
from ..core.query_source import QuerySource

__version__ = VERSION
__author__ = "Ian Hellen"


class DriverProps:
    """Defined driver properties."""

    PUBLIC_ATTRS = "public_attribs"
    FORMATTERS = "formatters"
    USE_QUERY_PATHS = "use_query_paths"
    HAS_DRIVER_QUERIES = "has_driver_queries"
    EFFECTIVE_ENV = "effective_environment"
    SUPPORTS_THREADING = "supports_threading"
    SUPPORTS_ASYNC = "supports_async"
    MAX_PARALLEL = "max_parallel"
    FILTER_ON_CONNECT = "filter_queries_on_connect"

    PROPERTY_TYPES: Dict[str, Any] = {
        PUBLIC_ATTRS: dict,
        FORMATTERS: dict,
        USE_QUERY_PATHS: bool,
        HAS_DRIVER_QUERIES: bool,
        EFFECTIVE_ENV: (str, DataEnvironment),
        SUPPORTS_THREADING: bool,
        SUPPORTS_ASYNC: bool,
        MAX_PARALLEL: int,
        FILTER_ON_CONNECT: bool,
    }

    @classmethod
    def defaults(cls):
        """Return default values for driver properties."""
        return {
            cls.PUBLIC_ATTRS: {},
            cls.FORMATTERS: {},
            cls.USE_QUERY_PATHS: True,
            cls.HAS_DRIVER_QUERIES: False,
            cls.EFFECTIVE_ENV: None,
            cls.SUPPORTS_THREADING: False,
            cls.SUPPORTS_ASYNC: False,
            cls.MAX_PARALLEL: 4,
            cls.FILTER_ON_CONNECT: False,
        }

    @classmethod
    def valid_type(cls, property_name: str, value: Any) -> bool:
        """Return expected property type."""
        if property_name not in cls.PROPERTY_TYPES:
            return True
        return isinstance(value, cls.PROPERTY_TYPES[property_name])


# pylint: disable=too-many-instance-attributes
class DriverBase(ABC):
    """Base class for data providers."""

    def __init__(self, **kwargs):
        """Initialize new instance."""
        self._kwargs = kwargs
        self._loaded = False
        self._connected = False
        self.current_connection = None
        # self.public_attribs: Dict[str, Any] = {}
        # self.formatters: Dict[str, Callable] = {}
        # self.use_query_paths = True
        # self.has_driver_queries = False
        self._previous_connection = False
        self.data_environment = kwargs.get("data_environment")
        self._query_filter: Dict[str, Set[str]] = defaultdict(set)
        self._instance: Optional[str] = None

        self.properties = DriverProps.defaults()
        self.set_driver_property(
            name=DriverProps.EFFECTIVE_ENV,
            value=(
                self.data_environment.name
                if isinstance(self.data_environment, DataEnvironment)
                else self.data_environment or ""
            ),
        )
        self.set_driver_property(DriverProps.SUPPORTS_THREADING, False)
        self.set_driver_property(DriverProps.MAX_PARALLEL, kwargs.get("max_threads", 4))

    def __getattr__(self, attrib):
        """Return item from the properties dictionary as an attribute."""
        if attrib in self.properties:
            return self.properties[attrib]
        raise AttributeError(f"{self.__class__.__name__} has no attribute '{attrib}'")

    @property
    def loaded(self) -> bool:
        """
        Return true if the provider is loaded.

        Returns
        -------
        bool
            True if the provider is loaded.

        Notes
        -----
        This is not relevant for some providers.

        """
        return self._loaded

    @property
    def connected(self) -> bool:
        """
        Return true if at least one connection has been made.

        Returns
        -------
        bool
            True if a successful connection has been made.

        Notes
        -----
        This does not guarantee that the last data source
        connection was successful. It is a best effort to track
        whether the provider has made at least one successful
        authentication.

        """
        return self._connected

    @property
    def instance(self) -> Optional[str]:
        """
        Return instance name, if one is set.

        Returns
        -------
        Optional[str]
            The name of driver instance or None if
            the driver does not support multiple instances

        """
        return self._instance

    @property
    def schema(self) -> Dict[str, Dict]:
        """
        Return current data schema of connection.

        Returns
        -------
        Dict[str, Dict]
            Data schema of current connection.

        """
        return {}

    @abc.abstractmethod
    def connect(self, connection_str: Optional[str] = None, **kwargs):
        """
        Connect to data source.

        Parameters
        ----------
        connection_str : Optional[str]
            Connect to a data source

        """
        return None

    @abc.abstractmethod
    def query(
        self, query: str, query_source: Optional[QuerySource] = None, **kwargs
    ) -> Union[pd.DataFrame, Any]:
        """
        Execute query string and return DataFrame of results.

        Parameters
        ----------
        query : str
            The query to execute
        query_source : QuerySource
            The query definition object

        Other Parameters
        ----------------
        kwargs :
            Are passed to the underlying provider query method,
            if supported.

        Returns
        -------
        Union[pd.DataFrame, Any]
            A DataFrame (if successful) or
            the underlying provider result if an error.

        """

    @abc.abstractmethod
    def query_with_results(self, query: str, **kwargs) -> Tuple[pd.DataFrame, Any]:
        """
        Execute query string and return DataFrame plus native results.

        Parameters
        ----------
        query : str
            The query to execute

        Returns
        -------
        Tuple[pd.DataFrame,Any]
            A DataFrame and native results.

        """

    @property
    def service_queries(self) -> Tuple[Dict[str, str], str]:
        """
        Return queries retrieved from the service after connecting.

        Returns
        -------
        Tuple[Dict[str, str], str]
            Dictionary of query_name, query_text.
            Name of container to add queries to.

        """
        return {}, ""

    @property
    def driver_queries(self) -> Iterable[Dict[str, Any]]:
        """
        Return queries retrieved from the service after connecting.

        Returns
        -------
        List[Dict[str, str]]
            List of Dictionary of query_name, query_text.
            Name of container to add queries to.

        """
        return [{}]

    @property
    def query_attach_spec(self) -> Dict[str, Set[str]]:
        """Parameters that determine whether a query is relevant for the driver."""
        return self._query_filter

    def add_query_filter(self, name: str, query_filter: Union[str, Iterable]):
        """Add an expression to the query attach filter."""
        allowed_names = {"data_environments", "data_families", "data_sources"}
        if name not in allowed_names:
            raise ValueError(
                f"'name' {name} must be one of:",
                ", ".join(f"'{name}'" for name in allowed_names),
            )
        if isinstance(query_filter, str):
            self._query_filter[name].add(query_filter)
        else:
            self._query_filter[name].update(query_filter)

    def set_driver_property(self, name: str, value: Any):
        """Set an item in driver properties."""
        if not DriverProps.valid_type(name, value):
            raise TypeError(
                f"Property '{name}' is not the correct type.",
                f"Expected: '{DriverProps.PROPERTY_TYPES[name]}'.",
            )
        self.properties[name] = value

    def get_driver_property(self, name: str) -> Any:
        """Return value or KeyError from driver properties."""
        return self.properties[name]

    def query_usable(self, query_source: QuerySource) -> bool:
        """Return True if query should be exposed for this driver."""
        del query_source
        return True

    # Read values from configuration
    @staticmethod
    def _get_config_settings(prov_name) -> Dict[Any, Any]:
        """Get config from msticpyconfig."""
        data_provs = get_provider_settings(config_section="DataProviders")
        splunk_settings: Optional[ProviderSettings] = data_provs.get(prov_name)
        return getattr(splunk_settings, "args", {})

    @staticmethod
    def _create_not_connected_err(prov_name):
        return MsticpyNotConnectedError(
            "Please run the connect() method before running this method.",
            title=f"not connected to {prov_name}.",
            help_uri="https://msticpy.readthedocs.io/en/latest/DataProviders.html",
        )

    @staticmethod
    def get_http_timeout(**kwargs):
        """Get http timeout from settings or kwargs."""
        return get_http_timeout(**kwargs)
