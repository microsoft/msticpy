# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Data provider loader."""
from functools import partial
from pathlib import Path
from typing import Union, Any, List, Dict, Optional
import warnings

import pandas as pd

from .drivers import (
    DriverBase,
    KqlDriver,
    SecurityGraphDriver,
    MDATPDriver,
    LocalDataDriver,
    SplunkDriver,
)
from .query_store import QueryStore
from .query_container import QueryContainer
from .param_extractor import extract_query_params
from .query_defns import DataEnvironment
from ..common.utility import export, valid_pyname
from ..common import pkg_config as config
from .._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"

_PROVIDER_DIR = "providers"

_ENVIRONMENT_DRIVERS = {
    DataEnvironment.LogAnalytics: KqlDriver,
    DataEnvironment.AzureSecurityCenter: KqlDriver,
    DataEnvironment.SecurityGraph: SecurityGraphDriver,
    DataEnvironment.MDATP: MDATPDriver,
    DataEnvironment.LocalData: LocalDataDriver,
    DataEnvironment.Splunk: SplunkDriver,
}


@export
class QueryProvider:
    """
    Container for query store and query execution provider.

    Instances of this class hold the query set and execution
    methods for a specific data environment.

    """

    # pylint: disable=too-many-branches
    def __init__(  # noqa: MC0001
        self,
        data_environment: Union[str, DataEnvironment],
        driver: DriverBase = None,
        query_paths: List[str] = None,
        **kwargs,
    ):
        """
        Query provider interface to queries.

        Parameters
        ----------
        data_environment : Union[str, DataEnvironment]
            Name or Enum of environment for the QueryProvider
        driver : DriverBase, optional
            Override the builtin driver (query execution class)
            and use your own driver (must inherit from
            `DriverBase`)
        query_paths : List[str]
            Additional paths to look for query definitions.
        kwargs :
            Other arguments are passed to the data provider driver.

        See Also
        --------
        DataProviderBase : base class for data query providers.

        """
        if isinstance(data_environment, str):
            data_env = DataEnvironment.parse(data_environment)
            if data_env != DataEnvironment.Unknown:
                data_environment = data_env
            else:
                raise TypeError(f"Unknown data environment {data_environment}")

        self._environment = data_environment.name

        if driver is None:
            driver_class = _ENVIRONMENT_DRIVERS[data_environment]
            if issubclass(driver_class, DriverBase):
                driver = driver_class(**kwargs)  # type: ignore
            else:
                raise LookupError(
                    "Could not find suitable data provider for", f" {self._environment}"
                )

        self._query_provider = driver

        settings: Dict[str, Any] = config.settings.get(  # type: ignore
            "QueryDefinitions"
        )  # type: ignore
        all_query_paths = []
        for default_path in settings.get("Default"):  # type: ignore
            qry_path = self._resolve_path(default_path)
            if qry_path:
                all_query_paths.append(qry_path)

        if settings.get("Custom") is not None:
            for custom_path in settings.get("Custom"):  # type: ignore
                qry_path = self._resolve_path(custom_path)
                if qry_path:
                    all_query_paths.append(qry_path)
        if query_paths:
            all_query_paths.extend(query_paths)

        if not all_query_paths:
            raise RuntimeError(
                "No valid query definition files found. ",
                "Please check your msticpyconfig.yaml settings.",
            )
        data_env_queries = QueryStore.import_files(
            source_path=all_query_paths, recursive=True
        )

        if self._environment in data_env_queries:
            self._query_store = data_env_queries[self._environment]
            self.all_queries = QueryContainer()
            self._add_query_functions()
        else:
            warnings.warn(f"No queries found for environment {self._environment}")

    # pylint: disable=too-many-branches

    def __getattr__(self, name):
        """Return the value of the named property 'name'."""
        if "." in name:
            parent_name, child_name = name.split(".", maxsplit=1)
            parent = getattr(self, parent_name, None)
            if parent:
                return getattr(parent, child_name)
        raise AttributeError(f"{name} is not a valid attribute.")

    def connect(self, connection_str: str = None, **kwargs):
        """
        Connect to data source.

        Parameters
        ----------
        connection_str : str
            Connection string for the data source

        """
        # If the driver has any attributes to expose via the provider
        # add those here.
        for attr_name, attr in self._query_provider.public_attribs.items():
            setattr(self, attr_name, attr)
        self._query_provider.connect(connection_str=connection_str, **kwargs)
        dyn_queries, container = self._query_provider.service_queries
        if dyn_queries:
            self._add_service_queries(container=container, queries=dyn_queries)

    @property
    def connected(self) -> bool:
        """
        Return True if the provider is connected.

        Returns
        -------
        bool
            True if the provider is connected.

        """
        return self._query_provider.connected

    @property
    def connection_string(self) -> str:
        """
        Return provider connection string.

        Returns
        -------
        str
            Provider connection string.

        """
        return self._query_provider.current_connection

    @property
    def schema(self) -> Dict[str, Dict]:
        """
        Return current data schema of connection.

        Returns
        -------
        Dict[str, Dict]
            Data schema of current connection.

        """
        return self._query_provider.schema

    @property
    def schema_tables(self) -> List[str]:
        """
        Return list of tables in the data schema of the connection.

        Returns
        -------
        List[str]
            Tables in the of current connection.

        """
        return list(self._query_provider.schema.keys())

    def import_query_file(self, query_file: str):
        """
        Import a yaml data source definition.

        Parameters
        ----------
        query_file : str
            Path to the file to import

        """
        self._query_store.import_file(query_file)
        self._add_query_functions()

    @classmethod
    def list_data_environments(cls) -> List[str]:
        """
        Return list of current data environments.

        Returns
        -------
        List[str]
            List of current data environments

        """
        return [env for env in DataEnvironment.__members__ if env != "Unknown"]

    def list_queries(self) -> List[str]:
        """
        Return list of family.query in the store.

        Returns
        -------
        Iterable[str]
            List of queries

        """
        return list(self._query_store.query_names)

    def query_help(self, query_name):
        """Print help for query."""
        self._query_store[query_name].help()

    def get_query(self, query_name) -> str:
        """Return the raw query text."""
        return self._query_store[query_name].query

    def exec_query(self, query: str, **kwargs) -> Union[pd.DataFrame, Any]:
        """
        Execute simple query string.

        Parameters
        ----------
        query : str
            [description]

        Returns
        -------
        Union[pd.DataFrame, Any]
            Query results - a DataFrame if successful
            or a KqlResult if unsuccessful.

        """
        query_options = kwargs.pop("query_options", {}) or kwargs
        return self._query_provider.query(query, **query_options)

    def _execute_query(self, *args, **kwargs) -> Union[pd.DataFrame, Any]:
        if not self._query_provider.loaded:
            raise ValueError("Provider is not loaded.")
        if not self._query_provider.connected:
            raise ValueError(
                "No connection to a data source.",
                "Please call connect(connection_str) and retry.",
            )
        query_name = kwargs.pop("query_name")
        family = kwargs.pop("query_path")

        query_source = self._query_store.get_query(
            query_path=family, query_name=query_name
        )
        if "help" in args or "?" in args:
            query_source.help()
            return None

        params, missing = extract_query_params(query_source, *args, **kwargs)
        if missing:
            query_source.help()
            raise ValueError(f"No values found for these parameters: {missing}")

        param_formatters = self._query_provider.formatters
        query_str = query_source.create_query(formatters=param_formatters, **params)
        if "print" in args or "query" in args:
            return query_str

        # Handle any query options passed
        query_options = kwargs.pop("query_options", {})
        if not query_options:
            # Any kwargs left over we send to the query provider driver
            query_options = {
                key: val for key, val in kwargs.items() if key not in params
            }
        return self._query_provider.query(query_str, query_source, **query_options)

    def _add_query_functions(self):
        """Add queries to the module as callable methods."""
        for qual_query_name in self.list_queries():
            query_path = qual_query_name.split(".")
            query_name = query_path[-1]
            current_node = self
            for container_name in query_path[:-1]:
                container_name = valid_pyname(container_name)
                if hasattr(current_node, container_name):
                    current_node = getattr(current_node, container_name)
                else:
                    new_node = QueryContainer()
                    setattr(current_node, container_name, new_node)
                    current_node = new_node

            query_cont_name = ".".join(query_path[:-1])

            # Create the partial function
            query_func = partial(
                self._execute_query, query_path=query_cont_name, query_name=query_name
            )
            query_func.__doc__ = self._query_store.get_query(
                query_path=query_cont_name, query_name=query_name
            ).create_doc_string()

            query_name = valid_pyname(query_name)
            setattr(current_node, query_name, query_func)
            setattr(self.all_queries, query_name, query_func)

    def _add_service_queries(
        self, container: Union[str, List[str]], queries: Dict[str, str]
    ):
        """Add additional queries to the query store."""
        for q_name, q_text in queries.items():
            self._query_store.add_query(
                name=q_name, query=q_text, query_paths=container
            )

        # For now, just add all of the functions again (with any connect-time acquired
        # queries) - we could be more efficient than this but unless there are 1000s of
        # queries it should not be noticable.
        self._add_query_functions()

    @classmethod
    def _resolve_path(cls, config_path: str) -> Optional[str]:
        if not Path(config_path).is_absolute():
            config_path = str(Path(__file__).resolve().parent.joinpath(config_path))
        if not Path(config_path).is_dir():
            warnings.warn(f"Custom query definitions path {config_path} not found")
            return None
        return config_path
