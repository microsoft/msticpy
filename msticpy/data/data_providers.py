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

from .drivers import DriverBase, KqlDriver, SecurityGraphDriver
from .query_store import QueryStore
from .param_extractor import extract_query_params
from ..nbtools.query_defns import DataEnvironment
from ..nbtools.utility import export
from ..nbtools import pkg_config as config
from .._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"

_PROVIDER_DIR = "providers"

_ENVIRONMENT_DRIVERS = {
    DataEnvironment.LogAnalytics: KqlDriver,
    DataEnvironment.AzureSecurityCenter: KqlDriver,
    DataEnvironment.SecurityGraph: SecurityGraphDriver,
}


class AttribHolder:
    """Empty class used to create hierarchical attributes."""

    def __len__(self):
        """Return number of items in the attribute collection."""
        return len(self.__dict__)

    def __iter__(self):
        """Return iterator over the attributes."""
        return iter(self.__dict__.items())


@export
class QueryProvider:
    """
    Container for query store and query execution provider.

    Instances of this class hold the query set and execution
    methods for a specific data environment.

    """

    def __init__(  # noqa: MC001
        self, data_environment: Union[str, DataEnvironment], driver: DriverBase = None
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
        query_path: str, optional
            Override the default location for query files to import
            a custom set of query definitions at initialization.

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
                driver = driver_class()  # type: ignore
            else:
                raise LookupError(
                    "Could not find suitable data provider for",
                    f" {data_environment.name}",
                )

        self._query_provider = driver

        settings: Dict[str, Any] = config.settings.get(  # type: ignore
            "QueryDefinitions"
        )  # type: ignore
        query_paths = []
        for default_path in settings.get("Default"):  # type: ignore
            qry_path = self._resolve_path(default_path)
            if qry_path:
                query_paths.append(qry_path)

        if settings.get("Custom") is not None:
            for custom_path in settings.get("Custom"):  # type: ignore
                qry_path = self._resolve_path(custom_path)
                if qry_path:
                    query_paths.append(qry_path)

        if not query_paths:
            raise RuntimeError(
                "No valid query definition files found. "
                + "Please check your msticpyconfig.yaml settings."
            )
        data_environments = QueryStore.import_files(
            source_path=query_paths, recursive=True
        )

        self._query_store = data_environments[data_environment.name]
        self.all_queries = AttribHolder()
        self._add_query_functions()

    def __getattr__(self, name):
        """Return the value of the named property 'name'."""
        if "." in name:
            parent_name, child_name = name.split(".", maxsplit=1)
            parent = getattr(self, parent_name, None)
            if parent:
                return getattr(parent, child_name)
        raise AttributeError(f"{name} is not a valid attribute.")

    def connect(self, connection_str: str, **kwargs):
        """
        Connect to data source.

        Parameters
        ----------
        connection_string : str
            Connection string for the data source

        """
        return self._query_provider.connect(connection_str=connection_str, **kwargs)

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

    def exec_query(self, query: str) -> Union[pd.DataFrame, Any]:
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
        return self._query_provider.query(query)

    def _execute_query(self, *args, **kwargs) -> Union[pd.DataFrame, Any]:
        if not self._query_provider.loaded:
            raise ValueError("Provider is not loaded.")
        if not self._query_provider.connected:
            raise ValueError(
                "No connection to a data source.",
                "Please call connect(connection_str) and retry.",
            )
        query_name = kwargs.pop("query_name")
        family = kwargs.pop("data_family")

        query_source = self._query_store.get_query(
            data_family=family, query_name=query_name
        )
        if "help" in args or "?" in args:
            query_source.help()
            return None

        params, missing = extract_query_params(query_source, *args, **kwargs)
        if missing:
            query_source.help()
            raise ValueError(f"No values found for these parameters: {missing}")

        query_str = query_source.create_query(**params)
        if "print" in args or "query" in args:
            return query_str
        return self._query_provider.query(query_str)

    def _add_query_functions(self):
        """Add queries to the module as callable methods."""
        for qual_query_name in self.list_queries():

            family, query_name = qual_query_name.split(".")
            if not hasattr(self, family):
                setattr(self, family, AttribHolder())
            query_family = getattr(self, family)

            # Create the partial function
            query_func = partial(
                self._execute_query, data_family=family, query_name=query_name
            )
            query_func.__doc__ = self._query_store.get_query(
                data_family=family, query_name=query_name
            ).create_doc_string()

            setattr(query_family, query_name, query_func)
            setattr(self.all_queries, query_name, query_func)

    @classmethod
    def _resolve_path(cls, config_path: str) -> Optional[str]:
        if not Path(config_path).is_absolute():
            config_path = str(Path(__file__).resolve().parent.joinpath(config_path))
        if not Path(config_path).is_dir():
            warnings.warn(f"Custom query definitions path {config_path} not found")
            return None
        return config_path
