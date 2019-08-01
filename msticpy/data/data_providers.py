# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Data provider loader."""
from functools import partial
from pathlib import Path
from typing import Union, Any, List

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
        """Retrun number of items in the attribute collection."""
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

    def __init__(
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
            if data_env:
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

        settings = config.settings.get("QueryDefinitions")
        query_paths = []
        for default_path in settings.get("Default"):
            query_paths.append(Path(__file__).resolve().parent.joinpath(default_path))

        if settings.get("Custom") is not None:
            for custom_path in settings.get("Custom"):
                query_paths.append(
                    Path(__file__).resolve().parent.joinpath(custom_path)
                )

        data_environments = QueryStore.import_files(
            source_path=query_paths, recursive=True
        )

        self._query_store = data_environments[data_environment.name]
        self.all_queries = AttribHolder()
        self._add_query_functions()

    def connect(self, connection_str: str, **kwargs):
        """
        Connect to data source.

        Parameters
        ----------
        connection_string : str
            Connection string for the data source

        """
        return self._query_provider.connect(connection_str=connection_str, **kwargs)

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
        return list(DataEnvironment.__members__)

    def list_queries(self):
        """
        Return list of family.query in the store.

        Returns
        -------
        Iterable[str]
            List of queries

        """
        return self._query_store.query_names

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
        if "print" in args:
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
                family, query_name
            ).create_doc_string()

            setattr(query_family, query_name, query_func)
            setattr(self.all_queries, query_name, query_func)
