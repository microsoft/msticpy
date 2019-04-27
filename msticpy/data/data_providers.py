# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Data provider loader."""
from functools import partial
from os import path
from typing import Union

from . providers import DataProviderBase, KqlDriver, SecurityGraphDriver
from . query_store import QueryStore
from . param_extractor import extract_query_params
from .. nbtools.query_defns import DataEnvironment
from .. nbtools.utility import export
from .. _version import VERSION

__version__ = VERSION
__author__ = 'Ian Hellen'

_PROVIDER_DIR = 'providers'
_QUERY_DEF_DIR = 'queries'

_ENVIRONMENT_PROVIDERS = {
    DataEnvironment.LogAnalytics: KqlDriver,
    DataEnvironment.AzureSecurityCenter: KqlDriver,
    DataEnvironment.SecurityGraph: SecurityGraphDriver
}


class AttribHolder:
    """Empty class used to create hierarchical attributes."""

    pass


@export
class QueryProvider:
    """
    Container for query store and query execution provider.

    Instances of this class hold the query set and execution
    methods for a specific data environment.

    """

    def __init__(self,
                 data_environment: Union[str, DataEnvironment],
                 provider: DataProviderBase = None):
        """
        Query provider interface to queries.

        Parameters
        ----------
        data_environment : Union[str, DataEnvironment]
            Name or Enum of environment for the QueryProvider
        provider : DataProviderBase, optional
            Override the builtin provider (query execution class)
            and use your own provider (must inherit from
            `DataProviderBase`)

        See Also
        --------
        DataProviderBase : base class for data query providers.

        """
        if isinstance(data_environment, str):
            data_environment = DataEnvironment.parse(data_environment)

        self._environment = data_environment

        if provider is None:
            provider_class = _ENVIRONMENT_PROVIDERS[data_environment]
            if issubclass(provider_class, DataProviderBase):
                provider = provider_class()
            else:
                raise LookupError('Could not find suitable data provider for',
                                  f' {data_environment}')

        self._query_provider = provider

        # Load data query definitions for environment
        data_environments = QueryStore.import_files(source_path=path, recursive=True)
        self._query_store = data_environments[data_environment]

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

    def list_queries(self):
        """
        Return list of family.query in the store.

        Returns
        -------
        Iterable[str]
            List of queries

        """
        return self._query_store.query_names()

    def query_help(self, query_name):
        """Print help for query."""
        self._query_store[query_name].help()

    def _execute_query(self, *args, **kwargs):
        if not self._query_provider.loaded:
            raise ValueError('Provider is not loaded.')
        if not self._query_provider.connected:
            raise ValueError('No connection to a data source.',
                             'Please call connect(connection_str) and retry.')
        query_name = kwargs.pop('query_name')

        query_source = self._query_store[query_name]
        params, missing = extract_query_params(query_source, *args, *kwargs)

        if missing:
            raise ValueError(f'No values found for these parameters: {missing}')

        query_str = query_source.create_query(**params)
        return self._query_provider.query(query_str)

    def _add_query_functions(self):
        """Add queries to the module as callable methods."""
        for qual_query_name in self.list_queries():

            family, query_name = qual_query_name.split('.')
            if not hasattr(self, family):
                setattr(self, family, AttribHolder())
            query_family = getattr(self, family)

            # Create the partial function
            query_func = partial(self._execute_query, query_name=query_name)
            query_func.__doc__ = self._query_store[query_name].create_doc_string()

            setattr(query_family, query_name, query_func)
            setattr(self.all_queries, query_name, query_func)
