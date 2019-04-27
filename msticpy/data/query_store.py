# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Intake kql driver."""
from typing import Tuple, Dict, Iterable, List, Set
import glob
from os import path
from collections import defaultdict

import yaml

from ..nbtools.query_defns import DataFamily, DataEnvironment
from .._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"


def read_query_def_file(query_file: str) -> Tuple[Dict, Dict, Dict]:
    """
    Read a yaml data query definition file.

    Parameters
    ----------
    query_file : str
        Path to yaml query defintion file

    Returns
    -------
    Tuple[Dict, Dict, Dict]
        Tuple of dictionaries.
        sources - dictionary of query definitions
        defaults - the default parameters from the file
        metadata - the global metadata from the file

    """
    data_map = None
    with open(query_file) as f_handle:
        # use safe_load instead load
        data_map = yaml.safe_load(f_handle)

    if 'sources' not in data_map:
        raise ValueError('Imported file has no sources defined')

    defaults = data_map.get('defaults', {})
    sources = data_map.get('sources', {})
    metadata = data_map.get('metadata', {})

    return sources, defaults, metadata


def _get_dot_path(elem_path: str, data_map: dict) -> str:
    path_elems = elem_path.split('.')
    cur_node = data_map
    for elem in path_elems:
        cur_node = cur_node.get(elem, None)
        if cur_node is None:
            raise KeyError(f'{elem} value of {path} is not a valid path')
    return cur_node


def _read_yaml_files(source_path: str,
                     recursive: bool = False) -> Iterable[str]:
    for file_path in glob.glob(source_path, recursive=recursive):
        if not path.isfile(file_path):
            continue
        _, ext = path.splitext(file_path)
        if ext.lower() != '.yaml':
            continue
        yield file_path


class QueryStore:
    """
    Repository for query definitions for a data environment.

    Attributes
    ----------
    environment: DataEnvironment
        The data environment for the queries.
    data_families: Dict[DataFamily, Dict[str, QuerySource]]
        The set of data families and associated queries
        for each.

    """

    def __init__(self, environment: DataEnvironment):
        """
        Intialize a QueryStore for a new environment.

        Parameters
        ----------
        environment : DataEnvironment
            The data environment

        """
        self.environment = environment  # DataEnvironment
        self.data_families = defaultdict(dict)  # Dict[DataFamily, Dict[str, 'QuerySource']

    def __getattr__(self, name: str):
        """Return the item in dot-separated path `name`."""
        return _get_dot_path(elem_path=name, data_map=self.data_families)

    @property
    def query_names(self) -> Iterable[str]:
        """
        Return list of family.query in the store.

        Returns
        -------
        Iterable[str]
            List of queries

        """
        for family in sorted(self.data_families):
            yield [f'{family}.{query}' for query
                   in sorted(self.data_families[family].keys())]

    def add_data_source(self, source: 'QuerySource'):
        """
        Add a datasource/query to the store.

        Parameters
        ----------
        source : QuerySource
            The source to add. An existing item with
            the same name will be overwritten

        """
        source.query_store = self
        for family in source.data_families:
            self.data_families[family][source.name] = source

    def import_file(self, query_file: str):
        """
        Import a yaml data source definition.

        Parameters
        ----------
        query_file : str
            Path to the file to import

        """
        sources, defaults, metadata = read_query_def_file(query_file)

        if str(self.environment) not in metadata.get('data_environments', []):
            raise ValueError(f'Data environment {self.environment} ',
                             'not in file environments: ',
                             f'{metadata.get("data_environments", [])}')

        for source_name, source in sources.items():
            new_source = QuerySource(source_name, source,
                                     defaults, metadata)
            self.add_data_source(new_source)

    @classmethod
    def import_files(cls,
                     source_path: str,
                     recursive: bool = False) -> Dict[DataEnvironment, 'QueryStore']:
        """
        Import multiple query definition files from directory path.

        Parameters
        ----------
        source_path : str
            The folder containing the yaml definition files.
        recursive : bool, optional
            True to recurse sub-directories
            (the default is False, which only reads from the top level)

        Returns
        -------
        Dict[DataEnvironment, 'QueryStore']
            Dictionary of one or more environments and the
            QueryStore containing the queries for each environment.

        """
        if not path.isdir(source_path):
            raise ValueError(f'{source_path} is not a directory')

        env_stores = dict()
        for file in _read_yaml_files(source_path, recursive):
            sources, defaults, metadata = read_query_def_file(file)
            if 'data_environments' not in metadata:
                raise ValueError(f'"data_environments" value not found in metadata',
                                 f'section of {file}')
            for env_value in metadata['data_environments']:
                if '.' in env_value:
                    env_value = env_value.split('.')[1]
                environment = DataEnvironment.parse(env_value)
                if not environment:
                    raise ValueError(f'Unknown environment {env_value}')

                if environment not in env_stores:
                    env_stores[environment] = cls(environment=environment)
                for source_name, source in sources.items():
                    new_source = QuerySource(source_name, source,
                                             defaults, metadata)
                    env_stores[environment].add_data_source(new_source)

        return env_stores

    def find_query(self, query_name: str) -> Set['QuerySource']:
        """
        Return set of queries with name `query_name`.

        Parameters
        ----------
        query_name: str
            Name of the query

        Returns
        -------
        Set[QuerySource]
            Set (distinct) queries matching name.

        """
        return (query_dict.get(query_name) for family, query_dict
                in self.data_families.items()
                if query_name in query_dict)


class QuerySource:
    """
    Query definition class for templated queries.

    Attributes
    ----------
    name: str
        The query name
    metadata: Dict[str, Any]
        The consolidated metadata for the query
    params: dict[str, Any]
        The dictionary of parameter definitions for the query.
    query_store: QueryStore
        The query store object that the query belongs to

    """

    def __init__(self,
                 name: str,
                 source: dict,
                 defaults: dict,
                 metadata: dict):
        """
        Initialize query source definition.

        Parameters
        ----------
        name : str
            The query name
        source : dict
            The data source definition settings
        defaults : dict
            The default settings (if source-specific setting
            not supplied)
        metadata : dict
            The global metadata from the source file.

        Notes
        -----
        A data source can belong to multiple families (e.g. a query
        that joins data from several sources)

        """
        self.name = name
        self._source = source
        self._defaults = defaults
        self._global_metadata = dict(metadata)
        self.query_store = None  # Optional[QueryStore]

        # consolidate source metadata - source-specifc
        # overrides global
        self.metadata = (self._global_metadata
                         .update(self._defaults.get('metadata', {})))
        self.metadata.update(self._source.get('metadata', {}))
        self._query = self['args.query']

        # take parameters from the default
        self.params = self._defaults.get('parameters', {})
        self.params.update(self._source.get('parameters', {}))

    def __getitem__(self, key: str):
        """
        Getitem override - allows access to properties via dotted notation.

        Parameters
        ----------
        key : str
            The hiearchical path to the property (e.g. `source.description`)

        """
        path_elems = key.split('.')
        cur_node = self._source
        for elem in path_elems:
            cur_node = cur_node.get(elem, None)
            if cur_node is None:
                raise KeyError(f'{elem} value of {key} is not a valid path')
        return cur_node

    @property
    def description(self) -> str:
        """
        Return description of the query.

        Returns
        -------
        str
            Query description.

        """
        return self['description']

    @property
    def query(self) -> str:
        """
        Return the query template.

        Returns
        -------
        str
            The template query.

        """
        return self['args.query']

    @property
    def default_params(self) -> Dict[str, dict]:
        """
        Return the set of parameters with default values.

        Returns
        -------
        Iterable[dict]
            List of parameters

        """
        return {p_key: p_props for p_key, p_props in self.params.items()
                if 'default' in p_props}

    @property
    def required_params(self) -> Dict[str, dict]:
        """
        Return the set of parameters with no default values.

        Returns
        -------
        Iterable[dict]
            List of parameters

        """
        return {p_key: p_props for p_key, p_props in self.params.items()
                if 'default' not in p_props}

    @property
    def data_families(self) -> List[DataFamily]:
        """
        Return the list of data families used by the query.

        Returns
        -------
        List[DataFamily]
            The list of data families. A data family is
            usually equivalent to a table or entity set.

        """
        return self.metadata['data_families']

    def create_query(self, **kwargs) -> str:
        """
        Return query with values from kwargs and defaults substituted.

        Parameters
        ----------
        kwargs: Mapping[str, Any]
            Set of parameter name, value pairs used to
            populate the template query.

        Returns
        -------
        str
            The populated query

        Raises
        ------
        ValueError
            If one or more parameters with no default values
            are not supplied.

        Notes
        -----
        Parameters supplied as arguments will override any
        parameter defaults (see `default_params` property).

        """
        param_dict = {name: value.get('default', None)
                      for name, value in self.params.items()}
        param_dict.update(kwargs)
        missing_params = {name: value
                          for name, value in param_dict.items()
                          if value is None}
        if missing_params:
            raise ValueError('These required parameters were not set: ',
                             f'{missing_params.keys()}')
        return self._query.format(**param_dict)

    def help(self):
        """Print help for query."""
        print('Query: ', self.name)
        if self.query_store is not None:
            print('Data source: ', self.query_store.environment)
        print(self.create_doc_string())
        print('Query:')
        print(self.query)

    def create_doc_string(self) -> str:
        """
        Return a doc string for the query.

        Returns
        -------
        str
            New-line delimited docstring dynamically
            created from query definition properties.

        """
        param_block = ['Parameters', '----------']
        for p_name, p_props in sorted(self.params):
            if 'default' in p_props:
                optional = ' (optional)'
                def_value = p_props['default']
                if len(def_value) > 50:
                    def_value = def_value[:50] + '...'
            else:
                optional = ''
                def_value = None
            param_block.append(f'{p_name}: {p_props["type"]}{optional}')
            param_block.append(f'    {p_props.get("description", "no description")}')
            if def_value:
                param_block.append(f'    {def_value}')
        doc_string = [f'{self.description}', '']
        return '\n'.join(doc_string + param_block)
