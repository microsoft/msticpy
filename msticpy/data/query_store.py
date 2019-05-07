# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Intake kql driver."""
from datetime import datetime, timedelta
from numbers import Number
from typing import Tuple, Dict, Iterable, List, Set, Union
import glob
from os import path
import re
from collections import defaultdict, ChainMap

import yaml

from .. nbtools.query_defns import DataFamily, DataEnvironment
from .._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"



def _value_or_default(src_dict: Dict, prop_name: str, default: Dict):
    """Return value from dict or emtpy dict."""
    src_value = src_dict.get(prop_name)
    return src_value if src_value is not None else default


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

    try:
        validate_query_defs(query_def_dict=data_map)
    except ValueError as valid_error:
        # If the yaml file is not a valid query definition file
        # skip it
        print(f'Error found in input file {query_file}')
        print(valid_error.args)
        raise

    defaults = data_map.get('defaults', {})
    sources = data_map.get('sources', {})
    metadata = data_map.get('metadata', {})

    return sources, defaults, metadata


def validate_query_defs(query_def_dict: dict) -> bool:

    # verify that sources and metadata are in the data dict
    if 'sources' not in query_def_dict or not query_def_dict['sources']:
        raise ValueError('Imported file has no sources defined')
    if 'metadata' not in query_def_dict or not query_def_dict['metadata']:
        raise ValueError('Imported file has no metadata defined')

    # data_environments and data_families must be defined at with at least
    # one value
    if ('data_environments' not in query_def_dict['metadata']
            or not query_def_dict['metadata']['data_environments']):
        raise ValueError('Imported file has no data_environments defined')

    for env in query_def_dict['metadata']['data_environments']:
        if not DataEnvironment.parse(env):
            raise ValueError(f'Unknown data evironment {env} in metadata. ',
                             'Valid values are\n',
                             ', '.join([e.name for e in DataEnvironment]))
    if ('data_families' not in query_def_dict['metadata']
            or not query_def_dict['metadata']['data_families']):
        raise ValueError('Imported file has no data families defined')

    for fam in query_def_dict['metadata']['data_families']:
        if not DataFamily.parse(fam):
            raise ValueError(f'Unknown data family {fam} in metadata. ',
                             'Valid values are\n',
                             ', '.join([f.name for f in DataFamily]))

    # For each source we need to verify that it has the required members
    req_source_items = {'driver', 'args'}
    param_pattern = r'{([^}]+)}'
    defaults_elem = query_def_dict.get('defaults', {})
    if defaults_elem is None:
        default_params = set()
    else:
        # if defaults item is defined, get the default parameters
        # definitions
        default_params = defaults_elem.get('parameters', {})
        if default_params is None:
            default_params = set()
        else:
            default_params = default_params.keys()
        # if driver is defined in defaults, we don't need it in the source
        if defaults_elem.get('driver'):
            req_source_items.remove('driver')

    # iterate through the query/source definitions
    for query, queryd in query_def_dict['sources'].items():
        # Need req_source_items AND query item to be present
        if not req_source_items.issubset(set(queryd.keys())):
            raise ValueError(f'Source {query} does not have all required ',
                             f'elements: {req_source_items}')
        if 'query' not in queryd['args'] or not queryd['args']['query']:
            raise ValueError(f'Source {query} does not have "query" property ',
                             'in args element.')

        # Now get the query and the parameter definitions from the source and
        # check that every parameter specified in the query has a corresponding
        # 'parameter definition in either the source or the defaults.
        query_str = queryd['args']['query']
        source_params = queryd.get('parameters', {})
        if source_params is None:
            source_params = set()
        else:
            source_params = source_params.keys()

        q_params = set(re.findall(param_pattern, query_str))

        missing_params = q_params - default_params - source_params
        if missing_params:
            raise ValueError(f'Source {query} has parameters that are defined in ',
                             'the query but not included in either defaults or ',
                             'query-specific parameters element(s)\n',
                             f'Missing parameters are {missing_params}')


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
    file_glob = f'{source_path}/*.yaml'
    for file_path in glob.glob(file_glob, recursive=recursive):
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
    environment: str
        The data environment for the queries.
    data_families: Dict[str, Dict[str, QuerySource]]
        The set of data families and associated queries
        for each.

    """

    def __init__(self, environment: str):
        """
        Intialize a QueryStore for a new environment.

        Parameters
        ----------
        environment : str
            The data environment

        """
        self.environment = environment  # str
        self.data_families = defaultdict(dict)  # Dict[str, Dict[str, 'QuerySource']

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
            if '.' in family:
                family = family.split('.')[1]

            for q_name in [f'{family}.{query}' for query
                           in sorted(self.data_families[family].keys())]:
                yield q_name

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
                     recursive: bool = False) -> Dict[str, 'QueryStore']:
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
        Dict[str, 'QueryStore']
            Dictionary of one or more environments and the
            QueryStore containing the queries for each environment.

        """
        if not path.isdir(source_path):
            raise ValueError(f'{source_path} is not a directory')

        env_stores = dict()
        for file in _read_yaml_files(source_path, recursive):
            try:
                sources, defaults, metadata = read_query_def_file(file)
            except ValueError:
                # If the yaml file is not a valid query definition file
                # skip it
                continue

            if 'data_environments' not in metadata:
                continue

            for env_value in metadata['data_environments']:
                if '.' in env_value:
                    env_value = env_value.split('.')[1]
                environment = DataEnvironment.parse(env_value)
                if not environment:
                    raise ValueError(f'Unknown environment {env_value}')

                if environment.name not in env_stores:
                    env_stores[environment.name] = cls(environment=environment.name)
                for source_name, source in sources.items():
                    new_source = QuerySource(source_name, source,
                                             defaults, metadata)
                    env_stores[environment.name].add_data_source(new_source)

        return env_stores

    def get_query(self,
                  data_family: Union[str, DataFamily],
                  query_name: str) -> 'QuerySource':
        """
        Return query with name `data_family` and `query_name`.

        Parameters
        ----------
        data_family: Union[str, DataFamily]
            The data family for the query
        query_name: str
            Name of the query

        Returns
        -------
        QuerySource
            Query matching name and family.

        """
        if isinstance(data_family, str) and '.' not in data_family:
            data_family = DataFamily.parse(data_family)
        return self.data_families[data_family.name][query_name]

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
        self._global_metadata = dict(metadata) if metadata else dict()
        self.query_store = None  # Optional[QueryStore]

        # consolidate source metadata - source-specifc
        # overrides global
        # add an empty dict in case neither has defined params
        self.metadata = ChainMap(_value_or_default(self._source, 'metadata', {}),
                                 _value_or_default(self._defaults, 'metadata', {}),
                                 self._global_metadata)
        # make ChainMap for parameters from with source
        # higher priority than default
        # add an empty dict in case neither has defined params
        self.params = ChainMap(_value_or_default(self._source, 'parameters', {}),
                               _value_or_default(self._defaults, 'parameters', {}))

        self._query = self['args.query']

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
    def data_families(self) -> List[str]:
        """
        Return the list of data families used by the query.

        Returns
        -------
        List[str]
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

        for p_name, settings in self.params.items():
            # special case of datetime specified as a number - we
            # interpret this as an offset from utcnow
            if (settings['type'] == 'datetime'
                    and isinstance(param_dict[p_name], Number)):
                if param_dict[p_name] < 0:
                    param_dict[p_name] = datetime.utcnow() - timedelta(abs(param_dict[p_name]))
                else:
                    param_dict[p_name] = datetime.utcnow() + timedelta(abs(param_dict[p_name]))
            # if the parameter requires custom formatting
            fmt_template = settings.get('format', None)
            if fmt_template:
                param_dict[p_name] = fmt_template.format(param_dict[p_name])
            elif (settings['type'] == 'datetime'
                  and isinstance(param_dict[p_name], datetime)):
                # If this is a datetime and no specific formattin requested,
                # format as a isoformat (Odata requires strings with no spaces)
                param_dict[p_name] = param_dict[p_name].isoformat(sep='T')

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
        for p_name, p_props in sorted(self.params.items()):
            if 'default' in p_props:
                optional = ' (optional)'
                def_value = p_props['default']
                if isinstance(def_value, str) and len(def_value) > 50:
                    def_value = def_value[:50] + '...'
            else:
                optional = ''
                def_value = None
            param_block.append(f'{p_name}: {p_props.get("type", "Any")}{optional}')
            param_block.append(f'    {p_props.get("description", "no description")}')
            if def_value:
                param_block.append(f'    (default value is: {def_value})')
        doc_string = [f'{self.description}', '']
        return '\n'.join(doc_string + param_block)
