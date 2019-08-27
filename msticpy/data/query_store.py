# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""QueryStore class - holds a collection of QuerySources."""
from collections import defaultdict
from os import path
from typing import Any, Dict, Iterable, Set, Union, Optional

from .._version import VERSION
from ..nbtools.query_defns import DataEnvironment, DataFamily
from .data_query_reader import find_yaml_files, read_query_def_file
from .query_source import QuerySource

__version__ = VERSION
__author__ = "Ian Hellen"


def _get_dot_path(elem_path: str, data_map: dict) -> Any:
    path_elems = elem_path.split(".")
    cur_node = data_map
    for elem in path_elems:
        cur_node = cur_node.get(elem, None)
        if cur_node is None:
            raise KeyError(f"{elem} value of {path} is not a valid path")
    return cur_node


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
        self.environment: str = environment
        self.data_families: Dict[str, Dict[str, QuerySource]] = defaultdict(dict)
        self.data_family_defaults: Dict[str, Dict[str, Any]] = defaultdict(dict)

    def __getattr__(self, name: str):
        """Return the item in dot-separated path `name`."""
        return _get_dot_path(elem_path=name, data_map=self.data_families)

    def __getitem__(self, key: str):
        """Allow query retrieval using dotted key path."""
        return _get_dot_path(elem_path=key, data_map=self.data_families)

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
            if "." in family:
                family = family.split(".")[1]

            for q_name in [
                f"{family}.{query}"
                for query in sorted(self.data_families[family].keys())
            ]:
                yield q_name

    def add_data_source(self, source: QuerySource):
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
            # we want to update any new defaults for the data family
            self.data_family_defaults[family].update(source.defaults)

            # we also replace the defaults dict in each source with
            # a reference to the data family defaults so that as
            # new files are read in defaults are shared by all sources.
            source.defaults = self.data_family_defaults[family]

            valid, failures = source.validate()
            if not valid:
                raise ImportError(source.name, failures)

    def import_file(self, query_file: str):
        """
        Import a yaml data source definition.

        Parameters
        ----------
        query_file : str
            Path to the file to import

        Raises
        ------
        ImportError
            File read error or Syntax or semantic error found in
            the source file.

        """
        sources, defaults, metadata = read_query_def_file(query_file)

        for source_name, source in sources.items():
            new_source = QuerySource(source_name, source, defaults, metadata)
            self.add_data_source(new_source)

    @classmethod
    def import_files(
        cls, source_path: list, recursive: bool = False
    ) -> Dict[str, "QueryStore"]:
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

        Raises
        ------
        ImportError
            File read error or Syntax or semantic error found in
            a source file.

        """
        env_stores: Dict[str, QueryStore] = dict()
        for query_dir in source_path:
            if not path.isdir(query_dir):
                raise ImportError(f"{query_dir} is not a directory")
            for file_path in find_yaml_files(query_dir, recursive):
                sources, defaults, metadata = read_query_def_file(str(file_path))

                for env_value in metadata["data_environments"]:
                    if "." in env_value:
                        env_value = env_value.split(".")[1]
                    environment = DataEnvironment.parse(env_value)
                    if environment == DataEnvironment.Unknown:
                        raise ValueError(f"Unknown environment {env_value}")

                    if environment.name not in env_stores:
                        env_stores[environment.name] = cls(environment=environment.name)
                    for source_name, source in sources.items():
                        new_source = QuerySource(
                            source_name, source, defaults, metadata
                        )
                        env_stores[environment.name].add_data_source(new_source)

        return env_stores

    def get_query(
        self, query_name: str, data_family: Union[str, DataFamily] = None
    ) -> "QuerySource":
        """
        Return query with name `data_family` and `query_name`.

        Parameters
        ----------
        query_name: str
            Name of the query
        data_family: Union[str, DataFamily]
            The data family for the query

        Returns
        -------
        QuerySource
            Query matching name and family.

        """
        if isinstance(query_name, str) and "." in query_name:
            data_family, query_name = query_name.split(".", maxsplit=1)
        if not data_family:
            raise LookupError("No data family specified.")
        if isinstance(data_family, DataFamily):
            data_family = data_family.name
        return self.data_families[data_family][query_name]

    def find_query(self, query_name: str) -> Set[Optional[QuerySource]]:
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
        return {
            query_dict.get(query_name)
            for family, query_dict in self.data_families.items()
            if query_name in query_dict
        }
