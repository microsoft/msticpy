# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Query Provider mixin methods."""
import re
from collections import abc
from typing import Dict, Iterable, List, NamedTuple, Optional, Pattern, Protocol, Union

from ..._version import VERSION
from ...common.utility.package import lazy_import
from ..drivers.driver_base import DriverBase
from .query_defns import DataEnvironment
from .query_source import QuerySource
from .query_store import QueryStore

__version__ = VERSION
__author__ = "Ian Hellen"

query_browser = lazy_import("msticpy.vis.query_browser", "browse_queries")


# pylint: disable=too-few-public-methods
class QueryProviderProtocol(Protocol):
    """Protocol for required properties of QueryProvider class."""

    _query_provider: DriverBase
    query_store: QueryStore

    def _add_query_functions(self):
        pass


class QueryParam(NamedTuple):
    """
    Named tuple for custom query parameters.

    name and data_type are mandatory.
    description and default are optional.

    """

    name: str
    data_type: str
    description: Optional[str] = None
    default: Optional[str] = None


# pylint: disable=super-init-not-called
class QueryProviderUtilsMixin(QueryProviderProtocol):
    """Mixin utility methods for QueryProvider class."""

    create_param = QueryParam

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

    @property
    def instance(self) -> Optional[str]:
        """
        Return instance name, if any for provider.

        Returns
        -------
        Optional[str]
            The instance name or None for drivers that do not
            support multiple instances.

        """
        return self._query_provider.instance

    def import_query_file(self, query_file: str):
        """
        Import a yaml data source definition.

        Parameters
        ----------
        query_file : str
            Path to the file to import

        """
        self.query_store.import_file(query_file)
        self._add_query_functions()

    def driver_help(self):
        """Display help for the driver."""
        print(self._query_provider.__doc__)

    @classmethod
    def list_data_environments(cls) -> List[str]:
        """
        Return list of current data environments.

        Returns
        -------
        List[str]
            List of current data environments

        """
        # pylint: disable=not-an-iterable
        return [
            de
            for de in dir(DataEnvironment)
            if de != "Unknown" and not de.startswith("_")
        ]
        # pylint: enable=not-an-iterable

    def list_queries(self, substring: Optional[str] = None) -> List[str]:
        """
        Return list of family.query in the store.

        Parameters
        ----------
        substring : Optional[str]
            Optional pattern - will return only queries matching the pattern,
            default None.

        Returns
        -------
        List[str]
            List of queries

        """
        if substring:
            return list(
                filter(
                    lambda x: substring in x.lower(),  # type: ignore
                    self.query_store.query_names,
                )
            )
        return list(self.query_store.query_names)

    def search(
        self,
        search: Union[str, Iterable[str]] = None,
        table: Union[str, Iterable[str]] = None,
        param: Union[str, Iterable[str]] = None,
        ignore_case: bool = True,
    ) -> List[str]:
        """
        Search queries for match properties.

        Parameters
        ----------
        search : Union[str, Iterable[str]], optional
            String or iterable of search terms to match on
            any property of the query, by default None.
            The properties include: name, description, table,
            parameter names and query_text.
        table : Union[str, Iterable[str]], optional
            String or iterable of search terms to match on
            the query table name, by default None
        param : Union[str, Iterable[str]], optional
            String or iterable of search terms to match on
            the query parameter names, by default None
        ignore_case : bool
            Use case-insensitive search, default is True.

        Returns
        -------
        List[str]
            A list of matched queries

        Notes
        -----
        Search terms are treated as regular expressions.
        Supplying multiple parameters returns the intersection
        of matches for each category. For example:
        `qry_prov.search(search="account", table="syslog")` will
        match queries that have a table parameter of "syslog" AND
        have the term "Account" somewhere in the query properties.

        """
        if not (search or table or param):
            return []

        glob_searches = _normalize_to_regex(search, ignore_case)
        table_searches = _normalize_to_regex(table, ignore_case)
        param_searches = _normalize_to_regex(param, ignore_case)
        search_hits: List[str] = []
        for query, search_data in self.query_store.search_items.items():
            glob_match = (not glob_searches) or any(
                re.search(term, prop)
                for term in glob_searches
                for prop in search_data.values()
            )
            table_match = (not table_searches) or any(
                re.search(term, search_data["table"]) for term in table_searches
            )
            param_match = (not param_searches) or any(
                re.search(term, search_data["params"]) for term in param_searches
            )
            if glob_match and table_match and param_match:
                search_hits.append(query)
        return sorted(search_hits)

    def query_help(self, query_name: str):
        """
        Print help for `query_name`.

        Parameters
        ----------
        query_name : str
            The name of the query.

        """
        self.query_store[query_name].help()

    def get_query(self, query_name: str) -> str:
        """
        Return the raw query text for `query_name`.

        Parameters
        ----------
        query_name : str
            The name of the query.

        """
        return self.query_store[query_name].query

    def browse_queries(self, **kwargs):
        """
        Return QueryProvider query browser.

        Other Parameters
        ----------------
        kwargs :
            passed to SelectItem constructor.

        Returns
        -------
        SelectItem
            SelectItem browser for TI Data.

        """
        return query_browser()(self, **kwargs)

    # alias for browse_queries
    browse = browse_queries

    def add_custom_query(
        self,
        name: str,
        query: str,
        family: Union[str, Iterable[str]],
        description: Optional[str] = None,
        parameters: Optional[Iterable[QueryParam]] = None,
    ):
        """
        Add a custom function to the provider.

        Parameters
        ----------
        name : str
            The name of the query.
        query : str
            The query text (optionally parameterized).
        family : Union[str, Iterable[str]]
            The query group/family or list of families. The query will
            be added to attributes of the query provider with these
            names.
        description : Optional[str], optional
            Optional description (for query help), by default None
        parameters : Optional[Iterable[QueryParam]], optional
            Optional list of parameter definitions, by default None.
            If the query is parameterized you must supply definitions
            for the parameters here - at least name and type.
            Parameters can be the named tuple QueryParam (also
            exposed as QueryProvider.Param) or a 4-value

        Examples
        --------
        >>> qp = QueryProvider("MSSentinel")
        >>> qp_host = qp.create_param("host_name", "str", "Name of Host")
        >>> qp_start = qp.create_param("start", "datetime")
        >>> qp_end = qp.create_param("end", "datetime")
        >>> qp_evt = qp.create_param("event_id", "int", None, 4688)
        >>>
        >>> query = '''
        >>> SecurityEvent
        >>> | where EventID == {event_id}
        >>> | where TimeGenerated between (datetime({start}) .. datetime({end}))
        >>> | where Computer has "{host_name}"
        >>> '''
        >>>
        >>> qp.add_custom_query(
        >>>     name="test_host_proc",
        >>>     query=query,
        >>>     family="Custom",
        >>>     parameters=[qp_host, qp_start, qp_end, qp_evt]
        >>> )

        """
        if parameters:
            param_dict = {
                param[0]: {
                    "type": param[1],
                    "default": param[2],
                    "description": param[3],
                }
                for param in parameters
            }
        else:
            param_dict = {}
        source = {
            "args": {"query": query},
            "description": description,
            "parameters": param_dict,
        }
        metadata = {"data_families": [family] if isinstance(family, str) else family}
        query_source = QuerySource(
            name=name, source=source, defaults={}, metadata=metadata
        )
        self.query_store.add_data_source(query_source)
        self._add_query_functions()


def _normalize_to_regex(
    search_term: Union[str, Iterable[str], None], ignore_case: bool
) -> List[Pattern[str]]:
    """Return iterable or str search term as list of compiled reg expressions."""
    if not search_term:
        return []
    regex_opts = [re.IGNORECASE] if ignore_case else []
    if isinstance(search_term, str):
        return [re.compile(search_term, *regex_opts)]
    if isinstance(search_term, abc.Iterable):
        return [re.compile(term, *regex_opts) for term in set(search_term)]
    return []
