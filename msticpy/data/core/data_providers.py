# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Data provider loader."""
import re
from collections import abc
from datetime import datetime
from functools import partial
from itertools import tee
from pathlib import Path
from typing import Any, Dict, Iterable, List, NamedTuple, Optional, Pattern, Union

import pandas as pd
from tqdm.auto import tqdm

from ..._version import VERSION
from ...common import pkg_config as config
from ...common.exceptions import MsticpyDataQueryError
from ...common.utility import export, valid_pyname
from ...nbwidgets import QueryTime
from ..drivers import DriverBase, import_driver
from .param_extractor import extract_query_params
from .query_container import QueryContainer
from .query_defns import DataEnvironment
from .query_source import QuerySource
from .query_store import QueryStore

__version__ = VERSION
__author__ = "Ian Hellen"

_HELP_FLAGS = ("help", "?")
_DEBUG_FLAGS = ("print", "debug_query", "print_query")
_COMPATIBLE_DRIVER_MAPPINGS = {"mssentinel": ["m365d"], "mde": ["m365d"]}


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


@export
class QueryProvider:
    """
    Container for query store and query execution provider.

    Instances of this class hold the query set and execution
    methods for a specific data environment.

    """

    create_param = QueryParam

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
        # import at runtime to prevent circular import
        # pylint: disable=import-outside-toplevel, cyclic-import
        from ...init.pivot_init.pivot_data_queries import add_data_queries_to_entities

        # pylint: enable=import-outside-toplevel
        setattr(self.__class__, "_add_pivots", add_data_queries_to_entities)

        if isinstance(data_environment, str):
            data_env = DataEnvironment.parse(data_environment)
            if data_env != DataEnvironment.Unknown:
                data_environment = data_env
            else:
                raise TypeError(f"Unknown data environment {data_environment}")
        self.environment = data_environment.name

        self._driver_kwargs = kwargs.copy()
        if driver is None:
            self.driver_class = import_driver(data_environment)
            if issubclass(self.driver_class, DriverBase):
                driver = self.driver_class(data_environment=data_environment, **kwargs)
            else:
                raise LookupError(
                    "Could not find suitable data provider for", f" {self.environment}"
                )
        else:
            self.driver_class = driver.__class__
        self._additional_connections: Dict[str, DriverBase] = {}
        self._query_provider = driver
        self.all_queries = QueryContainer()

        # Add any query files
        data_env_queries: Dict[str, QueryStore] = {}
        if driver.use_query_paths:
            data_env_queries.update(
                self._read_queries_from_paths(query_paths=query_paths)
            )
        self.query_store = data_env_queries.get(
            self.environment, QueryStore(self.environment)
        )
        self._add_query_functions()
        self._query_time = QueryTime(units="day")

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
        self._query_provider.connect(connection_str=connection_str, **kwargs)

        # If the driver has any attributes to expose via the provider
        # add those here.
        for attr_name, attr in self._query_provider.public_attribs.items():
            setattr(self, attr_name, attr)

        # Add any built-in or dynamically retrieved queries from driver
        if self._query_provider.has_driver_queries:
            driver_queries = self._query_provider.driver_queries
            self._add_driver_queries(queries=driver_queries)
        # Since we're now connected, add Pivot functions
        self._add_pivots(lambda: self._query_time.timespan)

    def add_connection(
        self,
        connection_str: Optional[str] = None,
        alias: Optional[str] = None,
        **kwargs,
    ):
        """
        Add an additional connection for the query provider.

        Parameters
        ----------
        connection_str : Optional[str], optional
            Connection string for the provider, by default None
        alias : Optional[str], optional
            Alias to use for the connection, by default None

        Other Parameters
        ----------------
        kwargs : Dict[str, Any]
            Other parameters passed to the driver constructor.

        Notes
        -----
        Some drivers may accept types other than strings for the
        `connection_str` parameter.

        """
        # create a new instance of the driver class
        new_driver = self.driver_class(**(self._driver_kwargs))
        # connect
        new_driver.connect(connection_str=connection_str, **kwargs)
        # add to collection
        driver_key = alias or str(len(self._additional_connections))
        self._additional_connections[driver_key] = new_driver

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
        return [env for env in DataEnvironment.__members__ if env != "Unknown"]
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

    def list_connections(self) -> List[str]:
        """
        Return a list of current connections or the default connection.

        Returns
        -------
        List[str]
            The alias and connection string for each connection.

        """
        add_connections = [
            f"{alias}: {driver.current_connection}"
            for alias, driver in self._additional_connections.items()
        ]
        return [f"Default: {self._query_provider.current_connection}", *add_connections]

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

    def exec_query(self, query: str, **kwargs) -> Union[pd.DataFrame, Any]:
        """
        Execute simple query string.

        Parameters
        ----------
        query : str
            [description]
        use_connections : Union[str, List[str]]

        Other Parameters
        ----------------
        query_options : Dict[str, Any]
            Additional options passed to query driver.
        kwargs : Dict[str, Any]
            Additional options passed to query driver.

        Returns
        -------
        Union[pd.DataFrame, Any]
            Query results - a DataFrame if successful
            or a KqlResult if unsuccessful.

        """
        query_options = kwargs.pop("query_options", {}) or kwargs
        query_source = kwargs.pop("query_source", None)
        result = self._query_provider.query(
            query, query_source=query_source, **query_options
        )
        if not self._additional_connections:
            return result
        # run query against all connections
        results = [result]
        print(f"Running query for {len(self._additional_connections)} connections.")
        for con_name, connection in self._additional_connections.items():
            print(f"{con_name}...")
            try:
                results.append(
                    connection.query(query, query_source=query_source, **query_options)
                )
            except MsticpyDataQueryError:
                print(f"Query {con_name} failed.")
        return pd.concat(results)

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
        # pylint: disable=import-outside-toplevel
        from ...vis.query_browser import browse_queries

        return browse_queries(self, **kwargs)

    # alias for browse_queries
    browse = browse_queries

    @property
    def query_time(self):
        """Return the default QueryTime control for queries."""
        return self._query_time

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
        >>> qp_host = qp.create_paramramram("host_name", "str", "Name of Host")
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

    def _execute_query(self, *args, **kwargs) -> Union[pd.DataFrame, Any]:
        if not self._query_provider.loaded:
            raise ValueError("Provider is not loaded.")
        if (
            not self._query_provider.connected
            and not _help_flag(*args)
            and not _debug_flag(*args, **kwargs)
        ):
            raise ValueError(
                "No connection to a data source.",
                "Please call connect(connection_str) and retry.",
            )
        query_name = kwargs.pop("query_name")
        family = kwargs.pop("query_path")

        query_source = self.query_store.get_query(
            query_path=family, query_name=query_name
        )
        if _help_flag(*args):
            query_source.help()
            return None

        params, missing = extract_query_params(query_source, *args, **kwargs)
        self._check_for_time_params(params, missing)
        if missing:
            query_source.help()
            raise ValueError(f"No values found for these parameters: {missing}")

        split_by = kwargs.pop("split_query_by", None)
        if split_by:
            split_result = self._exec_split_query(
                split_by=split_by,
                query_source=query_source,
                query_params=params,
                args=args,
                **kwargs,
            )
            if split_result is not None:
                return split_result
            # if split queries could not be created, fall back to default
        query_str = query_source.create_query(
            formatters=self._query_provider.formatters, **params
        )
        # This looks for any of the "print query" debug args in args or kwargs
        if _debug_flag(*args, **kwargs):
            return query_str

        # Handle any query options passed
        query_options = _get_query_options(params, kwargs)
        return self.exec_query(query_str, query_source=query_source, **query_options)

    def _check_for_time_params(self, params, missing):
        """Fall back on builtin query time if no time parameters were supplied."""
        if "start" in missing:
            missing.remove("start")
            params["start"] = self._query_time.start
        if "end" in missing:
            missing.remove("end")
            params["end"] = self._query_time.end

    def _get_query_folder_for_env(self, root_path: str, environment: str) -> List[Path]:
        """Return query folder for current environment."""
        data_envs = [environment]
        if environment.casefold() in _COMPATIBLE_DRIVER_MAPPINGS:
            data_envs += _COMPATIBLE_DRIVER_MAPPINGS[environment.casefold()]
        return [
            _resolve_package_path(root_path).joinpath(data_env.casefold())
            for data_env in data_envs
        ]

    def _read_queries_from_paths(self, query_paths) -> Dict[str, QueryStore]:
        """Fetch queries from YAML files in specified paths."""
        settings: Dict[str, Any] = config.settings.get(  # type: ignore
            "QueryDefinitions"
        )  # type: ignore
        all_query_paths: List[Union[Path, str]] = []
        for def_qry_path in settings.get("Default"):  # type: ignore
            # only read queries from environment folder
            builtin_qry_paths = self._get_query_folder_for_env(
                def_qry_path, self.environment
            )
            all_query_paths.extend(
                str(qry_path) for qry_path in builtin_qry_paths if qry_path.is_dir()
            )

        if settings.get("Custom") is not None:
            for custom_path in settings.get("Custom"):  # type: ignore
                custom_qry_path = _resolve_path(custom_path)
                if custom_qry_path:
                    all_query_paths.append(custom_qry_path)
        if query_paths:
            for param_path in query_paths:
                param_qry_path = _resolve_path(param_path)
                if param_qry_path:
                    all_query_paths.append(param_qry_path)
        if all_query_paths:
            return QueryStore.import_files(
                source_path=all_query_paths,
                recursive=True,
                driver_query_filter=self._query_provider.query_attach_spec,
            )
        # if no queries - just return an empty store
        return {self.environment: QueryStore(self.environment)}

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
            query_func.__doc__ = self.query_store.get_query(
                query_path=query_cont_name, query_name=query_name
            ).create_doc_string()

            query_name = valid_pyname(query_name)
            setattr(current_node, query_name, query_func)
            setattr(self.all_queries, query_name, query_func)

    def _add_driver_queries(self, queries: Iterable[Dict[str, str]]):
        """Add driver queries to the query store."""
        for query in queries:
            self.query_store.add_query(
                name=query["name"],
                query=query["query"],
                query_paths=query["query_container"],
                description=query["description"],
            )
        # For now, just add all of the functions again (with any connect-time acquired
        # queries) - we could be more efficient than this but unless there are 1000s of
        # queries it should not be noticeable.
        self._add_query_functions()

    def _exec_split_query(
        self,
        split_by: str,
        query_source: QuerySource,
        query_params: Dict[str, Any],
        args,
        **kwargs,
    ) -> Union[pd.DataFrame, str, None]:
        start = query_params.pop("start", None)
        end = query_params.pop("end", None)
        if not (start or end):
            print(
                "Cannot split a query that does not have 'start' and 'end' parameters"
            )
            return None
        try:
            split_delta = pd.Timedelta(split_by)
        except ValueError:
            split_delta = pd.Timedelta("1D")

        ranges = _calc_split_ranges(start, end, split_delta)

        split_queries = [
            query_source.create_query(
                formatters=self._query_provider.formatters,
                start=q_start,
                end=q_end,
                **query_params,
            )
            for q_start, q_end in ranges
        ]
        # This looks for any of the "print query" debug args in args or kwargs
        if _debug_flag(*args, **kwargs):
            return "\n\n".join(split_queries)

        # Retrieve any query options passed (other than query params)
        # and send to query function.
        query_options = _get_query_options(query_params, kwargs)
        query_dfs = [
            self.exec_query(query_str, query_source=query_source, **query_options)
            for query_str in tqdm(split_queries, unit="sub-queries", desc="Running")
        ]

        return pd.concat(query_dfs)


def _calc_split_ranges(start: datetime, end: datetime, split_delta: pd.Timedelta):
    """Return a list of time ranges split by `split_delta`."""
    # Use pandas date_range and split the result into 2 iterables
    s_ranges, e_ranges = tee(pd.date_range(start, end, freq=split_delta))
    next(e_ranges, None)  # skip to the next item in the 2nd iterable
    # Zip them together to get a list of (start, end) tuples of ranges
    # Note: we subtract 1 nanosecond from the 'end' value of each range so
    # to avoid getting duplicated records at the boundaries of the ranges.
    # Some providers don't have nanosecond granularity so we might
    # get duplicates in these cases
    ranges = [
        (s_time, e_time - pd.Timedelta("1ns"))
        for s_time, e_time in zip(s_ranges, e_ranges)
    ]

    # Since the generated time ranges are based on deltas from 'start'
    # we need to adjust the end time on the final range.
    # If the difference between the calculated last range end and
    # the query 'end' that the user requested is small (< 10% of a delta),
    # we just replace the last "end" time with our query end time.
    if (ranges[-1][1] - end) < (split_delta / 10):
        ranges[-1] = ranges[-1][0], end
    else:
        # otherwise append a new range starting after the last range
        # in ranges and ending in 'end"
        # note - we need to add back our subtracted 1 nanosecond
        ranges.append((ranges[-1][0] + pd.Timedelta("1ns"), end))
    return ranges


def _resolve_package_path(config_path: str) -> Path:
    """Resolve path relative to current package."""
    return (
        Path(config_path)
        if Path(config_path).is_absolute()
        else Path(__file__).resolve().parent.parent.joinpath(config_path)
    )


def _resolve_path(config_path: str) -> Optional[str]:
    """Resolve path."""
    if not Path(config_path).is_absolute():
        config_path = str(Path(config_path).expanduser().resolve())
    if not Path(config_path).is_dir():
        print(f"Warning: Custom query definitions path {config_path} not found")
        return None
    return config_path


def _help_flag(*args) -> bool:
    """Return True if help parameter passed."""
    return any(help_flag for help_flag in _HELP_FLAGS if help_flag in args)


def _debug_flag(*args, **kwargs) -> bool:
    """Return True if debug/print args passed."""
    return any(db_arg for db_arg in _DEBUG_FLAGS if db_arg in args) or any(
        db_arg for db_arg in _DEBUG_FLAGS if kwargs.get(db_arg, False)
    )


def _get_query_options(
    params: Dict[str, Any], kwargs: Dict[str, Any]
) -> Dict[str, Any]:
    # sourcery skip: inline-immediately-returned-variable, use-or-for-fallback
    """Return any kwargs not already in params."""
    query_options = kwargs.pop("query_options", {})
    if not query_options:
        # Any kwargs left over we send to the query provider driver
        query_options = {key: val for key, val in kwargs.items() if key not in params}
    return query_options


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
