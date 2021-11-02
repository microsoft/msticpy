# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Data provider loader."""
from datetime import datetime
from functools import partial
from itertools import tee
from pathlib import Path
from typing import Any, Dict, Iterable, List, Optional, Union

import pandas as pd
from tqdm.auto import tqdm

from .._version import VERSION
from ..common import pkg_config as config
from ..common.utility import export, valid_pyname
from ..nbtools.nbwidgets import QueryTime
from .browsers.query_browser import browse_queries
from .drivers import import_driver, DriverBase
from .param_extractor import extract_query_params
from .query_container import QueryContainer
from .query_defns import DataEnvironment
from .query_source import QuerySource
from .query_store import QueryStore

__version__ = VERSION
__author__ = "Ian Hellen"

_DB_QUERY_FLAGS = ("print", "debug_query", "print_query")


@export
class QueryProvider:
    """
    Container for query store and query execution provider.

    Instances of this class hold the query set and execution
    methods for a specific data environment.

    """

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

        self.environment = data_environment.name

        if driver is None:
            driver_class = import_driver(data_environment)
            if issubclass(driver_class, DriverBase):
                driver = driver_class(**kwargs)  # type: ignore
            else:
                raise LookupError(
                    "Could not find suitable data provider for", f" {self.environment}"
                )

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

    def list_queries(self) -> List[str]:
        """
        Return list of family.query in the store.

        Returns
        -------
        Iterable[str]
            List of queries

        """
        return list(self.query_store.query_names)

    def query_help(self, query_name):
        """Print help for query."""
        self.query_store[query_name].help()

    def get_query(self, query_name) -> str:
        """Return the raw query text."""
        return self.query_store[query_name].query

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
        return browse_queries(self, **kwargs)

    # alias for browse_queries
    browse = browse_queries

    @property
    def query_time(self):
        """Return the default QueryTime control for queries."""
        return self._query_time

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

        query_source = self.query_store.get_query(
            query_path=family, query_name=query_name
        )
        if "help" in args or "?" in args:
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
        if any(db_arg for db_arg in _DB_QUERY_FLAGS if db_arg in args) or any(
            db_arg for db_arg in _DB_QUERY_FLAGS if kwargs.get(db_arg, False)
        ):
            return query_str

        # Handle any query options passed
        query_options = self._get_query_options(params, kwargs)
        return self._query_provider.query(query_str, query_source, **query_options)

    def _check_for_time_params(self, params, missing):
        """Fall back on builtin query time if no time parameters were supplied."""
        if "start" in missing:
            missing.remove("start")
            params["start"] = self._query_time.start
        if "end" in missing:
            missing.remove("end")
            params["end"] = self._query_time.end

    @staticmethod
    def _get_query_options(
        params: Dict[str, Any], kwargs: Dict[str, Any]
    ) -> Dict[str, Any]:
        """Return any kwargs not already in params."""
        query_options = kwargs.pop("query_options", {})
        if not query_options:
            # Any kwargs left over we send to the query provider driver
            query_options = {
                key: val for key, val in kwargs.items() if key not in params
            }
        return query_options

    def _read_queries_from_paths(self, query_paths) -> Dict[str, QueryStore]:
        """Fetch queries from YAML files in specified paths."""
        settings: Dict[str, Any] = config.settings.get(  # type: ignore
            "QueryDefinitions"
        )  # type: ignore
        all_query_paths = []
        for default_path in settings.get("Default"):  # type: ignore
            qry_path = self._resolve_package_path(default_path)
            if qry_path:
                all_query_paths.append(qry_path)

        if settings.get("Custom") is not None:
            for custom_path in settings.get("Custom"):  # type: ignore
                qry_path = self._resolve_path(custom_path)
                if qry_path:
                    all_query_paths.append(qry_path)
        if query_paths:
            for custom_path in query_paths:
                qry_path = self._resolve_path(custom_path)
                if qry_path:
                    all_query_paths.append(qry_path)

        if not all_query_paths:
            raise RuntimeError(
                "No valid query definition files found. ",
                "Please check your msticpyconfig.yaml settings.",
            )
        return QueryStore.import_files(source_path=all_query_paths, recursive=True)

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

        ranges = self._calc_split_ranges(start, end, split_delta)

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
        if any(db_arg for db_arg in _DB_QUERY_FLAGS if db_arg in args) or any(
            db_arg for db_arg in _DB_QUERY_FLAGS if kwargs.get(db_arg, False)
        ):
            return "\n\n".join(split_queries)

        # Retrive any query options passed (other than query params)
        # and send to query function.
        query_options = self._get_query_options(query_params, kwargs)
        query_dfs = [
            self._query_provider.query(query_str, query_source, **query_options)
            for query_str in tqdm(split_queries, unit="sub-queries", desc="Running")
        ]

        return pd.concat(query_dfs)

    @staticmethod
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

    @classmethod
    def _resolve_package_path(cls, config_path: str) -> Optional[str]:
        """Resolve path relative to current package."""
        if not Path(config_path).is_absolute():
            config_path = str(Path(__file__).resolve().parent.joinpath(config_path))
        if not Path(config_path).is_dir():
            print(f"Warning: Custom query definitions path {config_path} not found")
            return None
        return config_path

    @classmethod
    def _resolve_path(cls, config_path: str) -> Optional[str]:
        """Resolve path."""
        if not Path(config_path).is_absolute():
            config_path = str(Path(config_path).expanduser().resolve())
        if not Path(config_path).is_dir():
            print(f"Warning: Custom query definitions path {config_path} not found")
            return None
        return config_path
