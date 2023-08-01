# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Local Osquery Data Driver class - osquery.{results,snapshots}.log."""
import json
import logging

# pickle used for caching data. However, it is not the default. The
# user would have to specify or configure a cache file to be read.
import pickle  # nosec
import re
from collections import defaultdict
from pathlib import Path
from typing import Any, Dict, List, Optional, Union

import pandas as pd
from pandas import to_datetime, to_numeric
from tqdm.auto import tqdm

from ..._version import VERSION
from ...common.exceptions import MsticpyDataQueryError, MsticpyNoDataSourceError
from ...common.provider_settings import get_provider_settings
from ...common.utility import export, valid_pyname
from .driver_base import DriverBase, QuerySource

__version__ = VERSION
__author__ = "juju4"

logger = logging.getLogger(__name__)


# pylint: disable=too-many-instance-attributes
@export
class OSQueryLogDriver(DriverBase):
    """OSQueryLogDriver class to execute kql queries."""

    OS_QUERY_DATEIME_COLS = {
        "unixTime",
        "columns_time",
        "columns_atime",
        "columns_ctime",
        "columns_mtime",
    }

    def __init__(self, connection_str: str = None, **kwargs):
        """
        Instantiate OSQueryLogDriver and optionally connect.

        Parameters
        ----------
        connection_str : str, optional
            Connection string (not used)
        data_paths : List[str], optional
            Paths from which to load data files
        cache_file : str, optional
            Store extracted data to `cache_file` path, or
            read from this file, if it exists.
        progress : bool, optional
            Show progress with tqdm, by default, True

        """
        del connection_str
        self._debug = kwargs.get("debug", False)
        super().__init__()
        self._cache_file: Optional[str] = None
        self._paths: List[str] = ["."]
        # If data paths specified, use these
        # from kwargs or settings
        if data_paths := kwargs.get("data_paths"):
            self._paths = [path.strip() for path in data_paths]
            logger.info("data paths read from param %s", str(self._paths))
        else:
            prov_settings = get_provider_settings(config_section="DataProviders").get(
                "OSQueryLogs"
            )
            if prov_settings:
                self._paths = prov_settings.args.get("data_paths", []) or self._paths
                self._cache_file = prov_settings.args.get("cache_file")
                logger.info("data paths read from config %s", str(self._paths))

        self._progress = kwargs.pop("progress", True)
        self.data_files: Dict[str, str] = self._get_logfile_paths()
        self._schema: Dict[str, Any] = {}
        self._data_cache: Dict[str, pd.DataFrame] = {}
        self._query_map: Dict[str, List[str]]
        self._cache_file = kwargs.pop("cache_file", self._cache_file)
        self._loaded = True
        self.has_driver_queries = True
        logger.info("data files to read %s", ",".join(self.data_files.values()))
        logger.info("cache file %s", self._cache_file)

    def _get_logfile_paths(self) -> Dict[str, str]:
        """Read files in data paths."""
        data_files = {}
        for input_path in (Path(path_str) for path_str in self._paths):
            if input_path.is_file():
                data_files[str(input_path.name).casefold()] = str(input_path)
                continue
            if input_path.is_dir():
                for pattern in ["**/*.log"]:
                    found_files = Path(input_path).resolve().glob(pattern)
                    data_files.update(
                        {
                            str(file_path.name).casefold(): str(file_path)
                            for file_path in found_files
                            if file_path.is_file()
                        }
                    )
        return data_files

    def connect(self, connection_str: Optional[str] = None, **kwargs):
        """
        Connect to data source.

        Parameters
        ----------
        connection_str : str
            Connect to a data source

        """
        del connection_str
        self._read_data_files()
        self._connected = True
        print("Data loaded.")

    # pylint: disable=too-many-branches
    @property
    def schema(self) -> Dict[str, Dict]:
        """
        Return current data schema of connection.

        Returns
        -------
        Dict[str, Dict]
            Data schema of current connection.

        """
        if not self._schema:
            self.connect()
            self._schema = {
                table: {col: dtype.name for col, dtype in df.dtypes.to_dict().items()}
                for table, df in self._data_cache.items()
            }

        return self._schema

    def query(
        self, query: str, query_source: QuerySource = None, **kwargs
    ) -> Union[pd.DataFrame, Any]:
        """
        Execute query string and return DataFrame of results.

        Parameters
        ----------
        query : str
            The query to execute
        query_source : QuerySource
            The query definition object

        Returns
        -------
        Union[pd.DataFrame, results.ResultSet]
            A DataFrame (if successful) or
            the underlying provider result if an error.

        """
        del kwargs
        if not self._data_cache:
            self.connect()
        query_name = query_source.name if query_source else query
        if query_name not in self._query_map:
            raise MsticpyDataQueryError(
                f"No data loaded for query {query_name}.",
                "Please check that the data loaded from the log paths",
                "has this data type.",
            )
        df_names = self._query_map[query_name]
        query_df = pd.concat([self._data_cache[df] for df in df_names])
        for date_column in self.OS_QUERY_DATEIME_COLS & set(query_df.columns):
            query_df[date_column] = to_datetime(
                query_df[date_column], unit="s", origin="unix"
            )
        logger.info("Query %s, returned %d rows", query_name, len(query_df))
        return query_df

    def query_with_results(self, query, **kwargs):
        """Return query with fake results."""
        return self.query(query, **kwargs), "OK"

    @property
    def driver_queries(self) -> List[Dict[str, Any]]:
        """
        Return dynamic queries available on connection to data.

        Returns
        -------
        List[Dict[str, Any]]
            List of queries with properties: "name", "query", "container"
            and (optionally) "description"

        Raises
        ------
        MsticpyNotConnectedError
            If called before driver is connected.

        """
        if not self.connected:
            self.connect()
        if self._query_map:
            return [
                {
                    "name": query,
                    "query": query,
                    "query_paths": "osquery",
                    "description": f"OS Query {query} table.",
                }
                for query in self._query_map
            ]
        return []

    def _read_data_files(self):
        """Read data files into memory."""
        if self._data_cache:
            return
        # If a cache file is specified and is a valid file,
        # try to use that.
        if self._cache_file and Path(self._cache_file).is_file():
            self._read_from_cache()
            logger.info("data read from cache %s", self._cache_file)
            if self._data_cache:
                return

        # Otherwise read in the data files.
        data_files = (
            tqdm(self.data_files.values())
            if self._progress
            else self.data_files.values()
        )
        for log_file in data_files:
            self._read_log_file(log_file)

        # if a cache file name was specified, write data cache to that file.
        if self._cache_file:
            self._write_to_cache()
            logger.info("data written to  %s", self._cache_file)

    def _write_to_cache(self):
        if Path(self._cache_file).is_file():
            print("Overwriting exist cache file.")
        with open(self._cache_file, "wb") as cache_handle:
            try:
                pickle.dump(self._data_cache, cache_handle)
            except pickle.PickleError:
                print(f"Could not write to cache file {self._data_cache}")

    def _read_from_cache(self):
        with open(self._cache_file, "rb") as cache_handle:
            try:
                self._data_cache = pickle.load(cache_handle)  # nosec
            except pickle.PickleError:
                print(f"Invalid cache file {self._data_cache}")

    def _extract_event_type(self, df_all_queries: pd.DataFrame, event_name: str):
        """Extract tidied DF from all queries."""
        query_df = df_all_queries[df_all_queries["name"] == event_name].dropna(
            axis=1, how="all"
        )
        for date_column in self.OS_QUERY_DATEIME_COLS & set(query_df.columns):
            query_df[date_column] = to_datetime(
                to_numeric(query_df[date_column]),
                origin="unix",
                utc=True,
            )
        return query_df

    def _read_log_file(self, log_path: str):
        """Read log file into cache."""
        if Path(log_path).suffix.casefold() not in (".log", ".json"):
            return
        # We can't use directly read_json as we need to extract nested json
        # Split log files in multiple queries table here
        # Likely resource intensive and better way to do.
        # Likely issue, multiple log files can contain same query mostly
        # because of log rotation
        list_lines: List[Dict[str, Any]] = []
        try:
            with open(log_path, mode="r", encoding="utf-8") as logfile:
                json_lines = logfile.readlines()
                list_lines = [json.loads(line) for line in json_lines]

        except (IOError, json.JSONDecodeError, ValueError) as exc:
            raise MsticpyDataQueryError(
                f"Read error on file {log_path}: {exc}."
            ) from exc
        if not list_lines:
            raise MsticpyNoDataSourceError(
                f"No log data retrieved from {log_path}",
                "Please check the format of the log file.",
            )
        logger.info("log %s read, %d lines read", log_path, len(list_lines))
        df_all_queries = pd.json_normalize(list_lines, max_level=3)
        # Don't want dot in columns name
        df_all_queries.columns = df_all_queries.columns.str.replace(
            ".", "_", regex=False
        )

        for event_name in df_all_queries["name"].unique().tolist():
            combined_dfs = []
            # we might already have events of this type in the cache
            if event_name in self._data_cache:
                # if so, we combine with newly-extracted data
                combined_dfs.append(self._data_cache[event_name])
            combined_dfs.append(self._extract_event_type(df_all_queries, event_name))
            # update the cache
            self._data_cache[event_name] = _rename_columns(pd.concat(combined_dfs))
            logger.info(
                "event type %s read to _data_cache, len: %d",
                event_name,
                len(self._data_cache[event_name]),
            )

        self._query_map = self._get_table_names()

    _OSQ_TABLE_REGEX = r"(?:pack_[^_]+_)?(?P<table_name>[\w_]+)"

    def _get_table_names(self):
        """Extract simple table names from the data_cache entries."""
        table_map = defaultdict(list)
        for name in self._data_cache:
            match = re.search(self._OSQ_TABLE_REGEX, name, re.IGNORECASE)
            table_name = match.groupdict().get("table_name") if match else name
            table_name = valid_pyname(table_name.strip())
            table_map[table_name].append(name)
        return table_map


_PREFIXES = ("decorations_", "columns_")


def _rename_columns(data: pd.DataFrame):
    """Rename nested columns."""
    df_cols = set(data.columns)
    rename_cols = {}
    for prefix in _PREFIXES:
        source_cols = data.filter(regex=f"{prefix}.*").columns
        rename_cols.update({col: col.replace(prefix, "") for col in source_cols})
    rename_cols = {
        col: ren_col if ren_col not in df_cols else f"{ren_col}_"
        for col, ren_col in rename_cols.items()
    }
    return data.rename(columns=rename_cols)
