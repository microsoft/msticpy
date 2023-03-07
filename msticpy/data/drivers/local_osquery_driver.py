# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Local Osquery Data Driver class - osquery.{results,snapshots}.log."""
import json
import pickle
from pathlib import Path
from typing import Any, Dict, List, Optional, Union

import pandas as pd
from pandas import to_datetime
from tqdm.auto import tqdm

from ..._version import VERSION
from ...common.exceptions import MsticpyDataQueryError, MsticpyNoDataSourceError
from ...common.provider_settings import get_provider_settings
from ...common.utility import export
from .driver_base import DriverBase, QuerySource

__version__ = VERSION
__author__ = "juju4"


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

        self._paths: List[str] = ["."]
        # If data paths specified, use these
        # from kwargs or settings
        if data_paths := kwargs.get("data_paths"):
            self._paths = [path.strip() for path in data_paths]
        else:
            prov_settings = get_provider_settings(config_section="DataProviders").get(
                "OSQueryLogs"
            )
            if prov_settings:
                self._paths = prov_settings.args.get("data_paths", []) or self._paths
                self._cache_file = prov_settings.args.get("cache_file")

        self._progress = kwargs.pop("progress", True)
        self.data_files: Dict[str, str] = self._get_data_paths()
        self._schema: Dict[str, Any] = {}
        self._data_cache: Dict[str, pd.DataFrame] = {}
        self._cache_file = kwargs.pop("cache_file", self._cache_file)
        self._loaded = True

    def _get_data_paths(self) -> Dict[str, str]:
        """Read files in data paths."""
        data_files = {}
        for path in self._paths:
            for pattern in ["**/*.log"]:
                found_files = Path(path).resolve().glob(pattern)
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
        if query_name in self._data_cache:
            query_df = self._data_cache[query_name]
            for date_column in self.OS_QUERY_DATEIME_COLS | set(query_df.columns):
                query_df[date_column] = to_datetime(
                    query_df[date_column], unit="s", origin="unix"
                )
            return query_df
        raise MsticpyDataQueryError(
            f"No data loaded for query {query_name}.",
            "Please check that the data loaded from the log paths",
            "has this data type.",
        )

    def query_with_results(self, query, **kwargs):
        """Return query with fake results."""
        return self.query(query, **kwargs), "OK"

    def _read_data_files(self):
        """Read data files into memory."""
        if self._data_cache():
            return
        # If a cache file is specified and is a valid file,
        # try to use that.
        if self._cache_file and Path(self._cache_file).is_file():
            with open(self._cache_file, "rb") as cache_handle:
                try:
                    self._data_cache = pickle.load(cache_handle)
                except pickle.PickleError:
                    print(f"Invalid cache file {self._data_cache}")
            if self._data_cache:
                return

        # Otherwise read in the data files.
        data_files = (
            tqdm(self.data_files.values())
            if self._progress
            else self.data_files.values()
        )
        for path in data_files:
            self._read_log_file(path)

        # if a cache file name was specified, write data cache to that file.
        if self._cache_file:
            if not Path(self._cache_file).is_file():
                print("Overwriting exist cache file.")
            with open(self._cache_file, "wb") as cache_handle:
                try:
                    pickle.dump(cache_handle)
                except pickle.PickleError:
                    print(f"Could not write to cache file {self._data_cache}")

    def _extract_event_type(self, df_all_queries: pd.DataFrame, event_name: str):
        """Extract tidied DF from all queries."""
        query_df = df_all_queries[df_all_queries["name"] == event_name].dropna(
            axis=1, how="all"
        )
        for date_column in self.OS_QUERY_DATEIME_COLS & set(query_df.columns):
            query_df[date_column] = pd.to_datetime(
                query_df[date_column],
                unit="s",
                origin="unix",
                utc=True,
            )
        return query_df

    def _read_log_file(self, log_path: str):
        """Read log file into cache."""
        if Path(log_path).suffix.casefold() in (".log", ".json"):
            # We can't use directly read_json as we need to extract nested json
            # Split log files in multiple queries table here
            # Likely resource intensive and better way to do.
            # Likely issue, multiple log files can contain same query mostly
            # because of log rotation
            list_lines: List[Dict[str, Any]] = []
            try:
                with open(log_path, mode="r", encoding="utf-8") as logfile:
                    # json_out: Dict[str, Dict[Any]]
                    # json_out = {"lines": []}
                    # while True:
                    #     next_line = logfile.readline()

                    #     if not next_line:
                    #         break
                    #     json_out["lines"].append(json.loads(next_line.strip()))
                    #     df_all_queries = pd.json_normalize(
                    #         json_out["lines"], max_level=3
                    #     )
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
            df_all_queries = pd.json_normalize(list_lines, max_level=3)
            # Don't want dot in columns name
            df_all_queries.columns = df_all_queries.columns.str.replace(
                ".", "_", regex=False
            )

            event_types = df_all_queries["name"].unique().tolist()
            self._data_cache = {
                event_name: self._extract_event_type(df_all_queries, event_name)
                for event_name in event_types
            }
