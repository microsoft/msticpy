# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Local Osquery Data Driver class - osquery.{results,snapshots}.log."""
from pathlib import Path
from typing import Union, Any, Dict, Optional, List

import os
import json
import pandas as pd

from .driver_base import DriverBase, QuerySource
from ...common.pkg_config import settings
from ...common.utility import export
from ..._version import VERSION

__version__ = VERSION
__author__ = "juju4"


@export
class LocalOsqueryDriver(DriverBase):
    """LocalOsqueryDriver class to execute kql queries."""

    def __init__(self, connection_str: str = None, **kwargs):
        """
        Instantiaite LocalOsqueryDriver and optionally connect.

        Parameters
        ----------
        connection_str : str, optional
            Connection string (not used)
        data_paths : List[str], optional
            Paths from which to load data files

        """
        del connection_str
        self._debug = kwargs.get("debug", False)
        super().__init__()

        # If data paths specified, use these
        data_paths = kwargs.get("data_paths")
        self._paths: List[str] = ["."]
        if data_paths:
            self._paths = [path.strip() for path in data_paths]
        elif "LocalOsquery" in settings:
            self._paths = settings.get("LocalOsquery", {}).get("data_paths")

        self.data_files: Dict[str, str] = self._get_data_paths()
        self._schema: Dict[str, Any] = {}
        self._loaded = True
        self._connected = True

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
        self._connected = True
        print("Connected.")

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
        if self._schema:
            return self._schema
        schema_backup = self.data_files
        for df_fname in self.data_files.copy():

            test_df = self.query(df_fname)

            # save entries which are for a different file else losing them
            for key in schema_backup.copy().keys():
                if key == df_fname:
                    del schema_backup[key]

            if ".log" in df_fname:
                # split the many queries in file
                new_schema = {}
                for df_fname2 in test_df["name"].unique().tolist():
                    test_df2 = test_df[test_df["name"] == df_fname2]
                    test_df3 = test_df2.dropna(axis=1, how="all").copy()

                    # fix columns type
                    # https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.to_datetime.html
                    if "unixTime" in test_df3.columns:
                        test_df3["unixTime"] = pd.to_datetime(
                            test_df3["unixTime"], unit="s", origin="unix"
                        )
                    if "columns_time" in test_df3.columns:
                        test_df3["columns_time"] = pd.to_datetime(
                            test_df3["columns_time"], unit="s", origin="unix"
                        )
                    if "columns_atime" in test_df3.columns:
                        test_df3["columns_atime"] = pd.to_datetime(
                            test_df3["columns_atime"], unit="s", origin="unix"
                        )
                    if "columns_ctime" in test_df3.columns:
                        test_df3["columns_ctime"] = pd.to_datetime(
                            test_df3["columns_ctime"], unit="s", origin="unix"
                        )
                    if "columns_mtime" in test_df3.columns:
                        test_df3["columns_mtime"] = pd.to_datetime(
                            test_df3["columns_mtime"], unit="s", origin="unix"
                        )

                    if not isinstance(test_df3, pd.DataFrame):
                        continue
                    df_schema = test_df3.dtypes
                    new_schema[df_fname2] = {
                        key: dtype.name for key, dtype in df_schema.to_dict().items()
                    }
                self._schema = new_schema | schema_backup
            else:
                if not isinstance(test_df, pd.DataFrame):
                    continue
                df_schema = test_df.dtypes
                self._schema[df_fname] = {
                    key: dtype.name for key, dtype in df_schema.to_dict().items()
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
            A DataFrame (if successfull) or
            the underlying provider result if an error.

        """
        del kwargs
        query_name = query_source.name if query_source else query
        file_path = self.data_files.get(query.casefold())
        # print(f"Data file path {file_path} (data_files: {self.data_files}).")
        if not file_path:
            raise FileNotFoundError(
                f"Data file ({query}) for query {query_name} not found."
            )
        if file_path.endswith("log") or file_path.endswith("json"):
            # We can't use directly read_json as we need to extract nested json
            # Split log files in multiple queries table here
            # Likely resource intensive and better way to do.
            # Likely issue, multiple log files can contain same query mostly
            # because of log rotation
            try:
                with open(file_path, mode="r", encoding="utf-8") as logfile:
                    json_out: Dict[Dict[Any]]
                    json_out = {"lines": []}
                    while True:
                        next_line = logfile.readline()

                        if not next_line:
                            break
                        json_out["lines"].append(json.loads(next_line.strip()))
                        df_all_queries = pd.json_normalize(
                            json_out["lines"], max_level=3
                        )
            except ValueError as exc:
                raise ValueError(f"Read error on file {file_path}: {exc}.") from exc

            try:
                # Don't want dot in columns name
                df_all_queries.columns = df_all_queries.columns.str.replace(
                    ".", "_", regex=False
                )
                # fill available queries and preserve other files
                self.data_files = self.data_files | {
                    k: file_path for k in df_all_queries["name"].unique().tolist()
                }
                if os.path.basename(file_path) in self.data_files:
                    del self.data_files[os.path.basename(file_path)]

                if query in self.data_files and ".log" not in query:
                    df_query = df_all_queries[df_all_queries["name"] == query].dropna(
                        axis=1, how="all"
                    )
                    return df_query

                return df_all_queries
            except ValueError as exc:
                raise ValueError(f"Error when processing data from file {file_path}: {exc}.") from exc

        data_df = pd.read_pickle(file_path)
        if isinstance(data_df, pd.DataFrame):
            return data_df
        return f"{query} is not a DataFrame ({file_path})."

    def query_with_results(self, query, **kwargs):
        """Return query with fake results."""
        return self.query(query, **kwargs), "OK"
