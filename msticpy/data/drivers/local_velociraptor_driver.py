# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Local Velociraptor Data Driver class."""
import logging
from collections import defaultdict
from functools import lru_cache
from pathlib import Path
from typing import Any, Dict, List, Optional, Union

import pandas as pd
from tqdm.auto import tqdm

from ..._version import VERSION
from ...common.exceptions import MsticpyDataQueryError
from ...common.provider_settings import get_provider_settings
from ...common.utility import export, valid_pyname
from .driver_base import DriverBase, QuerySource

__version__ = VERSION
__author__ = "ianhelle"

logger = logging.getLogger(__name__)

_VELOCIRATOR_DOC_URL = (
    "https://msticpy.readthedocs.io/en/latest/data_acquisition/"
    "DataProv-Velociraptor.html"
)


# pylint: disable=too-many-instance-attributes
@export
class VelociraptorLogDriver(DriverBase):
    """Velociraptor driver class to ingest log data."""

    def __init__(self, connection_str: Optional[str] = None, **kwargs):
        """
        Instantiate Velociraptor driver and optionally connect.

        Parameters
        ----------
        connection_str : str, optional
            Connection string (not used)
        data_paths : List[str], optional
            Paths from which to load data files
        progress : bool, optional
            Show progress with tqdm, by default, True

        """
        del connection_str
        if kwargs.get("debug", False):
            logger.setLevel(logging.DEBUG)
        super().__init__()

        self._paths: List[str] = ["."]
        # If data paths specified, use these
        # from kwargs or settings
        if data_paths := kwargs.get("data_paths"):
            self._paths = [path.strip() for path in data_paths]
            logger.info("data paths read from param %s", str(self._paths))
        else:
            prov_settings = get_provider_settings(config_section="DataProviders").get(
                "VelociraptorLogs"
            )
            if prov_settings:
                self._paths = prov_settings.args.get("data_paths", []) or self._paths
                logger.info("data paths read from config %s", str(self._paths))

        self.data_files: Dict[str, List[Path]] = {}
        self._schema: Dict[str, Any] = {}
        self._query_map: Dict[str, List[str]]
        self._progress = kwargs.pop("progress", True)
        self._loaded = True
        self.has_driver_queries = True
        logger.info("data files to read %s", ",".join(self.data_files))

    def connect(self, connection_str: Optional[str] = None, **kwargs):
        """
        Connect to data source.

        Parameters
        ----------
        connection_str : str
            Connect to a data source

        """
        del connection_str
        self.data_files = self._get_logfile_paths()
        self._connected = True

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
            if not self.data_files:
                self.connect()
            # read the first row of each file to get the schema
            iter_data_files = (
                tqdm(self.data_files.items())
                if self._progress
                else self.data_files.items()
            )
            for table, files in iter_data_files:
                if not files:
                    continue
                sample_df = pd.read_json(files[0], lines=True, nrows=1)
                self._schema[table] = {
                    col: dtype.name for col, dtype in sample_df.dtypes.to_dict().items()
                }
            logger.info("Reading schema for %d tables", len(self.data_files))

        return self._schema

    def query(
        self, query: str, query_source: Optional[QuerySource] = None, **kwargs
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
        if not self.data_files:
            self.connect()

        if query not in self.data_files:
            raise MsticpyDataQueryError(
                f"No data loaded for query {query}.",
                "Please check that the data loaded from the log paths",
                "has this data type.",
            )
        return self._cached_query(query)

    @lru_cache(maxsize=256)
    def _cached_query(self, query: str) -> pd.DataFrame:
        iter_data_files = (
            tqdm(self.data_files[query]) if self._progress else self.data_files[query]
        )
        dfs = [pd.read_json(file, lines=True) for file in iter_data_files]
        query_df = pd.concat(dfs, ignore_index=True)

        logger.info("Query %s, returned %d rows", query, len(query_df))
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
        if self.data_files:
            return [
                {
                    "name": query,
                    "query": query,
                    "query_paths": "velociraptor",
                    "description": f"Velociraptor {query} table.",
                }
                for query in self.data_files
            ]
        return []

    def _get_logfile_paths(self) -> Dict[str, List[Path]]:
        """Read files in data paths."""
        data_files: Dict[str, List[Path]] = defaultdict(list)

        for input_path in (Path(path_str) for path_str in self._paths):
            files = {
                file.relative_to(input_path): file
                for file in input_path.rglob("*.json")
            }

            file_names = [valid_pyname(str(file.with_suffix(""))) for file in files]
            path_files = dict(zip(file_names, files.values()))
            for file_name, file_path in path_files.items():
                data_files[file_name].append(file_path)

        logger.info("Found %d data file types", len(data_files))
        logger.info("Total data files: %d", sum(len(v) for v in data_files.values()))
        if not data_files:
            raise MsticpyDataQueryError(
                "No usable data files found in supplied paths.",
                f"Data paths supplied: {', '.join(self._paths)}",
                title="No data files found",
                help_uri=_VELOCIRATOR_DOC_URL,
            )
        return data_files
