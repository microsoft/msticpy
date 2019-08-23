# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""KQL Driver class."""
from typing import Tuple, Union, Any, Dict

import pandas as pd
from IPython import get_ipython

from .driver_base import DriverBase
from ...nbtools.utility import export
from ..._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"


@export
class KqlDriver(DriverBase):
    """KqlDriver class to execute kql queries."""

    def __init__(self, connection_str: str = None, **kwargs):
        """
        Instantiaite KqlDriver and optionally connect.

        Parameters
        ----------
        connection_str : str, optional
            Connection string

        """
        self._ip = get_ipython()
        self._debug = kwargs.get("debug", False)
        super().__init__()

        self._loaded = self._is_kqlmagic_loaded()

        if not self._loaded:
            self._load_kql_magic()

        if connection_str:
            self.current_connection = connection_str
            self.connect(connection_str)

        self._schema: Dict[str, Any] = {}

    def connect(self, connection_str: str, **kwargs):
        """
        Connect to data source.

        Parameters
        ----------
        connection_string : str
            Connect to a data source

        """
        self.current_connection = connection_str
        result = self._ip.run_cell_magic("kql", line="", cell=connection_str)
        self._connected = True
        self._schema = self._get_schema()
        return result

    @property
    def schema(self) -> Dict[str, Dict]:
        """
        Return current data schema of connection.

        Returns
        -------
        Dict[str, Dict]
            Data schema of current connection.

        """
        return self._schema

    def query(self, query: str) -> Union[pd.DataFrame, Any]:
        """
        Execute query string and return DataFrame of results.

        Parameters
        ----------
        query : str
            The kql query to execute

        Returns
        -------
        Union[pd.DataFrame, results.ResultSet]
            A DataFrame (if successfull) or
            Kql ResultSet if an error.

        """
        data, result = self.query_with_results(query)
        return data if data is not None else result

    # pylint: disable=too-many-branches
    def query_with_results(self, query: str) -> Tuple[pd.DataFrame, Any]:
        """
        Execute query string and return DataFrame of results.

        Parameters
        ----------
        query : str
            The kql query to execute

        Returns
        -------
        Tuple[pd.DataFrame, results.ResultSet]
            A DataFrame (if successfull) and
            Kql ResultSet.

        """
        # connect or switch the connection if our connection string
        # is not the current KqlMagic connection.
        self.connect(self.current_connection)
        if not self.connected:
            raise ConnectionError(
                "Source is not connected. Please call connect() and retry."
            )

        if self._debug:
            print(query)

        # save current auto_dataframe setting so that we can set to false
        # and restore current setting
        auto_dataframe = self._ip.run_line_magic(
            "config", line="Kqlmagic.auto_dataframe"
        )
        self._ip.run_line_magic("config", line="Kqlmagic.auto_dataframe=False")
        # run the query
        result = self._ip.run_cell_magic("kql", line="", cell=query)
        self._ip.run_line_magic(
            "config", line=f"Kqlmagic.auto_dataframe={auto_dataframe}"
        )
        if result is not None:
            if isinstance(result, pd.DataFrame):
                return result, None
            if (
                hasattr(result, "completion_query_info")
                and result.completion_query_info["StatusCode"] == 0
            ):
                data_frame = result.to_dataframe()
                if result.is_partial_table:
                    print("Warning - query returned partial results.")
                return data_frame, result

        print("Warning - query did not complete successfully.")
        if hasattr(result, "completion_query_info"):
            print(
                result.completion_query_info["StatusCode"],
                "(code: {}".format(result.completion_query_info["StatusCode"]),
            )
        return None, result

    def _load_kql_magic(self):
        """Load KqlMagic if not loaded."""
        # KqlMagic
        print("Please wait. Loading Kqlmagic extension...")

        self._ip.run_line_magic("reload_ext", "Kqlmagic")
        self._loaded = True

    def _is_kqlmagic_loaded(self) -> bool:
        """Return true if kql magic is loaded."""
        if self._ip is not None:
            return self._ip.find_magic("kql") is not None
        return False

    def _get_schema(self) -> Dict[str, Dict]:
        return self._ip.run_line_magic("kql", line="--schema")
