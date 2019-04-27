# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""KQL Driver class."""
from typing import Tuple, Union, Any

import pandas as pd
from IPython import get_ipython
from Kqlmagic import results

from . provider_base import DataProviderBase
from ... nbtools.utility import export
from ... _version import VERSION

__version__ = VERSION
__author__ = 'Ian Hellen'


@export
class KqlDriver(DataProviderBase):
    """KqlDriver class to execute kql queries."""

    def __init__(self, connection_str: str = None, **kwargs):
        """
        Instantiaite KqlDriver and optionally connect.

        Parameters
        ----------
        connection_str : str, optional
            Connection string

        """
        self._debug = kwargs.get('debug', False)
        super().__init__()

        self._loaded = self._is_kqlmagic_loaded()

        if not self._loaded:
            self._load_kql_magic()

        self._ip = get_ipython()
        if connection_str:
            self.current_connection = connection_str
            self.connect(connection_str)

    def connect(self, connection_str: str, **kwargs):
        """
        Connect to data source.

        Parameters
        ----------
        connection_string : str
            Connect to a data source

        """
        self.current_connection = connection_str
        result = self._ip.run_cell_magic('kql', line='', cell=connection_str)
        self._connected = True
        return result

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
        return self.query_with_results(query)[0]

    def query_with_results(self, query: str) -> Tuple[pd.DataFrame,
                                                      results.ResultSet]:
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
        if not self.connected:
            raise ConnectionError('Source is not connected. Please call connect() and retry.')

        if self._debug:
            print(query)
        result = self._ip.run_cell_magic('kql', line='', cell=query)
        if result is not None and result.completion_query_info['StatusCode'] == 0:
            data_frame = result.to_dataframe()
            if result.is_partial_table:
                print("Warning - query returned partial results.")
            # Did user want both dataframe and ResultSet

            return data_frame, result

        print("Warning - query did not complete successfully.")
        print("Kql ResultSet returned - check  \'completion_query_info\' property.")
        return None, result

    def _load_kql_magic(self):
        """Load KqlMagic if not loaded."""
        # KqlMagic
        print('Please wait. Loading Kqlmagic extension...')
        self._ip.run_line_magic('reload_ext', 'Kqlmagic')

    def _is_kqlmagic_loaded(self) -> bool:
        """Return true if kql magic is loaded."""
        if self._ip is not None:
            return self._ip.find_magic('kql') is not None
        return False
