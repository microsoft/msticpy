# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Query Provider additional connection methods."""
from typing import Any, Dict, List, Optional, Protocol

import pandas as pd

from ..._version import VERSION
from ...common.exceptions import MsticpyDataQueryError
from ..drivers.driver_base import DriverBase

__version__ = VERSION
__author__ = "Ian Hellen"


# pylint: disable=too-few-public-methods
class QueryProviderProtocol(Protocol):
    """Protocol for required properties of QueryProvider class."""

    driver_class: Any
    _driver_kwargs: Dict[str, Any]
    _additional_connections: Dict[str, Any]
    _query_provider: DriverBase


# pylint: disable=super-init-not-called
class QueryProviderConnectionsMixin(QueryProviderProtocol):
    """Mixin additional connection handling QueryProvider class."""

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

    def _exec_additional_connections(self, query, result, **kwargs) -> pd.DataFrame:
        """Return results of query run query against additional connections."""
        query_source = kwargs.get("query_source")
        query_options = kwargs.get("query_options", {})
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
