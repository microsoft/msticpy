# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Kusto Driver subclass."""
from typing import Any, Union

import pandas as pd

from ...common.exceptions import MsticpyParameterError
from ...common.utility import export
from ..query_defns import DataEnvironment
from .kql_driver import KqlDriver, QuerySource

from ..._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"

_KCS_TEMPLATE = (
    "azure_data-Explorer://code;cluster='{cluster}';"
    "database='{database}';alias='{alias}'"
)


@export
class KustoDriver(KqlDriver):
    """Kusto Driver class to execute kql queries for Azure Data Explorer."""

    def __init__(self, connection_str: str = None, **kwargs):
        """
        Instantiate KustoDriver.

        Parameters
        ----------
        connection_str : str, optional
            Connection string

        Other Parameters
        ----------------
        debug : bool
            print out additional diagnostic information.

        """
        super().__init__(**kwargs)
        self.environment = kwargs.get("data_environment", DataEnvironment.Kusto)

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

        Other Parameters
        ----------------
        cluster : str, Optional
            Supply or override the Kusto cluster name
        db : str, Optional
            Supply or override the Kusto database name
        data_source : str, Optional
            alias for `db`
        connection_str : str, Optional


        Returns
        -------
        Union[pd.DataFrame, results.ResultSet]
            A DataFrame (if successfull) or
            the underlying provider result if an error.

        """
        # If the connection string is supplied as a parameter, use that
        self.current_connection = kwargs.get("connection_str")
        if not self.current_connection:
            # try to get cluster and db from kwargs or query_source metadata
            cluster = kwargs.get("cluster")
            database = kwargs.get("db", kwargs.get("data_source"))
            if cluster and database:
                self.current_connection = _KCS_TEMPLATE.format(
                    cluster=cluster, database=database, alias=""
                )
        if not self.current_connection and query_source:
            # try to connection string from query_source metadata
            self.current_connection = query_source.metadata.get("connection_str")
        if not self.current_connection and query_source:
            # try to get cluster and db from query_source metadata
            cluster = query_source.metadata.get("cluster")
            if not cluster:
                clusters = query_source.metadata.get("data_families", [None])
                cluster = clusters[0]
            database = query_source.metadata.get("data_source")
            if cluster and database:
                self.current_connection = _KCS_TEMPLATE.format(
                    cluster=cluster, database=database, alias=""
                )

        if not self.current_connection:
            raise MsticpyParameterError(
                "Could not determine the database or cluster name for the query.",
                "Please update the query with the correct values or specify",
                "explicitly with the 'cluster' and 'database' parameters to",
                "this function.",
                "In the query template these values are specified in the metadata:",
                "cluster - either 'cluster' or 'data_families'",
                "database - 'data_source",
                title="Missing cluster or database names.",
            )
        return super().query(query, query_source, **kwargs)
