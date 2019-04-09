# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Intake kql driver."""
from typing import Mapping, Tuple

# Need some more work before the use of Intake is ready.
# Currently pip installing errors out
# pylint: disable=import-error
from intake.source.base import DataSource, Schema

import pandas as pd
from IPython import get_ipython
from Kqlmagic import results

from ..nbtools import kql
from ..nbtools.query_defns import DataEnvironment, DataFamily
from .._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"


class KqlSentinelSource(DataSource):
    """
    Kql Sentinel driver for Intake.

    Parameters
    ----------
    DataSource : intake.source.base.DataSource
        Base class of Intake data sources.

    Attributes
    ----------
        container : str
            The container type for the data
        name : str
            The name of the driver
        version : str
            The driver version
        partition_access : bool
            Whether the driver supports partitioned query
            of the data
        query : str
            The Kql query used to retrieve the data

    """

    container = "dataframe"
    name = "kqlsentinel"
    version = "0.0.1"
    partition_access = False

    _DATA_ENVIRONMENT = "data_env"
    _DATA_FAMILY = "data_family"

    def __init__(self, uri: str, query: str, metadata: Mapping = None, **kwargs):
        """
        Initialize the driver.

        Parameters
        ----------
        uri : str
            Kql connection string
        query : str
            The query used to access the data
        metadata : Mapping, optional
            Metadata supplied from the data source definition
            (the default is None)

        Other Parameters
        ----------------
        data_env : str
            The data environment.
        data_family : str
            The data family for this query.

        """
        super(KqlSentinelSource, self).__init__(metadata=metadata)

        self._ip = get_ipython()
        self.query = query
        self._uri = uri
        if self._DATA_ENVIRONMENT in kwargs:
            self._data_environment = DataEnvironment.parse(
                kwargs[self._DATA_ENVIRONMENT]
            )
        else:
            self._data_environment = DataEnvironment.LogAnalytics

        if self._DATA_FAMILY in kwargs:
            self._data_family = DataFamily.parse(kwargs[self._DATA_FAMILY])
        else:
            self._data_family = DataEnvironment.LogAnalytics
        kql.load_kql_magic()

        self._dataframe = None
        self._kql_result = None
        self._connection = None

    def _get_schema(self):
        if self._dataframe is None:
            self._kql_connect(self._uri)
            limit_query = f"{self.query} | limit 1"
            _, top_row = self._exec_query(limit_query)
            dtype = {k: str(v) for k, v in top_row.dtypes.to_dict().items()}
            shape = (None, top_row.shape[1])
        else:
            dtype = {k: str(v) for k, v in self._dataframe.dtypes.to_dict().items()}
            shape = (None, self._dataframe.shape[1])

        return Schema(datashape=None, dtype=dtype, shape=shape, npartitions=1)

    def _get_partition(self, i):
        del i
        # Return the appropriate container of data here
        self._kql_result, self._dataframe = self._exec_query(self.query)
        return self._dataframe

    def _exec_query(self, query: str) -> Tuple[results.ResultSet, pd.DataFrame]:
        if not self._connection:
            self._kql_connect(self._uri)

        kql_result = self._ip.run_cell_magic("kql", line="", cell=query)
        data_frame = None
        if (
                kql_result is not None
                and kql_result.completion_query_info["StatusCode"] == 0
        ):
            data_frame = kql_result.to_dataframe()
            if kql_result.is_partial_table:
                # Change to issue warning
                print("Warning - query returned partial results.")

            return kql_result, data_frame

        return kql_result, None

    def _kql_connect(self, uri: str):
        self._connection = self._ip.run_cell_magic("kql", line="", cell=uri)

    def read(self) -> pd.DataFrame:
        """
        Execute the query and return the result.

        Returns
        -------
        pd.DataFrame
            The query results

        """
        self._load_metadata()
        return pd.concat([self.read_partition(i) for i in self.npartitions])

    def _close(self):
        self._dataframe = None
        self._kql_result = None

    def to_dask(self):
        """Not implemented."""
        raise NotImplementedError

    def to_spark(self):
        """Not implemented."""
        raise NotImplementedError
