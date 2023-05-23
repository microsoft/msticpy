# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Test module for plugin import tests."""
from typing import Any, Optional, Tuple, Union

import pandas as pd

from msticpy.data.core.query_source import QuerySource
from msticpy.data.drivers import DriverBase

__author__ = "Ian Hellen"

results_df = pd.DataFrame(
    [{"ID": "tests results", **{name: str(idx) for idx, name in enumerate("ABC")}}]
)


class CustomDataProvA(DriverBase):
    """Custom provider 1."""

    def __init__(self, **kwargs):
        """Initialize the class."""
        super().__init__(**kwargs)
        self._loaded = True

    def connect(self, connection_str: Optional[str] = None, **kwargs):
        """Connect to data source."""
        self._connected = True

    def query(
        self, query: str, query_source: QuerySource = None, **kwargs
    ) -> Union[pd.DataFrame, Any]:
        """Execute query string and return DataFrame of results."""
        return self.query_with_results(query, **kwargs)[0]

    def query_with_results(self, query: str, **kwargs) -> Tuple[pd.DataFrame, Any]:
        """Return results with optional status object."""
        return results_df, True


class CustomDataProvB(CustomDataProvA):
    """Custom provider 2."""

    DATA_ENVIRONMENTS = ["SQLTestProvider", "SQLProdProvider"]
