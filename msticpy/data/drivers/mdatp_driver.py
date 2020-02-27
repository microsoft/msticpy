# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""MDATP OData Driver class."""
from typing import Union, Any
import pandas as pd

from .odata_driver import OData
from ...nbtools.utility import export
from ..._version import VERSION

__version__ = VERSION
__author__ = "Pete Bryan"


@export
class MDATPDriver(OData):
    """KqlDriver class to retreive date from MDATP."""

    def __init__(self, connection_str: str = None, **kwargs):
        """
        Instantiaite MDATPDriver and optionally connect.

        Parameters
        ----------
        connection_str : str, optional
            Connection string

        """
        super().__init__()
        self.req_body = {
            "client_id": None,
            "client_secret": None,
            "grant_type": "client_credentials",
            "resource": "https://api.securitycenter.windows.com",
        }
        self.oauth_url = "https://login.windows.net/{tenantId}/oauth2/token"
        self.api_root = "https://api.securitycenter.windows.com/"
        self.api_ver = "api"

        if connection_str:
            self.current_connection = connection_str
            self.connect(connection_str)

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
        return self.query_with_results(
            query, body=True, api_end="/advancedqueries/run"
        )[0]
