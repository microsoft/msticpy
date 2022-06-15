# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Security Graph OData Driver class."""
from typing import Any, Union

import pandas as pd

from ..._version import VERSION
from ...auth.azure_auth_core import AzureCloudConfig
from ...common.utility import export
from .odata_driver import OData, QuerySource

__version__ = VERSION
__author__ = "Ian Hellen"


@export
class SecurityGraphDriver(OData):
    """Driver to query security graph."""

    CONFIG_NAME = "MicrosoftGraph"
    _ALT_CONFIG_NAMES = ["SecurityGraphApp"]

    def __init__(self, connection_str: str = None, **kwargs):
        """
        Instantiate MSGraph driver and optionally connect.

        Parameters
        ----------
        connection_str : str, optional
            Connection string

        """
        super().__init__(**kwargs)
        azure_cloud = AzureCloudConfig()
        self.scopes = ["User.Read"]
        self.req_body = {
            "client_id": None,
            "client_secret": None,
            "grant_type": "client_credentials",
            "scope": f"{azure_cloud.endpoints.microsoft_graph_resource_id}/.default",
        }
        self.oauth_url = (
            f"{azure_cloud.endpoints.active_directory}/{{tenantId}}/oauth2/v2.0/token"
        )
        self.api_root = azure_cloud.endpoints.microsoft_graph_resource_id
        self.api_ver = kwargs.get("api_ver", "v1.0")

        if connection_str:
            self.current_connection = connection_str
            self.connect(connection_str)

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
        del query_source, kwargs
        return self.query_with_results(query, body=False)[0]
