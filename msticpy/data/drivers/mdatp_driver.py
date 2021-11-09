# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""MDATP OData Driver class."""
from typing import Union, Any
import pandas as pd

from .odata_driver import OData, QuerySource
from ..query_defns import DataEnvironment
from ...common.azure_auth import AzureCloudConfig
from ...common.utility import export
from ..._version import VERSION

__version__ = VERSION
__author__ = "Pete Bryan"


@export
class MDATPDriver(OData):
    """KqlDriver class to retreive date from MS Defender APIs."""

    def __init__(self, connection_str: str = None, **kwargs):
        """
        Instantiate MSDefenderDriver and optionally connect.

        Parameters
        ----------
        connection_str : str, optional
            Connection string

        """
        super().__init__()
        api_uri, oauth_uri, api_suffix = _select_api_uris(
            data_environment=kwargs.get("data_environment")
        )
        self.req_body = {
            "client_id": None,
            "client_secret": None,
            "grant_type": "client_credentials",
            "resource": api_uri,
        }
        self.oauth_url = oauth_uri
        self.api_root = api_uri
        self.api_ver = "api"
        self.api_suffix = api_suffix

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
        return self.query_with_results(query, body=True, api_end=self.api_suffix)[0]


def _select_api_uris(data_environment):
    """Return API and login URIs for selected provider type."""
    cloud_config = AzureCloudConfig()
    login_uri = cloud_config.endpoints.active_directory
    if data_environment == DataEnvironment.MD365:
        return (
            "https://api.security.microsoft.com",
            f"{login_uri}/{{tenantId}}/oauth2/token",
            "/advancedhunting/run",
        )
    return (
        "https://api.securitycenter.microsoft.com",
        f"{login_uri}/{{tenantId}}/oauth2/token",
        "/advancedqueries/run",
    )
