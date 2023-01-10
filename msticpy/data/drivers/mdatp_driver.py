# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""MDATP OData Driver class."""
from typing import Any, Union

import pandas as pd

from ..._version import VERSION
from ...auth.azure_auth import AzureCloudConfig
from ...auth.cloud_mappings import get_defender_endpoint, get_m365d_endpoint
from ...common.data_utils import ensure_df_datetimes
from ...common.utility import export
from ..core.query_defns import DataEnvironment
from .odata_driver import OData, QuerySource, _get_driver_settings

__version__ = VERSION
__author__ = "Pete Bryan"


@export
class MDATPDriver(OData):
    """KqlDriver class to retreive date from MS Defender APIs."""

    CONFIG_NAME = "MicrosoftDefender"
    _ALT_CONFIG_NAMES = ["MDATPApp"]

    def __init__(self, connection_str: str = None, instance: str = "Default", **kwargs):
        """
        Instantiate MSDefenderDriver and optionally connect.

        Parameters
        ----------
        connection_str : str, optional
            Connection string
        instance : str, optional
            The instance name from config to use

        """
        super().__init__(**kwargs)
        cs_dict = _get_driver_settings(
            self.CONFIG_NAME, self._ALT_CONFIG_NAMES, instance
        )
        self.cloud = cs_dict.pop("cloud", "global")
        if "cloud" in kwargs and kwargs["cloud"]:
            self.cloud = kwargs["cloud"]

        api_uri, oauth_uri, api_suffix = _select_api_uris(
            self.data_environment, self.cloud
        )
        self.add_query_filter("data_environments", "MDE")
        self.add_query_filter("data_environments", "M365D")
        self.add_query_filter("data_environments", "MDATP")

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
        if self.data_environment == DataEnvironment.M365D:
            self.scopes = ["https://api.security.microsoft.com/AdvancedHunting.Read"]
        else:
            self.scopes = [
                "https://api.securitycenter.microsoft.com/AdvancedQuery.Read"
            ]

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
        data, response = self.query_with_results(
            query, body=True, api_end=self.api_suffix
        )
        if isinstance(data, pd.DataFrame):
            # If we got a schema we should convert the DateTimes to pandas datetimes
            if "Schema" not in response:
                return data
            date_fields = [
                field["Name"]
                for field in response["Schema"]
                if field["Type"] == "DateTime"
            ]
            data = ensure_df_datetimes(data, columns=date_fields)
            return data
        return response


def _select_api_uris(data_environment, cloud):
    """Return API and login URIs for selected provider type."""
    cloud_config = AzureCloudConfig()
    login_uri = cloud_config.endpoints.active_directory
    if data_environment == DataEnvironment.M365D:
        base_url = get_m365d_endpoint(cloud)
        return (
            base_url,
            f"{login_uri}/{{tenantId}}/oauth2/token",
            "/advancedhunting/run",
        )
    base_url = get_defender_endpoint(cloud)
    return (
        base_url,
        f"{login_uri}/{{tenantId}}/oauth2/token",
        "/advancedqueries/run",
    )
