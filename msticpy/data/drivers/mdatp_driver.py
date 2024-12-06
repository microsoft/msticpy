# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""MDATP OData Driver class."""
from dataclasses import dataclass, field
from typing import Any, Dict, List, Optional, Union

import pandas as pd

from ..._version import VERSION
from ...auth.azure_auth_core import AzureCloudConfig
from ...auth.cloud_mappings import (
    get_defender_endpoint,
    get_m365d_endpoint,
    get_m365d_login_endpoint,
)
from ...common.data_utils import ensure_df_datetimes
from ...common.utility import export
from ..core.query_defns import DataEnvironment
from .odata_driver import OData, QuerySource, _get_driver_settings

__version__ = VERSION
__author__ = "Pete Bryan"


@dataclass
class M365DConfiguration:
    """A container for M365D API settings.

    This is based on the data environment of the query provider.
    """

    login_uri: str
    resource_uri: str
    api_version: str
    api_endpoint: str
    api_uri: str
    scopes: List[str]
    oauth_v2: bool = field(init=False)

    def __post_init__(self):
        """Determine if the selected API supports Entra ID OAuth v2.0.

        This is important because the fields in the request body
        are different between the two versions.
        """
        if "/oauth2/v2.0" in self.login_uri:
            self.oauth_v2 = True
        else:
            self.oauth_v2 = False


@export
class MDATPDriver(OData):
    """KqlDriver class to retrieve date from MS Defender APIs."""

    CONFIG_NAME = "MicrosoftDefender"
    _ALT_CONFIG_NAMES = ["MDATPApp"]

    def __init__(
        self, connection_str: Optional[str] = None, instance: str = "Default", **kwargs
    ):
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

        m365d_params = _select_api(self.data_environment, self.cloud)
        self._m365d_params: M365DConfiguration = m365d_params
        self.oauth_url = m365d_params.login_uri
        self.api_root = m365d_params.resource_uri
        self.api_ver = m365d_params.api_version
        self.api_suffix = m365d_params.api_endpoint
        self.scopes = m365d_params.scopes

        self.add_query_filter(
            "data_environments", ("MDE", "M365D", "MDATP", "M365DGraph", "GraphHunting")
        )

        self.req_body: Dict[str, Any] = {}
        if "username" in cs_dict:
            delegated_auth = True

        else:
            delegated_auth = False
            self.req_body["grant_type"] = "client_credentials"

        if not m365d_params.oauth_v2:
            self.req_body["resource"] = self.scopes

        if connection_str:
            self.current_connection = connection_str
            self.connect(
                connection_str,
                delegated_auth=delegated_auth,
                auth_type=kwargs.get("auth_type", "interactive"),
                location=cs_dict.get("location", "token_cache.bin"),
            )

    def query(
        self, query: str, query_source: Optional[QuerySource] = None, **kwargs
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
            A DataFrame (if successful) or
            the underlying provider result if an error.

        """
        del query_source, kwargs
        data, response = self.query_with_results(
            query, body=True, api_end=self.api_suffix
        )
        if isinstance(data, pd.DataFrame):
            # If we got a schema we should convert the DateTimes to pandas datetimes
            if ("Schema" or "schema") not in response:
                return data

            if self.data_environment == DataEnvironment.M365DGraph:
                date_fields = [
                    field["name"]
                    for field in response["schema"]
                    if field["type"] == "DateTime"
                ]
            else:
                date_fields = [
                    field["Name"]
                    for field in response["Schema"]
                    if field["Type"] == "DateTime"
                ]
            data = ensure_df_datetimes(data, columns=date_fields)
            return data
        return response


def _select_api(data_environment, cloud) -> M365DConfiguration:
    # pylint: disable=line-too-long
    """Return API and login URIs for selected provider type.

    Note that the Microsoft Graph is the preferred API.

    | API Name | Resource ID | Scopes Requested | API URI (global cloud) | API Endpoint | Login URI | MSTICpy Data Environment |
    | -------- | ----------- | ---------------- | ---------------------- | ------------ | --------- | ------------------------ |
    | WindowsDefenderATP | fc780465-2017-40d4-a0c5-307022471b92 | `AdvancedQuery.Read` | `https://api.securitycenter.microsoft.com` | `/advancedqueries/run` | `https://login.microsoftonline.com/<tenantId>/oauth2/token` | `MDE`, `MDATP` |
    | Microsoft Threat Protection | 8ee8fdad-f234-4243-8f3b-15c294843740 | `AdvancedHunting.Read` | `https://api.security.microsoft.com` | `/advancedhunting/run` | `https://login.microsoftonline.com/<tenantId>/oauth2/token` | `M365D` |
    | Microsoft Graph | 00000003-0000-0000-c000-000000000000 | `ThreatHunting.Read.All` | `https://graph.microsoft.com/<version>/` | `/security/runHuntingQuery` | `https://login.microsoftonline.com/<tenantId>/oauth2/v2.0/token` | `M365DGraph` |

    """
    # pylint: enable=line-too-long
    if data_environment == DataEnvironment.M365DGraph:
        az_cloud_config = AzureCloudConfig(cloud=cloud)
        login_uri = f"{az_cloud_config.authority_uri}{{tenantId}}/oauth2/v2.0/token"
        resource_uri = az_cloud_config.endpoints["microsoftGraphResourceId"]
        api_version = "v1.0"
        api_endpoint = "/security/runHuntingQuery"
        scopes = [f"{resource_uri}ThreatHunting.Read.All"]

    elif data_environment == DataEnvironment.M365D:
        login_uri = f"{get_m365d_login_endpoint(cloud)}{{tenantId}}/oauth2/token"
        resource_uri = get_m365d_endpoint(cloud)
        api_version = "api"
        api_endpoint = "/advancedhunting/run"
        scopes = [f"{resource_uri}AdvancedHunting.Read"]

    else:
        login_uri = f"{get_m365d_login_endpoint(cloud)}{{tenantId}}/oauth2/token"
        resource_uri = get_defender_endpoint(cloud)
        api_version = "api"
        api_endpoint = "/advancedqueries/run"
        scopes = [f"{resource_uri}AdvancedQuery.Read"]

    api_uri = f"{resource_uri}{api_version}{api_endpoint}"

    return M365DConfiguration(
        login_uri=login_uri,
        resource_uri=resource_uri,
        api_version=api_version,
        api_endpoint=api_endpoint,
        api_uri=api_uri,
        scopes=scopes,
    )
