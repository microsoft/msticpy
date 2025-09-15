# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""MDATP OData Driver class."""
from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any, ClassVar, Iterable

import pandas as pd
from typing_extensions import Self

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
    scopes: list[str]
    oauth_v2: bool = field(init=False)

    def __post_init__(self: Self) -> None:
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

    CONFIG_NAME: ClassVar[str] = "MicrosoftDefender"
    _ALT_CONFIG_NAMES: ClassVar[Iterable[str]] = ["MDATPApp"]

    def __init__(
        self: MDATPDriver,
        connection_str: str | None = None,
        instance: str = "Default",
        *,
        cloud: str | None = None,
        auth_type: str = "interactive",
        debug: bool = False,
        max_threads: int = 4,
        **kwargs,
    ) -> None:
        """
        Instantiate MSDefenderDriver and optionally connect.

        Parameters
        ----------
        connection_str : str, optional
            Connection string
        instance : str, optional
            The instance name from config to use
        cloud: str
            Name of the Azure Cloud to connect to.

        """
        super().__init__(
            debug=debug,
            max_threads=max_threads,
            **kwargs,
        )

        cs_dict: dict[str, str] = _get_driver_settings(
            self.CONFIG_NAME, self._ALT_CONFIG_NAMES, instance
        )

        self.cloud: str = cs_dict.pop("cloud", "global")
        if cloud:
            self.cloud = cloud

        m365d_params: M365DConfiguration = _select_api(
            self.data_environment, self.cloud
        )
        self._m365d_params: M365DConfiguration = m365d_params
        self.oauth_url = m365d_params.login_uri
        self.api_root = m365d_params.resource_uri
        self.api_ver = m365d_params.api_version
        self.api_suffix: str = m365d_params.api_endpoint
        self.scopes = m365d_params.scopes

        self.add_query_filter(
            "data_environments", ("MDE", "M365D", "MDATP", "M365DGraph", "GraphHunting")
        )

        self.req_body: dict[str, Any] = {}
        if "username" in cs_dict:
            delegated_auth = True

        else:
            delegated_auth = False
            self.req_body["grant_type"] = "client_credentials"

        if not m365d_params.oauth_v2:
            # For OAuth 1.0, use resource parameter with the base URI (without .default)
            resource_base = self.scopes[0].rstrip(".default")
            self.req_body["resource"] = resource_base
        # For OAuth 2.0, scopes are handled by the OData parent class authentication methods
        # No additional processing needed here as self.scopes is already set correctly

        if connection_str:
            self.current_connection = connection_str
            self.connect(
                connection_str,
                delegated_auth=delegated_auth,
                auth_type=auth_type,
                location=cs_dict.get("location", "token_cache.bin"),
            )

    def query(
        self: Self,
        query: str,
        query_source: QuerySource | None = None,
        **kwargs,
    ) -> pd.DataFrame | str | None:
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
                date_fields: list[str] = [
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
        return str(response)


def _select_api(data_environment: DataEnvironment, cloud: str) -> M365DConfiguration:
    """Return API and login URIs for selected provider type.

    OAuth 2.0 (v2.0 endpoint + scope) is now the default for all Defender APIs.

    Summary (all use /.default scope):
    - MDE (MDATP): api base https://api.securitycenter.microsoft.com
      token: https://login.microsoftonline.com/<tenantId>/oauth2/v2.0/token
      endpoint: /advancedqueries/run
    - M365D: api base https://api.security.microsoft.com
      token: https://login.microsoftonline.com/<tenantId>/oauth2/v2.0/token
      endpoint: /advancedhunting/run
    - M365DGraph: api base https://graph.microsoft.com (v1.0)
      token: https://login.microsoftonline.com/<tenantId>/oauth2/v2.0/token
      endpoint: /security/runHuntingQuery

    Scopes use: {resource_uri}/.default (normalized to avoid double slashes).
    If a legacy v1 (resource) flow is ever needed, supply a v1 token endpoint
    (oauth_v2 flag will become False and caller will add resource parameter).

    """
    if data_environment == DataEnvironment.M365DGraph:
        az_cloud_config = AzureCloudConfig(cloud=cloud)
        login_uri: str = (
            f"{az_cloud_config.authority_uri}{{tenantId}}/oauth2/v2.0/token"
        )
        resource_uri: str = az_cloud_config.endpoints["microsoftGraphResourceId"]
        api_version = "v1.0"
        api_endpoint = "/security/runHuntingQuery"
    elif data_environment == DataEnvironment.M365D:
        login_uri = f"{get_m365d_login_endpoint(cloud)}{{tenantId}}/oauth2/v2.0/token"
        resource_uri = get_m365d_endpoint(cloud)
        api_version = "api"
        api_endpoint = "/advancedhunting/run"
    else:
        # Default to MDE
        login_uri = f"{get_m365d_login_endpoint(cloud)}{{tenantId}}/oauth2/v2.0/token"
        resource_uri = get_defender_endpoint(cloud)
        api_version = "api"
        api_endpoint = "/advancedqueries/run"

    scope_base = resource_uri.rstrip("/")
    scopes: list[str] = [f"{scope_base}/.default"]
    api_uri: str = f"{resource_uri}{api_version}{api_endpoint}"

    return M365DConfiguration(
        login_uri=login_uri,
        resource_uri=resource_uri,
        api_version=api_version,
        api_endpoint=api_endpoint,
        api_uri=api_uri,
        scopes=scopes,
    )
