# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""MDATP OData Driver class."""
from __future__ import annotations

import logging
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

logger: logging.Logger = logging.getLogger(__name__)


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
            logger.debug("OAuth v2.0 flow detected")
        else:
            self.oauth_v2 = False
            logger.debug("OAuth v1.0 flow detected")


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
        logger.info(
            "Initializing MDATPDriver - Instance: %s, Cloud: %s, Auth type: %s, Debug: %s",
            instance,
            cloud,
            auth_type,
            debug,
        )
        super().__init__(
            debug=debug,
            max_threads=max_threads,
            **kwargs,
        )

        logger.debug("Loading driver settings for instance: %s", instance)
        cs_dict: dict[str, str] = _get_driver_settings(
            self.CONFIG_NAME, self._ALT_CONFIG_NAMES, instance
        )

        self.cloud: str = cs_dict.pop("cloud", "global")
        if cloud:
            logger.info("Overriding configured cloud with: %s", cloud)
            self.cloud = cloud
        else:
            logger.debug("Using cloud from configuration: %s", self.cloud)

        logger.info(
            "Selecting API configuration for environment: %s", self.data_environment
        )
        m365d_params: M365DConfiguration = _select_api(
            self.data_environment, self.cloud
        )
        self._m365d_params: M365DConfiguration = m365d_params
        self.oauth_url = m365d_params.login_uri
        self.api_root = m365d_params.resource_uri
        self.api_ver = m365d_params.api_version
        self.api_suffix: str = m365d_params.api_endpoint
        self.scopes = m365d_params.scopes

        logger.debug(
            "API configuration - Root: %s, Version: %s, Endpoint: %s, Scopes: %s",
            self.api_root,
            self.api_ver,
            self.api_suffix,
            self.scopes,
        )

        self.add_query_filter(
            "data_environments", ("MDE", "M365D", "MDATP", "M365DGraph", "GraphHunting")
        )

        self.req_body: dict[str, Any] = {}
        if "username" in cs_dict:
            logger.info("Username detected in configuration, using delegated auth")
            delegated_auth = True
        else:
            logger.info("No username in configuration, using application auth")
            delegated_auth = False
            self.req_body["grant_type"] = "client_credentials"

        if not m365d_params.oauth_v2:
            logger.debug("Configuring OAuth v1 request body with resource parameter")
            self.req_body["resource"] = self.scopes
        else:
            logger.debug("Configuring OAuth v2 request body with scope parameter")
            self.req_body["scope"] = " ".join(self.scopes)

        if connection_str:
            logger.info("Connection string provided, connecting immediately")
            self.current_connection = connection_str
            self.connect(
                connection_str,
                delegated_auth=delegated_auth,
                auth_type=auth_type,
                location=cs_dict.get("location", "token_cache.bin"),
            )
        else:
            logger.debug("No connection string provided, skipping immediate connection")

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
        if self._debug:
            logger.debug(
                "Executing query on environment: %s - Query: %s",
                self.data_environment,
                query,
            )
        else:
            logger.debug("Executing query on environment: %s", self.data_environment)

        del query_source, kwargs
        logger.info("Sending query to API endpoint: %s", self.api_suffix)
        data, response = self.query_with_results(
            query, body=True, api_end=self.api_suffix
        )
        if isinstance(data, pd.DataFrame):
            logger.debug("Query returned DataFrame with %d rows", len(data))
            # If we got a schema we should convert the DateTimes to pandas datetimes
            if ("Schema" or "schema") not in response:
                logger.debug("No schema in response, returning raw DataFrame")
                return data

            if self.data_environment == DataEnvironment.M365DGraph:
                logger.debug("Processing M365DGraph response schema")
                date_fields: list[str] = [
                    field["name"]
                    for field in response["schema"]
                    if field["type"] == "DateTime"
                ]
            else:
                logger.debug("Processing MDE/M365D response schema")
                date_fields = [
                    field["Name"]
                    for field in response["Schema"]
                    if field["Type"] == "DateTime"
                ]

            if date_fields:
                logger.debug(
                    "Converting %d DateTime field(s): %s", len(date_fields), date_fields
                )
            else:
                logger.debug("No DateTime fields found in schema")

            data = ensure_df_datetimes(data, columns=date_fields)
            return data

        logger.warning("Query did not return a DataFrame, returning response as string")
        return str(response)


def _select_api(data_environment: DataEnvironment, cloud: str) -> M365DConfiguration:
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
    logger.debug(
        "Selecting API configuration for environment: %s, cloud: %s",
        data_environment,
        cloud,
    )

    if data_environment == DataEnvironment.M365DGraph:
        logger.info("Using Microsoft Graph Security Hunting API")
        az_cloud_config = AzureCloudConfig(cloud=cloud)
        login_uri: str = (
            f"{az_cloud_config.authority_uri}{{tenantId}}/oauth2/v2.0/token"
        )
        resource_uri: str = az_cloud_config.endpoints["microsoftGraphResourceId"]
        api_version = "v1.0"
        api_endpoint = "/security/runHuntingQuery"
        scopes: list[str] = [f"{resource_uri}ThreatHunting.Read.All"]

    elif data_environment == DataEnvironment.M365D:
        logger.info("Using M365 Defender Advanced Hunting API")
        login_uri = f"{get_m365d_login_endpoint(cloud)}{{tenantId}}/oauth2/token"
        resource_uri = get_m365d_endpoint(cloud)
        api_version = "api"
        api_endpoint = "/advancedhunting/run"
        scopes = [f"{resource_uri}AdvancedHunting.Read"]

    else:
        logger.info("Using MDE Advanced Queries API (default)")
        login_uri = f"{get_m365d_login_endpoint(cloud)}{{tenantId}}/oauth2/token"
        resource_uri = get_defender_endpoint(cloud)
        api_version = "api"
        api_endpoint = "/advancedqueries/run"
        scopes = [f"{resource_uri}AdvancedQuery.Read"]

    api_uri: str = f"{resource_uri}{api_version}{api_endpoint}"

    logger.debug(
        "API URIs configured - Login: %s, Resource: %s, Full API: %s, Scopes: %s",
        login_uri,
        resource_uri,
        api_uri,
        scopes,
    )

    return M365DConfiguration(
        login_uri=login_uri,
        resource_uri=resource_uri,
        api_version=api_version,
        api_endpoint=api_endpoint,
        api_uri=api_uri,
        scopes=scopes,
    )
