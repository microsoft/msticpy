# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Mixin Classes for Sentinel Utilties."""
from __future__ import annotations

import logging
from collections import Counter
from dataclasses import dataclass
from typing import Any

import httpx
import pandas as pd
from azure.common.exceptions import CloudError
from azure.mgmt.core import tools as az_tools
from typing_extensions import Self

from ..._version import VERSION
from ...auth.azure_auth_core import AzureCloudConfig
from ...common.exceptions import MsticpyAzureConfigError, MsticpyAzureConnectionError
from ...common.pkg_config import get_http_timeout
from .azure_data import AzureData, get_api_headers

__version__ = VERSION
__author__ = "Pete Bryan"

logger: logging.Logger = logging.getLogger(__name__)

_PATH_MAPPING: dict[str, str] = {
    "ops_path": "/providers/Microsoft.SecurityInsights/operations",
    "alert_rules": "/providers/Microsoft.SecurityInsights/alertRules",
    "ss_path": "/savedSearches",
    "bookmarks": "/providers/Microsoft.SecurityInsights/bookmarks",
    "incidents": "/providers/Microsoft.SecurityInsights/incidents",
    "data_connectors": "/providers/Microsoft.SecurityInsights/dataConnectors",
    "watchlists": "/providers/Microsoft.SecurityInsights/watchlists",
    "alert_template": "/providers/Microsoft.SecurityInsights/alertRuleTemplates",
    "search": "/tables",
    "ti": "/providers/Microsoft.SecurityInsights/threatIntelligence/main",
    "dynamic_summary": "/providers/Microsoft.SecurityInsights/dynamicSummaries",
}


@dataclass
class SentinelInstanceDetails:
    """Dataclass for Sentinel workspace details."""

    subscription_id: str
    resource_group: str
    workspace_name: str

    @property
    def resource_id(self) -> str:
        """Return the resource ID for the workspace."""
        return build_sentinel_resource_id(
            self.subscription_id,
            self.resource_group,
            self.workspace_name,
        )

    @classmethod
    def from_resource_id(cls: type[Self], resource_id: str) -> Self:
        """Return SentinelInstanceDetails from a resource ID."""
        return cls(**parse_resource_id(resource_id))


class SentinelUtilsMixin(AzureData):
    """Mixin class for Sentinel core feature integrations."""

    def _get_items(self: Self, url: str, params: dict | None = None) -> httpx.Response:
        """Get items from the API."""
        self.check_connected()
        if params is None:
            params = {"api-version": "2020-01-01"}
        logger.debug("_get_items request to %s.", url)
        if not self._token:
            err_msg = "Token not found, can't get items."
            raise ValueError(err_msg)
        return httpx.get(
            url,
            headers=get_api_headers(self._token),
            params=params,
            timeout=get_http_timeout(),
        )

    def _list_items(  # noqa:PLR0913
        self: Self,
        item_type: str,
        api_version: str = "2020-01-01",
        appendix: str | None = None,
        *,
        next_follow: bool = False,
        params: dict[str, Any] | None = None,
    ) -> pd.DataFrame:
        """
        Return lists of core resources from APIs.

        Parameters
        ----------
        item_type : str
            The type of resource you want to list.
        api_version: str, optional
            The API version to use, by default '2020-01-01'
        appendix: str, optional
            Any appendix that needs adding to the URI, default is None
        next_follow: bool, optional
            If True, follow the nextLink to get all results, by default False
        params: Dict, optional
            Any additional parameters to pass to the API call, by default None

        Returns
        -------
        pd.DataFrame
            A DataFrame containing the requested items.

        Raises
        ------
        CloudError
            If a valid result is not returned.

        """
        item_url: str = (self.url or "") + _PATH_MAPPING[item_type]
        if appendix:
            item_url = item_url + appendix
        if params is None:
            params = {}
        params["api-version"] = api_version
        response: httpx.Response = self._get_items(item_url, params)
        if response.is_success:
            results_df: pd.DataFrame = _azs_api_result_to_df(response)
        else:
            raise CloudError(response=response)
        j_resp: dict[str, Any] = response.json()
        results: list[pd.DataFrame] = [results_df]
        # If nextLink in response, go get that data as well
        if next_follow:
            i = 0
            # Limit to 5 nextLinks to prevent infinite loop
            while "nextLink" in j_resp and i < 5:
                next_url: str = j_resp["nextLink"]
                next_response: httpx.Response = self._get_items(next_url, params)
                next_results_df: pd.DataFrame = _azs_api_result_to_df(next_response)
                results.append(next_results_df)
                j_resp = next_response.json()
                i += 1
        results_df = pd.concat(results)
        logger.info(
            "list_items request to %s returned %d rows",
            item_url,
            len(results_df),
        )
        return results_df

    def _build_sent_res_id(
        self: Self,
        subscription_id: str,
        resource_group: str,
        workspace_name: str,
    ) -> str:
        """
        Build a resource ID.

        Parameters
        ----------
        subscription_id : str
            Subscription ID to use
        resource_group : str
            Resource Group name to use
        workspace_name : str
            Workspace name to user

        Returns
        -------
        str
            The formatted resource ID.

        """
        return build_sentinel_resource_id(
            subscription_id,
            resource_group,
            workspace_name,
        )

    def _build_sentinel_api_root(
        self: Self,
        sentinel_instance: SentinelInstanceDetails,
        base_url: str | None = None,
    ) -> str:
        """
        Build an API URL from an Azure resource ID.

        Parameters
        ----------
        sentinel_instance : SentinelInstanceDetails
            Details of a Sentinel workspace
        base_url : str, optional
            The base URL of the Azure cloud to connect to.
            Defaults to resource manager for configured cloud.
            If no cloud configuration, defaults to resource manager
            endpoint for public cloud.

        Returns
        -------
        str
            A URI to that resource.

        """
        if not base_url:
            base_url = AzureCloudConfig(self.cloud).resource_manager
        resource_id: str | None = sentinel_instance.resource_id
        if base_url.endswith("/"):
            base_url = base_url[:-1]

        sentinel_api_url: str = f"{base_url}{resource_id}"
        logger.info("Sentinel API URL built: %s", sentinel_api_url)
        return sentinel_api_url

    def check_connected(self: Self) -> None:
        """Check that Sentinel workspace is connected."""
        if not self.connected:
            err_msg: str = (
                "Not connected to Sentinel, ensure you run `.connect`"
                "before calling functions."
            )
            raise MsticpyAzureConnectionError(err_msg)


def _azs_api_result_to_df(response: httpx.Response) -> pd.DataFrame:
    """
    Convert API response to a Pandas dataframe.

    Parameters
    ----------
    response : httpx.Response
        A response object from an Azure REST API call.

    Returns
    -------
    pd.DataFrame
        The API response as a Pandas dataframe.

    Raises
    ------
    ValueError
        If the response is not valid JSON.

    """
    j_resp: dict[str, Any] = response.json()
    if not response.is_success or not j_resp:
        err_msg: str = "No valid JSON result in response"
        raise ValueError(err_msg)
    if "value" in j_resp:
        j_resp = j_resp["value"]
    return pd.json_normalize(j_resp)


def build_sentinel_resource_id(
    subscription_id: str,
    resource_group: str,
    workspace_name: str,
) -> str:
    """
    Build a MS Sentinel resource ID.

    Parameters
    ----------
    subscription_id : str
        Subscription ID to use
    resource_group : str
        Resource Group name to use
    workspace_name : str
        Workspace name to user

    Returns
    -------
    str
        The formatted resource ID.

    """
    resource_id: str = (
        f"/subscriptions/{subscription_id}/resourcegroups/{resource_group}"
        f"/providers/Microsoft.OperationalInsights/workspaces/{workspace_name}"
    )
    logger.info("Resource ID built: %s", resource_id)
    return resource_id


def extract_sentinel_response(
    items: dict,
    *,
    props: bool = False,
    etag: dict[str, str] | None = None,
) -> dict:
    """
    Build request data body from items.

    Parameters
    ----------
    items : dict
        A set pf items to be formated in the request body.
    props: bool, optional
        Whether all items are to be built as properities. Default is false.
    etag: dict[str, str], Optional
        If defined, set the etag body value

    Returns
    -------
    dict
        The request body formatted for the API.

    """
    data_body: dict[str, dict[str, str]] = {"properties": {}}
    for key in items:
        if key in ["severity", "status", "title", "message", "searchResults"] or props:
            data_body["properties"].update({key: items[key]})
        else:
            data_body[key] = items[key]
    if etag:
        data_body["etag"] = etag
    return data_body


def validate_resource_id(res_id: str) -> str:
    """Validate a Resource ID String and fix if needed."""
    valid: bool = _validator(res_id)
    if not valid:
        res_id = _fix_res_id(res_id)
        valid = _validator(res_id)
    if not valid:
        err_msg: str = "The Resource ID provided is not valid."
        raise MsticpyAzureConfigError(err_msg)

    return res_id


def parse_resource_id(res_id: str) -> dict[str, Any]:
    """Extract components from workspace resource ID."""
    if not res_id.startswith("/"):
        res_id = f"/{res_id}"
    res_id_parts: dict[str, Any] = az_tools.parse_resource_id(res_id)
    workspace_name: str | None = None
    if (
        res_id_parts.get("namespace") == "Microsoft.OperationalInsights"
        and res_id_parts.get("type") == "workspaces"
    ):
        workspace_name = res_id_parts.get("name")
    return {
        "subscription_id": res_id_parts.get("subscription"),
        "resource_group": res_id_parts.get("resource_group"),
        "workspace_name": workspace_name,
    }


def _validator(res_id: str) -> bool:
    """Check Resource ID string matches pattern expected."""
    return az_tools.is_valid_resource_id(res_id)


def _fix_res_id(res_id: str) -> str:
    """Try to fix common issues with Resource ID string."""
    if res_id.startswith("https:"):
        res_id = "/".join(res_id.split("/")[5:])
    if not res_id.startswith("/"):
        res_id = "/" + res_id
    if res_id.endswith("/"):
        res_id = res_id[:-1]
    counts = Counter(res_id)
    if counts["/"] > 8:
        res_id = "/".join(res_id.split("/")[:9])
    return res_id
