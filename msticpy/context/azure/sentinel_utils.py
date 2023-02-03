# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Mixin Classes for Sentinel Utilities."""
import re
from collections import Counter
from typing import Dict, List, Optional, Tuple, Union

import httpx
import pandas as pd

from ..._version import VERSION
from ...auth.azure_auth_core import AzureCloudConfig
from ...common.exceptions import MsticpyAzureConfigError, MsticpyAzureConnectionError
from ...common.pkg_config import get_http_timeout
from ...common.wsconfig import WorkspaceConfig
from .azure_data import get_api_headers

__version__ = VERSION
__author__ = "Pete Bryan"

_PATH_MAPPING = {
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

RTD_SENTINEL_URL = (
    "https://msticpy.readthedocs.io/en/latest/data_acquisition/Sentinel.html"
)


class SentinelUtilsMixin:
    """Mixin class for Sentinel core feature integrations."""

    def _get_items(self, url: str, params: Optional[dict] = None) -> httpx.Response:
        """Get items from the API."""
        self.check_connected()  # type: ignore
        if params is None:
            params = {"api-version": "2020-01-01"}
        return httpx.get(
            url,
            headers=get_api_headers(self.token),  # type: ignore
            params=params,
            timeout=get_http_timeout(),
        )

    def _list_items(
        self,
        item_type: str,
        api_version: str = "2020-01-01",
        appendix: Optional[str] = None,
        next_follow: bool = False,
        params: Optional[Dict[str, Any]] = None,
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
        MsticpyAzureConnectionError
            If a valid result is not returned.

        """
        item_url = self.url + _PATH_MAPPING[item_type]  # type: ignore
        if appendix:
            item_url = item_url + appendix
        if params is None:
            params = {}
        params["api-version"] = api_version
        response = self._get_items(item_url, params)
        if response.status_code == 200:
            results_df = _azs_api_result_to_df(response)
        else:
            raise MsticpyAzureConnectionError(
                f"Azure error: {response.content.decode('utf-8')}",
                f"HTTP Error: {response.status_code}",
                title="Error response from Azure API",
                help_uri=RTD_SENTINEL_URL,
            )
        j_resp = response.json()
        results = [results_df]
        # If nextLink in response, go get that data as well
        if next_follow:
            i = 0
            # Limit to 5 nextLinks to prevent infinite loop
            while "nextLink" in j_resp and i < 5:
                next_url = j_resp["nextLink"]
                next_response = self._get_items(next_url, params)
                next_results_df = _azs_api_result_to_df(next_response)
                results.append(next_results_df)
                j_resp = next_response.json()
                i += 1
        return pd.concat(results)

    def _check_config(self, items: List, workspace_name: Optional[str] = None) -> Dict:
        """
        Get parameters from default config files.

        Parameters
        ----------
        items : List
            The items to get from the config.
        workspace_name : str
            The workspace name supplied by the user.

        Returns
        -------
        Dict
            The config items.

        """
        config_items = {}
        if not self.workspace_config:  # type: ignore
            self.workspace_config = WorkspaceConfig(workspace_name)  # type: ignore
        for item in items:
            if item in self.workspace_config:  # type: ignore
                config_items[item] = self.workspace_config[item]  # type: ignore
            else:
                raise MsticpyAzureConfigError(
                    f"No {item} available in config for workspace {workspace_name}."
                )

        return config_items

    @staticmethod
    def build_sentinel_resource_id(
        subscription_id: str, resource_group: str, workspace_name: str
    ) -> str:
        """
        Build MS Sentinel resource ID from components.

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
        return "".join(
            [
                f"/subscriptions/{subscription_id}",
                f"/resourcegroups/{resource_group}",
                f"/providers/Microsoft.OperationalInsights/workspaces/{workspace_name}",
            ]
        )

    def _build_workspace_url(self, res_id: str, base_url: str = None) -> str:
        """
        Build an API URL from an Azure resource ID.

        Parameters
        ----------
        res_id : str
            An Azure resource ID.
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
            base_url = AzureCloudConfig(self.cloud).endpoints.resource_manager  # type: ignore
        norm_res_id = self.build_sentinel_resource_id(**(parse_resource_id(res_id)))
        if base_url.endswith("/") and norm_res_id.startswith("/"):  # type: ignore
            norm_res_id = norm_res_id[1:]
        return f"{base_url}{norm_res_id}"

    def check_connected(self):
        """Check that Sentinel workspace is connected."""
        if not self.connected:  # type: ignore
            raise MsticpyAzureConnectionError(
                "Not connected to Sentinel, ensure you run `.connect` before calling functions."
            )


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
    j_resp = response.json()
    if response.status_code != 200 or not j_resp:
        raise ValueError("No valid JSON result in response")
    if "value" in j_resp:
        j_resp = j_resp["value"]
    return pd.json_normalize(j_resp)


def build_sentinel_api_req_data(items: dict, props: bool = False, **kwargs) -> dict:
    """
    Build request data body from items.

    Parameters
    ----------
    items : dict
        A set pf items to be formatted in the request body.
    props: bool, optional
        Whether all items are to be built as properties. Default is false.

    Returns
    -------
    dict
        The request body formatted for the API.

    """
    data_body = {"properties": {}}  # type: Dict[str, Dict[str, str]]
    for key in items:
        if key in ["severity", "status", "title", "message", "searchResults"] or props:
            data_body["properties"].update({key: items[key]})  # type:ignore
        else:
            data_body[key] = items[key]
    if "etag" in kwargs:
        data_body["etag"] = kwargs.get("etag")  # type:ignore
    return data_body


def validate_res_id(res_id) -> str:
    """Validate a Resource ID String and fix if needed."""
    valid = _validator(res_id)
    if not valid:
        res_id = _fix_res_id(res_id)
        valid = _validator(res_id)
    if not valid:
        raise MsticpyAzureConfigError("The Resource ID provided is not valid.")
    return res_id


_workspace_regex = re.compile(
    (
        r".*subscriptions/(?P<subscription_id>[^/]+)"
        r"(/resourceGroups/(?P<resource_group>[^/]+))?"
        r"(/.*/workspaces/(?P<workspace_name>\w+))?"
    ),
    re.IGNORECASE,
)


def parse_resource_id(res_id: str) -> Dict[str, str]:
    """Extract components from workspace resource ID."""
    res_id_match = _workspace_regex.search(res_id)
    if res_id_match:
        return res_id_match.groupdict()
    raise ValueError(f"Invalid format for Azure resource ID: {res_id}")


def _validator(res_id):
    """Check Resource ID string matches pattern expected."""
    counts = Counter(res_id)
    return bool(
        res_id.startswith("/") and counts["/"] == 8 and not res_id.endswith("/")
    )


def _fix_res_id(res_id):
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


def get_config_for_workspace_name(
    workspace_name: str,
) -> Union[Tuple[str, Dict[str, str]], Tuple[None, None]]:
    """Return WorkspaceConfig for matching key or WorkspaceName value."""
    return next(
        (
            (workspace, config)
            for workspace, config in WorkspaceConfig.list_workspaces().items()
            if workspace_name
            in (
                workspace,
                config.get(WorkspaceConfig.CONF_WS_NAME_KEY),
            )
        ),
        (None, None),
    )


def workspace_in_config(workspace_name: str):
    """Return True if workspace name found in a config entry."""
    return any(
        workspace_name
        in (
            workspace,
            config.get(WorkspaceConfig.CONF_WS_NAME_KEY),
        )
        for workspace, config in WorkspaceConfig.list_workspaces().items()
    )
