# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Mixin Classes for Sentinel Utilties."""
from collections import Counter
from typing import Dict, List

import pandas as pd
import requests

from azure.common.exceptions import CloudError

from ..._version import VERSION
from ...common.exceptions import MsticpyAzureConfigError
from ...common.azure_auth_core import AzureCloudConfig
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
}


# pylint: disable=too-few-public-methods
class SentinelUtilsMixin:
    """Mixin class for Sentinel core feature integrations."""

    def _get_items(self, url: str, params: str = "2020-01-01") -> requests.Response:
        """Get items from the API."""
        return requests.get(
            url,
            headers=get_api_headers(self.token),  # type: ignore
            params={"api-version": params},
        )

    def _list_items(
        self,
        item_type: str,
        api_version: str = "2020-01-01",
        appendix: str = None,
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

        Returns
        -------
        pd.DataFrame
            A DataFrame containing the requested items.

        Raises
        ------
        CloudError
            If a valid result is not returned.

        """
        item_url = self.url + _PATH_MAPPING[item_type]  # type: ignore
        if appendix:
            item_url = item_url + appendix
        response = self._get_items(item_url, api_version)
        if response.status_code == 200:
            results_df = _azs_api_result_to_df(response)
        else:
            raise CloudError(response=response)
        return results_df

    def _check_config(self, items: List) -> Dict:
        """
        Get parameters from default config files.

        Parameters
        ----------
        items : List
            The items to get from the config.

        Returns
        -------
        Dict
            The config items.

        """
        config_items = {}
        if not self.config:  # type: ignore
            self.config = WorkspaceConfig()
        for item in items:
            if item in self.config:
                config_items[item] = self.config[item]
            else:
                raise MsticpyAzureConfigError(f"No {item} avaliable in config.")

        return config_items

    def _build_sent_res_id(
        self, sub_id: str = None, res_grp: str = None, ws_name: str = None
    ) -> str:
        """
        Build a resource ID.

        Parameters
        ----------
        sub_id : str, optional
            Subscription ID to use, by default None
        res_grp : str, optional
            Resource Group name to use, by default None
        ws_name : str, optional
            Workspace name to user, by default None

        Returns
        -------
        str
            The formatted resource ID.

        """
        if not sub_id or not res_grp or not ws_name:
            config = self._check_config(
                ["subscription_id", "resource_group", "workspace_name"]
            )
            sub_id = config["subscription_id"]
            res_grp = config["resource_group"]
            ws_name = config["workspace_name"]
        return "".join(
            [
                f"/subscriptions/{sub_id}/resourcegroups/{res_grp}",
                f"/providers/Microsoft.OperationalInsights/workspaces/{ws_name}",
            ]
        )

    def _build_sent_paths(self, res_id: str, base_url: str = None) -> str:
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
        res_info = {
            "subscription_id": res_id.split("/")[2],
            "resource_group": res_id.split("/")[4],
            "workspace_name": res_id.split("/")[-1],
        }

        return "".join(
            [
                f"{base_url}/subscriptions/{res_info['subscription_id']}",
                f"/resourceGroups/{res_info['resource_group']}",
                "/providers/Microsoft.OperationalInsights/workspaces"
                f"/{res_info['workspace_name']}",
            ]
        )


def _azs_api_result_to_df(response: requests.Response) -> pd.DataFrame:
    """
    Convert API response to a Pandas dataframe.

    Parameters
    ----------
    response : requests.Response
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


def _build_sent_data(items: dict, props: bool = False, **kwargs) -> dict:
    """
    Build request data body from items.

    Parameters
    ----------
    items : dict
        A set pf items to be formated in the request body.
    props: bool, optional
        Whether all items are to be built as properities. Default is false.

    Returns
    -------
    dict
        The request body formatted for the API.

    """
    data_body = {"properties": {}}  # type: Dict[str, Dict[str, str]]
    for key, _ in items.items():
        if key in ["severity", "status", "title", "message"] or props:
            data_body["properties"].update({key: items[key]})
        else:
            data_body[key] = items[key]
    if "etag" in kwargs:
        data_body["etag"] = kwargs.get("etag")  # type:ignore
    return data_body


def validate_res_id(res_id):
    """Validate a Resource ID String and fix if needed."""
    valid = _validator(res_id)
    if not valid:
        res_id = _fix_res_id(res_id)
        valid = _validator(res_id)
    if not valid:
        raise MsticpyAzureConfigError("The Resource ID provided is not valid.")

    return res_id


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
