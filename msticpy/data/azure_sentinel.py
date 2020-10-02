# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Uses the Azure Python SDK to collect and return details related to Azure."""
from typing import Dict

import pandas as pd
import requests
from azure.common.exceptions import CloudError

from .azure_data import AzureData

_PATH_MAPPING = {
    "ops_path": "/providers/Microsoft.SecurityInsights/operations",
    "alert_rules": "/providers/Microsoft.SecurityInsights/alertRules",
    "ss_path": "/savedSearches",
    "bookmarks": "/providers/Microsoft.SecurityInsights/bookmarks",
}


class AzureSentinel(AzureData):
    """Class for reutrning key Azure Sentinel elements."""

    def __init__(self, connect: bool = False):
        """Initialize connector for Azure APIs."""
        super().__init__()

    def connect(
        self,
        client_id: str = None,
        tenant_id: str = None,
        secret: str = None,
        silent: bool = False,
    ):
        """Authenticate with the SDK & API."""
        super().connect()
        self.token = _get_token(self.credentials)
        self.res_group_url = None
        self.prov_path = None

    def get_sentinel_workspaces(self, sub_id: str) -> Dict:
        """
        Return a list of Azure Sentinel workspaces in a Subscription.

        Parameters
        ----------
        sub_id : str
            The subscription ID to get a list of workspaces from

        Returns
        -------
        Dict
            A dictionary of workspace names and ids

        """
        print("Finding Azure Sentinel Workspaces...")
        res = self.get_resources(sub_id=sub_id)
        # handle no results
        if isinstance(res, pd.DataFrame) and not res.empty:
            sentinel = res[
                (res["resource_type"] == "Microsoft.OperationsManagement/solutions")
                & (res["name"].str.startswith("SecurityInsights"))
            ]
            workspaces = []
            for wrkspace in sentinel["resource_id"]:
                res_details = self.get_resource_details(
                    sub_id=sub_id, resource_id=wrkspace
                )
                workspaces.append(res_details["properties"]["workspaceResourceId"])

            workspaces_dict = {}
            for wrkspace in workspaces:
                name = wrkspace.split("/")[-1]
                workspaces_dict.update({name: wrkspace})

            return workspaces_dict

        print(f"No Azure Sentinel workspaces in {sub_id}")
        return {}

    def get_hunting_queries(self, res_id) -> pd.DataFrame:
        """
        Return all hunting queries in an Azure Sentinel workspace.

        Parameters
        ----------
        res_id : str
            The resource ID of the Azure Sentinel workspace to get hunting queries from.

        Returns
        -------
        pd.DataFrame
            A table of the hunting queries.

        """
        url = _build_paths(res_id)
        saved_searches_url = url + _PATH_MAPPING["ss_path"]
        params = {"api-version": "2017-04-26-preview"}

        response = requests.get(
            saved_searches_url, headers=_get_api_headers(self.token), params=params
        )
        if response.status_code == 200:
            queries_df = _azs_api_result_to_df(response)
        else:
            raise CloudError("Could not get alert rules.")

        return queries_df[queries_df["Category"] == "Hunting Queries"]

    def get_alert_rules(self, res_id) -> pd.DataFrame:
        """
        Return all Azure Sentinel alert rules for a workspace.

        Parameters
        ----------
        res_id : str
            The resource ID of the Azure Sentinel workspace to get details from.

        Returns
        -------
        pd.DataFrame
            A table of the workspace's alert rules.

        """
        url = _build_paths(res_id)
        alert_rules_url = url + _PATH_MAPPING["alert_rules"]
        params = {"api-version": "2020-01-01"}

        response = requests.get(
            alert_rules_url, headers=_get_api_headers(self.token), params=params
        )
        if response.status_code == 200:
            alerts_df = _azs_api_result_to_df(response)
        else:
            raise CloudError("Could not get alert rules.")

        return alerts_df

    def get_bookmarks(self, res_id) -> pd.DataFrame:
        """
        Return all bookmarks from an Azure Sentinel workspace.

        Parameters
        ----------
        res_id : str
            The Azure Sentinel workspace to get bookmarks from.

        Returns
        -------
        pd.DataFrame
            A table of all bookmarks in a workspace

        """
        url = _build_paths(res_id)
        bookmarks_url = url + _PATH_MAPPING["bookmarks"]
        params = {"api-version": "2020-01-01"}

        response = requests.get(
            bookmarks_url, headers=_get_api_headers(self.token), params=params
        )
        if response.status_code == 200:
            bookmarks_df = _azs_api_result_to_df(response)
        else:
            raise CloudError("Could not get bookmarks.")

        return bookmarks_df


def _build_paths(resid) -> str:
    """Build a API URL from an Azure resource ID."""
    res_info = {
        "subscription_id": resid.split("/")[2],
        "resource_group": resid.split("/")[4],
        "workspace_name": resid.split("/")[-1],
    }

    url_part1 = f"https://management.azure.com/subscriptions/{res_info['subscription_id']}"
    url_part2 = f"/resourceGroups/{res_info['resource_group']}"
    url_part3 = f"/providers/Microsoft.OperationalInsights/workspaces/{res_info['workspace_name']}"

    return url_part1 + url_part2 + url_part3


def _get_token(credential) -> str:
    """Extract token from a azure.identity object."""
    token = credential.modern.get_token("https://management.azure.com/.default")
    return token.token


def _get_api_headers(token):
    """Return authorization header with current token."""
    return {
        "Authorization": f"Bearer {token}",
        "Content-Type": "application/json",
    }


def _azs_api_result_to_df(response) -> pd.DataFrame:
    """Convert API reponse to a Pandas dataframe."""
    j_resp = response.json()
    if response.status_code != 200 or not j_resp or "value" not in j_resp:
        raise ValueError("No valid JSON result in response")
    queries_raw_df = pd.DataFrame(j_resp["value"])
    query_props_df = pd.json_normalize(queries_raw_df["properties"])
    return pd.concat([queries_raw_df, query_props_df], axis=1).drop(
        columns="properties"
    )
