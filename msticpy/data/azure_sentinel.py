# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Uses the Azure Python SDK to collect and return details related to Azure."""
from typing import Dict, List
from uuid import uuid4

import pandas as pd
import requests
from azure.common.exceptions import CloudError

from .azure_data import AzureData
from ..common.azure_auth_core import AzCredentials
from ..common.exceptions import MsticpyAzureConfigError
from ..common.wsconfig import WorkspaceConfig

_PATH_MAPPING = {
    "ops_path": "/providers/Microsoft.SecurityInsights/operations",
    "alert_rules": "/providers/Microsoft.SecurityInsights/alertRules",
    "ss_path": "/savedSearches",
    "bookmarks": "/providers/Microsoft.SecurityInsights/bookmarks",
    "incidents": "/providers/Microsoft.SecurityInsights/incidents",
}


class AzureSentinel(AzureData):
    """Class for returning key Azure Sentinel elements."""

    def __init__(self, connect: bool = False):
        """Initialize connector for Azure APIs.

        Parameters
        ----------
        connect : bool, optional
            Set true if you want to connect to API on initalization, by default False
        """
        super().__init__()
        self.config = None

    def connect(self, auth_methods: List = None, silent: bool = False, **kwargs):
        """Authenticate with the SDK & API.

        Parameters
        ----------
        auth_methods : List, optional
            list of prefered authentication methods to use, by default None
        silent : bool, optional
            Set true to prevent output during auth process, by default False
        """
        super().connect(auth_methods=auth_methods, silent=silent)
        if "token" in kwargs:
            self.token = kwargs["token"]
        else:
            self.token = _get_token(self.credentials)

        self.res_group_url = None
        self.prov_path = None

    def get_sentinel_workspaces(self, sub_id: str = None) -> Dict:
        """
        Return a list of Azure Sentinel workspaces in a Subscription.

        Parameters
        ----------
        sub_id : str
            The subscription ID to get a list of workspaces from.
            If not provided it will attempt to get sub_id from config files.

        Returns
        -------
        Dict
            A dictionary of workspace names and ids

        """
        # If a subscription ID isn't provided try and get one from config files.
        if not sub_id:
            config = self._check_config(["subscription_id"])
            sub_id = config["subscription_id"]

        print("Finding Azure Sentinel Workspaces...")
        res = self.get_resources(sub_id=sub_id)  # type: ignore
        # handle no results
        if isinstance(res, pd.DataFrame) and not res.empty:
            sentinel = res[
                (res["resource_type"] == "Microsoft.OperationsManagement/solutions")
                & (res["name"].str.startswith("SecurityInsights"))
            ]
            workspaces = []
            for wrkspace in sentinel["resource_id"]:
                res_details = self.get_resource_details(
                    sub_id=sub_id, resource_id=wrkspace  # type: ignore
                )
                workspaces.append(res_details["properties"]["workspaceResourceId"])

            workspaces_dict = {}
            for wrkspace in workspaces:
                name = wrkspace.split("/")[-1]
                workspaces_dict.update({name: wrkspace})

            return workspaces_dict

        print(f"No Azure Sentinel workspaces in {sub_id}")
        return {}

    def get_hunting_queries(
        self,
        res_id: str = None,
        sub_id: str = None,
        res_grp: str = None,
        ws_name: str = None,
    ) -> pd.DataFrame:
        """
        Return all hunting queries in an Azure Sentinel workspace.

        Parameters
        ----------
        res_id : str, optional
            Resource ID of the workspace, if not provided details from config file will be used.
        sub_id : str, optional
            Sub ID of the workspace, to be used if not providing Resource ID.
        res_grp : str, optional
            Resource Group name of the workspace, to be used if not providing Resource ID.
        ws_name : str, optional
            Workspace name of the workspace, to be used if not providing Resource ID.

        Returns
        -------
        pd.DataFrame
            A table of the hunting queries.

        """
        # If res_id isn't provided try and get them from config
        if not res_id:
            if not sub_id or not res_grp or not ws_name:
                config = self._check_config(
                    ["subscription_id", "resource_group", "workspace_name"]
                )
                sub_id = config["subscription_id"]
                res_grp = config["resource_group"]
                ws_name = config["workspace_name"]
            res_id = f"/subscriptions/{sub_id}/resourcegroups/{res_grp}"
            res_id = (
                res_id
                + f"/providers/Microsoft.OperationalInsights/workspaces/{ws_name}"
            )

        url = _build_paths(res_id)
        saved_searches_url = url + _PATH_MAPPING["ss_path"]
        params = {"api-version": "2017-04-26-preview"}

        response = requests.get(
            saved_searches_url, headers=_get_api_headers(self.token), params=params
        )
        if response.status_code == 200:
            queries_df = _azs_api_result_to_df(response)
        else:
            raise CloudError(response=response)

        return queries_df[queries_df["properties.Category"] == "Hunting Queries"]

    def get_alert_rules(
        self,
        res_id: str = None,
        sub_id: str = None,
        res_grp: str = None,
        ws_name: str = None,
    ) -> pd.DataFrame:
        """
        Return all Azure Sentinel alert rules for a workspace.

        Parameters
        ----------
        res_id : str, optional
            Resource ID of the workspace, if not provided details from config file will be used.
        sub_id : str, optional
            Sub ID of the workspace, to be used if not providing Resource ID.
        res_grp : str, optional
            Resource Group name of the workspace, to be used if not providing Resource ID.
        ws_name : str, optional
            Workspace name of the workspace, to be used if not providing Resource ID.

        Returns
        -------
        pd.DataFrame
            A table of the workspace's alert rules.

        """
        if not res_id:
            if not sub_id or not res_grp or not ws_name:
                config = self._check_config(
                    ["subscription_id", "resource_group", "workspace_name"]
                )
                sub_id = config["subscription_id"]
                res_grp = config["resource_group"]
                ws_name = config["workspace_name"]
            res_id = f"/subscriptions/{sub_id}/resourcegroups/{res_grp}"
            res_id = (
                res_id
                + f"/providers/Microsoft.OperationalInsights/workspaces/{ws_name}"
            )

        url = _build_paths(res_id)
        alert_rules_url = url + _PATH_MAPPING["alert_rules"]
        params = {"api-version": "2020-01-01"}

        response = requests.get(
            alert_rules_url, headers=_get_api_headers(self.token), params=params
        )
        if response.status_code == 200:
            alerts_df = _azs_api_result_to_df(response)
        else:
            raise CloudError(response=response)

        return alerts_df

    def get_bookmarks(
        self,
        res_id: str = None,
        sub_id: str = None,
        res_grp: str = None,
        ws_name: str = None,
    ) -> pd.DataFrame:
        """
        Return a list of Bookmarks from a Sentinel workspace.

        Parameters
        ----------
        res_id : str, optional
            Resource ID of the workspace, if not provided details from config file will be used.
        sub_id : str, optional
            Sub ID of the workspace, to be used if not providing Resource ID.
        res_grp : str, optional
            Resource Group name of the workspace, to be used if not providing Resource ID.
        ws_name : str, optional
            Workspace name of the workspace, to be used if not providing Resource ID.

        Returns
        -------
        pd.DataFrame
            A set of bookmarks.

        Raises
        ------
        CloudError
            If bookmark collection fails.

        """
        if not res_id:
            if not sub_id or not res_grp or not ws_name:
                config = self._check_config(
                    ["subscription_id", "resource_group", "workspace_name"]
                )
                sub_id = config["subscription_id"]
                res_grp = config["resource_group"]
                ws_name = config["workspace_name"]
            res_id = f"/subscriptions/{sub_id}/resourcegroups/{res_grp}"
            res_id = (
                res_id
                + f"/providers/Microsoft.OperationalInsights/workspaces/{ws_name}"
            )

        url = _build_paths(res_id)
        bookmarks_url = url + _PATH_MAPPING["bookmarks"]
        params = {"api-version": "2020-01-01"}

        response = requests.get(
            bookmarks_url, headers=_get_api_headers(self.token), params=params
        )
        if response.status_code == 200:
            bookmarks_df = _azs_api_result_to_df(response)
        else:
            raise CloudError(response=response)

        return bookmarks_df

    def get_incidents(
        self,
        res_id: str = None,
        sub_id: str = None,
        res_grp: str = None,
        ws_name: str = None,
    ) -> pd.DataFrame:
        """
        Get a list of incident for a Sentinel workspace.

        Parameters
        ----------
        res_id : str, optional
            Resource ID of the workspace, if not provided details from config file will be used.
        sub_id : str, optional
            Sub ID of the workspace, to be used if not providing Resource ID.
        res_grp : str, optional
            Resource Group name of the workspace, to be used if not providing Resource ID.
        ws_name : str, optional
            Workspace name of the workspace, to be used if not providing Resource ID.

        Returns
        -------
        pd.DataFrame
            A table of incidents.

        Raises
        ------
        CloudError
            If incidents could not be retreived.

        """
        if not res_id:
            if not sub_id or not res_grp or not ws_name:
                config = self._check_config(
                    ["subscription_id", "resource_group", "workspace_name"]
                )
                sub_id = config["subscription_id"]
                res_grp = config["resource_group"]
                ws_name = config["workspace_name"]
            res_id = f"/subscriptions/{sub_id}/resourcegroups/{res_grp}"
            res_id = (
                res_id
                + f"/providers/Microsoft.OperationalInsights/workspaces/{ws_name}"
            )
        url = _build_paths(res_id)
        incidents_url = url + _PATH_MAPPING["incidents"]
        params = {"api-version": "2020-01-01"}
        response = requests.get(
            incidents_url, headers=_get_api_headers(self.token), params=params
        )
        if response.status_code == 200:
            incidents_df = _azs_api_result_to_df(response)
        else:
            raise CloudError(response=response)

        return incidents_df

    def get_incident(
        self,
        incident_id: str,
        res_id: str = None,
        sub_id: str = None,
        res_grp: str = None,
        ws_name: str = None,
        entities: bool = False,
    ) -> pd.DataFrame:
        """
        Get details on a specific incident.

        Parameters
        ----------
        incident_id : str
            Incident ID GUID.
        res_id : str, optional
            Resource ID of the workspace, if not provided details from config file will be used.
        sub_id : str, optional
            Sub ID of the workspace, to be used if not providing Resource ID.
        res_grp : str, optional
            Resource Group name of the workspace, to be used if not providing Resource ID.
        ws_name : str, optional
            Workspace name of the workspace, to be used if not providing Resource ID.
        entities : bool, optional
            If True, include all entities in the response. Default is False.

        Returns
        -------
        pd.DataFrame
            Table containing incident details.

        Raises
        ------
        CloudError
            If incident could not be retrieved.

        """
        if not res_id:
            if not sub_id or not res_grp or not ws_name:
                config = self._check_config(
                    ["subscription_id", "resource_group", "workspace_name"]
                )
                sub_id = config["subscription_id"]
                res_grp = config["resource_group"]
                ws_name = config["workspace_name"]
            res_id = f"/subscriptions/{sub_id}/resourcegroups/{res_grp}"
            res_id = (
                res_id
                + f"/providers/Microsoft.OperationalInsights/workspaces/{ws_name}"
            )
        url = _build_paths(res_id)
        incidents_url = url + _PATH_MAPPING["incidents"]
        incident_url = incidents_url + f"/{incident_id}"
        params = {"api-version": "2020-01-01"}
        response = requests.get(
            incident_url, headers=_get_api_headers(self.token), params=params
        )
        if response.status_code == 200:
            incident_df = _azs_api_result_to_df(response)
        else:
            raise CloudError(response=response)

        if entities:
            entities_url = incident_url + "/entities"
            ent_parameters = {"api-version": "2019-01-01-preview"}
            ents = requests.post(
                entities_url,
                headers=_get_api_headers(self.token),
                params=ent_parameters,
            )
            if ents.status_code == 200:
                unique_entities = [
                    (ent["kind"], ent["properties"]) for ent in ents.json()["entities"]
                ]
                incident_df["Entities"] = [unique_entities]

        return incident_df

    def update_incident(
        self,
        incident_id: str,
        update_items: dict,
        res_id: str = None,
        sub_id: str = None,
        res_grp: str = None,
        ws_name: str = None,
    ):
        """
        Update properties of an incident.

        Parameters
        ----------
        incident_id : str
            Incident ID GUID.
        update_items : dict
            Dictionary of properties to update and their values.
            Ref: https://docs.microsoft.com/en-us/rest/api/securityinsights/incidents/createorupdate
        res_id : str, optional
            Resource ID of the workspace, if not provided details from config file will be used.
        sub_id : str, optional
            Sub ID of the workspace, to be used if not providing Resource ID.
        res_grp : str, optional
            Resource Group name of the workspace, to be used if not providing Resource ID.
        ws_name : str, optional
            Workspace name of the workspace, to be used if not providing Resource ID.

        Raises
        ------
        CloudError
            If incident could not be updated.

        """
        if not res_id:
            if not sub_id or not res_grp or not ws_name:
                config = self._check_config(
                    ["subscription_id", "resource_group", "workspace_name"]
                )
                sub_id = config["subscription_id"]
                res_grp = config["resource_group"]
                ws_name = config["workspace_name"]
            res_id = f"/subscriptions/{sub_id}/resourcegroups/{res_grp}"
            res_id = (
                res_id
                + f"/providers/Microsoft.OperationalInsights/workspaces/{ws_name}"
            )

        incident_dets = self.get_incident(incident_id=incident_id, res_id=res_id)
        url = _build_paths(res_id)
        incidents_url = url + _PATH_MAPPING["incidents"]
        incident_url = incidents_url + f"/{incident_id}"
        params = {"api-version": "2020-01-01"}
        if "title" not in update_items.keys():
            update_items.update({"title": incident_dets.iloc[0]["properties.title"]})
        if "status" not in update_items.keys():
            update_items.update({"status": incident_dets.iloc[0]["properties.status"]})
        data = _build_data(update_items, etag=incident_dets.iloc[0]["etag"])
        response = requests.put(
            incident_url,
            headers=_get_api_headers(self.token),
            params=params,
            data=str(data),
        )
        if response.status_code == 200:
            print("Incident updated.")
        else:
            raise CloudError(response=response)

    def post_comment(
        self,
        incident_id: str,
        comment: str,
        res_id: str = None,
        sub_id: str = None,
        res_grp: str = None,
        ws_name: str = None,
    ):
        """
        Write a comment for an incident.

        Parameters
        ----------
        incident_id : str
            Incident ID GUID.
        comment : str
            Comment message to post.
        res_id : str, optional
            Resource ID of the workspace, if not provided details from config file will be used.
        sub_id : str, optional
            Sub ID of the workspace, to be used if not providing Resource ID.
        res_grp : str, optional
            Resource Group name of the workspace, to be used if not providing Resource ID.
        ws_name : str, optional
            Workspace name of the workspace, to be used if not providing Resource ID.

        Raises
        ------
        CloudError
            If message could not be posted.

        """
        if not res_id:
            if not sub_id or not res_grp or not ws_name:
                config = self._check_config(
                    ["subscription_id", "resource_group", "workspace_name"]
                )
                sub_id = config["subscription_id"]
                res_grp = config["resource_group"]
                ws_name = config["workspace_name"]
            res_id = f"/subscriptions/{sub_id}/resourcegroups/{res_grp}"
            res_id = (
                res_id
                + f"/providers/Microsoft.OperationalInsights/workspaces/{ws_name}"
            )
        url = _build_paths(res_id)
        incident_url = url + _PATH_MAPPING["incidents"]
        comment_url = incident_url + f"/{incident_id}/comments/{str(uuid4())}"
        params = {"api-version": "2020-01-01"}
        data = _build_data({"message": comment})
        response = requests.put(
            comment_url,
            headers=_get_api_headers(self.token),
            params=params,
            data=str(data),
        )
        if response.status_code == 201:
            print("Comment posted.")
        else:
            raise CloudError(response=response)

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
        if not self.config:
            self.config = WorkspaceConfig()  # type: ignore
        for item in items:
            if item in self.config:  # type: ignore
                config_items.update({item: self.config[item]})  # type: ignore
            else:
                raise MsticpyAzureConfigError(f"No {item} avaliable in config.")

        return config_items


def _build_paths(resid: str) -> str:
    """Build a API URL from an Azure resource ID.

    Parameters
    ----------
    resid : str
        An Azure resource ID.

    Returns
    -------
    str
        A URI to that resource.
    """
    res_info = {
        "subscription_id": resid.split("/")[2],
        "resource_group": resid.split("/")[4],
        "workspace_name": resid.split("/")[-1],
    }

    url_part1 = (
        f"https://management.azure.com/subscriptions/{res_info['subscription_id']}"
    )
    url_part2 = f"/resourceGroups/{res_info['resource_group']}"
    url_part3 = f"/providers/Microsoft.OperationalInsights/workspaces/{res_info['workspace_name']}"

    return url_part1 + url_part2 + url_part3


def _get_token(credential: AzCredentials) -> str:
    """Extract token from a azure.identity object.

    Parameters
    ----------
    credential : AzCredentials
        Azure OAuth credentials.

    Returns
    -------
    str
        A token to be used in API calls.
    """
    token = credential.modern.get_token("https://management.azure.com/.default")
    return token.token


def _get_api_headers(token: str) -> Dict:
    """Return authorization header with current token.

    Parameters
    ----------
    token : str
        Auzre auth token.

    Returns
    -------
    Dict
        A dictionary of headers to be used in API calls.
    """
    return {
        "Authorization": f"Bearer {token}",
        "Content-Type": "application/json",
    }


def _azs_api_result_to_df(response: requests.Response) -> pd.DataFrame:
    """Convert API response to a Pandas dataframe.

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


def _build_data(items: dict, **kwargs) -> dict:
    """Build request data body from items.

    Parameters
    ----------
    items : dict
        A set pf items to be formated in the request body.

    Returns
    -------
    dict
        The request body formatted for the API.
    """
    data_body = {"properties": {}}  # type: Dict[str, Dict[str, str]]
    for key in items.keys():
        if key in ["severity", "status", "title", "message"]:
            data_body["properties"].update({key: items[key]})  # type:ignore
        else:
            data_body.update({key: items[key]})

    if "etag" in kwargs:
        data_body.update({"etag": kwargs.get("etag")})  # type:ignore

    return data_body
