# pylint: disable=too-many-lines
# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Uses the Microsoft Sentinel APIs to interact with Microsoft Sentinel Workspaces."""
from datetime import datetime
from typing import Dict, List, Optional, Tuple, Union
from uuid import UUID, uuid4

import pandas as pd
import requests
from azure.common.exceptions import CloudError
from IPython.core.display import display

from ..common.azure_auth_core import AzCredentials, AzureCloudConfig
from ..common.exceptions import MsticpyAzureConfigError, MsticpyUserError
from ..common.wsconfig import WorkspaceConfig
from .azure_data import AzureData, get_api_headers, validate_res_id

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


class AzureSentinel(AzureData):  # pylint: disable=too-many-public-methods
    """Class for returning key Microsoft Sentinel elements."""

    def __init__(
        self,
        connect: bool = False,
        cloud: str = None,
        res_id: str = None,
        sub_id: str = None,
        res_grp: str = None,
        ws_name: str = None,
    ):
        """
        Initialize connector for Azure APIs.

        Parameters
        ----------
        connect : bool, optional
            Set true if you want to connect to API on initialization, by default False
        cloud : str, optional
            Specify cloud to use, overriding any configuration value.
            Default is to use configuration setting or public cloud if no
            configuration setting is available.
        res_id : str, optional
            Set the Sentinel workspace resource ID you want to use, if not specified
            defaults will be looked for or details can be passed seperately with functions.
        sub_id : str, optional
            If not specifying a resource ID the Subscription ID of the Sentinel Workspace
            by default None
        res_grp : str, optional
            If not specifying a resource ID the Resource Group name of the
            Sentinel Workspace, by default None
        ws_name : str, optional
            If not specifying a resource ID the Workspace name of the
            Sentinel Workspace, by default None

        """
        super().__init__(connect=connect, cloud=cloud)
        self.config = None
        self.base_url = self.endpoints.resource_manager
        self.default_subscription: Optional[str] = None
        self.default_workspace: Optional[Tuple[str, str]] = None
        res_id = res_id or self._get_default_workspace()
        if not res_id:
            res_id = self._build_sent_res_id(sub_id, res_grp, ws_name)
        res_id = validate_res_id(res_id)
        self.url = self._build_sent_paths(res_id, self.base_url)
        self.sent_urls = {
            "bookmarks": self.url + _PATH_MAPPING["bookmarks"],
            "incidents": self.url + _PATH_MAPPING["incidents"],
            "alert_rules": self.url + _PATH_MAPPING["alert_rules"],
            "watchlists": self.url + _PATH_MAPPING["watchlists"],
        }

    def connect(self, auth_methods: List = None, silent: bool = False, **kwargs):
        """
        Authenticate with the SDK & API.

        Parameters
        ----------
        auth_methods : List, optional
            list of preferred authentication methods to use, by default None
        silent : bool, optional
            Set true to prevent output during auth process, by default False

        """
        super().connect(auth_methods=auth_methods, silent=silent)
        if "token" in kwargs:
            self.token = kwargs["token"]
        else:
            self.token = _get_token(self.credentials)  # type: ignore

        self.res_group_url = None
        self.prov_path = None

    def set_default_subscription(self, subscription_id: str):
        """Set the default subscription to use to `subscription_id`."""
        subs_df = self.get_subscriptions()
        if subscription_id in subs_df["Subscription ID"].values:
            self.default_subscription = subscription_id
        else:
            print(f"Subscription ID {subscription_id} not found.")
            print(
                f"Subscriptions found: {', '.join(subs_df['Subscription ID'].values)}"
            )

    def list_sentinel_workspaces(self, sub_id: str = None) -> Dict[str, str]:
        """
        Return a list of Microsoft Sentinel workspaces in a Subscription.

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
        sub_id = sub_id or self.default_subscription
        if not sub_id:
            config = self._check_config(["subscription_id"])
            sub_id = config["subscription_id"]

        print("Finding Microsoft Sentinel Workspaces...")
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
                workspaces_dict[name] = wrkspace
            return workspaces_dict

        print(f"No Microsoft Sentinel workspaces in {sub_id}")
        return {}

    def set_default_workspace(
        self, sub_id: Optional[str], workspace: Optional[str] = None
    ):
        """
        Set the default workspace.

        Parameters
        ----------
        sub_id : Optional[str], optional
            Subscription ID containing the workspace. If not specified,
            the subscription will be taken from the `default_subscription`
            or from configuration.
        workspace : Optional[str], optional
            Name of the workspace, by default None.
            If not specified and there is only one workspace in the
            subscription, this will be set as the default.

        """
        sub_id = sub_id or self.default_subscription
        workspaces = self.get_sentinel_workspaces(sub_id=sub_id)
        if len(workspaces) == 1:
            self.default_workspace = next(iter(workspaces.items()))
        elif workspace in workspaces:
            self.default_workspace = workspace, workspaces[workspace]

    def _get_default_workspace(self):
        """Return the default workspace ResourceID."""
        if self.default_workspace:
            return self.default_workspace[0]
        return None

    def list_hunting_queries(self) -> pd.DataFrame:
        """
        Return all hunting queries in a Microsoft Sentinel workspace.

        Returns
        -------
        pd.DataFrame
            A table of the hunting queries.

        """
        saved_query_df = self._list_items(
            item_type="alert_rules", api_version="2017-04-26-preview"
        )
        return saved_query_df[
            saved_query_df["properties.Category"] == "Hunting Queries"
        ]

    def list_alert_rules(self) -> pd.DataFrame:
        """
        Return all Microsoft Sentinel alert rules for a workspace.

        Returns
        -------
        pd.DataFrame
            A table of the workspace's alert rules.

        """
        return self._list_items(item_type="alert_rules")

    def list_bookmarks(self) -> pd.DataFrame:
        """
        Return a list of Bookmarks from a Sentinel workspace.

        Returns
        -------
        pd.DataFrame
            A set of bookmarks.

        """
        return self._list_items(item_type="bookmarks")

    def create_bookmark(
        self,
        name: str,
        query: str,
        results: str = None,
        notes: str = None,
        labels: List[str] = None,
    ):
        """
        Create a bookmark in the Sentinel Workpsace.

        Parameters
        ----------
        name : str
            The name of the bookmark to use
        query : str
            The KQL query for the bookmark
        results : str, optional
            The results of the query to include with the bookmark
        notes : str, optional
            Any notes you want associated with the bookmark, by default None
        labels : List[str], optional
            Any labels you want associated with the bookmark, by default None

        Raises
        ------
        CloudError
            If API retunrs an error.

        """
        # Generate or use resource ID
        bkmark_id = str(uuid4())
        bookmark_url = self.sent_urls["bookmarks"] + f"/{bkmark_id}"
        data_items = {
            "displayName": name,
            "query": query,
        }  # type: Dict[str, Union[str, List]]
        if results:
            data_items["queryResults"] = results
        if notes:
            data_items["notes"] = notes
        if labels:
            data_items["labels"] = labels
        data = _build_sent_data(data_items, props=True)
        params = {"api-version": "2020-01-01"}
        response = requests.put(
            bookmark_url,
            headers=get_api_headers(self.token),
            params=params,
            data=str(data),
        )
        if response.status_code == 200:
            print("Bookmark created.")
        else:
            raise CloudError(response=response)

    def delete_bookmark(
        self,
        bookmark: str,
    ):
        """
        Delete the selected bookmark.

        Parameters
        ----------
        bookmark: str, optional
            The name or GIUD of the bookmark to delete.

        Raises
        ------
        CloudError
            If the API returns an error.

        """
        bookmark_id = self._get_bookmark_id(bookmark)
        bookmark_url = self.sent_urls["bookmarks"] + f"/{bookmark_id}"
        params = {"api-version": "2020-01-01"}
        response = requests.delete(
            bookmark_url,
            headers=get_api_headers(self.token),
            params=params,
        )
        if response.status_code == 200:
            print("Bookmark deleted.")
        else:
            raise CloudError(response=response)

    def list_incidents(self) -> pd.DataFrame:
        """
        Get a list of incident for a Sentinel workspace.

        Returns
        -------
        pd.DataFrame
            A table of incidents.

        Raises
        ------
        CloudError
            If incidents could not be retrieved.

        """
        return self._list_items(item_type="incidents")

    def get_incident(
        self,
        incident: str,
        entities: bool = False,
        alerts: bool = False,
        comments: bool = False,
        bookmarks: bool = False,
    ) -> pd.DataFrame:
        """
        Get details on a specific incident.

        Parameters
        ----------
        incident : str
            Incident ID GUID.
        entities : bool, optional
            If True include all entities in the response. Default is False.
        alerts : bool, optional
            If True include all alerts in the response. Default is False.
        comments: bool, optional
             If True include all comments in the response. Default is False.
        bookmarks: bool, optional
             If True include all bookmarks in the response. Default is False.

        Returns
        -------
        pd.DataFrame
            Table containing incident details.

        Raises
        ------
        CloudError
            If incident could not be retrieved.

        """
        incident_id = self._get_incident_id(incident)
        incident_url = self.sent_urls["incidents"] + f"/{incident_id}"
        params = {"api-version": "2020-01-01"}
        response = requests.get(
            incident_url, headers=get_api_headers(self.token), params=params
        )
        if response.status_code != 200:
            raise CloudError(response=response)

        incident_df = _azs_api_result_to_df(response)

        if entities:
            incident_df["Entities"] = [self.get_entities(incident_id)]

        if alerts:
            incident_df["Alerts"] = [self.get_incident_alerts(incident_id)]

        if comments:
            incident_df["Bookmarks"] = [self.get_incident_comments(incident_id)]

        if bookmarks:
            incident_df["Bookmarks"] = [self.get_incident_bookmarks(incident_id)]

        return incident_df

    def get_entities(self, incident: str) -> list:
        """
        Get the entities from an incident.

        Parameters
        ----------
        incident : str
            Incident GUID or Name .

        Returns
        -------
        list
            A list of entities.

        """
        incident_id = self._get_incident_id(incident)
        entities_url = self.sent_urls["incidents"] + f"/{incident_id}/entities"
        ent_parameters = {"api-version": "2019-01-01-preview"}
        ents = requests.post(
            entities_url,
            headers=get_api_headers(self.token),
            params=ent_parameters,
        )
        return (
            [(ent["kind"], ent["properties"]) for ent in ents.json()["entities"]]
            if ents.status_code == 200
            else []
        )

    def get_incident_alerts(self, incident: str) -> list:
        """
        Get the alerts from an incident.

        Parameters
        ----------
        incident : str
            Incident GUID or Name.

        Returns
        -------
        list
            A list of alerts.

        """
        incident_id = self._get_incident_id(incident)
        alerts_url = self.sent_urls["incidents"] + f"/{incident_id}/alerts"
        alerts_parameters = {"api-version": "2021-04-01"}
        alerts_resp = requests.post(
            alerts_url,
            headers=get_api_headers(self.token),
            params=alerts_parameters,
        )
        return (
            [
                {
                    "ID": alrts["properties"]["systemAlertId"],
                    "Name": alrts["properties"]["alertDisplayName"],
                }
                for alrts in alerts_resp.json()["value"]
            ]
            if alerts_resp.status_code == 200
            else []
        )

    def get_incident_comments(self, incident: str) -> list:
        """
        Get the comments from an incident.

        Parameters
        ----------
        incident : str
            Incident GUID or Name.

        Returns
        -------
        list
            A list of comments.

        """
        incident_id = self._get_incident_id(incident)
        comments_url = self.sent_urls["incidents"] + f"/{incident_id}/comments"
        comment_params = {"api-version": "2021-04-01"}
        comments_response = requests.get(
            comments_url,
            headers=get_api_headers(self.token),
            params=comment_params,
        )

        comment_details = comments_response.json()
        return (
            [
                {
                    "Message": comment["properties"]["message"],
                    "Author": comment["properties"]["author"]["name"],
                }
                for comment in comment_details["value"]
            ]
            if comments_response.status_code == 200
            else []
        )

    def get_incident_bookmarks(self, incident: str) -> list:
        """
        Get the comments from an incident.

        Parameters
        ----------
        incident : str
            Incident GUID or name.

        Returns
        -------
        list
            A list of bookmarks.

        """
        bookmarks_list = []
        incident_id = self._get_incident_id(incident)
        relations_url = self.sent_urls["incidents"] + f"/{incident_id}/relations"
        relations_params = {"api-version": "2021-04-01"}
        relations_response = requests.get(
            relations_url,
            headers=get_api_headers(self.token),
            params=relations_params,
        )
        if relations_response.status_code == 200 and relations_response.json()["value"]:
            for relationship in relations_response.json()["value"]:
                if (
                    relationship["properties"]["relatedResourceType"]
                    == "Microsoft.SecurityInsights/Bookmarks"
                ):
                    bkmark_id = relationship["properties"]["relatedResourceName"]
                    bookmarks_df = self.list_bookmarks()
                    bookmark = bookmarks_df[bookmarks_df["name"] == bkmark_id].iloc[0]
                    bookmarks_list.append(
                        {
                            "Bookmark ID": bkmark_id,
                            "Bookmark Title": bookmark["properties.displayName"],
                        }
                    )

        return bookmarks_list

    def update_incident(
        self,
        incident_id: str,
        update_items: dict,
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

        Raises
        ------
        CloudError
            If incident could not be updated.

        """
        incident_dets = self.get_incident(incident_id)
        incident_url = self.sent_urls["incidents"] + f"/{incident_id}"
        params = {"api-version": "2020-01-01"}
        if "title" not in update_items.keys():
            update_items["title"] = incident_dets.iloc[0]["properties.title"]
        if "status" not in update_items.keys():
            update_items["status"] = incident_dets.iloc[0]["properties.status"]
        data = _build_sent_data(update_items, etag=incident_dets.iloc[0]["etag"])
        response = requests.put(
            incident_url,
            headers=get_api_headers(self.token),
            params=params,
            data=str(data),
        )
        if response.status_code != 200:
            raise CloudError(response=response)
        print("Incident updated.")

    def create_incident(  # pylint: disable=too-many-arguments, too-many-locals, too-many-branches
        self,
        title: str,
        severity: str,
        status: str = "New",
        description: str = None,
        first_activity_time: datetime = None,
        last_activity_time: datetime = None,
        labels: List = None,
        bookmarks: List = None,
    ):
        """
        Create a Sentinel Incident.

        Parameters
        ----------
        title : str
            The title of the incident to create
        severity : str
            The severity to assign the incident, options are:
               Informational, Low, Medium, High
        status : str, optional
            The status to assign the incident, by default "New"
            Options are:
                New, Active, Closed
        description : str, optional
            A description of the incident, by default None
        first_activity_time : datetime, optional
            The start time of the incident activity, by default None
        last_activity_time : datetime, optional
            The end time of the incident activity, by default None
        labels : List, optional
            Any labels to apply to the incident, by default None
        bookmarks : List, optional
            A list of bookmark GUIDS you want to associate with the incident

        Raises
        ------
        CloudError
            If the API returns an error

        """
        incident_id = uuid4()
        incident_url = self.sent_urls["incidents"] + f"/{incident_id}"
        params = {"api-version": "2020-01-01"}
        data_items = {
            "title": title,
            "severity": severity.capitalize(),
            "status": status.capitalize(),
        }  # type: Dict[str, Union[str, List]]
        if description:
            data_items["description"] = description
        if labels:
            labels = [{"labelName": lab, "labelType": "User"} for lab in labels]
            data_items["labels"] = labels
        if first_activity_time:
            data_items["firstActivityTimeUtc"] = first_activity_time.isoformat()
        if last_activity_time:
            data_items["lastActivityTimeUtc"] = last_activity_time.isoformat()
        data = _build_sent_data(data_items, props=True)
        response = requests.put(
            incident_url,
            headers=get_api_headers(self.token),
            params=params,
            data=str(data),
        )
        if response.status_code != 201:
            raise CloudError(response=response)
        if bookmarks:
            for mark in bookmarks:
                relation_id = uuid4()
                bookmark_id = self._get_bookmark_id(mark)
                mark_res_id = self.sent_urls["bookmarks"] + f"/{bookmark_id}"
                relations_url = incident_url + f"/relations/{relation_id}"
                bkmark_data_items = {"relatedResourceId": mark_res_id}
                data = _build_sent_data(bkmark_data_items, props=True)
                params = {"api-version": "2021-04-01"}
                response = requests.put(
                    relations_url,
                    headers=get_api_headers(self.token),
                    params=params,
                    data=str(data),
                )
        print("Incident created.")

    def _get_incident_id(self, incident: str) -> str:
        """
        Get an incident ID.

        Parameters
        ----------
        incident : str
            An incident identifier

        Returns
        -------
        str
            The Incident GUID

        Raises
        ------
        MsticpyUserError
            If incident can't be found or multiple matching incidents found.

        """
        try:
            UUID(incident)
            return incident
        except ValueError as incident_name:
            incidents = self.list_incidents()
            filtered_incidents = incidents[
                incidents["properties.title"].str.contains(incident)
            ]
            if len(filtered_incidents) > 1:
                display(filtered_incidents[["name", "properties.title"]])
                raise MsticpyUserError(
                    "More than one incident found, please specify by GUID"
                ) from incident_name
            if (
                not isinstance(filtered_incidents, pd.DataFrame)
                or filtered_incidents.empty
            ):
                raise MsticpyUserError(
                    f"Incident {incident} not found"
                ) from incident_name
            return filtered_incidents["name"].iloc[0]

    def _get_bookmark_id(self, bookmark: str) -> str:
        """
        Get the ID of a bookmark.

        Parameters
        ----------
        bookmark : str
            GUID or name of a bookmark

        Returns
        -------
        str
            Bookmark GUID
        Raises
        ------
        MsticpyUserError
            If Bookmark not found or multiple matching bookmarks found.

        """
        try:
            UUID(bookmark)
            return bookmark
        except ValueError as bkmark_name:
            bookmarks = self.list_bookmarks()
            filtered_bookmarks = bookmarks[
                bookmarks["properties.displayName"].str.contains(bookmark)
            ]
            if len(filtered_bookmarks) > 1:
                display(filtered_bookmarks[["name", "properties.displayName"]])
                raise MsticpyUserError(
                    "More than one incident found, please specify by GUID"
                ) from bkmark_name
            if (
                not isinstance(filtered_bookmarks, pd.DataFrame)
                or filtered_bookmarks.empty
            ):
                raise MsticpyUserError(
                    f"Incident {bookmark} not found"
                ) from bkmark_name
            return filtered_bookmarks["name"].iloc[0]

    def add_bookmark_to_incident(self, incident: str, bookmark: str):
        """
        Add a bookmark to an incident.

        Parameters
        ----------
        incident : str
            Either an incident name or an incident GUID
        bookmark : str
            Either a bookmark name or bookmark GUID

        Raises
        ------
        CloudError
            If API returns error

        """
        incident_id = self._get_incident_id(incident)
        incident_url = self.sent_urls["incidents"] + f"/{incident_id}"
        bookmark_id = self._get_bookmark_id(bookmark)
        mark_res_id = self.sent_urls["bookmarks"] + f"/{bookmark_id}"
        relations_id = uuid4()
        bookmark_url = incident_url + f"/relations/{relations_id}"
        bkmark_data_items = {"relatedResourceId": mark_res_id}
        data = _build_sent_data(bkmark_data_items, props=True)
        params = {"api-version": "2021-04-01"}
        response = requests.put(
            bookmark_url,
            headers=get_api_headers(self.token),
            params=params,
            data=str(data),
        )
        if response.status_code != 201:
            raise CloudError(response=response)
        print("Bookmark added to incident.")

    def post_comment(
        self,
        incident_id: str,
        comment: str,
    ):
        """
        Write a comment for an incident.

        Parameters
        ----------
        incident_id : str
            Incident ID GUID.
        comment : str
            Comment message to post.

        Raises
        ------
        CloudError
            If message could not be posted.

        """
        comment_url = self.sent_urls["incidents"] + f"/{incident_id}/comments/{uuid4()}"
        params = {"api-version": "2020-01-01"}
        data = _build_sent_data({"message": comment})
        response = requests.put(
            comment_url,
            headers=get_api_headers(self.token),
            params=params,
            data=str(data),
        )
        if response.status_code != 201:
            raise CloudError(response=response)
        print("Comment posted.")

    def list_data_connectors(self) -> pd.DataFrame:
        """
        List deployed data connectors.

        Returns
        -------
        pd.DataFrame
            A DataFrame containing the deployed data connectors

        Raises
        ------
        CloudError
            If a valid result is not returned.

        """
        return self._list_items(item_type="data_connectors")

    def _get_template_id(
        self,
        template: str,
    ) -> str:
        """
        Get an analytic template ID.

        Parameters
        ----------
        template : str
            Template ID or Name
        res_id : str
            Sentinel workspace to get template from

        Returns
        -------
        str
            Template ID

        Raises
        ------
        MsticpyUserError
            If template not found or multiple templates found.

        """
        try:
            UUID(template)
            return template
        except ValueError as template_name:
            templates = self.list_analytic_templates()
            template_details = templates[
                templates["properties.displayName"].str.contains(template)
            ]
            if len(template_details) > 1:
                display(template_details[["name", "properties.displayName"]])
                raise MsticpyUserError(
                    "More than one template found, please specify by GUID"
                ) from template_name
            if not isinstance(template_details, pd.DataFrame) or template_details.empty:
                raise MsticpyUserError(
                    f"Template {template_details} not found"
                ) from template_name
            return template_details["name"].iloc[0]

    def create_analytic_rule(  # pylint: disable=too-many-arguments, too-many-locals
        self,
        template: str = None,
        name: str = None,
        enabled: bool = True,
        query: str = None,
        query_frequency: str = "PT5H",
        query_period: str = "PT5H",
        severity: str = "Medium",
        suppression_duration: str = "PT1H",
        suppression_enabled: bool = False,
        trigger_operator: str = "GreaterThan",
        trigger_threshold: int = 0,
        description: str = None,
        tactics: list = None,
    ):
        """
        Create a Sentinel Analytics Rule.

        Parameters
        ----------
        template : str, optional
            The GUID or name of a templated to create the analytic from, by default None
        name : str, optional
            The name to give the analytic, by default None
        enabled : bool, optional
            Whether you want the analytic to be enabled once deployed, by default True
        query : str, optional
            The query string to use in the anlaytic, by default None
        query_frequency : str, optional
            How often the query should run in ISO8601 format, by default "PT5H"
        query_period : str, optional
            How far back the query should look in ISO8601 format, by default "PT5H"
        severity : str, optional
            The severity to raise incidents as, by default "Medium"
            Options are; Informational, Low, Medium, or High
        suppression_duration : str, optional
            How long to suppress duplicate alerts in ISO8601 format, by default "PT1H"
        suppression_enabled : bool, optional
            Whether you want to suppress duplicates, by default False
        trigger_operator : str, optional
            The operator for the trigger, by default "GreaterThan"
        trigger_threshold : int, optional
            The threshold of events required to create the incident, by default 0
        description : str, optional
            A description of the analytic, by default None
        tactics : list, optional
            A list of MITRE ATT&CK tactics related to the analytic, by default None
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
        MsticpyUserError
            If template provided isn't found.
        CloudError
            If the API returns an error.

        """
        if template:
            template_id = self._get_template_id(template)
            templates = self.list_analytic_templates()
            template_details = templates[templates["name"] == template_id].iloc[0]
            name = template_details["properties.displayName"]
            query = template_details["properties.query"]
            query_frequency = template_details["properties.queryFrequency"]
            query_period = template_details["properties.queryPeriod"]
            severity = template_details["properties.severity"]
            trigger_operator = template_details["properties.triggerOperator"]
            trigger_threshold = template_details["properties.triggerThreshold"]
            description = template_details["properties.description"]
            tactics = (
                template_details["properties.tactics"]
                if not pd.isna(template_details["properties.tactics"])
                else []
            )

        if not tactics:
            tactics = []

        if not name:
            raise MsticpyUserError(
                "Please specify either a template ID or analytic details."
            )

        rule_id = uuid4()
        analytic_url = self.sent_urls["alert_rules"] + f"/{rule_id}"
        data_items = {
            "displayName": name,
            "query": query,
            "queryFrequency": query_frequency,
            "queryPeriod": query_period,
            "severity": severity,
            "suppressionDuration": suppression_duration,
            "suppressionEnabled": str(suppression_enabled).lower(),
            "triggerOperator": trigger_operator,
            "triggerThreshold": trigger_threshold,
            "description": description,
            "tactics": tactics,
            "enabled": str(enabled).lower(),
        }
        data = _build_sent_data(data_items, props=True)
        data["kind"] = "Scheduled"
        params = {"api-version": "2020-01-01"}
        response = requests.put(
            analytic_url,
            headers=get_api_headers(self.token),
            params=params,
            data=str(data),
        )
        if response.status_code != 201:
            raise CloudError(response=response)
        print("Analytic Created.")

    def _get_analytic_id(self, analytic: str) -> str:
        """
        Get the GUID of an analytic rule.

        Parameters
        ----------
        analytic : str
            The GUID or name of the analytic

        Returns
        -------
        str
            The analytic GUID

        Raises
        ------
        MsticpyUserError
            If analytic not found or multiple matching analytics found

        """
        try:
            UUID(analytic)
            return analytic
        except ValueError as analytic_name:
            analytics = self.list_analytic_rules()
            analytic_details = analytics[
                analytics["properties.displayName"].str.contains(analytic)
            ]
            if len(analytic_details) > 1:
                display(analytic_details[["name", "properties.displayName"]])
                raise MsticpyUserError(
                    "More than one analytic found, please specify by GUID"
                ) from analytic_name
            if not isinstance(analytic_details, pd.DataFrame) or analytic_details.empty:
                raise MsticpyUserError(
                    f"Analytic {analytic_details} not found"
                ) from analytic_name
            return analytic_details["name"].iloc[0]

    def delete_analytic_rule(
        self,
        analytic_rule: str,
    ):
        """
        Delete a deployed Analytic rule from a Sentinel workspace.

        Parameters
        ----------
        analytic_rule : str
            The GUID or name of the analytic.

        Raises
        ------
        CloudError
            If the API returns an error.

        """
        analytic_id = self._get_analytic_id(analytic_rule)
        analytic_url = self.sent_urls["alert_rules"] + f"/{analytic_id}"
        params = {"api-version": "2020-01-01"}
        response = requests.delete(
            analytic_url,
            headers=get_api_headers(self.token),
            params=params,
        )
        if response.status_code != 200:
            raise CloudError(response=response)
        print("Analytic Deleted.")

    def list_analytic_templates(self) -> pd.DataFrame:
        """
        List Analytic Templates.

        Returns
        -------
        pd.DataFrame
            A DataFrame containing the analytics templates

        Raises
        ------
        CloudError
            If a valid result is not returned.

        """
        return self._list_items(item_type="alert_template")

    def list_watchlists(self) -> pd.DataFrame:
        """
        List Deployed Watchlists.

        Returns
        -------
        pd.DataFrame
            A DataFrame containing the watchlists

        Raises
        ------
        CloudError
            If a valid result is not returned.

        """
        return self._list_items(
            item_type="watchlists",
            api_version="2021-04-01",
        )

    def create_watchlist(
        self,
        watchlist_name: str,
        description: str,
        search_key: str,
        provider: str = "MSTICPy",
        source: str = "Notebook",
        data: pd.DataFrame = None,
    ):
        """
        Create a new watchlist.

        Parameters
        ----------
        watchlist_name : str
            The name of the watchlist you want to create,
            this can't be the name of an existing watchlist.
        description : str
            A description of the watchlist to be created.
        search_key : str
            The search key is used to optimize query performance when using watchlists
            for joins with other data.
            This should be the key column that will be used in the watchlist when joining
            to other data tables.
        provider : str, optional
            This is the label attached to the watchlist showing who created it, by default "MSTICPy"
        source : str, optional
            The source of the data to be put in the watchlist, by default "Notebook"
        data: pd.DataFrame, optional
            The data you want to upload to the watchlist

        Raises
        ------
        MsticpyUserError
            Raised if the watchlist name already exists.
        CloudError
            If there is an issue creating the watchlist.

        """
        if not self._check_watchlist_exists(watchlist_name):
            raise MsticpyUserError(f"Watchlist {watchlist_name} does not exist.")
        watchlist_url = self.sent_urls["watchlists"] + f"/{watchlist_name}"
        params = {"api-version": "2021-04-01"}
        data_items = {
            "displayName": watchlist_name,
            "source": source,
            "provider": provider,
            "description": description,
            "itemsSearchKey": search_key,
            "contentType": "text/csv",
        }  # type: Dict[str, str]
        if isinstance(data, pd.DataFrame) and not data.empty:
            data_csv = data.to_csv(index=False)
            data_items["rawContent"] = str(data_csv)
        request_data = _build_sent_data(data_items, props=True)
        response = requests.put(
            watchlist_url,
            headers=get_api_headers(self.token),
            params=params,
            data=str(request_data),
        )
        if response.status_code != 200:
            raise CloudError(response=response)

        print("Watchlist created.")

    def list_watchlist_items(
        self,
        watchlist_name: str,
    ) -> pd.DataFrame:
        """
        List items in a watchlist.

        Parameters
        ----------
        watchlist_name : str
            The name of the watchlist to get items from

        Returns
        -------
        pd.DataFrame
            A DataFrame containing the watchlists

        Raises
        ------
        CloudError
            If a valid result is not returned.

        """
        watchlist_name_str = f"/{watchlist_name}/watchlistItems"
        return self._list_items(
            item_type="watchlists",
            api_version="2021-04-01",
            appendix=watchlist_name_str,
        )

    def add_watchlist_item(
        self,
        watchlist_name: str,
        item: Union[Dict, pd.Series, pd.DataFrame],
        overwrite: bool = False,
    ):
        """
        Add or update an item in a Watchlist.

        Parameters
        ----------
        watchlist_name : str
            The name of the watchlist to add items to
        item : Union[Dict, pd.Series, pd.DataFrame]
            The item to add, this can be a dictionary of valies, a Pandas Series, or DataFrame
        overwrite : bool, optional
            Wether you want to overwrite an item if it already exists in the watchlist,
            by default False

        Raises
        ------
        MsticpyUserError
            If the specified Watchlist does not exist.
        MsticpyUserError
            If the item already exists in the Watchlist and overwrite is set to False
        CloudError
            If the API returns an error.

        """
        # Check requested watchlist actually exists
        if not self._check_watchlist_exists(watchlist_name):
            raise MsticpyUserError(f"Watchlist {watchlist_name} does not exist.")

        new_items = []
        # Convert items to add to dictionary format
        if isinstance(item, pd.Series):
            new_items = [dict(item)]
        elif isinstance(item, Dict):
            new_items = [item]
        elif isinstance(item, pd.DataFrame):
            for _, line_item in item.iterrows():
                new_items.append(dict(line_item))

        current_items = self.list_watchlist_items(watchlist_name)
        current_items_values = current_items.filter(
            regex="^properties.itemsKeyValue.", axis=1
        )
        current_items_values.columns = current_items_values.columns.str.replace(
            "properties.itemsKeyValue.", "", regex=False
        )

        for new_item in new_items:
            # See if item already exists, if it does get the item ID
            current_df, item_series = current_items_values.align(
                pd.Series(new_item), axis=1, copy=False
            )
            if (current_df == item_series).all(axis=1).any() and overwrite:
                watchlist_id = current_items[
                    current_items.isin(list(new_item.values())).any(axis=1)
                ]["properties.watchlistItemId"].iloc[0]
            # If not in watchlist already generate new ID
            elif not (current_df == item_series).all(axis=1).any():
                watchlist_id = str(uuid4())
            else:
                raise MsticpyUserError(
                    "Item already exists in the watchlist. Set overwrite = True to replace."
                )

            watchlist_url = (
                self.sent_urls["watchlists"]
                + f"/{watchlist_name}/watchlistItems/{watchlist_id}"
            )
            response = requests.put(
                watchlist_url,
                headers=get_api_headers(self.token),
                params={"api-version": "2021-04-01"},
                data=str({"properties": {"itemsKeyValue": item}}),
            )
            if response.status_code != 200:
                raise CloudError(response=response)

        print(f"Items added to {watchlist_name}")

    def delete_watchlist(
        self,
        watchlist_name: str,
    ):
        """
        Delete a selected Watchlist.

        Parameters
        ----------
        watchlist_name : str
            The name of the Watchlist to deleted

        Raises
        ------
        MsticpyUserError
            If Watchlist does not exist.
        CloudError
            If the API returns an error.

        """
        # Check requested watchlist actually exists
        if not self._check_watchlist_exists(watchlist_name):
            raise MsticpyUserError(f"Watchlist {watchlist_name} does not exist.")
        watchlist_url = self.sent_urls["watchlists"] + f"/{watchlist_name}"
        params = {"api-version": "2021-04-01"}
        response = requests.delete(
            watchlist_url,
            headers=get_api_headers(self.token),
            params=params,
        )
        if response.status_code != 200:
            raise CloudError(response=response)
        print(f"Watchlist {watchlist_name} deleted")

    def _check_watchlist_exists(
        self,
        watchlist_name: str,
    ):
        """
        Check whether a Watchlist exists or not.

        Parameters
        ----------
        watchlist_name : str
            The Watchlist to check for.
        res_id : str, optional
            The Resource ID of the Sentinel workspace to check in, by default None

        Returns
        -------
        bool
            Whether the Watchlist exists or not.

        """
        # Check requested watchlist actually exists
        existing_watchlists = self.list_watchlists()["name"].values
        return watchlist_name in existing_watchlists

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
        item_url = self.url + _PATH_MAPPING[item_type]
        if appendix:
            item_url = item_url + appendix
        params = {"api-version": api_version}
        response = requests.get(
            item_url, headers=get_api_headers(self.token), params=params
        )
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
        if not self.config:
            self.config = WorkspaceConfig()  # type: ignore
        for item in items:
            if item in self.config:  # type: ignore
                config_items[item] = self.config[item]  # type: ignore
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
            base_url = AzureCloudConfig(self.cloud).endpoints.resource_manager
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

    # Get > List Aliases
    get_alert_rules = list_alert_rules
    list_analytic_rules = list_alert_rules
    get_analytic_rules = list_alert_rules
    get_sentinel_workspaces = list_sentinel_workspaces
    get_hunting_queries = list_hunting_queries
    get_bookmarks = list_bookmarks
    get_incidents = list_incidents


def _get_token(credential: AzCredentials) -> str:
    """
    Extract token from a azure.identity object.

    Parameters
    ----------
    credential : AzCredentials
        Azure OAuth credentials.

    Returns
    -------
    str
        A token to be used in API calls.

    """
    token = credential.modern.get_token(AzureCloudConfig().token_uri)
    return token.token


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
            data_body["properties"].update({key: items[key]})  # type:ignore
        else:
            data_body[key] = items[key]
    if "etag" in kwargs:
        data_body["etag"] = kwargs.get("etag")  # type:ignore
    return data_body


# Microsoft Sentinel rename
MicrosoftSentinel = AzureSentinel
