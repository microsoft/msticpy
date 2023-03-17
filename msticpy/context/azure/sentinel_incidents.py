# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Mixin Classes for Sentinel Incident Features."""
from datetime import datetime
from typing import Dict, List, Optional, Union
from uuid import UUID, uuid4

import httpx
import pandas as pd
from azure.common.exceptions import CloudError
from IPython.display import display

from ..._version import VERSION
from ...common.exceptions import MsticpyUserError
from .azure_data import get_api_headers
from .sentinel_utils import _azs_api_result_to_df, _build_sent_data, get_http_timeout

__version__ = VERSION
__author__ = "Pete Bryan"


class SentinelIncidentsMixin:
    """Mixin class for Sentinel Incidents feature integrations."""

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
        incident_url = self.sent_urls["incidents"] + f"/{incident_id}"  # type: ignore
        response = self._get_items(incident_url)  # type: ignore
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
        self.check_connected()  # type: ignore
        incident_id = self._get_incident_id(incident)
        entities_url = self.sent_urls["incidents"] + f"/{incident_id}/entities"  # type: ignore
        ent_parameters = {"api-version": "2021-04-01"}
        ents = httpx.post(
            entities_url,
            headers=get_api_headers(self.token),  # type: ignore
            params=ent_parameters,
            timeout=get_http_timeout(),
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
        self.check_connected()  # type: ignore
        incident_id = self._get_incident_id(incident)
        alerts_url = self.sent_urls["incidents"] + f"/{incident_id}/alerts"  # type: ignore
        alerts_parameters = {"api-version": "2021-04-01"}
        alerts_resp = httpx.post(
            alerts_url,
            headers=get_api_headers(self.token),  # type: ignore
            params=alerts_parameters,
            timeout=get_http_timeout(),
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
        comments_url = self.sent_urls["incidents"] + f"/{incident_id}/comments"  # type: ignore
        comments_response = self._get_items(comments_url, "2021-04-01")  # type: ignore
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
        relations_url = self.sent_urls["incidents"] + f"/{incident_id}/relations"  # type: ignore
        relations_response = self._get_items(relations_url, "2021-04-01")  # type: ignore
        if relations_response.status_code == 200 and relations_response.json()["value"]:
            for relationship in relations_response.json()["value"]:
                if (
                    relationship["properties"]["relatedResourceType"]
                    == "Microsoft.SecurityInsights/Bookmarks"
                ):
                    bkmark_id = relationship["properties"]["relatedResourceName"]
                    bookmarks_df = self.list_bookmarks()  # type: ignore
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
            https://docs.microsoft.com/rest/api/securityinsights/
            stable/incidents/create-or-update

        Raises
        ------
        CloudError
            If incident could not be updated.

        """
        self.check_connected()  # type: ignore
        incident_dets = self.get_incident(incident_id)
        incident_url = self.sent_urls["incidents"] + f"/{incident_id}"  # type: ignore
        params = {"api-version": "2020-01-01"}
        if "title" not in update_items.keys():
            update_items["title"] = incident_dets.iloc[0]["properties.title"]
        if "status" not in update_items.keys():
            update_items["status"] = incident_dets.iloc[0]["properties.status"]
        data = _build_sent_data(update_items, etag=incident_dets.iloc[0]["etag"])
        response = httpx.put(
            incident_url,
            headers=get_api_headers(self.token),  # type: ignore
            params=params,
            content=str(data),
            timeout=get_http_timeout(),
        )
        if response.status_code not in (200, 201):
            raise CloudError(response=response)
        print("Incident updated.")
        return response.json().get("name")

    def create_incident(  # pylint: disable=too-many-arguments, too-many-locals
        self,
        title: str,
        severity: str,
        status: str = "New",
        description: Optional[str] = None,
        first_activity_time: Optional[datetime] = None,
        last_activity_time: Optional[datetime] = None,
        labels: Optional[List] = None,
        bookmarks: Optional[List] = None,
    ) -> Optional[str]:
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
            Options are: New, Active, Closed
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

        Returns
        -------
        Optional[str]
            The name/ID of the incident.

        Raises
        ------
        CloudError
            If the API returns an error

        """
        self.check_connected()  # type: ignore
        incident_id = uuid4()
        incident_url = self.sent_urls["incidents"] + f"/{incident_id}"  # type: ignore
        params = {"api-version": "2020-01-01"}
        data_items: Dict[str, Union[str, List]] = {
            "title": title,
            "severity": severity.capitalize(),
            "status": status.capitalize(),
        }
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
        response = httpx.put(
            incident_url,
            headers=get_api_headers(self.token),  # type: ignore
            params=params,
            content=str(data),
            timeout=get_http_timeout(),
        )
        if response.status_code not in (200, 201):
            raise CloudError(response=response)
        if bookmarks:
            for mark in bookmarks:
                relation_id = uuid4()
                bookmark_id = self._get_bookmark_id(mark)  # type: ignore
                mark_res_id = self.sent_urls["bookmarks"] + f"/{bookmark_id}"  # type: ignore
                relations_url = incident_url + f"/relations/{relation_id}"
                bkmark_data_items = {"relatedResourceId": mark_res_id}
                data = _build_sent_data(bkmark_data_items, props=True)
                params = {"api-version": "2021-04-01"}
                response = httpx.put(
                    relations_url,
                    headers=get_api_headers(self.token),  # type: ignore
                    params=params,
                    content=str(data),
                    timeout=get_http_timeout(),
                )
        print("Incident created.")
        return response.json().get("name")

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
        self.check_connected()  # type: ignore
        comment_url = (
            self.sent_urls["incidents"] + f"/{incident_id}/comments/{uuid4()}"  # type: ignore
        )
        params = {"api-version": "2020-01-01"}
        data = _build_sent_data({"message": comment})
        response = httpx.put(
            comment_url,
            headers=get_api_headers(self.token),  # type: ignore
            params=params,
            content=str(data),
            timeout=get_http_timeout(),
        )
        if response.status_code not in (200, 201):
            raise CloudError(response=response)
        print("Comment posted.")
        return response.json().get("name")

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
        self.check_connected()  # type: ignore
        incident_id = self._get_incident_id(incident)
        incident_url = self.sent_urls["incidents"] + f"/{incident_id}"  # type: ignore
        bookmark_id = self._get_bookmark_id(bookmark)  # type: ignore
        mark_res_id = self.sent_urls["bookmarks"] + f"/{bookmark_id}"  # type: ignore
        relations_id = uuid4()
        bookmark_url = incident_url + f"/relations/{relations_id}"
        bkmark_data_items = {
            "relatedResourceId": mark_res_id.split("https://management.azure.com")[1]
        }
        data = _build_sent_data(bkmark_data_items, props=True)
        params = {"api-version": "2021-04-01"}
        response = httpx.put(
            bookmark_url,
            headers=get_api_headers(self.token),  # type: ignore
            params=params,
            content=str(data),
            timeout=get_http_timeout(),
        )
        if response.status_code not in (200, 201):
            raise CloudError(response=response)
        print("Bookmark added to incident.")
        return response.json().get("name")

    def list_incidents(self, params: Optional[dict] = None) -> pd.DataFrame:
        """
        Get a list of incident for a Sentinel workspace.

        Parameters
        ----------
        params : Optional[dict], optional
            Additional parameters to pass to the API call, by default None

        Returns
        -------
        pd.DataFrame
            A table of incidents.

        Raises
        ------
        CloudError
            If incidents could not be retrieved.

        """
        if params is None:
            params = {"$top": 50}
        return self._list_items(item_type="incidents", params=params)  # type: ignore

    get_incidents = list_incidents
