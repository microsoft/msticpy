# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Mixin Classes for Sentinel Incident Features."""
from __future__ import annotations

import logging
from typing import TYPE_CHECKING, Any, Callable
from uuid import UUID, uuid4

import httpx
import pandas as pd
from azure.common.exceptions import CloudError
from IPython.display import display
from typing_extensions import Self

from msticpy.context.azure.sentinel_bookmarks import SentinelBookmarksMixin

from ..._version import VERSION
from ...common.exceptions import MsticpyUserError
from .azure_data import get_api_headers
from .sentinel_utils import (
    _azs_api_result_to_df,
    extract_sentinel_response,
    get_http_timeout,
)

if TYPE_CHECKING:
    from datetime import datetime

__version__ = VERSION
__author__ = "Pete Bryan"

logger: logging.Logger = logging.getLogger(__name__)


class SentinelIncidentsMixin(SentinelBookmarksMixin):
    """Mixin class for Sentinel Incidents feature integrations."""

    def get_incident(  # noqa:PLR0913
        self: Self,
        incident: str,
        *,
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
        incident_id: str = self._get_incident_id(incident)
        incident_url: str = self.sent_urls["incidents"] + f"/{incident_id}"
        response: httpx.Response = super(SentinelBookmarksMixin, self)._get_items(
            incident_url,
        )
        if not response.is_success:
            raise CloudError(response=response)

        incident_df: pd.DataFrame = _azs_api_result_to_df(response)

        if entities:
            incident_df["Entities"] = [self.get_entities(incident_id)]

        if alerts:
            incident_df["Alerts"] = [self.get_incident_alerts(incident_id)]

        if comments:
            incident_df["Bookmarks"] = [self.get_incident_comments(incident_id)]

        if bookmarks:
            incident_df["Bookmarks"] = [self.get_incident_bookmarks(incident_id)]

        return incident_df

    def get_entities(self: Self, incident: str) -> list:
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
        self.check_connected()
        incident_id: str = self._get_incident_id(incident)
        entities_url: str = self.sent_urls["incidents"] + f"/{incident_id}/entities"
        ent_parameters: dict[str, str] = {"api-version": "2021-04-01"}
        if not self._token:
            err_msg = "Token not found, can't get entities."
            raise ValueError(err_msg)
        ents: httpx.Response = httpx.post(
            entities_url,
            headers=get_api_headers(self._token),
            params=ent_parameters,
            timeout=get_http_timeout(),
        )
        return (
            [(ent["kind"], ent["properties"]) for ent in ents.json()["entities"]]
            if ents.is_success
            else []
        )

    def get_incident_alerts(self: Self, incident: str) -> list:
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
        self.check_connected()
        incident_id: str = self._get_incident_id(incident)
        alerts_url: str = self.sent_urls["incidents"] + f"/{incident_id}/alerts"
        alerts_parameters: dict[str, str] = {"api-version": "2021-04-01"}
        if not self._token:
            err_msg = "Token not found, can't get incident alerts."
            raise ValueError(err_msg)
        alerts_resp: httpx.Response = httpx.post(
            alerts_url,
            headers=get_api_headers(self._token),
            params=alerts_parameters,
            timeout=get_http_timeout(),
        )
        return (
            [
                {
                    "ID": alert["properties"]["systemAlertId"],
                    "Name": alert["properties"]["alertDisplayName"],
                    "AlertProperties": alert["properties"],
                }
                for alert in alerts_resp.json()["value"]
            ]
            if alerts_resp.is_success
            else []
        )

    def get_incident_comments(self: Self, incident: str) -> list:
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
        incident_id: str = self._get_incident_id(incident)
        comments_url: str = self.sent_urls["incidents"] + f"/{incident_id}/comments"
        comments_response: httpx.Response = self._get_items(
            comments_url,
            {"api-version": "2020-04-01"},
        )
        comment_details: dict[str, Any] = comments_response.json()
        return (
            [
                {
                    "Message": comment["properties"]["message"],
                    "Author": comment["properties"]["author"]["name"],
                }
                for comment in comment_details["value"]
            ]
            if comments_response.is_success
            else []
        )

    def get_incident_bookmarks(self: Self, incident: str) -> list:
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
        bookmarks_list: list[dict[str, Any]] = []
        incident_id: str = self._get_incident_id(incident)
        relations_url: str = self.sent_urls["incidents"] + f"/{incident_id}/relations"
        relations_response: httpx.Response = self._get_items(
            relations_url,
            {"api-version": "2020-04-01"},
        )
        if relations_response.is_success and relations_response.json()["value"]:
            for relationship in relations_response.json()["value"]:
                if (
                    relationship["properties"]["relatedResourceType"]
                    == "Microsoft.SecurityInsights/Bookmarks"
                ):
                    bkmark_id: str = relationship["properties"]["relatedResourceName"]
                    bookmarks_df: pd.DataFrame = self.list_bookmarks()
                    bookmark: pd.Series = bookmarks_df[
                        bookmarks_df["name"] == bkmark_id
                    ].iloc[0]
                    bookmarks_list.append(
                        {
                            "Bookmark ID": bkmark_id,
                            "Bookmark Title": bookmark["properties.displayName"],
                        },
                    )

        return bookmarks_list

    def update_incident(
        self: Self,
        incident_id: str,
        update_items: dict,
    ) -> str:
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
        self.check_connected()
        incident_dets: pd.DataFrame = self.get_incident(incident_id)
        incident_url: str = self.sent_urls["incidents"] + f"/{incident_id}"
        params: dict[str, str] = {"api-version": "2020-01-01"}
        if "title" not in update_items:
            update_items["title"] = incident_dets.iloc[0]["properties.title"]
        if "status" not in update_items:
            update_items["status"] = incident_dets.iloc[0]["properties.status"]
        data: dict[str, Any] = extract_sentinel_response(
            update_items,
            props=True,
            etag=incident_dets.iloc[0]["etag"],
        )
        if not self._token:
            err_msg = "Token not found, can't update incident."
            raise ValueError(err_msg)
        response: httpx.Response = httpx.put(
            incident_url,
            headers=get_api_headers(self._token),
            params=params,
            content=str(data),
            timeout=get_http_timeout(),
        )
        if response.status_code not in (200, 201):
            raise CloudError(response=response)
        logger.info("Incident updated.")
        return response.json().get("name")

    def create_incident(  # pylint: disable=too-many-arguments, too-many-locals #noqa:PLR0913
        self: Self,
        title: str,
        severity: str,
        status: str = "New",
        description: str | None = None,
        first_activity_time: datetime | None = None,
        last_activity_time: datetime | None = None,
        labels: list[dict[str, Any]] | None = None,
        bookmarks: list[str] | None = None,
    ) -> str | None:
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
        self.check_connected()
        incident_id: UUID = uuid4()
        incident_url: str = self.sent_urls["incidents"] + f"/{incident_id}"
        params: dict[str, str] = {"api-version": "2020-01-01"}
        data_items: dict[str, str | list] = {
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
        data: dict[str, Any] = extract_sentinel_response(data_items, props=True)
        if not self._token:
            err_msg: str = "Token not found, can't create incident."
            raise ValueError(err_msg)
        response: httpx.Response = httpx.put(
            incident_url,
            headers=get_api_headers(self._token),
            params=params,
            content=str(data),
            timeout=get_http_timeout(),
        )
        if not response.is_success:
            raise CloudError(response=response)
        if bookmarks:
            for mark in bookmarks:
                relation_id: UUID = uuid4()
                bookmark_id: str = self._get_bookmark_id(mark)
                mark_res_id: str = self.sent_urls["bookmarks"] + f"/{bookmark_id}"
                relations_url: str = incident_url + f"/relations/{relation_id}"
                bkmark_data_items: dict[str, Any] = {"relatedResourceId": mark_res_id}
                data = extract_sentinel_response(bkmark_data_items, props=True)
                params = {"api-version": "2021-04-01"}
                if not self._token:
                    err_msg = "Token not found, can't create relations."
                    raise ValueError(err_msg)
                response = httpx.put(
                    relations_url,
                    headers=get_api_headers(self._token),
                    params=params,
                    content=str(data),
                    timeout=get_http_timeout(),
                )
        logger.info("Incident created.")
        return response.json().get("name")

    def _get_incident_id(self: Self, incident: str) -> str:
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
        except ValueError as incident_name:
            incidents: pd.DataFrame = self.list_incidents()
            filtered_incidents: pd.DataFrame = incidents[
                incidents["properties.title"].str.contains(incident)
            ]
            if len(filtered_incidents) > 1:
                display(filtered_incidents[["name", "properties.title"]])
                err_msg: str = "More than one incident found, please specify by GUID"
                raise MsticpyUserError(err_msg) from incident_name
            if (
                not isinstance(filtered_incidents, pd.DataFrame)
                or filtered_incidents.empty
            ):
                err_msg = f"Incident {incident} not found"
                raise MsticpyUserError(err_msg) from incident_name
            return filtered_incidents["name"].iloc[0]
        return incident

    def post_comment(
        self: Self,
        incident_id: str,
        comment: str,
    ) -> str:
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
        self.check_connected()
        comment_url: str = (
            self.sent_urls["incidents"] + f"/{incident_id}/comments/{uuid4()}"
        )
        params: dict[str, str] = {"api-version": "2020-01-01"}
        data: dict[str, Any] = extract_sentinel_response({"message": comment})
        if not self._token:
            err_msg = "Token not found, can't post comment."
            raise ValueError(err_msg)
        response: httpx.Response = httpx.put(
            comment_url,
            headers=get_api_headers(self._token),
            params=params,
            content=str(data),
            timeout=get_http_timeout(),
        )
        if not response.is_success:
            raise CloudError(response=response)
        logger.info("Comment posted.")
        return response.json().get("name")

    def add_bookmark_to_incident(self: Self, incident: str, bookmark: str) -> str:
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
        self.check_connected()
        incident_id: str = self._get_incident_id(incident)
        incident_url: str = self.sent_urls["incidents"] + f"/{incident_id}"
        bookmark_id: str = self._get_bookmark_id(bookmark)
        mark_res_id: str = self.sent_urls["bookmarks"] + f"/{bookmark_id}"
        relations_id: UUID = uuid4()
        bookmark_url: str = incident_url + f"/relations/{relations_id}"
        bkmark_data_items: dict[str, Any] = {
            "relatedResourceId": mark_res_id.split(self.base_url)[1],
        }
        data: dict[str, Any] = extract_sentinel_response(bkmark_data_items, props=True)
        params: dict[str, str] = {"api-version": "2021-04-01"}
        if not self._token:
            err_msg = "Token not found, can't add bookmark to incident."
            raise ValueError(err_msg)
        response: httpx.Response = httpx.put(
            bookmark_url,
            headers=get_api_headers(self._token),
            params=params,
            content=str(data),
            timeout=get_http_timeout(),
        )
        if not response.is_success:
            raise CloudError(response=response)
        logger.info("Bookmark added to incident.")
        return response.json().get("name")

    def list_incidents(self: Self, params: dict | None = None) -> pd.DataFrame:
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
        return self._list_items(item_type="incidents", params=params)

    get_incidents: Callable[..., pd.DataFrame] = list_incidents
