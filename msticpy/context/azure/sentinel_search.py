# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Mixin Classes for Sentinel Search Features."""
from datetime import datetime, timedelta
from uuid import uuid4

import httpx
from azure.common.exceptions import CloudError

from ..._version import VERSION
from .azure_data import get_api_headers
from .sentinel_utils import _build_sent_data

__version__ = VERSION
__author__ = "Pete Bryan"


class SentinelSearchlistsMixin:
    """Mixin class for Sentinel Watchlist feature integrations."""

    def create_search(
        self,
        query: str,
        start: datetime = None,
        end: datetime = None,
        search_name: str = None,
        **kwargs,
    ):
        """
        Create a Search job.

        Parameters
        ----------
        query : str
            The KQL query to run as a job.
        start : datetime, optional
            The start time for the query, by default 90 days ago.
        end : datetime, optional
            The end time for the query, by default now.
        search_name : str, optional
            A name to apply to the search, by default a random GUID is generated.

        Raises
        ------
        CloudError
            If there is an error creating the search job.

        """
        limit = 1000
        if "limit" in kwargs:
            limit = kwargs.pop("limit")
        if "timespan" in kwargs:
            start = kwargs.get("timespan").start  # type: ignore
            end = kwargs.get("timespan").end  # type: ignore
        search_end = end or datetime.now()
        search_start = start or (search_end - timedelta(days=90))
        search_name = search_name or uuid4()  # type: ignore
        search_name = search_name.replace("_", "")  # type: ignore
        search_url = (
            self.sent_urls["search"]  # type: ignore
            + f"/{search_name}_SRCH?api-version=2021-12-01-preview"
        )
        search_items = {
            "searchResults": {
                "query": f"{query}",
                "limit": limit,
                "startSearchTime": f"{search_start.isoformat()}",
                "endSearchTime": f"{search_end.isoformat()}",
            }
        }
        search_body = _build_sent_data(search_items)
        search_create_response = httpx.put(
            search_url,
            headers=get_api_headers(self.token),  # type: ignore
            json=search_body,
            timeout=60,
        )
        if search_create_response.status_code != 202:
            raise CloudError(response=search_create_response)
        print(f"Search job created with for {search_name}_SRCH.")

    def check_search_status(self, search_name: str) -> bool:
        """
        Check the status of a search job.

        Parameters
        ----------
        search_name : str
            The name of the search job to check.

        Returns
        -------
        bool
            Returns True if search is ready.

        Raises
        ------
        CloudError
            If error in checking the search job status.

        """
        search_name = search_name.strip("_SRCH")
        search_url = (
            self.sent_urls["search"]  # type: ignore
            + f"/{search_name}_SRCH?api-version=2021-12-01-preview"
        )
        search_check_response = httpx.get(
            search_url, headers=get_api_headers(self.token)  # type: ignore
        )
        if search_check_response.status_code != 200:
            raise CloudError(response=search_check_response)

        check_result = search_check_response.json()["properties"]["provisioningState"]
        print(f"{search_name}_SRCH status is '{check_result}'")
        if check_result == "Succeeded":
            return True
        return False

    def delete_search(self, search_name: str):
        """
        Delete a search result.

        Parameters
        ----------
        search_name: str
            The name of the search to delete.

        Raises
        ------
        CloudError
            If an error occurs when attempting to delete the search

        """
        search_name = search_name.strip("_SRCH")
        search_url = (
            self.sent_urls["search"]  # type: ignore
            + f"/{search_name}_SRCH?api-version=2021-12-01-preview"
        )
        search_delete_response = httpx.delete(
            search_url, headers=get_api_headers(self.token)  # type: ignore
        )
        if search_delete_response.status_code != 202:
            raise CloudError(response=search_delete_response)
        print(f"{search_name}_SRCH set for deletion.")
