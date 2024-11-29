# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Mixin Classes for Sentinel Search Features."""
from __future__ import annotations

import datetime as dt
import logging
from typing import TYPE_CHECKING, Any
from uuid import uuid4

import httpx
from azure.common.exceptions import CloudError
from typing_extensions import Self

from ..._version import VERSION
from .azure_data import get_api_headers
from .sentinel_utils import SentinelUtilsMixin, extract_sentinel_response

if TYPE_CHECKING:
    from ...common.timespan import TimeSpan
__version__ = VERSION
__author__ = "Pete Bryan"

logger: logging.Logger = logging.getLogger(__name__)


class SentinelSearchlistsMixin(SentinelUtilsMixin):
    """Mixin class for Sentinel Watchlist feature integrations."""

    def create_search(  # noqa: PLR0913
        self: Self,
        query: str,
        start: dt.datetime | None = None,
        end: dt.datetime | None = None,
        search_name: str | None = None,
        *,
        timespan: TimeSpan | None = None,
        limit: int = 1000,
    ) -> None:
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
        timespan: Timespan, optional
           If defined, overwrite start and end variables.
        limit: int, optional
           Set the maximum number of results to return. Defaults to 1000.

        Raises
        ------
        CloudError
            If there is an error creating the search job.

        """
        if timespan:
            start = timespan.start
            end = timespan.end
        search_end: dt.datetime = end or dt.datetime.now(tz=dt.timezone.utc)
        search_start: dt.datetime = start or (search_end - dt.timedelta(days=90))
        search_name = (search_name or str(uuid4())).replace("_", "")
        search_url: str = (
            self.sent_urls["search"]
            + f"/{search_name}_SRCH?api-version=2021-12-01-preview"
        )
        search_items: dict[str, dict[str, Any]] = {
            "searchResults": {
                "query": f"{query}",
                "limit": limit,
                "startSearchTime": f"{search_start.isoformat()}",
                "endSearchTime": f"{search_end.isoformat()}",
            },
        }
        search_body: dict[str, Any] = extract_sentinel_response(search_items)
        if not self._token:
            err_msg = "Token not found, can't create search."
            raise ValueError(err_msg)
        search_create_response: httpx.Response = httpx.put(
            search_url,
            headers=get_api_headers(self._token),
            json=search_body,
            timeout=60,
        )
        if not search_create_response.is_success:
            raise CloudError(response=search_create_response)
        logger.info("Search job created with for %s_SRCH.", search_name)

    def check_search_status(self: Self, search_name: str) -> bool:
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
        search_url: str = (
            self.sent_urls["search"]
            + f"/{search_name}_SRCH?api-version=2021-12-01-preview"
        )
        if not self._token:
            err_msg = "Token not found, can't check search status."
            raise ValueError(err_msg)
        search_check_response: httpx.Response = httpx.get(
            search_url,
            headers=get_api_headers(self._token),
        )
        if not search_check_response.is_success:
            raise CloudError(response=search_check_response)

        check_result: str = search_check_response.json()["properties"][
            "provisioningState"
        ]
        logger.info("%s_SRCH status is '%s'", search_name, check_result)
        return check_result == "Succeeded"

    def delete_search(self: Self, search_name: str) -> None:
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
        search_url: str = (
            self.sent_urls["search"]
            + f"/{search_name}_SRCH?api-version=2021-12-01-preview"
        )
        if not self._token:
            err_msg = "Token not found, can't delete search."
            raise ValueError(err_msg)
        search_delete_response: httpx.Response = httpx.delete(
            search_url,
            headers=get_api_headers(self._token),
        )
        if not search_delete_response.is_success:
            raise CloudError(response=search_delete_response)
        logger.info("%s_SRCH set for deletion.", search_name)
