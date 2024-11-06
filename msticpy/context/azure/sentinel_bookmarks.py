# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Mixin Classes for Sentinel Bookmark Features."""
from __future__ import annotations

import logging
from typing import Any, Callable
from uuid import UUID, uuid4

import httpx
import pandas as pd
from azure.common.exceptions import CloudError
from IPython.display import display
from typing_extensions import Self

from ..._version import VERSION
from ...common.exceptions import MsticpyUserError
from .azure_data import get_api_headers
from .sentinel_utils import (
    SentinelUtilsMixin,
    extract_sentinel_response,
    get_http_timeout,
)

__version__ = VERSION
__author__ = "Pete Bryan"

logger: logging.Logger = logging.getLogger(__name__)


class SentinelBookmarksMixin(SentinelUtilsMixin):
    """Mixin class with Sentinel Bookmark integrations."""

    def list_bookmarks(self: Self) -> pd.DataFrame:
        """
        Return a list of Bookmarks from a Sentinel workspace.

        Returns
        -------
        pd.DataFrame
            A set of bookmarks.

        """
        return self._list_items(item_type="bookmarks")

    def create_bookmark(  # noqa:PLR0913
        self: Self,
        name: str,
        query: str,
        results: str | None = None,
        notes: str | None = None,
        labels: list[str] | None = None,
    ) -> str | None:
        """
        Create a bookmark in the Sentinel Workspace.

        Parameters
        ----------
        name : str
            The name of the bookmark to use
        query : str
            The KQL query for the bookmark
        results : str, optional
            The results of the query to include with the bookmark, by default None
        notes : str, optional
            Any notes you want associated with the bookmark, by default None
        labels : List[str], optional
            Any labels you want associated with the bookmark, by default None

        Returns
        -------
        str|None
            The name/ID of the bookmark.

        Raises
        ------
        CloudError
            If API returns an error.

        """
        self.check_connected()
        # Generate or use resource ID
        bkmark_id = str(uuid4())
        bookmark_url: str = self.sent_urls["bookmarks"] + f"/{bkmark_id}"
        data_items: dict[str, str | list] = {
            "displayName": name,
            "query": query,
        }
        if results:
            data_items["queryResult"] = results
        if notes:
            data_items["notes"] = notes
        if labels:
            data_items["labels"] = labels
        data: dict[str, Any] = extract_sentinel_response(data_items, props=True)
        params: dict[str, str] = {"api-version": "2020-01-01"}
        if not self._token:
            err_msg = "Token not found, can't create bookmark."
            raise ValueError(err_msg)
        response: httpx.Response = httpx.put(
            bookmark_url,
            headers=get_api_headers(self._token),
            params=params,
            content=str(data),
            timeout=get_http_timeout(),
        )
        if response.is_success:
            logger.info("Bookmark created.")
            return response.json().get("name")
        raise CloudError(response=response)

    def delete_bookmark(
        self: Self,
        bookmark: str,
    ) -> None:
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
        self.check_connected()
        bookmark_id: str = self._get_bookmark_id(bookmark)
        bookmark_url: str = self.sent_urls["bookmarks"] + f"/{bookmark_id}"
        params: dict[str, str] = {"api-version": "2020-01-01"}
        if not self._token:
            err_msg = "Token not found, can't delete bookmatk."
            raise ValueError(err_msg)
        response: httpx.Response = httpx.delete(
            bookmark_url,
            headers=get_api_headers(self._token),
            params=params,
            timeout=get_http_timeout(),
        )
        if response.is_success:
            logger.info("Bookmark deleted.")
        else:
            raise CloudError(response=response)

    def _get_bookmark_id(self: Self, bookmark: str) -> str:
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
        except ValueError as bkmark_name:
            bookmarks: pd.DataFrame = self.list_bookmarks()
            filtered_bookmarks: pd.DataFrame = bookmarks[
                bookmarks["properties.displayName"].str.contains(bookmark)
            ]
            if len(filtered_bookmarks) > 1:
                display(filtered_bookmarks[["name", "properties.displayName"]])
                err_msg: str = "More than one incident found, please specify by GUID"
                raise MsticpyUserError(err_msg) from bkmark_name
            if (
                not isinstance(filtered_bookmarks, pd.DataFrame)
                or filtered_bookmarks.empty
            ):
                err_msg = f"Incident {bookmark} not found"
                raise MsticpyUserError(err_msg) from bkmark_name
            return filtered_bookmarks["name"].iloc[0]
        return bookmark

    get_bookmarks: Callable[..., pd.DataFrame] = list_bookmarks
