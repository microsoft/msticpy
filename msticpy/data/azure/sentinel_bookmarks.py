# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Mixin Classes for Sentinel Bookmark Features."""
from typing import Dict, List, Union
from uuid import UUID, uuid4

import pandas as pd
import httpx
from IPython.display import display

from azure.common.exceptions import CloudError

from ..._version import VERSION
from .azure_data import get_api_headers
from .sentinel_utils import _build_sent_data
from ...common.exceptions import MsticpyUserError

__version__ = VERSION
__author__ = "Pete Bryan"


class SentinelBookmarksMixin:
    """Mixin class with Sentinel Bookmark integrations."""

    def list_bookmarks(self) -> pd.DataFrame:
        """
        Return a list of Bookmarks from a Sentinel workspace.

        Returns
        -------
        pd.DataFrame
            A set of bookmarks.

        """
        return self._list_items(item_type="bookmarks")  # type: ignore

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
            The results of the query to include with the bookmark, by default None
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
        bookmark_url = self.sent_urls["bookmarks"] + f"/{bkmark_id}"  # type: ignore
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
        response = httpx.put(
            bookmark_url,
            headers=get_api_headers(self.token),  # type: ignore
            params=params,
            content=str(data),
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
        bookmark_url = self.sent_urls["bookmarks"] + f"/{bookmark_id}"  # type: ignore
        params = {"api-version": "2020-01-01"}
        response = httpx.delete(
            bookmark_url,
            headers=get_api_headers(self.token),  # type: ignore
            params=params,
        )
        if response.status_code == 200:
            print("Bookmark deleted.")
        else:
            raise CloudError(response=response)

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

    get_bookmarks = list_bookmarks
