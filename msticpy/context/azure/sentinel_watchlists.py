# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Mixin Classes for Sentinel Watchlist Features."""
from typing import Dict, Optional, Union
from uuid import uuid4

import httpx
import pandas as pd
from azure.common.exceptions import CloudError

from ..._version import VERSION
from ...common.exceptions import MsticpyUserError
from .azure_data import get_api_headers
from .sentinel_utils import _build_sent_data, get_http_timeout

__version__ = VERSION
__author__ = "Pete Bryan"


class SentinelWatchlistsMixin:
    """Mixin class for Sentinel Watchlist feature integrations."""

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
        return self._list_items(  # type: ignore
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
    ) -> Optional[str]:
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

        Returns
        -------
        Optional[str]
            The name/ID of the watchlist.

        Raises
        ------
        MsticpyUserError
            Raised if the watchlist name already exists.
        CloudError
            If there is an issue creating the watchlist.

        """
        self.check_connected()  # type: ignore
        if self._check_watchlist_exists(watchlist_name):
            raise MsticpyUserError(f"Watchlist {watchlist_name} already exist.")
        watchlist_url = self.sent_urls["watchlists"] + f"/{watchlist_name}"  # type: ignore
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
        response = httpx.put(
            watchlist_url,
            headers=get_api_headers(self.token),  # type: ignore
            params=params,
            content=str(request_data),
            timeout=get_http_timeout(),
        )
        if response.status_code != 200:
            raise CloudError(response=response)

        print("Watchlist created.")
        return response.json().get("name")

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
        return self._list_items(  # type: ignore
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
        self.check_connected()  # type: ignore
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
                pd.Series(new_item), axis=1, copy=False  # type: ignore
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
                self.sent_urls["watchlists"]  # type: ignore
                + f"/{watchlist_name}/watchlistItems/{watchlist_id}"
            )
            response = httpx.put(
                watchlist_url,
                headers=get_api_headers(self.token),  # type: ignore
                params={"api-version": "2021-04-01"},
                content=str({"properties": {"itemsKeyValue": item}}),
                timeout=get_http_timeout(),
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
        self.check_connected()  # type: ignore
        # Check requested watchlist actually exists
        if not self._check_watchlist_exists(watchlist_name):
            raise MsticpyUserError(f"Watchlist {watchlist_name} does not exist.")
        watchlist_url = self.sent_urls["watchlists"] + f"/{watchlist_name}"  # type: ignore
        params = {"api-version": "2021-04-01"}
        response = httpx.delete(
            watchlist_url,
            headers=get_api_headers(self.token),  # type: ignore
            params=params,
            timeout=get_http_timeout(),
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
