# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Azure Sentinel unit tests."""
import re
from unittest.mock import patch

import pandas as pd
import pytest
import respx
from msticpy.data.azure import MicrosoftSentinel

_WATCHLISTS = {
    "value": [
        {
            "id": "subscriptions/123/resourceGroups/RG/providers/Microsoft.OperationalInsights/workspaces/WSNAME/providers/Microsoft.SecurityInsights/Watchlists/watchlist1",
            "name": "watchlist1",
            "type": "Microsoft.SecurityInsights/Watchlists",
            "properties": {
                "watchlistId": "fc128205-8b8e-482f-8e09-f8220c8fe25d",
                "displayName": "watchlist1",
                "provider": "Microsoft",
                "source": "data.csv",
                "itemsSearchKey": "IpAddress",
                "created": "2020-10-05T18:21:05.7786597+00:00",
                "updated": "2020-10-05T18:21:05.7786597+00:00",
                "createdBy": {
                    "objectId": "90a3c369-b812-4f6e-ac76-9fdf8a7e7b4d",
                    "email": "user@contoso.com",
                    "name": "A User",
                },
                "updatedBy": {
                    "objectId": "90a3c369-b812-4f6e-ac76-9fdf8a7e7b4d",
                    "email": "user@contoso.com",
                    "name": "A User",
                },
                "description": "A test watchlist",
                "watchlistType": "watchlist",
                "watchlistAlias": "watclist1",
                "isDeleted": False,
                "labels": [],
                "defaultDuration": "P1DT3H",
                "tenantId": "9b1ac0b8-77aa-4abd-a300-daa4607be7b5",
                "numberOfLinesToSkip": 0,
            },
        },
    ]
}

_WATCHLIST_ITEM = {
    "value": [
        {
            "id": "subscriptions/123/resourceGroups/RG/providers/Microsoft.OperationalInsights/workspaces/WSNAME/providers/Microsoft.SecurityInsights/Watchlists/watchlist1/WatchlistItems/a681a611-8a33-41d2-a6b6-3eeaa88fd87d",
            "name": "a681a611-8a33-41d2-a6b6-3eeaa88fd87d",
            "etag": '"05003b1a-0000-0a00-0000-5f7b64120000"',
            "type": "Microsoft.SecurityInsights/Watchlists/WatchlistItems",
            "properties": {
                "watchlistItemType": "watchlist-item",
                "watchlistItemId": "a681a611-8a33-41d2-a6b6-3eeaa88fd87d",
                "tenantId": "72f988bf-86f1-41af-91ab-2d7cd011db47",
                "isDeleted": False,
                "created": "2020-10-05T18:21:05.7786597+00:00",
                "updated": "2020-10-05T18:21:05.7786597+00:00",
                "createdBy": {
                    "objectId": "90a3c369-b812-4f6e-ac76-9fdf8a7e7b4d",
                    "email": "user@contoso.com",
                    "name": "A User",
                },
                "updatedBy": {
                    "objectId": "90a3c369-b812-4f6e-ac76-9fdf8a7e7b4d",
                    "email": "user@contoso.com",
                    "name": "A User",
                },
                "itemsKeyValue": {"Type": "Owned", "IpAddress": "13.67.128.10"},
                "entityMapping": {},
            },
        }
    ]
}


@pytest.fixture(scope="module")
@patch(MicrosoftSentinel.__module__ + ".MicrosoftSentinel.connect")
def sent_loader(mock_creds):
    """Generate MicrosoftSentinel for testing."""
    mock_creds.return_value = None
    sent = MicrosoftSentinel(
        sub_id="fd09863b-5cec-4833-ab9c-330ad07b0c1a", res_grp="RG", ws_name="WSName"
    )
    sent.connect()
    sent.token = "fd09863b-5cec-4833-ab9c-330ad07b0c1a"
    return sent


@respx.mock
def test_sent_watchlists(sent_loader):
    """Test Sentinel Watchlist feature."""
    respx.get(re.compile("https://management.azure.com/.*")).respond(
        200, json=_WATCHLISTS
    )
    watchlists = sent_loader.list_watchlists()
    assert isinstance(watchlists, pd.DataFrame)
    assert watchlists["properties.displayName"].iloc[0] == "watchlist1"
    assert watchlists["properties.provider"].iloc[0] == "Microsoft"


@respx.mock
def test_sent_watchlists_create(sent_loader):
    """Test Sentinel Watchlist feature."""
    respx.put(re.compile("https://management.azure.com/.*")).respond(200)
    respx.get(re.compile("https://management.azure.com/.*/watchlists")).respond(
        200, json=_WATCHLISTS
    )
    sent_loader.create_watchlist(
        watchlist_name="Test Watchlist",
        description="A test watchlist",
        search_key="testdata",
    )


@respx.mock
def test_sent_watchlists_delete(sent_loader):
    """Test Sentinel Watchlist feature."""
    respx.delete(re.compile("https://management.azure.com/.*")).respond(200)
    respx.get(re.compile("https://management.azure.com/.*/watchlists")).respond(
        200, json=_WATCHLISTS
    )
    sent_loader.delete_watchlist(watchlist_name="watchlist1")


@respx.mock
def test_sent_watchlists_items(sent_loader):
    """Test Sentinel Watchlist feature."""
    respx.get(re.compile("https://management.azure.com/.*/watchlistItems")).respond(
        200, json=_WATCHLIST_ITEM
    )
    watchlist_items = sent_loader.list_watchlist_items(watchlist_name="Test Watchlist")
    assert isinstance(watchlist_items, pd.DataFrame)
    assert watchlist_items["properties.itemsKeyValue.Type"].iloc[0] == "Owned"
    assert (
        watchlist_items["properties.itemsKeyValue.IpAddress"].iloc[0] == "13.67.128.10"
    )


@respx.mock
def test_sent_watchlists_items_add(sent_loader):
    """Test Sentinel Watchlist feature."""
    respx.get(re.compile("https://management.azure.com/.*/watchlistItems")).respond(
        200, json=_WATCHLIST_ITEM
    )
    respx.put(re.compile("https://management.azure.com/.*/watchlistItems/.*")).respond(
        200
    )
    respx.get(re.compile("https://management.azure.com/.*/watchlists")).respond(
        200, json=_WATCHLISTS
    )
    sent_loader.add_watchlist_item(
        watchlist_name="watchlist1",
        item={"Type": "Owned", "IpAddress": "13.67.128.10"},
        overwrite=True,
    )
    sent_loader.add_watchlist_item(
        watchlist_name="watchlist1", item={"Type": "Owned", "IpAddress": "13.67.128.11"}
    )
