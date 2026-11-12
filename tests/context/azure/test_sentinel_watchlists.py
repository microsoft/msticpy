# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Azure Sentinel unit tests."""
import csv
import io
import json
import re
from copy import deepcopy

import pandas as pd
import pytest
import respx

from msticpy.common.exceptions import MsticpyUserError

# pylint: disable=redefined-outer-name, unused-import, no-name-in-module
from .sentinel_test_fixtures import sent_loader

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


@respx.mock
def test_sent_watchlists(sent_loader):
    """Test Sentinel Watchlist feature."""
    respx.get(re.compile(r"https://management\.azure\.com/.*")).respond(
        200, json=_WATCHLISTS
    )
    watchlists = sent_loader.list_watchlists()
    assert isinstance(watchlists, pd.DataFrame)
    assert watchlists["properties.displayName"].iloc[0] == "watchlist1"
    assert watchlists["properties.provider"].iloc[0] == "Microsoft"


@respx.mock
def test_sent_watchlists_create(sent_loader):
    """Test Sentinel Watchlist feature."""
    respx.put(re.compile(r"https://management\.azure\.com/.*")).respond(
        200, json=_WATCHLISTS["value"][0]
    )
    respx.get(re.compile(r"https://management\.azure\.com/.*/watchlists")).respond(
        200, json=_WATCHLISTS
    )
    sent_loader.create_watchlist(
        watchlist_name="Test Watchlist",
        description="A test watchlist",
        search_key="testdata",
    )


@respx.mock
def test_sent_watchlists_create_json_body(sent_loader):
    """Serialize watchlist properties and CSV content as JSON."""
    create = respx.put(re.compile(r"https://management\.azure\.com/.*")).respond(
        200, json=_WATCHLISTS["value"][0]
    )
    respx.get(re.compile(r"https://management\.azure\.com/.*/watchlists")).respond(
        200, json=_WATCHLISTS
    )
    description = 'Analyst\'s "watchlist"\nwith notes'
    data = pd.DataFrame(
        [{"IpAddress": "13.67.128.10", "Note": 'Owner\'s "server",\nsecond line'}]
    )

    sent_loader.create_watchlist(
        watchlist_name="Test Watchlist",
        description=description,
        search_key="IpAddress",
        data=data,
    )

    request = create.calls[0].request
    assert request.headers["Content-Type"] == "application/json"
    properties = json.loads(request.content)["properties"]
    assert properties["displayName"] == "Test Watchlist"
    assert properties["description"] == description
    assert properties["itemsSearchKey"] == "IpAddress"
    assert properties["contentType"] == "text/csv"
    assert list(csv.DictReader(io.StringIO(properties["rawContent"]))) == data.to_dict(
        orient="records"
    )


@respx.mock
def test_sent_watchlists_delete(sent_loader):
    """Test Sentinel Watchlist feature."""
    respx.delete(re.compile(r"https://management\.azure\.com/.*")).respond(200)
    respx.get(re.compile(r"https://management\.azure\.com/.*/watchlists")).respond(
        200, json=_WATCHLISTS
    )
    sent_loader.delete_watchlist(watchlist_name="watchlist1")


@respx.mock
def test_sent_watchlists_items(sent_loader):
    """Test Sentinel Watchlist feature."""
    respx.get(re.compile(r"https://management\.azure\.com/.*/watchlistItems")).respond(
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
    respx.get(re.compile(r"https://management\.azure\.com/.*/watchlistItems")).respond(
        200, json=_WATCHLIST_ITEM
    )
    respx.put(
        re.compile(r"https://management\.azure\.com/.*/watchlistItems/.*")
    ).respond(200)
    respx.get(re.compile(r"https://management\.azure\.com/.*/watchlists")).respond(
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


@respx.mock
def test_sent_watchlists_item_json_body(sent_loader):
    """Serialize a single watchlist item as JSON without changing its values."""
    respx.get(re.compile(r"https://management\.azure\.com/.*/watchlistItems")).respond(
        200, json=_WATCHLIST_ITEM
    )
    respx.get(re.compile(r"https://management\.azure\.com/.*/watchlists")).respond(
        200, json=_WATCHLISTS
    )
    update = respx.put(
        re.compile(r"https://management\.azure\.com/.*/watchlistItems/.*")
    ).respond(200)
    item = {"Type": 'Owner\'s "server"\nwith notes', "IpAddress": "13.67.128.11"}

    sent_loader.add_watchlist_item("watchlist1", item)

    request = update.calls[0].request
    assert request.headers["Content-Type"] == "application/json"
    assert json.loads(request.content) == {"properties": {"itemsKeyValue": item}}


@pytest.mark.parametrize("input_type", [dict, pd.Series, pd.DataFrame])
@respx.mock
def test_sent_watchlists_items_normalized_rows(sent_loader, input_type):
    """Send one normalized row per PUT for every supported input type."""
    respx.get(re.compile(r"https://management\.azure\.com/.*/watchlistItems")).respond(
        200, json=_WATCHLIST_ITEM
    )
    respx.get(re.compile(r"https://management\.azure\.com/.*/watchlists")).respond(
        200, json=_WATCHLISTS
    )
    update = respx.put(
        re.compile(r"https://management\.azure\.com/.*/watchlistItems/.*")
    ).respond(200)
    rows = [
        {"Type": "Owned", "IpAddress": "13.67.128.11"},
        {"Type": "External", "IpAddress": "13.67.128.12"},
    ]
    if input_type is pd.DataFrame:
        item = pd.DataFrame(rows)
        expected = rows
    else:
        item = input_type(rows[0])
        expected = rows[:1]

    sent_loader.add_watchlist_item("watchlist1", item)

    assert update.call_count == len(expected)
    payloads = [json.loads(call.request.content) for call in update.calls]
    assert payloads == [{"properties": {"itemsKeyValue": row}} for row in expected]
    assert len({call.request.url.path for call in update.calls}) == len(expected)


@pytest.mark.parametrize("overwrite", [True, False])
@respx.mock
def test_sent_watchlists_overwrite_exact_match(sent_loader, overwrite):
    """Overwrite only the exact row when an earlier row shares a value."""
    items = deepcopy(_WATCHLIST_ITEM)
    partial_match = deepcopy(items["value"][0])
    partial_match["properties"]["watchlistItemId"] = "partial-match"
    partial_match["properties"]["itemsKeyValue"]["IpAddress"] = "13.67.128.11"
    items["value"].insert(0, partial_match)
    respx.get(re.compile(r"https://management\.azure\.com/.*/watchlistItems")).respond(
        200, json=items
    )
    respx.get(re.compile(r"https://management\.azure\.com/.*/watchlists")).respond(
        200, json=_WATCHLISTS
    )
    update = respx.put(
        re.compile(r"https://management\.azure\.com/.*/watchlistItems/.*")
    ).respond(200)
    item = {"Type": "Owned", "IpAddress": "13.67.128.10"}

    if overwrite:
        sent_loader.add_watchlist_item("watchlist1", item, overwrite=True)
        assert update.call_count == 1
        assert update.calls[0].request.url.path.endswith(
            "/watchlistItems/a681a611-8a33-41d2-a6b6-3eeaa88fd87d"
        )
    else:
        with pytest.raises(MsticpyUserError, match="Item already exists"):
            sent_loader.add_watchlist_item("watchlist1", item)
        assert not update.called
