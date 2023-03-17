# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Azure Sentinel unit tests."""
import re

import respx

# pylint: disable=redefined-outer-name, unused-import, no-name-in-module
from .sentinel_test_fixtures import sent_loader


@respx.mock
def test_sent_search_create(sent_loader):
    """Test Sentinel Watchlist feature."""
    respx.put(re.compile(r"https://management\.azure\.com/.*")).respond(202)
    sent_loader.create_search(query="Syslog| take 10", search_name="testsearch")


@respx.mock
def test_sent_search_delete(sent_loader):
    """Test Sentinel Watchlist feature."""
    respx.delete(re.compile(r"https://management\.azure\.com/.*")).respond(202)
    sent_loader.delete_search(search_name="testsearch")


@respx.mock
def test_sent_search_check_pending(sent_loader):
    """Test Sentinel Watchlist feature."""
    respx.get(re.compile(r"https://management\.azure\.com/.*")).respond(
        200, json={"properties": {"provisioningState": "InProgress"}}
    )
    assert not sent_loader.check_search_status(search_name="testsearch")


@respx.mock
def test_sent_search_check_complete(sent_loader):
    """Test Sentinel Watchlist feature."""
    respx.get(re.compile(r"https://management\.azure\.com/.*")).respond(
        200, json={"properties": {"provisioningState": "Succeeded"}}
    )
    assert sent_loader.check_search_status(search_name="testsearch")
