# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Azure Sentinel unit tests."""
import re
from typing import List
from unittest.mock import patch

import pandas as pd
import pytest
import respx
from msticpy.data.azure import MicrosoftSentinel

_INCIDENT = {
    "value": [
        {
            "id": "/subscriptions/123/resourceGroups/RG/providers/Microsoft.OperationalInsights/workspaces/WSName/providers/Microsoft.SecurityInsights/Incidents/13ffba29-971c-4d70-9cb4-ddd0ec1bbb84",
            "name": "13ffba29-971c-4d70-9cb4-ddd0ec1bbb84",
            "etag": '"0b0013eb-0000-0a00-0000-5fab48a40000"',
            "type": "Microsoft.SecurityInsights/Incidents",
            "properties": {
                "title": "Test Incident",
                "description": "Test",
                "severity": "Medium",
                "status": "New",
                "owner": {
                    "objectId": "null",
                    "email": "null",
                    "assignedTo": "null",
                    "userPrincipalName": "null",
                },
                "labels": [],
                "firstActivityTimeUtc": "2020-11-10T21:07:48.2446898Z",
                "lastActivityTimeUtc": "2020-11-11T02:07:48.2446898Z",
                "lastModifiedTimeUtc": "2020-11-11T02:12:52.389123Z",
                "createdTimeUtc": "2020-11-11T02:12:52.389123Z",
                "incidentNumber": 44271,
                "additionalData": {
                    "alertsCount": 1,
                    "bookmarksCount": 0,
                    "commentsCount": 0,
                    "alertProductNames": ["Azure Sentinel"],
                    "tactics": [],
                },
                "firstActivityTimeGenerated": "2020-11-11T02:12:52.1908593Z",
                "lastActivityTimeGenerated": "2020-11-11T02:12:52.1908593Z",
                "relatedAnalyticRuleIds": [
                    "/subscriptions/123/resourceGroups/RG/providers/Microsoft.OperationalInsights/workspaces/WSName/providers/Microsoft.SecurityInsights/alertRules/494bfb7c-e436-483d-800d-8385068c2e49"
                ],
                "incidentUrl": "https://portal.azure.com/#asset/Microsoft_Azure_Security_Insights/Incident/subscriptions/123/resourceGroups/RG/providers/Microsoft.OperationalInsights/workspaces/WSName/providers/Microsoft.SecurityInsights/Incidents/13ffba29-971c-4d70-9cb4-ddd0ec1bbb84",
            },
        },
    ]
}


@pytest.fixture(scope="module")
@patch(MicrosoftSentinel.__module__ + ".MicrosoftSentinel.connect")
def sent_loader(mock_creds):
    """Generate MicrosoftSentinel for testing."""
    mock_creds.return_value = None
    azs = MicrosoftSentinel(
        sub_id="fd09863b-5cec-4833-ab9c-330ad07b0c1a", res_grp="RG", ws_name="WSName"
    )
    azs.connect()
    azs.token = "fd09863b-5cec-4833-ab9c-330ad07b0c1a"
    return azs


@respx.mock
def test_sent_incidents(sent_loader):
    """Test Sentinel incidents feature."""
    respx.get(re.compile("https://management.azure.com/.*")).respond(
        200, json=_INCIDENT
    )
    incidents = sent_loader.list_incidents()
    assert isinstance(incidents, pd.DataFrame)
    assert incidents["name"].iloc[0] == "13ffba29-971c-4d70-9cb4-ddd0ec1bbb84"
    incident = sent_loader.get_incident(
        incident="13ffba29-971c-4d70-9cb4-ddd0ec1bbb84",
    )
    assert isinstance(incident, pd.DataFrame)
    assert incident["name"].iloc[0] == "13ffba29-971c-4d70-9cb4-ddd0ec1bbb84"


@respx.mock
def test_sent_updates(sent_loader):
    """Test Sentinel incident update feature."""
    respx.put(re.compile("https://management.azure.com/.*")).respond(201, json="")
    respx.get(re.compile("https://management.azure.com/.*")).respond(
        200, json=_INCIDENT
    )
    sent_loader.post_comment(
        incident_id="13ffba29-971c-4d70-9cb4-ddd0ec1bbb84", comment="test"
    )


@respx.mock
def test_sent_comments(sent_loader):
    """Test Sentinel comments feature."""
    respx.put(re.compile("https://management.azure.com/.*")).respond(200, json="")
    respx.get(re.compile("https://management.azure.com/.*")).respond(
        200, json=_INCIDENT
    )
    sent_loader.update_incident(
        incident_id="13ffba29-971c-4d70-9cb4-ddd0ec1bbb84",
        update_items={"severity": "High"},
    )


@respx.mock
def test_sent_entities(sent_loader):
    """Test getting Entities from a Sentinel Incident."""
    respx.post(re.compile("https://management.azure.com/.*")).respond(
        200, json={"entities": [{"kind": "ipv4", "properties": "13.67.128.10"}]}
    )
    ents = sent_loader.get_entities("0c7d4a60-46b3-45d0-a966-3b51373faef0")
    assert isinstance(ents, List)
    assert ents[0][0] == "ipv4"
    assert ents[0][1] == "13.67.128.10"


@respx.mock
def test_sent_alerts(sent_loader):
    """Test getting alerts from a Sentinel Incident."""
    respx.post(re.compile("https://management.azure.com/.*")).respond(
        200,
        json={
            "value": [
                {
                    "properties": {
                        "systemAlertId": "d8f5e9ab-d75b-42ad-9c01-e350ccfd383a",
                        "alertDisplayName": "Test Alert",
                    }
                }
            ]
        },
    )
    alerts = sent_loader.get_incident_alerts("0c7d4a60-46b3-45d0-a966-3b51373faef0")
    assert isinstance(alerts, List)
    assert alerts[0]["ID"] == "d8f5e9ab-d75b-42ad-9c01-e350ccfd383a"
    assert alerts[0]["Name"] == "Test Alert"


@respx.mock
def test_sent_comments(sent_loader):
    """Test getting alerts from a Sentinel Incident."""
    respx.get(re.compile("https://management.azure.com/.*")).respond(
        200,
        json={
            "value": [
                {
                    "properties": {
                        "message": "Test Message",
                        "author": {"name": "Test User"},
                    }
                }
            ]
        },
    )
    alerts = sent_loader.get_incident_comments("0c7d4a60-46b3-45d0-a966-3b51373faef0")
    assert isinstance(alerts, List)
    assert alerts[0]["Message"] == "Test Message"
    assert alerts[0]["Author"] == "Test User"


@respx.mock
def test_sent_bookmarks(sent_loader):
    """Test getting bookmarks from a Sentinel Incident."""
    respx.get(re.compile("https://management.azure.com/.*/relations")).respond(
        200,
        json={
            "value": [
                {
                    "properties": {
                        "relatedResourceType": "Microsoft.SecurityInsights/Bookmarks",
                        "relatedResourceName": "508f3c50-f6d3-45b3-8321-fb674afe3478",
                    }
                }
            ]
        },
    )
    respx.get(re.compile("https://management.azure.com/.*/bookmarks")).respond(
        200,
        json={
            "value": [
                {
                    "name": "508f3c50-f6d3-45b3-8321-fb674afe3478",
                    "properties": {"displayName": "Test Bookmark"},
                }
            ]
        },
    )
    alerts = sent_loader.get_incident_bookmarks("0c7d4a60-46b3-45d0-a966-3b51373faef0")
    assert isinstance(alerts, List)
    assert alerts[0]["Bookmark ID"] == "508f3c50-f6d3-45b3-8321-fb674afe3478"
    assert alerts[0]["Bookmark Title"] == "Test Bookmark"


@respx.mock
def test_sent_incident_create(sent_loader):
    respx.put(re.compile("https://management.azure.com/.*")).respond(201)
    sent_loader.create_incident(title="Test Incident", severity="Low")
