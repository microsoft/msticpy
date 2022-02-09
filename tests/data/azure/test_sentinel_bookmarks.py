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

_BOOKMARK = {
    "value": [
        {
            "id": "/subscriptions/123/resourceGroups/RG/providers/Microsoft.OperationalInsights/workspaces/WSName/providers/Microsoft.SecurityInsights/Bookmarks/123",
            "name": "Bookmark Test",
            "etag": '"123"',
            "type": "Microsoft.SecurityInsights/Bookmarks",
            "properties": {
                "displayName": "Bookmark Test",
                "created": "2020-04-27T16:55:02.8309802+00:00",
                "updated": "2020-04-27T16:55:02.8309802+00:00",
                "createdBy": {
                    "objectId": "123",
                    "email": "user@microsoft.com",
                    "name": "123",
                },
                "updatedBy": {
                    "objectId": "123",
                    "email": "user@microsoft.com",
                    "name": "123",
                },
                "eventTime": "2020-04-27T16:55:02.8309802+00:00",
                "labels": [],
                "query": "SecurityAlert\n| take 10\n",
                "queryResult": '{"TenantId":"52b1ab41-869e-4138-9e40-2a4457f09bf0","TimeGenerated":"2020-04-27T15:34:02Z","DisplayName":"Matches on commands seen in honeypot data","AlertName":"Matches on commands seen in honeypot data","AlertSeverity":"Medium","Description":"Looks for matches of Linux commands from auditd logs against \\nLinux commands sourced from \\nMicrosoft Threat Intelligence Center honeypot data.","ProviderName":"ASI Scheduled Alerts","VendorName":"Microsoft","VendorOriginalId":"393fe624-418f-44ea-92b1-1a18f7d19555","SystemAlertId":"8a22c48c-e583-41d7-a178-276d21925af0","ResourceId":"","SourceComputerId":"","AlertType":"52b1ab41-869e-4138-9e40-2a4457f09bf0_a35a8534-0b12-4138-8e00-83c5c68dc647","ConfidenceLevel":"Unknown","ConfidenceScore":null,"IsIncident":"false","StartTime":"2020-04-27T14:38:24Z","EndTime":"2020-04-27T14:38:24Z","ProcessingEndTime":"2020-04-27T15:34:02Z","RemediationSteps":"","ExtendedProperties":"{\\r\\n  \\"Query\\": \\"// Time scope parameters (in UTC format) were prepended to the query to reflect the time window on which the rule ran when the alert was triggered.\\\\n// To change the time scope, remove the set statements and select a new time range.\\\\nset query_datetimescope_column = \\\\\\"TimeGenerated\\\\\\";\\\\nset query_datetimescope_from = datetime(4/27/2020 10:28:54 AM);\\\\nset query_datetimescope_to = datetime(4/27/2020 3:28:54 PM);\\\\nSyslog \\\\r\\\\n| where TimeGenerated >= (datetime(4/27/2020 3:28:54 PM)-(7d)) \\\\r\\\\n| take 1\\",\\r\\n  \\"Query Period\\": \\"05:00:00\\",\\r\\n  \\"Query Start Time UTC\\": \\"2020-04-27 10:28:54Z\\",\\r\\n  \\"Query End Time UTC\\": \\"2020-04-27 15:28:54Z\\",\\r\\n  \\"Trigger Operator\\": \\"GreaterThan\\",\\r\\n  \\"Trigger Threshold\\": \\"0\\",\\r\\n  \\"Query Results Aggregation Kind\\": \\"SingleAlert\\",\\r\\n  \\"Search Query Results Overall Count\\": \\"1\\"\\r\\n}","Entities":"","SourceSystem":"Detection","WorkspaceSubscriptionId":"40dcc8bf-0478-4f3b-b275-ed0a94f2c013","WorkspaceResourceGroup":"asihuntomsworkspacerg","ExtendedLinks":"","ProductName":"Azure Sentinel","ProductComponentName":"Scheduled Alerts","AlertLink":"","Type":"SecurityAlert","__entityMapping":{}}',
                "queryStartTime": "2020-04-26T16:54:49.439+00:00",
                "queryEndTime": "2020-04-27T16:54:49.439+00:00",
                "incidentInfo": {
                    "incidentId": "9f63476e-3605-4377-a415-7626fdda46f3",
                    "title": "SecurityAlert - 123",
                    "relationName": "123",
                    "severity": "Medium",
                },
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
def test_sent_bookmarks(sent_loader):
    """Test Sentinel bookmarks feature."""
    respx.get(re.compile("https://management.azure.com/.*")).respond(
        200, json=_BOOKMARK
    )
    bkmarks = sent_loader.list_bookmarks()
    assert isinstance(bkmarks, pd.DataFrame)
    assert bkmarks["name"].iloc[0] == "Bookmark Test"


@respx.mock
def test_sent_bookmark_create(sent_loader):
    """Test Sentinel bookmark creation."""
    respx.put(re.compile("https://management.azure.com/.*")).respond(200, json={})
    sent_loader.create_bookmark(
        name="Test Bookmark",
        query="SecurityAlert | take 10",
        results="DataFrame",
        notes="Some notes",
        labels=["Label1"],
    )


@respx.mock
def test_sent_bookmark_delete(sent_loader):
    """Test Sentinel bookmark deletion."""
    respx.delete(re.compile("https://management.azure.com/.*")).respond(200, json={})
    sent_loader.delete_bookmark("a55463ed-dce0-4ba4-83ca-6f6d0e5d5acf")
