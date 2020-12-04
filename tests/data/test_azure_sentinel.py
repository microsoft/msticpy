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
import responses
from azure.core.exceptions import ClientAuthenticationError
from msticpy.data.azure_data import AzureData
from msticpy.data.azure_sentinel import AzureSentinel

_RESOURCES = pd.DataFrame(
    {
        "resource_type": [
            "Microsoft.OperationsManagement/solutions",
            "Microsoft.OperationsManagement/solutions",
            "Microsoft.Insights/components",
        ],
        "name": ["SecurityInsightsTest1", "Test2", "Test3"],
        "resource_id": ["123", "456", "789"],
    }
)
_RESOURCE_DETAILS = {"properties": {"workspaceResourceId": "ABC"}}
_HUNTING_QUERIES = {
    "__metadata": {},
    "value": [
        {
            "id": "subscriptions/123/resourceGroups/RG/providers/Microsoft.OperationalInsights/workspaces/WSNAME/savedSearches/123",
            "etag": "Tag",
            "properties": {
                "Category": "Hunting Queries",
                "DisplayName": "HuntingQuery",
                "Query": "QueryText",
                "Tags": [
                    {"Name": "description", "Value": ""},
                    {"Name": "tactics", "Value": ""},
                    {"Name": "t-skang@microsoft.com", "Value": "false"},
                ],
                "Version": 2,
            },
            "name": "123",
            "type": "Microsoft.OperationalInsights/savedSearches",
        }
    ],
}
_ALERT_RULES = {
    "value": [
        {
            "id": "/subscriptions/123/resourceGroups/RG/providers/Microsoft.OperationalInsights/workspaces/WSName/providers/Microsoft.SecurityInsights/alertRules/123",
            "name": "123",
            "etag": '"123"',
            "type": "Microsoft.SecurityInsights/alertRules",
            "kind": "Scheduled",
            "properties": {
                "severity": "Medium",
                "query": "AlertText",
                "queryFrequency": "PT5H",
                "queryPeriod": "PT5H",
                "triggerOperator": "GreaterThan",
                "triggerThreshold": 5,
                "suppressionDuration": "PT5H",
                "suppressionEnabled": False,
                "incidentConfiguration": {
                    "createIncident": True,
                    "groupingConfiguration": {
                        "enabled": False,
                        "reopenClosedIncident": False,
                        "lookbackDuration": "PT5H",
                        "entitiesMatchingMethod": "All",
                        "groupByEntities": [],
                    },
                },
                "eventGroupingSettings": {"aggregationKind": "SingleAlert"},
                "displayName": "Suspect logon from an IP address recently seen targeting a honeypot",
                "enabled": True,
                "description": "A successful Azure Active Directory sign-in event originates from an IP address seen accessing a storage honeybucket!",
                "tactics": ["InitialAccess"],
                "alertRuleTemplateName": None,
                "lastModifiedUtc": "2020-04-01T15:00:57.6401532Z",
            },
        }
    ]
}
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


def test_azuresent_init():
    """Test class initalization."""
    azs = AzureSentinel()
    assert isinstance(azs, AzureSentinel)


def test_azuresent_connect_exp():
    """Test connect failure."""
    with pytest.raises(ClientAuthenticationError):
        azs = AzureSentinel()
        azs.connect(auth_methods=["env"])


@pytest.fixture(scope="module")
@patch(AzureSentinel.__module__ + ".AzureSentinel.connect")
def azs_loader(mock_creds):
    """Generate AzureSentinel for testing."""
    mock_creds.return_value = None
    azs = AzureSentinel()
    azs.connect()
    azs.token = "123"
    return azs


@patch(AzureData.__module__ + ".AzureData.get_resources")
@patch(AzureData.__module__ + ".AzureData.get_resource_details")
def test_azuresent_workspaces(mock_res_dets, mock_res, azs_loader):
    """Test Sentinel workspaces feature."""
    mock_res.return_value = _RESOURCES
    mock_res_dets.return_value = _RESOURCE_DETAILS
    workspaces = azs_loader.get_sentinel_workspaces(sub_id="123")
    assert isinstance(workspaces, dict)
    assert workspaces["ABC"] == "ABC"


@responses.activate
def test_azuresent_hunting_queries(azs_loader):
    """Test Sentinel hunting feature."""
    responses.add(
        responses.GET,
        re.compile("https://management.azure.com/.*"),
        json=_HUNTING_QUERIES,
        status=200,
    )
    hqs = azs_loader.get_hunting_queries(sub_id="123", res_grp="RG", ws_name="WSName")
    assert isinstance(hqs, pd.DataFrame)
    assert hqs["properties.Query"].iloc[0] == "QueryText"


@responses.activate
def test_azuresent_alert_rules(azs_loader):
    """Test Sentinel alert feature."""
    responses.add(
        responses.GET,
        re.compile("https://management.azure.com/.*"),
        json=_ALERT_RULES,
        status=200,
    )
    alerts = azs_loader.get_alert_rules(sub_id="123", res_grp="RG", ws_name="WSName")
    assert isinstance(alerts, pd.DataFrame)
    assert alerts["properties.query"].iloc[0] == "AlertText"


@responses.activate
def test_azuresent_bookmarks(azs_loader):
    """Test Sentinel bookmarks feature."""
    responses.add(
        responses.GET,
        re.compile("https://management.azure.com/.*"),
        json=_BOOKMARK,
        status=200,
    )
    bkmarks = azs_loader.get_bookmarks(sub_id="123", res_grp="RG", ws_name="WSName")
    assert isinstance(bkmarks, pd.DataFrame)
    print(bkmarks.columns)
    assert bkmarks["name"].iloc[0] == "Bookmark Test"


@responses.activate
def test_azuresent_incidents(azs_loader):
    """Test Sentinel incidents feature."""
    responses.add(
        responses.GET,
        re.compile("https://management.azure.com/.*"),
        json=_INCIDENT,
        status=200,
    )
    incidents = azs_loader.get_incidents(sub_id="123", res_grp="RG", ws_name="WSName")
    assert isinstance(incidents, pd.DataFrame)
    assert incidents["name"].iloc[0] == "13ffba29-971c-4d70-9cb4-ddd0ec1bbb84"
    incident = azs_loader.get_incident(
        incident_id="13ffba29-971c-4d70-9cb4-ddd0ec1bbb84",
        sub_id="123",
        res_grp="RG",
        ws_name="WSName",
    )
    assert isinstance(incident, pd.DataFrame)
    assert incident["name"].iloc[0] == "13ffba29-971c-4d70-9cb4-ddd0ec1bbb84"


@responses.activate
def test_azuresent_updates(azs_loader):
    """Test Sentinel incident update feature."""
    responses.add(
        responses.PUT,
        re.compile("https://management.azure.com/.*"),
        json="",
        status=201,
    )
    responses.add(
        responses.GET,
        re.compile("https://management.azure.com/.*"),
        json=_INCIDENT,
        status=200,
    )
    azs_loader.post_comment(
        incident_id="13ffba29-971c-4d70-9cb4-ddd0ec1bbb84",
        comment="test",
        sub_id="123",
        res_grp="RG",
        ws_name="WSName",
    )


@responses.activate
def test_azuresent_comments(azs_loader):
    """Test Sentinel comments feature."""
    responses.add(
        responses.PUT,
        re.compile("https://management.azure.com/.*"),
        json="",
        status=200,
    )
    responses.add(
        responses.GET,
        re.compile("https://management.azure.com/.*"),
        json=_INCIDENT,
        status=200,
    )
    azs_loader.update_incident(
        incident_id="13ffba29-971c-4d70-9cb4-ddd0ec1bbb84",
        update_items={"severity": "High"},
        sub_id="123",
        res_grp="RG",
        ws_name="WSName",
    )
