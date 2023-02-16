# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Azure Sentinel unit tests."""
import re

import pandas as pd
import respx

# pylint: disable=redefined-outer-name, unused-import, no-name-in-module
from .sentinel_test_fixtures import sent_loader

_HUNTING_QUERIES = {
    "__metadata": {},
    "value": [
        {
            "id": "subscriptions/123/resourceGroups/RG/providers/Microsoft.OperationalInsights/workspaces/WSNAME/savedSearches/123",
            "etag": "Tag",
            "properties": {
                "category": "Hunting Queries",
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
        },
        {
            "id": "subscriptions/123/resourceGroups/RG/providers/Microsoft.OperationalInsights/workspaces/WSNAME/savedSearches/123",
            "etag": "Tag",
            "properties": {
                "category": "Saved Queries",
                "DisplayName": "SavedQueries",
                "Query": "SavedQueryText",
                "Tags": [
                    {"Name": "description", "Value": ""},
                    {"Name": "tactics", "Value": ""},
                    {"Name": "t-skang@microsoft.com", "Value": "false"},
                ],
                "Version": 2,
            },
            "name": "123",
            "type": "Microsoft.OperationalInsights/savedSearches",
        },
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


@respx.mock
def test_sent_hunting_queries(sent_loader):
    """Test Sentinel hunting feature."""
    respx.get(re.compile(r"https://management\.azure\.com/.*")).respond(
        200, json=_HUNTING_QUERIES
    )
    hqs = sent_loader.list_hunting_queries()
    assert isinstance(hqs, pd.DataFrame)
    assert 1 == len(hqs.index)
    assert hqs["properties.Query"].iloc[0] == "QueryText"


@respx.mock
def test_sent_saved_queries(sent_loader):
    """Test Sentinel hunting feature."""
    respx.get(re.compile(r"https://management\.azure\.com/.*")).respond(
        200, json=_HUNTING_QUERIES
    )
    sqs = sent_loader.list_saved_queries()
    assert isinstance(sqs, pd.DataFrame)
    assert 2 == len(sqs.index)
    assert sqs["properties.Query"].iloc[1] == "SavedQueryText"


@respx.mock
def test_sent_alert_rules(sent_loader):
    """Test Sentinel alert feature."""
    respx.get(re.compile(r"https://management\.azure\.com/.*")).respond(
        200, json=_ALERT_RULES
    )
    alerts = sent_loader.list_alert_rules()
    assert isinstance(alerts, pd.DataFrame)
    assert alerts["properties.query"].iloc[0] == "AlertText"


@respx.mock
def test_sent_analytic_create(sent_loader):
    """Test Sentinel analytics feature."""
    json_resp = {
        "name": "508f3c50-f6d3-45b3-8321-fb674afe3478",
        "properties": {
            "displayName": "Test Bookmark",
            "query": "SecurityAlert | take 10",
            "queryFrequency": "PT1H",
            "queryPeriod": "PT1H",
            "severity": "Low",
            "triggerOperator": "GreaterThan",
            "triggerThreshold": 0,
            "description": "Test Template",
            "tactics": ["test1"],
        },
    }
    respx.put(re.compile(r"https://management\.azure\.com/.*/alertRules/.*")).respond(
        201, json=json_resp
    )
    respx.get(
        re.compile(r"https://management\.azure\.com/.*/alertRuleTemplates")
    ).respond(200, json={"value": [json_resp]})
    sent_loader.create_analytic_rule("508f3c50-f6d3-45b3-8321-fb674afe3478")
    sent_loader.create_analytic_rule("Test Bookmark")
    sent_loader.create_analytic_rule(name="Test Rule", query="SecurityAlert | take 10")


@respx.mock
def test_sent_analytics_delete(sent_loader):
    """Test Sentinel analytics feature."""
    respx.delete(
        re.compile(r"https://management\.azure\.com/.*/alertRules/.*")
    ).respond(200)
    sent_loader.delete_analytic_rule("508f3c50-f6d3-45b3-8321-fb674afe3478")
