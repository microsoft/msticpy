# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Azure Sentinel unit tests."""
import re

import pandas as pd
import pytest
import respx

from msticpy.common.exceptions import MsticpyUserError

# pylint: disable=redefined-outer-name, unused-import, no-name-in-module
from .sentinel_test_fixtures import sent_loader

_TI_RESULTS = {
    "value": [
        {
            "id": "/subscriptions/123/resourceGroups/soc/providers/Microsoft.OperationalInsights/workspaces/CyberSecuritySoc/providers/Microsoft.SecurityInsights/threatIntelligence/123",
            "name": "123",
            "etag": '"123"',
            "type": "Microsoft.SecurityInsights/threatIntelligence",
            "kind": "indicator",
            "properties": {
                "confidence": 100,
                "created": "2022-09-30T21:16:51.7325435Z",
                "createdByRef": "identity--1234",
                "extensions": {
                    "sentinel-ext": {"severity": None},
                    "IndicatorProvider": "Microsoft",
                },
                "externalId": "indicator--7604154e-bd9b-ebb7-6c53-8fb7fae5969f",
                "externalLastUpdatedTimeUtc": "2022-09-30T21:16:51.7325435Z",
                "externalReferences": [
                    {
                        "description": "This is some TI",
                        "externalId": "7604154ebd9bebb76c538fb7fae5969f9ac0cfdb15308567d1177890e6dc7b22",
                        "sourceName": "TIData",
                    }
                ],
                "lastUpdatedTimeUtc": "2022-09-30T21:21:28.6388624Z",
                "objectMarkingRefs": [
                    "marking-definition--34098fce-860f-48ae-8e50-ebd3cc5e41da"
                ],
                "source": "Microsoft Emerging Threat Feed",
                "displayName": "Microsoft Identified Botnet",
                "description": "MSTIC HoneyPot: An attacker used a brute force attack to gain access to a service or device",
                "threatTypes": ["Botnet"],
                "parsedPattern": [
                    {
                        "patternTypeKey": "network-traffic",
                        "patternTypeValues": [
                            {"valueType": "src_ref.value", "value": "999.999.999.999"}
                        ],
                    }
                ],
                "pattern": "[network-traffic:src_ref.value = '999.999.999.999']",
                "patternType": "stix",
                "validFrom": "2022-09-30T21:16:51.7325435Z",
                "validUntil": "2022-10-01T02:02:43.8744244Z",
            },
        },
        {
            "id": "/subscriptions/123/resourceGroups/soc/providers/Microsoft.OperationalInsights/workspaces/CyberSecuritySoc/providers/Microsoft.SecurityInsights/threatIntelligence/123",
            "name": "234",
            "etag": '"234"',
            "type": "Microsoft.SecurityInsights/threatIntelligence",
            "kind": "indicator",
            "properties": {
                "confidence": 100,
                "created": "2022-09-30T21:16:51.7325435Z",
                "createdByRef": "identity--1234",
                "extensions": {
                    "sentinel-ext": {"severity": None},
                    "IndicatorProvider": "Microsoft",
                },
                "externalId": "indicator--aba98e7c-473d-861d-5833-c01848fa7809",
                "externalLastUpdatedTimeUtc": "2022-09-30T21:16:51.7325435Z",
                "externalReferences": [
                    {
                        "description": "This is some TI",
                        "externalId": "aba98e7c473d861d5833c01848fa780983a996e3c8dc5cba8dfdb4e81973a4b0",
                        "sourceName": "TIData",
                    }
                ],
                "lastUpdatedTimeUtc": "2022-09-30T21:21:02.9017668Z",
                "objectMarkingRefs": [
                    "marking-definition--34098fce-860f-48ae-8e50-ebd3cc5e41da"
                ],
                "source": "Microsoft Emerging Threat Feed",
                "displayName": "Microsoft Identified Botnet",
                "description": "MSTIC HoneyPot: An attacker used a brute force attack to gain access to a service or device",
                "threatTypes": ["Botnet"],
                "parsedPattern": [
                    {
                        "patternTypeKey": "network-traffic",
                        "patternTypeValues": [
                            {"valueType": "src_ref.value", "value": "999.999.999.997"}
                        ],
                    }
                ],
                "pattern": "[network-traffic:src_ref.value = '999.999.999.997']",
                "patternType": "stix",
                "validFrom": "2022-09-30T21:16:51.7325435Z",
                "validUntil": "2022-10-01T02:03:34.4705677Z",
            },
        },
    ]
}
_TI_RESULT = {
    "id": "/subscriptions/123/resourceGroups/soc/providers/Microsoft.OperationalInsights/workspaces/CyberSecuritySoc/providers/Microsoft.SecurityInsights/threatIntelligence/456",
    "name": "456",
    "etag": '"1d009607-0000-0100-0000-63375dda0000"',
    "type": "Microsoft.SecurityInsights/threatIntelligence",
    "kind": "indicator",
    "properties": {
        "confidence": 100,
        "created": "2022-09-30T21:16:51.7325435Z",
        "createdByRef": "identity--d7adaba4-c743-4ac3-ac90-798880696e84",
        "extensions": {
            "sentinel-ext": {"severity": None},
            "IndicatorProvider": "Microsoft",
        },
        "externalId": "indicator--7604154e-bd9b-ebb7-6c53-8fb7fae5969f",
        "externalLastUpdatedTimeUtc": "2022-09-30T21:16:51.7325435Z",
        "externalReferences": [
            {
                "description": "This STIX Object was created from a Microsoft OneIndicator Object.",
                "externalId": "7604154ebd9bebb76c538fb7fae5969f9ac0cfdb15308567d1177890e6dc7b22",
                "sourceName": "Interflow",
            }
        ],
        "lastUpdatedTimeUtc": "2022-09-30T21:21:28.6388624Z",
        "objectMarkingRefs": [
            "marking-definition--34098fce-860f-48ae-8e50-ebd3cc5e41da"
        ],
        "source": "Microsoft Emerging Threat Feed",
        "displayName": "Microsoft Identified Botnet",
        "threatIntelligenceTags": ["test"],
        "description": "MSTIC HoneyPot: An attacker used a brute force attack to gain access to a service or device",
        "threatTypes": ["Botnet"],
        "parsedPattern": [
            {
                "patternTypeKey": "network-traffic",
                "patternTypeValues": [
                    {"valueType": "src_ref.value", "value": "999.999.999.999"}
                ],
            }
        ],
        "pattern": "[network-traffic:src_ref.value = '999.999.999.999']",
        "patternType": "stix",
        "validFrom": "2022-09-30T21:16:51.7325435Z",
        "validUntil": "2022-10-01T02:02:43.8744244Z",
    },
}

_TI_METRICS = {
    "value": [
        {
            "properties": {
                "lastUpdatedTimeUtc": "2022-09-30T12:00:11.2609563Z",
                "threatTypeMetrics": [
                    {"metricName": "anomalous-activity", "metricValue": 0},
                    {"metricName": "anonymization", "metricValue": 0},
                    {"metricName": "attribution", "metricValue": 0},
                    {"metricName": "benign", "metricValue": 0},
                    {"metricName": "eq", "metricValue": 0},
                    {"metricName": "turla", "metricValue": 0},
                ],
                "patternTypeMetrics": [
                    {"metricName": "domain-name", "metricValue": 0},
                    {"metricName": "ipv4-addr", "metricValue": 0},
                    {"metricName": "url", "metricValue": 0},
                ],
                "sourceMetrics": [
                    {"metricName": "Azure Sentinel", "metricValue": 22},
                    {"metricName": "MSTICPy", "metricValue": 0},
                    {"metricName": "Microsoft Sentinel", "metricValue": 0},
                ],
            }
        }
    ]
}


@respx.mock
def test_sent_ti_results(sent_loader):
    """Test Sentinel hunting feature."""
    respx.get(re.compile(r"https://management\.azure\.com/.*")).respond(
        200, json=_TI_RESULTS
    )
    ti_items = sent_loader.get_all_indicators()
    assert isinstance(ti_items, pd.DataFrame)
    assert 2 == len(ti_items.index)
    assert ti_items["properties.confidence"].iloc[0] == 100


@respx.mock
def test_sent_ti_result(sent_loader):
    """Test Sentinel hunting feature."""
    respx.get(re.compile(r"https://management\.azure\.com/.*")).respond(
        200, json=_TI_RESULT
    )
    ti_ind = sent_loader.get_indicator("456")
    assert isinstance(ti_ind, dict)
    assert 6 == len(ti_ind.keys())
    assert ti_ind["properties"]["source"] == "Microsoft Emerging Threat Feed"


@respx.mock
def test_sent_ti_create(sent_loader):
    """Test Sentinel alert feature."""
    respx.post(re.compile(r"https://management\.azure\.com/.*")).respond(
        200, json=_TI_RESULT
    )
    sent_loader.create_indicator(indicator="1.1.1.1", ioc_type="ipv4")
    with pytest.raises(MsticpyUserError):
        sent_loader.create_indicator(indicator="1.1.1.1", ioc_type="not a type")
    with pytest.raises(MsticpyUserError):
        sent_loader.create_indicator(
            indicator="1.1.1.1", ioc_type="nipv4-addr", confidence=40000
        )


@respx.mock
def test_sent_ti_bulk_create(sent_loader):
    """Test Sentinel analytics feature."""
    respx.post(re.compile(r"https://management\.azure\.com/.*")).respond(
        200, json=_TI_RESULT
    )
    df = pd.DataFrame(
        {
            "Observable": ["1.1.1.1", "2.2.2.2", "3.3.3.3"],
            "IoCType": ["ipv4", "ipv4", "ipv4"],
        }
    )
    sent_loader.bulk_create_indicators(df)


@respx.mock
def test_sent_ti_update_indicator(sent_loader):
    """Test Sentinel analytics feature."""
    respx.get(re.compile(r"https://management\.azure\.com/.*")).respond(
        200, json=_TI_RESULT
    )
    respx.put(re.compile(r"https://management\.azure\.com/.*")).respond(200)
    sent_loader.update_indicator(indicator_id="456", labels=["test"])


@respx.mock
def test_sent_ti_add_tag(sent_loader):
    """Test Sentinel analytics feature."""
    respx.get(re.compile(r"https://management\.azure\.com/.*")).respond(
        200, json=_TI_RESULT
    )
    respx.put(re.compile(r"https://management\.azure\.com/.*")).respond(200)
    sent_loader.add_tag("456", "test2")


@respx.mock
def test_sent_ti_delete_indicator(sent_loader):
    """Test Sentinel analytics feature."""
    respx.delete(re.compile(r"https://management\.azure\.com/.*")).respond(200)
    sent_loader.delete_indicator("456")


@respx.mock
def test_sent_ti_query_indicator(sent_loader):
    """Test Sentinel analytics feature."""
    respx.post(re.compile(r"https://management\.azure\.com/.*")).respond(
        200, json=_TI_RESULTS
    )
    sent_loader.query_indicators(minConfidence=10, maxConfidence=100)


@respx.mock
def test_sent_ti_metrics(sent_loader):
    """Test Sentinel analytics feature."""
    respx.get(re.compile(r"https://management\.azure\.com/.*")).respond(
        200, json=_TI_METRICS
    )
    sent_loader.get_ti_metrics()
