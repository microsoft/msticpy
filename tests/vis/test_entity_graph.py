# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Test module for EntityGraph."""
import pandas as pd
from bokeh.models.layouts import Column
from bokeh.plotting.figure import Figure
from msticpy.datamodel.entities import Alert, Entity, Incident
from msticpy.nbtools.security_alert import SecurityAlert

# pylint: disable=unused-import
from msticpy.vis import mp_pandas_plot  # noqa: F401
from msticpy.vis.entity_graph_tools import EntityGraph

from ..nbtools.test_security_alert import sample_alert
from ..unit_test_lib import get_test_data_path

__author__ = "Pete Bryan"

inc = {
    "id": "123",
    "name": "135a072e-77c3-4293-8c9a-7fdd53b5d620",
    "etag": '"d4fce673-dd4b-4d22-a39f-bda55d6079f3"',
    "type": "Microsoft.SecurityInsights/Incidents",
    "properties.title": "Sample Incident",
    "properties.description": "This is a sample incident to support unit tests",
    "properties.severity": "Medium",
    "properties.status": "Active",
    "properties.owner.objectId": "0a70480d-b1cd-4466-9b75-3814e34579eb",
    "properties.owner.email": "user@contoso.com",
    "properties.owner.assignedTo": "A User",
    "properties.owner.userPrincipalName": "user@contoso.com",
    "properties.labels": [{"labelName": "Tests Label", "labelType": "User"}],
    "properties.firstActivityTimeUtc": "2021-09-22T14:39:24.04Z",
    "properties.lastActivityTimeUtc": "2021-09-22T14:39:24.04Z",
    "properties.lastModifiedTimeUtc": "2021-09-22T15:09:34.376619Z",
    "properties.createdTimeUtc": "2021-09-22T15:09:09.2786667Z",
    "properties.incidentNumber": 123,
    "properties.additionalData.alertsCount": 1,
    "properties.additionalData.bookmarksCount": 0,
    "properties.additionalData.commentsCount": 0,
    "properties.additionalData.alertProductNames": ["Azure Sentinel"],
    "properties.additionalData.tactics": ["PrivilegeEscalation"],
    "properties.relatedAnalyticRuleIds": ["123"],
    "properties.incidentUrl": "https://portal.azure.com/#asset/Microsoft_Azure_Security_Insights/Incident/subscriptions/",
    "Entities": [
        (
            "Host",
            {
                "dnsDomain": "demo.contoso.com",
                "hostName": "demo",
                "omsAgentID": "ce7903cf-2d8f-47e9-a338-2259f02a9779",
                "osFamily": "Windows",
                "osVersion": "10",
                "additionalData": {
                    "DataSource": "SecurityEvent",
                    "AzureResourceId": "/subscriptions/ce7903cf-2d8f-47e9-a338-2259f02a9779/resourcegroups/test/providers/microsoft.compute/virtualmachines/demo",
                    "SubscriptionId": "ce7903cf-2d8f-47e9-a338-2259f02a9779",
                    "ResourceId": "/subscriptions/ce7903cf-2d8f-47e9-a338-2259f02a9779/resourceGroups/test/providers/Microsoft.Compute/virtualMachines/demo",
                    "VMUUID": "ce7903cf-2d8f-47e9-a338-2259f02a9779",
                    "ShouldResolveIp": "False",
                },
                "friendlyName": "demo",
            },
        ),
        (
            "Account",
            {
                "accountName": "auser",
                "displayName": "CONTOSO\\auser",
                "friendlyName": "CONTOSO\\auser",
            },
        ),
    ],
    "Alerts": [
        {
            "ID": "8b7d06d8-dbae-4b23-87ed-1a27b75437d5",
            "Name": "User Added to Privileged Group in CONTOSO Domain",
            "Entities": None,
        }
    ],
    "Type": "incident",
}


incident = Incident(inc)
alert = Alert(sample_alert)
sec_alert = SecurityAlert(pd.Series(sample_alert))
entity = Entity.instantiate_entity(
    {
        "$id": "10",
        "Url": "https://www.contoso.com",
        "Type": "url",
        "ClickCount": 0,
        "EmailCount": 12,
        "Urn": "urn:UrlEntity:bdae5760b0104bac99e8a3f03f0ede2b",
        "Source": "OATP",
        "FirstSeen": "0001-01-01T00:00:00",
    }
)
pd_file = get_test_data_path().joinpath("sent_incidents.pkl")
sent_incidents = pd.read_pickle(pd_file)


def test_entity_graph_incident():
    """Test graph creation with an incident."""
    graph = EntityGraph(incident)
    assert len(graph.alertentity_graph.nodes()) == 4
    assert "Incident: Sample Incident" in graph.alertentity_graph.nodes()


def test_entity_alert_graph():
    """Test graph creation with an alert."""
    graph = EntityGraph(alert)
    assert len(graph.alertentity_graph.nodes()) == 17
    assert "cmd.exe" in graph.alertentity_graph.nodes()


def test_entity_sec_alert_graph():
    """Test graph creation with a security alert."""
    graph = EntityGraph(sec_alert)
    assert len(graph.alertentity_graph.nodes()) == 17
    assert "cmd.exe" in graph.alertentity_graph.nodes()


def test_entity_entity_graph():
    """Test graph creation with an entity."""
    graph = EntityGraph(entity)
    assert len(graph.alertentity_graph.nodes()) == 1
    assert "https://www.contoso.com" in graph.alertentity_graph.nodes()


def test_entity_add():
    """Test adding an entity to an existing graph."""
    graph = EntityGraph(incident)
    graph.add_entity(entity, attached_to="demo")
    assert len(graph.alertentity_graph.nodes()) == 5
    assert "https://www.contoso.com" in graph.alertentity_graph.nodes()
    assert graph.alertentity_graph.has_edge("https://www.contoso.com", "demo")


def test_incident_add():
    """Test adding an incident to an existing graph."""
    graph = EntityGraph(incident)
    graph.add_incident(alert)
    assert len(graph.alertentity_graph.nodes()) == 21
    assert "cmd.exe" in graph.alertentity_graph.nodes()


def test_note_add():
    """Test adding a note to a graph."""
    graph = EntityGraph(incident)
    graph.add_note("Test Note", attached_to="demo")
    assert len(graph.alertentity_graph.nodes()) == 5
    assert "Test Note" in graph.alertentity_graph.nodes()
    assert graph.alertentity_graph.has_edge("Test Note", "demo")


def test_link_add_remove():
    """Test adding and removing a link in an graph."""
    graph = EntityGraph(incident)
    graph.add_link("demo", "Alert: User Added to Privileged Group in CONTOSO Domain")
    assert graph.alertentity_graph.has_edge(
        "demo", "Alert: User Added to Privileged Group in CONTOSO Domain"
    )
    graph.remove_link("demo", "Alert: User Added to Privileged Group in CONTOSO Domain")
    assert not graph.alertentity_graph.has_edge(
        "demo", "Alert: User Added to Privileged Group in CONTOSO Domain"
    )


def test_node_remove():
    """Test removing a node from a graph."""
    graph = EntityGraph(incident)
    graph.remove_node("demo")
    assert not graph.alertentity_graph.has_node("demo")


def test_to_df():
    """Test exporting graph nodes to a dataframe."""
    graph = EntityGraph(incident)
    df = graph.to_df()
    assert len(df.index) == 4
    assert "demo" in df["Name"].values
    assert (
        "Alert: User Added to Privileged Group in CONTOSO Domain" in df["Name"].values
    )


def test_plot():
    """Test plotting produces Bokeh objects."""
    graph = EntityGraph(incident)
    plot = graph.plot(hide=True)
    tl_plot = graph.plot(hide=True, timeline=True)
    assert isinstance(plot, Figure)
    assert isinstance(tl_plot, Column)


def test_df_plot():
    """Test plotting from DataFrame"""
    plot = sent_incidents.mp_plot.incident_graph()
    assert isinstance(plot, Figure)
    plot = sent_incidents.mp_plot.incident_graph(timeline=True)
    assert isinstance(plot, Column)
