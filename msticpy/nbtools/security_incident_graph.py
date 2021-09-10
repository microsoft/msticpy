# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
Creates an entity graph for an Azure Sentinel Incident
"""
import networkx as nx
import pandas as pd

from .security_alert import SecurityAlert
from .nbdisplay import draw_alert_entity_graph
from ..datamodel.entities import Entity
from ..common.utility import export, is_not_empty
from .._version import VERSION

__version__ = VERSION
__author__ = "Pete Bryan"


class IncidentGraph():

    def __init__(self, incident:pd.Series):
        self.alertentity_graph = nx.Graph(id="IncidentGraph")
        self.incident_name = "Incident: " + incident["DisplayName"]
        self._add_incident_node(incident)

        if incident.Alerts:
            for alert in incident.Alerts:
                alert_name = "Alert: " + alert["name"]
                self._add_alert_node(alert, alert_name)
                if alert["entities"]:
                    for ent in alert["entities"]:
                        self._add_entity_node(ent, alert_name)

        if incident.Entities:
            entities = _dedupe_entities(incident.Alerts, incident.Entities)
            for ent in entities:
                self._add_entity_node(ent, self.incident_name)

    def _add_entity_node(self, ent, attached_to=None):
        e_name = ent.name_str
        e_desc = ent.description_str
        self.alertentity_graph.add_node(
            e_name,
            entitytype=ent.Type,
            name=e_name,
            description=e_desc,
            color="green",
            node_type="entity",
            source=str(ent),
        )
        if attached_to:
            self.alertentity_graph.add_edge(attached_to, e_name)

    def _add_alert_node(self, alert, alert_name):
        self.alertentity_graph.add_node(
            alert_name,
            name=alert["name"],
            description=alert["id"],
            color="orange",
            node_type="alert",
            source=str(alert),
        )
        if self.incident_name:
            self.alertentity_graph.add_edge(self.incident_name, alert_name)

    def _add_incident_node(self, incident):
        self.incident_name = "Incident: " + incident.DisplayName
        self.alertentity_graph.add_node(
            self.incident_name,
            name=incident.DisplayName,
            time=str(incident.TimeGenerated),
            description=self.incident_name,
            color="red",
            node_type="incident",
        )

    def plot(self):
        draw_alert_entity_graph(self.alertentity_graph)

    @property
    def __graph__(self):
        return self.alertentity_graph

    def _find_graph_node(self, node_type, target_name):
        """Find a node with a given name and type."""
        node_prefix = "{}: {}".format(node_type, target_name)
        nodes = [
            n
            for (n, n_type) in nx.get_node_attributes(self.alertentity_graph, "entitytype").items()
            if n_type == node_type and n.startswith(node_prefix)
        ]
        if nodes:
            return nodes[0]
        return None

    @export
    def add_related_alerts(self, related_alerts: pd.DataFrame) -> nx.Graph:
        """
        Add related alerts to the graph.

        Link to the entity that is common to both alerts.
        """

        alert_host_node = self._find_graph_node("host", "")

        related_alerts.apply(lambda x: self._add_alert_node(x), axis=1)
        if alert_host_node:
            related_alerts.apply(
                lambda x: self._add_related_alert_edges(
                    x, alert_host_node
                ),
                axis=1,
            )


    def _add_related_alert_edges(self, alert_row, default_node):
        related_alert = SecurityAlert(alert_row)
        if related_alert.primary_account is not None:
            acct_node = self._find_graph_node(
                "account",
                related_alert.primary_account.qualified_name,
                )
            if acct_node is not None:
                self._add_related_alert_edge(acct_node, related_alert)

        if related_alert.primary_process is not None:
            proc_node = self._find_graph_node(
                "process",
                related_alert.primary_process.ProcessFilePath,
                )
            if proc_node is not None:
                self._add_related_alert_edge(proc_node, related_alert)

        if related_alert.primary_host is not None:
            host_node = self._find_graph_node("host", related_alert.primary_host["HostName"]
                )
            if host_node is not None:
                self._add_related_alert_edge(host_node, related_alert)

        # if we haven't added an edge to this entity from anything else,
        # add one to the alert
        if not self.alertentity_graph[related_alert["AlertType"] + "(R)"]:
            self._add_related_alert_edge(default_node, related_alert)


    def _add_related_alert_edge(self, source, target):
        """Add related alert to an existing graph."""
        count_attrs = nx.get_node_attributes(self.alertentity_graph, "count")
        target_node = target["AlertType"] + "(R)"
        current_count = count_attrs[target_node] if target_node in count_attrs else 0
        current_count += 1

        description = "Related alert: {}  Count:{}".format(
            target["AlertType"], current_count
        )
        node_attrs = {target_node: {"count": current_count, "description": description}}
        nx.set_node_attributes(self.alertentity_graph, node_attrs)
        self.alertentity_graph.add_edge(source, target_node, weight=0.7, description="Related Alert")


def _dedupe_entities(alerts, ents):
    alrt_ents = [ent.__hash__() for alrt in alerts for ent in alrt["entities"]]
    for ent in ents:
        if ent.__hash__() in alrt_ents:
            ents.remove(ent)
    return ents
