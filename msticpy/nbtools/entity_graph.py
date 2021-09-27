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
from typing import Union

from ..datamodel.entities.alert import Alert
from ..datamodel.soc.incident import Incident
from .nbdisplay import draw_alert_entity_graph
from ..datamodel.entities import Entity
from .._version import VERSION

__version__ = VERSION
__author__ = "Pete Bryan"


class EntityGraph():

    def __init__(self, entity: Union[Incident, Alert, pd.DataFrame, pd.Series, Entity]):
        self.alertentity_graph = nx.Graph(id="IncidentGraph")
        if isinstance(entity, Incident) or isinstance(entity, Alert):
            self._check_type_create(entity)
        if isinstance(entity, pd.DataFrame):
            self.add_incident(entity)
        if isinstance(entity, pd.Series):
            self.add_incident(entity.iloc[0].to_frame().T)
        if isinstance(entity, Entity):
            self._add_entity_node(entity)

    def plot(self):
        """Plot the entity graph using Bokeh."""
        draw_alert_entity_graph(self.alertentity_graph)

    def add_entity(self, ent: Entity, attached_to: str = None):
        """
        Add an entity to the graph

        Parameters
        ----------
        ent : Entity
            The entity object to add the graph
        attached_to : str, optional
            The name of the node to attach the entity to, by default None

        """
        self._add_entity_node(ent, attached_to)

    def add_incident(self, incident: Union[Incident, Alert, pd.DataFrame]):
        """
        Add another incident or set of incidents to the graph.

        Parameters
        ----------
        incident : Union[Incident, Alert, pd.DataFrame]
            This can be an alert, and incident or a DataFrame of alerts or incidents
        """
        inc = None
        if isinstance(incident, pd.DataFrame):
            for row in incident.iterrows():
                if "name" in row[1]:
                    inc = Incident(src_event=row[1])
                elif "AlertName" in row[1]:
                    inc = Alert(src_event=row[1])
                self._check_type_create(inc)
        else:
            self._check_type_create(incident)

    def add_link(self, name1, name2):
        #Check names are present
        if name1 in self.alertentity_graph.nodes() and name2 in self.alertentity_graph.nodes():
            self.alertentity_graph.add_edge(name1, name2)
        else:
            missing = [name for name in [name1, name2] if name not in self.alertentity_graph.nodes()]
            print(f"Node(s) {missing} not found in graph")

    def remove_node(self, name):
        #Check node is present
        if name in self.alertentity_graph.nodes():
            self.alertentity_graph.remove_node(name)
        else:
            print(f"Node named {name} not found")


    def _check_type_create(self, incident: Union[Incident, Alert]):
        if isinstance(incident, Incident):
            self._create_incident_graph(incident)
        elif isinstance(incident, Alert):
            self._create_alert_graph(incident)

    def _create_incident_graph(self, incident):
        """Create graph of an incident entity"""
        incident_name = "Incident: " + incident["DisplayName"]
        self._add_incident_node(incident)
        if incident.Alerts:
            for alert in incident.Alerts:
                alert_name = "Alert: " + alert["DisplayName"]
                self._add_alert_node(alert, alert_name, "Incident: " + incident["DisplayName"])
                if alert["Entities"]:
                    for ent in alert["Entities"]:
                        self._add_entity_node(ent, alert_name)
        if incident.Entities:
            entities = _dedupe_entities(incident.Alerts, incident.Entities)
            for ent in entities:
                self._add_entity_node(ent, incident_name)

    def _create_alert_graph(self, incident):
        """Create graph of an alert entity"""
        alert_name = "Alert: " + incident["DisplayName"]
        self._add_alert_node(incident, alert_name)
        if incident.Entities:
            for ent in incident.Entities:
                self._add_entity_node(ent, alert_name)

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
            source=str(ent)
        )
        if attached_to:
            self.add_link(e_name, attached_to)
        if ent.edges:
            for edge in ent.edges:
                try:
                    self.add_link(e_name, edge)
                except:
                    pass

    def _add_alert_node(self, alert, alert_name=None, incident_name=None):
        if not alert_name:
            alert_name = "Alert: " + alert["DisplayName"]
        self.alertentity_graph.add_node(
            alert_name,
            name=alert["DisplayName"],
            description=alert["ProviderName"],
            color="orange",
            node_type="alert",
            source=str(alert),
            entitytype="alert",
        )
        if incident_name:
            self.add_link(incident_name, alert_name)

    def _add_incident_node(self, incident):
        incident_name = "Incident: " + incident.DisplayName
        self.alertentity_graph.add_node(
            incident_name,
            name=incident.DisplayName,
            time=str(incident.TimeGenerated),
            description=incident.IncidentID,
            color="red",
            node_type="incident",
            entitytype="incident",
        )


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



def _dedupe_entities(alerts, ents):
    alrt_ents = []
    for alrt in alerts:
        if alrt["Entities"]:

            alrt_ents += [ent.__hash__() for ent in alrt["Entities"]]
    for ent in ents:
        if ent.__hash__() in alrt_ents:
            ents.remove(ent)
    return ents