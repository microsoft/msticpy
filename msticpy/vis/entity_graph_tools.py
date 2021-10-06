# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
Creates an entity graph for an Azure Sentinel Incident
"""
from datetime import datetime
from typing import List, Optional, Union

import networkx as nx
import pandas as pd
from bokeh.io import output_notebook, show
from bokeh.layouts import column
from bokeh.models import LayoutDOM

from .._version import VERSION
from ..common.exceptions import MsticpyUserError
from ..datamodel.entities import Entity
from ..datamodel.entities.alert import Alert
from ..datamodel.entities.soc.incident import Incident
from ..nbtools.nbdisplay import plot_entity_graph
from ..nbtools.security_alert import SecurityAlert
from ..nbtools.timeline_duration import display_timeline_duration
from ..nbtools.timeline import display_timeline

__version__ = VERSION
__author__ = "Pete Bryan"

req_alert_cols = ["DisplayName", "Severity", "AlertType"]
req_inc_cols = ["id", "name", "properties.severity"]


class EntityGraph:
    """Create a graph for visualizing and tracking links between entities."""

    def __init__(
        self,
        entity: Union[Incident, Alert, pd.DataFrame, pd.Series, Entity, SecurityAlert],
    ):
        output_notebook()
        self.alertentity_graph = nx.Graph(id="IncidentGraph")
        if isinstance(entity, (Incident, Alert)):
            self._check_type_create(entity)
        elif isinstance(entity, pd.DataFrame):
            self.add_incident(entity)
        elif isinstance(entity, pd.Series):
            self.add_incident(entity.to_frame().T)
        elif isinstance(entity, Entity):
            self._add_entity_node(entity)
        elif isinstance(entity, SecurityAlert):
            entity = Alert(entity)  # type: ignore
            self._check_type_create(entity)

    def plot(self, hide: bool = False) -> LayoutDOM:
        """
        Plot a graph of entities.

        Parameters
        ----------
        hide : bool, optional
            Set true to not display the graphic, by default False

        Returns
        -------
        LayoutDOM
            A Bokeh figure object

        """
        return plot_entity_graph(self.alertentity_graph, hide=hide)

    def plot_with_timeline(self, hide: bool = False) -> LayoutDOM:
        """
        Plot the entity graph with a timeline.

        Parameters
        ----------
        hide : bool, optional
            Set true to not display the graphic, by default False

        Returns
        -------
        LayoutDOM
            A Bokeh figure object

        """
        timeline = None
        tl_df = self.to_df()
        tl_type = "duration"
        if len(tl_df["EndTime"].unique()) == 1 and not tl_df["EndTime"].unique()[0]:
            tl_type = "discreet"
            if (
                len(tl_df["TimeGenerated"].unique()) == 1
                and not tl_df["TimeGenerated"].unique()[0]
            ):
                print("No timestamps avalaible to create timeline")
                return self.plot()
        tl_df["TimeGenerated"] = pd.to_datetime(tl_df["TimeGenerated"], utc=True)
        tl_df["StartTime"] = pd.to_datetime(tl_df["StartTime"], utc=True)
        tl_df["EndTime"] = pd.to_datetime(tl_df["EndTime"], utc=True)
        graph = self.plot(hide=True)
        if tl_type == "duration":
            timeline = display_timeline_duration(
                tl_df.dropna(subset=["TimeGenerated"]),
                group_by="Name",
                title="Entity Timeline",
                time_column="StartTime",
                end_time_column="EndTime",
                source_columns=["Name", "Description", "Type", "TimeGenerated"],
                hide=True,
                width=800,
            )
        elif tl_type == "discreet":
            timeline = display_timeline(
                tl_df.dropna(subset=["TimeGenerated"]),
                group_by="Type",
                title="Entity Timeline",
                time_column="TimeGenerated",
                source_columns=["Name", "Description", "Type", "TimeGenerated"],
                hide=True,
                width=800,
            )
        plot_layout = column(graph, timeline) if timeline else graph
        if not hide:
            show(plot_layout)
        return plot_layout

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
                    inc = Alert(src_event=row[1])  # type: ignore
                self._check_type_create(inc)
        else:
            self._check_type_create(incident)

    def add_note(
        self,
        name: str,
        description: Optional[str] = None,
        attached_to: Union[str, List] = None,
        user: str = "Analyst",
        color: str = "blue",
    ):
        """
        Add a node to the graph representing a note or comment.

        Parameters
        ----------
        name : str
            The name of the node to add
        description : Optional[str], optional
            A description of the note, by default None
        color : str, optional
            What color to make the node on the graph, by default "blue"
        attached_to : Union[str, List], optional
            What existing nodes on the graph to attach it the note to, by default None

        """
        self.alertentity_graph.add_node(
            name,
            name=name,
            description=description,
            color=color,
            node_type="analystnote",
            user=user,
            time_generated=datetime.now(),
        )
        if attached_to:
            if isinstance(attached_to, str):
                attached_to = [attached_to]
            for link in attached_to:
                self.add_link(name, link)

    def add_link(self, source: str, target: str):
        """
        Add a link between 2 nodes on the graph

        Parameters
        ----------
        source : str
            Name of node to link from
        target : str
            Name of node to link to

        Raises
        ------
        MsticpyUserError
            If nodes aren't present in the graph


        """
        # Check names are present
        if (
            source in self.alertentity_graph.nodes()
            and target in self.alertentity_graph.nodes()
        ):
            self.alertentity_graph.add_edge(source, target)
        else:
            missing = [
                name
                for name in [source, target]
                if name not in self.alertentity_graph.nodes()
            ]
            raise MsticpyUserError(title=f"Node(s) {missing} not found in graph")

    def remove_link(self, source: str, target: str):
        """
        Remove a link between 2 nodes on the graph

        Parameters
        ----------
        source : str
            Name of node to remove link from
        target : str
            name of node to remove link to

        Raises
        ------
        MsticpyUserError
            If edge isn't present in the graph

        """
        if (
            source in self.alertentity_graph.nodes()
            and target in self.alertentity_graph.nodes()
            and self.alertentity_graph.has_edge(source, target)
        ):
            self.alertentity_graph.remove_edge(source, target)
        else:
            raise MsticpyUserError(
                title=f"No edge exists between {source} and {target}"
            )

    def remove_node(self, name: str):
        """
        Remove a node from the graph

        Parameters
        ----------
        name : str
            The name of the node to remove.

        """
        # Check node is present
        if name in self.alertentity_graph.nodes():
            self.alertentity_graph.remove_node(name)
        else:
            raise MsticpyUserError(f"Node named {name} not found")

    def to_df(self) -> pd.DataFrame:
        """Generate a dataframe of nodes in the graph."""
        names = [node[1]["name"] for node in self.alertentity_graph.nodes.items()]
        descs = [
            node[1]["description"] for node in self.alertentity_graph.nodes.items()
        ]
        types = [node[1]["node_type"] for node in self.alertentity_graph.nodes.items()]
        times = [
            node[1]["time_generated"] if "time_generated" in node[1] else None
            for node in self.alertentity_graph.nodes.items()
        ]
        starttimes = [
            node[1]["start_time"]
            if "start_time" in node[1]
            else node[1]["time_generated"]
            for node in self.alertentity_graph.nodes.items()
        ]
        endtimes = [
            node[1]["end_time"] if "end_time" in node[1] else None
            for node in self.alertentity_graph.nodes.items()
        ]
        return pd.DataFrame(
            {
                "Name": names,
                "Description": descs,
                "Type": types,
                "TimeGenerated": times,
                "EndTime": endtimes,
                "StartTime": starttimes,
            }
        )

    def _check_type_create(self, incident: Union[Incident, Alert, None]):
        """Checks what type of entity is passed in and creates relevent graph."""
        if isinstance(incident, Incident):
            self._create_incident_graph(incident)
        elif isinstance(incident, Alert):
            self._create_alert_graph(incident)

    def _create_incident_graph(self, incident):
        """Create graph of an incident entity"""
        incident_name = "Incident: " + incident["DisplayName"]
        self._add_incident_node(incident)
        if incident.Alerts:
            # do the ref mapping stuff here
            for alert in incident.Alerts:
                alert_name = f"Alert: {alert['DisplayName']}"
                incident_name = f"Incident: {incident['DisplayName']}"
                self._add_alert_node(alert, alert_name, incident_name)
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
        """Add an Entity to the graph."""
        e_name = ent.name_str
        e_desc = ent.description_str
        e_startime = ent.StartTime or ent.TimeGenerated
        self.alertentity_graph.add_node(
            e_name,
            entitytype=ent.Type,
            name=e_name,
            description=e_desc,
            color="green",
            node_type="entity",
            source=str(ent),
            start_time=e_startime,
            end_time=ent.EndTime,
        )
        if attached_to:
            self.add_link(e_name, attached_to)
        if ent.has_edge:
            self._add_entity_edges(ent.edges, e_name)

    def _add_alert_node(self, alert, alert_name=None, incident_name=None):
        """Add an alert entity to the graph."""
        if not alert_name:
            alert_name = "Alert: " + alert.name_str
        self.alertentity_graph.add_node(
            alert_name,
            name=alert.name_str,
            description=alert.description_str,
            color="orange",
            node_type="alert",
            source=str(alert),
            entitytype="alert",
            time_generated=alert.TimeGenerated,
            start_time=alert.StartTime,
            end_time=alert.EndTime,
        )
        if incident_name:
            self.add_link(incident_name, alert_name)
        if alert.has_edge:
            self._add_entity_edges(alert.edges, alert_name)

    def _add_incident_node(self, incident):
        """Add an incident entity to the graph."""
        incident_name = "Incident: " + incident.name_str
        self.alertentity_graph.add_node(
            incident_name,
            name=incident.name_str,
            time_generated=incident.TimeGenerated,
            description=incident.description_str,
            color="red",
            node_type="incident",
            entitytype="incident",
            start_time=incident.StartTime,
            end_time=incident.EndTime,
        )
        if incident.has_edge:
            self._add_entity_edges(incident.edges, incident_name)

    def _add_entity_edges(self, edges: set, attached_to: str):
        """Check entity edges and add them."""
        for edge in edges:
            if isinstance(edge.target, Entity):
                if not self.alertentity_graph.has_node(edge.target.name_str):
                    self._add_entity_node(edge.target)
                try:
                    self.add_link(attached_to, edge.target.name_str)
                except MsticpyUserError:
                    pass

    @property
    def graph(self) -> nx.Graph:
        """Return the raw NetworkX graph."""
        return self.alertentity_graph


def _dedupe_entities(alerts, ents) -> list:
    """Deduplicate incedent and alert entities."""
    alrt_ents = []
    for alrt in alerts:
        if alrt["Entities"]:

            alrt_ents += [ent.__hash__() for ent in alrt["Entities"]]
    for ent in ents:
        if ent.__hash__() in alrt_ents:
            ents.remove(ent)
    return ents


@pd.api.extensions.register_dataframe_accessor("mp_incident_graph")
class EntityGraphAccessor:
    """Pandas api extension for Entinty Graph."""

    def __init__(self, pandas_obj):
        """Instantiate pandas extension class."""
        self._df = pandas_obj

    def plot(self) -> LayoutDOM:
        """
        Plot an incident graph if the dataframe contains incidents or alerts.

        Raises
        ------
        MsticpyUserError
            Raised if the dataframe does not contain incidents or alerts.

        """
        if not all(elem in self._df.columns for elem in req_alert_cols) and any(
            elem not in self._df.columns for elem in req_inc_cols
        ):
            raise MsticpyUserError("DataFrame must consist of Incidents or Alerts")

        graph = EntityGraph(self._df)
        graph.plot()

    def plot_with_timeline(self) -> LayoutDOM:
        """
        Plot an incident graph & timeline if the dataframe contains incidents or alerts.

        Raises
        ------
        MsticpyUserError
            Raised if the dataframe does not contain incidents or alerts.

        """
        if not all(elem in self._df.columns for elem in req_alert_cols) and any(
            elem not in self._df.columns for elem in req_inc_cols
        ):
            raise MsticpyUserError("DataFrame must consist of Incidents or Alerts")
        graph = EntityGraph(self._df)
        graph.plot_with_timeline()

    def build(self) -> EntityGraph:
        """Generate an incident graph from the dataframe but without plotting it."""
        return EntityGraph(self._df)
