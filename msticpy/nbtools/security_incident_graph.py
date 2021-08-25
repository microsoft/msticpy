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
from ..datamodel.entities import Entity
from ..common.utility import export, is_not_empty
from .._version import VERSION

__version__ = VERSION
__author__ = "Pete Bryan"


@export
def create_incident_graph(incident: pd.Series):
    """Create a networkx graph from the alert and contained entities."""
    alertentity_graph = nx.Graph(id="IncidentGraph")

    alertentity_graph.add_node(
        incident["properties.title"],
        name=incident["properties.title"],
        time=str(incident["properties.createdTimeUtc"]),
        description="Incident: " + incident["properties.title"],
        color="red",
        node_type="incident",
    )

    # Cycle through entities
    for entity in incident["Entities"]:
        (e_name, e_desc) = _get_name_and_description(entity)

        alertentity_graph.add_node(
            e_name,
            entitytype=entity[0],
            name=e_name,
            description=e_desc,
            color="green",
            node_type="entity",
            source=str(entity),
        )

        # add an edge by default to the alert
        alertentity_graph.add_edge(incident["properties.title"], e_name)

        # Rather than just add edges to the alert, we want to follow the 'natural'
        # relationships between entities and child entities
        # So if this entity has a property that is an entity, we add an edge to it
        # and prune any edge that it might have to the alert
        if isinstance(entity, Entity):
            ent_props = entity.properties
        elif isinstance(entity, dict):
            ent_props = entity
        else:
            continue
        for prop, rel_entity in [
            (p, v) for (p, v) in ent_props.items() if isinstance(v, Entity)
        ]:
            if rel_entity["Type"] == "host":
                # don't add a new edge to the host
                continue

            # get the node id of the related entity and add an edge if it
            # doesn't already exist
            (related_entity, _) = _get_name_and_description(rel_entity)
            if not alertentity_graph.has_edge(related_entity, e_name):
                alertentity_graph.add_edge(
                    e_name,
                    related_entity,
                    description=prop,
                    color="green",
                    weight=1,
                    line_type="SHORT_DASH",
                )

            # if we have a previously created an edge to the alert, remove it
            if alertentity_graph.has_edge(incident["properties.title"], related_entity):
                alertentity_graph.remove_edge(
                    incident["properties.title"], related_entity
                )

        # if we haven't added an edge to this entity from anything else,
        # add one to the alert
        if not alertentity_graph.neighbors(e_name):
            alertentity_graph.add_edge(incident["properties.title"], e_name)

    return alertentity_graph


@export
def add_related_alerts(related_alerts: pd.DataFrame, alertgraph: nx.Graph) -> nx.Graph:
    """
    Add related alerts to the graph.

    Link to the entity that is common to both alerts.
    """
    related_alerts_graph = alertgraph.copy()

    alert_host_node = _find_graph_node(related_alerts_graph, "host", "")

    related_alerts.apply(lambda x: _add_alert_node(related_alerts_graph, x), axis=1)
    if alert_host_node:
        related_alerts.apply(
            lambda x: _add_related_alert_edges(
                related_alerts_graph, x, alert_host_node
            ),
            axis=1,
        )
    return related_alerts_graph


def _add_related_alert_edges(related_alerts_graph, alert_row, default_node):
    related_alert = SecurityAlert(alert_row)
    if related_alert.primary_account is not None:
        acct_node = _find_graph_node(
            related_alerts_graph,
            "account",
            related_alert.primary_account.qualified_name,
        )
        if acct_node is not None:
            _add_related_alert_edge(related_alerts_graph, acct_node, related_alert)

    if related_alert.primary_process is not None:
        proc_node = _find_graph_node(
            related_alerts_graph,
            "process",
            related_alert.primary_process.ProcessFilePath,
        )
        if proc_node is not None:
            _add_related_alert_edge(related_alerts_graph, proc_node, related_alert)

    if related_alert.primary_host is not None:
        host_node = _find_graph_node(
            related_alerts_graph, "host", related_alert.primary_host["HostName"]
        )
        if host_node is not None:
            _add_related_alert_edge(related_alerts_graph, host_node, related_alert)

    # if we haven't added an edge to this entity from anything else,
    # add one to the alert
    if not related_alerts_graph[related_alert["AlertType"] + "(R)"]:
        _add_related_alert_edge(related_alerts_graph, default_node, related_alert)


def _add_alert_node(nx_graph, alert):
    """Add alert node to the graph."""
    nx_graph.add_node(
        alert["AlertType"] + "(R)",
        name=alert["AlertType"],
        time=str(alert["StartTimeUtc"]),
        displayname=alert["AlertDisplayName"],
        color="red",
        count=0,
        node_type="alert",
    )


def _find_graph_node(nx_graph, node_type, target_name):
    """Find a node with a given name and type."""
    node_prefix = "{}: {}".format(node_type, target_name)
    nodes = [
        n
        for (n, n_type) in nx.get_node_attributes(nx_graph, "entitytype").items()
        if n_type == node_type and n.startswith(node_prefix)
    ]
    if nodes:
        return nodes[0]
    return None


def _add_related_alert_edge(nx_graph, source, target):
    """Add related alert to an existing graph."""
    count_attrs = nx.get_node_attributes(nx_graph, "count")
    target_node = target["AlertType"] + "(R)"
    current_count = count_attrs[target_node] if target_node in count_attrs else 0
    current_count += 1

    description = "Related alert: {}  Count:{}".format(
        target["AlertType"], current_count
    )
    node_attrs = {target_node: {"count": current_count, "description": description}}
    nx.set_node_attributes(nx_graph, node_attrs)
    nx_graph.add_edge(source, target_node, weight=0.7, description="Related Alert")


def _get_name_and_description(entity):
    """Get name and description for entity."""
    e_name = None
    e_description = None

    if entity[0] == "Host":
        e_name, e_description = _get_host_name_desc(entity[1])
    elif entity[0] == "Account":
        e_name, e_description = _get_account_name_desc(entity[1])
    elif entity[0] == "Process":
        e_name, e_description = _get_process_name_desc(entity[1])
    elif entity[0] == "File":
        e_name, e_description = _get_file_name_desc(entity[1])
    elif entity[0] == "FileHash":
        e_name = entity[1]["hashValue"]
        e_description = f"{e_name}\n(algorithm: {entity[1]['algorithm']})"
    elif entity[0] == "Ip":
        e_name = entity[1]["address"]
        e_description = e_name
    elif entity[0] == "Url":
        e_name, e_description = _get_url_name_desc(entity[1])
    else:
        # Any other type of entity
        e_name, e_description = _get_other_name_desc(entity[1])
    e_name = f"{entity[0]}: {e_name}"

    return e_name, e_description


# Methods to construct name and description
def _get_other_name_desc(entity):
    e_name = entity["name"] if "name" in entity.keys() else ""
    if isinstance(entity, Entity):
        ent_props = entity.properties
    elif isinstance(entity, dict):
        ent_props = entity
    else:
        ent_props = {"unknown": None}

    # Nasty dict comprehension to join all other items in the
    # dictionary into a string
    e_properties = "\n".join(
        {
            "{}:{}".format(k, v)
            for (k, v) in ent_props.items()
            if (k not in ("name") and isinstance(v, str))
        }
    )
    e_description = "{}\n{})".format(e_name, e_properties)
    return e_name, e_description


def _get_file_name_desc(entity):
    e_name = entity["fileName"]
    directory = entity["directory"] if "directory" in entity.keys() else "Unknown"
    e_description = f"{e_name}\n(directory: {directory})"
    return e_name, e_description


def _get_process_name_desc(entity):
    pid = entity["processId"] or "PID unknown"
    process = entity["friendlyName"] or "Unknown"
    e_name = process + " [" + pid + "]"
    cmd_line = entity["commandLine"] if "commandLine" in entity.keys() else "Unknown"
    e_description = f"{e_name}\n(cmdline: {cmd_line})"
    return e_name, e_description


def _get_account_name_desc(entity):
    e_dom = entity["ntDomain"] if "ntDomain" in entity.keys() else None
    e_dom = e_dom + "\\" if e_dom else ""
    if "accountName" in entity.keys():
        e_name = e_dom + (entity["accountName"])
    elif "aadUserId" in entity.keys():
        e_name = e_dom + (entity["aadUserId"])
    elif "displayName" in entity.keys():
        e_name = e_dom + (entity["displayName"])
    else:
        e_name = "unknown"

    if "upnSuffix" in entity:
        upn = entity["accountName"] + "@" + entity["upnSuffix"]
    else:
        upn = None
    if "aadUserId" in entity:
        e_description = f"{e_name}\n(AADUserId: {entity['aadUserId']},"
        e_description = e_description + f" UPN: {upn})"
    else:
        e_description = f"{e_name}\n(UPN: {upn})"
    return e_name, e_description


def _get_host_name_desc(entity):
    if "dnsDomain" in entity and is_not_empty(entity["dnsDomain"]):
        e_name = "{}.{}".format(entity["hostName"], entity["dnsDomain"])
    elif "ntDomain" in entity and is_not_empty(entity["ntDomain"]):
        e_name = "{}/{}".format(entity["ntName"], entity["hostName"])
    else:
        e_name = entity["hostName"]

    os_family = entity["osFamily"] if "osFamily" in entity.keys() else "unknown"
    e_description = f"{e_name}\n(osfamily: {os_family})"

    return e_name, e_description


def _get_url_name_desc(entity):
    e_name = entity["url"]
    if (
        "additionalData" in entity.keys()
        and "DetonationVerdict" in entity["additionalData"]
    ):
        e_description = (
            f"{e_name}\n(verdict: {entity['additionalData']['DetonationVerdict']})"
        )
    else:
        e_description = e_name

    return e_name, e_description
