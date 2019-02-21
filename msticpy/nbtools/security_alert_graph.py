# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
security_alert_graph.

Creates an entity graph for the alert.
"""
import networkx as nx
import pandas as pd

from . security_alert import SecurityAlert
from . entityschema import Entity
from . utility import export, is_not_empty
from .. _version import VERSION

__version__ = VERSION
__author__ = 'Ian Hellen'


@export
def create_alert_graph(alert: SecurityAlert):
    """Create a networkx graph from the alert and contained entities."""
    alertentity_graph = nx.Graph(id='AlertGraph')

    alertentity_graph.add_node(alert['AlertType'],
                               name=alert['AlertType'],
                               time=str(alert['StartTimeUtc']),
                               description='Alert: ' + alert['AlertDisplayName'],
                               color='red',
                               node_type='alert')

    os_family = alert.os_family

    # Cycle through entities
    for entity in alert.entities:
        (e_name, e_desc) = _get_name_and_description(entity, os_family)

        alertentity_graph.add_node(e_name, entitytype=entity['Type'], name=e_name,
                                   description=e_desc, color='green',
                                   node_type='entity', source=str(entity))

        # add an edge by default to the alert
        alertentity_graph.add_edge(alert['AlertType'], e_name)

        # Rather than just add edges to the alert, we want to follow the 'natural'
        # relationships between entities and child entities
        # So if this entity has a property that is an entity, we add an edge to it
        # and prune any edge that it might have to the alert
        for prop, rel_entity in [(p, v) for (p, v) in entity.properties.items()
                                 if isinstance(v, Entity)]:
            if rel_entity['Type'] == 'host':
                # don't add a new edge to the host
                continue

            # get the node id of the related entity and add an edge if it
            # doesn't already exist
            (related_entity, _) = _get_name_and_description(rel_entity)
            if not alertentity_graph.has_edge(related_entity, e_name):
                alertentity_graph.add_edge(e_name, related_entity, description=prop,
                                           color='green', weight=1,
                                           line_type='SHORT_DASH')

            # if we have a previously created an edge to the alert, remove it
            if alertentity_graph.has_edge(alert['AlertType'], related_entity):
                alertentity_graph.remove_edge(alert['AlertType'], related_entity)

        # if we haven't added an edge to this entity from anything else,
        # add one to the alert
        if not alertentity_graph.neighbors(e_name):
            alertentity_graph.add_edge(alert['AlertType'], e_name)

    return alertentity_graph


@export
def add_related_alerts(related_alerts: pd.DataFrame, alertgraph: nx.Graph) ->nx.Graph:
    """
    Add related alerts to the graph.

    Link to the entity that is common to both alerts.
    """
    related_alerts_graph = alertgraph.copy()

    alert_host_node = _find_graph_node(related_alerts_graph, 'host', '')

    related_alerts.apply(lambda x: _add_alert_node(related_alerts_graph, x), axis=1)
    related_alerts.apply(lambda x: _add_related_alert_edges(related_alerts_graph,
                                                            x,
                                                            alert_host_node), axis=1)
    return related_alerts_graph


def _add_related_alert_edges(related_alerts_graph, alert_row, default_node):
    related_alert = SecurityAlert(alert_row)
    if related_alert.primary_account is not None:
        acct_node = _find_graph_node(related_alerts_graph, 'account',
                                     related_alert.primary_account.qualified_name)
        if acct_node is not None:
            _add_related_alert_edge(related_alerts_graph, acct_node, related_alert)

    if related_alert.primary_process is not None:
        proc_node = _find_graph_node(related_alerts_graph,
                                     'process',
                                     related_alert.primary_process.ProcessFilePath)
        if proc_node is not None:
            _add_related_alert_edge(related_alerts_graph, proc_node, related_alert)

    if related_alert.primary_host is not None:
        host_node = _find_graph_node(related_alerts_graph,
                                     'host', related_alert.primary_host['HostName'])
        if host_node is not None:
            _add_related_alert_edge(related_alerts_graph, host_node, related_alert)

    # if we haven't added an edge to this entity from anything else,
    # add one to the alert
    if not related_alerts_graph[related_alert['AlertType'] + '(R)']:
        _add_related_alert_edge(related_alerts_graph, default_node, related_alert)


def _add_alert_node(nx_graph, alert):
    """Add alert node to the graph."""
    nx_graph.add_node(alert['AlertType'] + '(R)',
                      name=alert['AlertType'],
                      time=str(alert['StartTimeUtc']),
                      displayname=alert['AlertDisplayName'],
                      color='red',
                      count=0,
                      node_type='alert')


def _find_graph_node(nx_graph, node_type, target_name):
    """Find a node with a given name and type."""
    node_prefix = '{}: {}'.format(node_type, target_name)
    nodes = [n for (n, n_type) in
             nx.get_node_attributes(nx_graph, 'entitytype').items()
             if n_type == node_type and n.startswith(node_prefix)]
    if nodes:
        return nodes[0]


def _add_related_alert_edge(nx_graph, source, target):
    """Add related alert to an existing graph."""
    count_attrs = nx.get_node_attributes(nx_graph, 'count')
    target_node = target['AlertType'] + '(R)'
    if target_node in count_attrs:
        current_count = count_attrs[target_node]
    else:
        current_count = 0
    current_count += 1

    description = 'Related alert: {}  Count:{}'.format(target['AlertType'],
                                                       current_count)
    node_attrs = {target_node: {'count': current_count, 'description': description}}
    nx.set_node_attributes(nx_graph, node_attrs)
    nx_graph.add_edge(source, target_node, weight=0.7, description='Related Alert')


def _get_account_qualified_name(account):
    if 'Name' in account:
        name = account['Name']
    if 'NTDomain' in account:
        return '{}\\{}'.format(account['NTDomain'], name)
    else:
        return name


def _get_name_and_description(entity, os_family='Windows'):
    """Get name and description for entity."""
    e_name = None
    e_description = None

    if entity['Type'] == 'host':
        e_name, e_description = _get_host_name_desc(entity, os_family)
    elif entity['Type'] == 'account':
        e_name, e_description = _get_account_name_desc(entity)
    elif entity['Type'] == 'host-logon-session':
        e_name = 'host-logon-session'
        e_description = 'Logon session {}\n(Start time: {})'.format(entity['SessionId'],
                                                                    entity['StartTimeUtc'])
    elif entity['Type'] == 'process':
        e_name, e_description = _get_process_name_desc(entity)
    elif entity['Type'] == 'file':
        e_name, e_description = _get_file_name_desc(entity)
    elif entity['Type'] == 'ip' or entity['Type'] == 'ipaddress':
        e_name, e_description = _get_ip_name_desc(entity)
    elif entity['Type'] == 'dns':
        e_name = '{}: {}'.format(entity['Type'], entity['DomainName'])
        e_description = e_name
    else:
        # Any other type of entity
        e_name, e_description = _get_other_name_desc(entity)

    return e_name, e_description


# Methods to construct name and description
def _get_other_name_desc(entity):
    if 'Name' in entity:
        e_name = entity['Name']
        e_name = '{}: {}'.format(entity['Type'], e_name)
    else:
        e_name = entity['Type']

    # Nasty dict comprehension to join all other items in the dictionary into a string
    e_properties = '\n'.join({'{}:{}'.format(k, v) for (k, v)
                              in entity.properties.items() if (k not in ('Type', 'Name') and
                                                               isinstance(v, str))})
    e_description = '{}\n{})'.format(e_name, e_properties)
    return e_name, e_description


def _get_ip_name_desc(entity):
    e_name = entity['Address']
    e_name = '{}: {}'.format(entity['Type'], e_name)
    if 'Location' in entity and entity['Location']:
        e_description = '{}\nc={}, st={}, city={}'.format(
            e_name,
            entity['Location'].CountryCode,
            entity['Location'].State,
            entity['Location'].City)
    else:
        e_description = e_name
    return e_name, e_description


def _get_file_name_desc(entity):
    e_name = entity['FullPath']
    e_name = '{}: {}'.format(entity['Type'], e_name)
    e_description = e_name
    return e_name, e_description


def _get_process_name_desc(entity):
    if 'ProcessFilePath' in entity:
        path = entity.ProcessFilePath
    elif 'ImageFile' in entity and entity['ImageFile']:
        path = entity['ImageFile']['FullPath']
    else:
        path = 'unknown'
    pid = entity.ProcessId if entity.ProcessId else 'PID unknown'
    e_name = path + ' [' + pid + ']'
    e_name = '{}: {}'.format(entity['Type'], e_name)
    e_description = '{}\n(cmdline: \'{}\')'.format(e_name, entity.CommandLine)
    return e_name, e_description


def _get_account_name_desc(entity):
    e_name = (entity['NTDomain'] + '\\' if 'NTDomain' in entity else '') + entity['Name']
    e_name = '{}: {}'.format(entity['Type'], e_name)
    if 'IsDomainJoined' in entity:
        domain_joined = entity['IsDomainJoined']
    else:
        domain_joined = 'false'
    if 'LogonId' in entity:
        e_description = '{}\n(LogonId: {}, Domain-joined: {})'.format(e_name,
                                                                      entity.LogonId,
                                                                      domain_joined)
    else:
        e_description = '{}\n(Domain-joined: {})'.format(e_name, domain_joined)
    return e_name, e_description


def _get_host_name_desc(entity, os_family):
    if 'DnsDomain' in entity and is_not_empty(entity['DnsDomain']):
        e_name = '{}.{}'.format(entity['HostName'], entity['DnsDomain'])
    elif 'NTDomain' in entity and is_not_empty(entity['NTDomain']):
        e_name = '{}/{}'.format(entity['NTDomain'], entity['HostName'])
    else:
        e_name = entity['HostName']
    e_name = '{}: {}'.format(entity['Type'], e_name)

    if 'IsDomainJoined' in entity:
        domain_joined = entity['IsDomainJoined']
    else:
        domain_joined = 'false'
    if 'OSFamily' in entity:
        os_family = entity['OSFamily']
    e_description = '{}\n({}, Domain-joined: {})'.format(e_name, os_family, domain_joined)

    return e_name, e_description
