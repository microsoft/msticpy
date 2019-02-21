# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Module for SecurityAlert class."""
import html
import re
from datetime import datetime
from collections import Counter

import pandas as pd

from .entityschema import Process
from .query_defns import QueryParamProvider, DataFamily, DataEnvironment
from .utility import is_not_empty, escape_windows_path
from . utility import export
from .. _version import VERSION

__version__ = VERSION
__author__ = 'Ian Hellen'

_ID_PROPERTIES = ['AzSubscriptionId', 'AzResourceId', 'WorkspaceId', 'AgentId',
                  'TenantId', 'SourceComputerId', 'ResourceId',
                  'WorkspaceSubscriptionId', 'WorkspaceResourceGroup',
                  'ProviderAlertId', 'SystemAlertId', 'ResourceId']


@export
class SecurityBase(QueryParamProvider):
    """
    Security Base Class for alerts and events.

    Instantiates a security event or alert from a pandas Series
    and provides convenience access methods to retrieve properties.
    """

    def __init__(self, src_row: pd.Series = None):
        """Instantiate a security alert from a pandas Series."""
        self._source_data = src_row
        self._custom_query_params = {}
        self._entities = []

        # Extract and cache alert ID properties
        self._ids = dict()
        for id_property in _ID_PROPERTIES:
            if id_property in self._source_data:
                self._ids[id_property] = self._source_data[id_property]

        self.path_separator = '\\'
        self.os_family = 'Windows'

    # Dunder methods
    def __getitem__(self, key):
        """Allow property get using dictionary key syntax."""
        if key in self.__dict__:
            return self.__dict__[key]
        elif key in self._source_data:
            return self._source_data[key]
        else:
            raise KeyError

    def __setitem__(self, key, value):
        """Allow property set using dictionary key syntax."""
        self.__dict__[key] = value

    def __contains__(self, key):
        """Allow property 'in' test."""
        # get attribute from source dictionary if not defined
        return key in self.__dict__ or key in self._source_data

    def __getattr__(self, name):
        """Return the value of the named property 'name'."""
        if name in self._source_data:
            return self._source_data[name]
        raise AttributeError(f'{name} is not a valid attribute.')

    def __str__(self):
        """Return string representation of object properties."""
        str_props = [f'{prop}: {val}' for prop, val in
                     self._source_data.items()]

        if self.entities:
            str_entities = []
            for ent in self.entities:
                str_entities.append(str(ent).replace('\n', ', '))
            str_props = str_props + str_entities
        return '\n'.join(str_props)

    # def __getstate__(self):
    #     """Return dictionary of state for serialization/pickling."""
    #     state_dict = {}
    #     state_dict['source_data'] = (self._source_data)
    #     state_dict['Entities'] = self._entities
    #     state_dict['path_separator'] = self.path_separator
    #     state_dict['os_family'] = self.os_family
    #     return state_dict

    # def __setstate__(self, state):
    #     """Set state from dictionary for deserialization/unpickling."""
    #     self._source_data = state['source_data']
    #     self._entities = list(state['Entities'])
    #     self.path_separator = state['path_separator']
    #     self.os_family = state['os_family']

    # Properties
    @property
    def entities(self):
        """Return a list of the Security Alert entities."""
        return self._entities

    @property
    def hostname(self):
        """Return the Hostname (not FQDN) of the host associated with the alert."""
        return self.get_entity_property(entity_type='host', entity_property='HostName')

    @property
    def computer(self):
        """
        Return the Computer name of the host associated with the alert.

        (host FQDN, if available)
        """
        return self.primary_host.computer if self.primary_host is not None else None

    @property
    def ids(self):
        """Return a collection of Identity properties for the alert."""
        return self._ids

    @property
    def is_in_workspace(self):
        """Return True if the alert has a Log Analytics WorkspaceID."""
        return 'WorkspaceId' in self._ids and 'AgentId' in self._ids

    @property
    def is_in_log_analytics(self):
        """Return True if the alert originates from a Log Analytics Workspace host."""
        return 'TenantId' in self._ids

    @property
    def is_in_azure_sub(self):
        """Return True if the alert originates from an Azure Security Center host."""
        if ('AzSubscriptionId' not in self._ids and
                'AzResourceId' not in self._ids and
                'ResourceId' in self._ids and self._ids['ResourceId']):
            self._ids['AzResourceId'] = self._id['ResourceId']
            self._ids['AzSubscriptionId'] = (
                self._get_subscription_from_resource(self._id['ResourceId']))

        return 'AzSubscriptionId' in self._ids and 'AzResourceId' in self._ids

    @property
    def primary_host(self):
        """Return the Primary host entity of the host associated with the alert."""
        hosts = self.get_entities_of_type('host')
        if hosts:
            return hosts[0]
        return None

    @property
    def primary_process(self):
        """Return the primary process entity (if any) associated with the alert."""
        procs = self.get_entities_of_type('process')
        if not procs:
            return None
        if len(procs) == 1:
            return procs[0]

        # find the first process that has a parent process property
        procs_with_parent = [p for p in procs if 'ParentProcess' in p]
        return procs_with_parent[0] if procs_with_parent else procs[0]

    @property
    def primary_account(self):
        """Return the primary account entity (if any) associated with this object."""
        accts = self.get_entities_of_type('account')
        return accts[0] if accts else None

    @property
    def query_params(self):
        """
        Query parameters derived from alert.

        Returns:
            dict(str, str) -- Dictionary of parameter names

        """
        host_name = self.primary_host.fqdn
        proc_name = (self.primary_process.ImageFile.FullPath if
                     self.primary_process and self.primary_process.ImageFile
                     else None)
        acct_name = self.primary_account.qualified_name if self.primary_account else None
        path_separator = self.path_separator
        if self.data_family == DataFamily.WindowsSecurity:
            proc_name = escape_windows_path(proc_name)
            acct_name = escape_windows_path(acct_name)
            path_separator = escape_windows_path(self.path_separator)

        dyn_query_params = {
            'subscription_filter': self.subscription_filter(),
            'host_filter_eq': self.host_filter(operator='=='),
            'host_filter_neq': self.host_filter(operator='!='),
            'host_name': host_name,
            'account_name': acct_name,
            'process_name': proc_name,
            'logon_session_id': self.get_logon_id(),
            'process_id': (self.primary_process.ProcessId if self.primary_process else None),
            'path_separator': path_separator,
            'data_family': self.data_family,
            'data_environment': self.data_environment
        }

        dyn_query_params.update(self._custom_query_params)
        return dyn_query_params

    @property
    def data_family(self) -> DataFamily:
        """Return the data family of the alert for subsequent queries."""
        if self.os_family == 'Linux':
            return DataFamily.LinuxSecurity
        elif self.os_family == 'Windows':
            return DataFamily.WindowsSecurity
        return None

    @property
    def data_environment(self) -> DataEnvironment:
        """Return the data environment of the alert for subsequent queries."""
        if self.is_in_log_analytics:
            return DataEnvironment.LogAnalytics
        else:
            return DataEnvironment.Kusto

    @property
    def origin_time(self) -> datetime:
        """Return the datetime of event."""
        return self.TimeGenerated

    def get_entity_property(self, entity_property, entity_type=None, entity=None):
        """
        Return the value of the named entity property.

            :param entity_property: the name of the property to return
            :param entity_type=None: the name of the entity type (optional if entity is supplied)
            :param entity=None: the target entity.

        If the entity parameter is not supplied the function will return the
        property value of the first entity of the current alert that
        matches the specified type and has a property of entity_property

        """
        if entity and entity_property in entity:
            return entity[entity_property]

        if self.entities is not None:
            for test_entity in [entity for entity in self.entities
                                if entity['Type'] == entity_type]:
                if (test_entity and entity_property in test_entity and
                        is_not_empty(test_entity[entity_property])):
                    return test_entity[entity_property]
        return None

    def get_logon_id(self, account=None):
        """
        Get the logon Id for the alert or the account, if supplied.

        if the account entity is not supplied, return the logon id
        of the first host-logon-session or account entity.
        """
        for session in [e for e in self.entities if e['Type'] == 'host-logon-session']:
            if account is None or session['Account'] == account:
                return session['SessionId']
        if account is None:
            for acct in [e for e in self.entities if e['Type'] == 'account' and 'LogonId' in e]:
                return acct['LogonId']
        elif 'LogonId' in account:
            return account['LogonId']
        return None

    def get_process_name(self, process):
        """Return the process (filename) of the process."""
        if isinstance(process, Process) and process.ProcessFilePath:
            return process.ProcessFilePath
        if 'ImageFile' in process:
            if 'FullPath' in process['ImageFile']:
                return process['ImageFile']['FullPath']
            elif 'Directory' in process['ImageFile']:
                return (process['ImageFile']['Directory'] +
                        self.path_separator + process['ImageFile']['Name'])
        return None

    def subscription_filter(self, operator='=='):
        """Return a KQL subscription filter clause derived from the alert properties."""
        if self.is_in_log_analytics:
            return 'true'
        if self.is_in_azure_sub:
            return ('AzureResourceSubscriptionId {} \'{}\''
                    .format(operator, self._ids['AzSubscriptionId']))
        if self.is_in_workspace:
            return 'WorkspaceId {} \'{}\''.format(operator, self._ids['WorkspaceId'])
        # Otherwise we default to including everything
        return 'true'

    def host_filter(self, operator='=='):
        """
        Return a KQL host filter clause derived from the alert properties.

            :param operator='==': the operator to use in the filter clause.
                '==' and '!=' typically.
        """
        if (self.is_in_log_analytics and 'SourceComputerId' in self._ids and
                self._ids['SourceComputerId']):
            return 'SourceComputerId {} \'{}\''.format(operator, self._ids['SourceComputerId'])
        if (self.is_in_azure_sub and 'AzureResourceId' in self._ids and
                self._ids['AzResourceId']):
            return 'AzureResourceId {} \'{}\''.format(operator, self._ids['AzResourceId'])
        if self.is_in_workspace and 'AgendId' in self._ids and self._ids['AgentId']:
            return 'AgentId {} \'{}\''.format(operator, self._ids['AgentId'])

        if self.primary_host:
            case_insens_op = '=~' if operator == '==' else '!~'
            return 'Computer {} \'{}\''.format(case_insens_op, self.primary_host.computer)
        return None

    def get_entities_of_type(self, entity_type='host'):
        """Return entity collection for a give entity type."""
        return [p for p in self.entities if p['Type'] == entity_type]

    def to_html(self, show_entities=False):
        """Return the item as HTML string."""
        html_doc = pd.DataFrame(self._source_data).to_html()

        if 'ExtendedProperties' in self._source_data:
            ext_prop_title = '<br/><h3>ExtendedProperties:</h3>'
            ext_prop_html = pd.DataFrame(
                pd.Series(self._source_data['ExtendedProperties'])).to_html()
            html_doc = html_doc + ext_prop_title + ext_prop_html

        if show_entities and self.entities:
            entity_title = '<br/><h3>Entities:</h3><br/>'
            entity_html = '<br/>'.join([self._format_entity(ent)
                                        for ent in self.entities])
            html_doc = html_doc + entity_title + entity_html
        else:
            e_counts = Counter([ent['Type'] for ent in self.entities])
            e_counts_str = ', '.join([f'{e}: {c}' for e, c in e_counts.items()])
            html_doc = html_doc + f'<h3>Entity counts: </h3>{e_counts_str}'
        return html_doc

    def _format_entity(self, entity):
        str_entity = str(entity)
        if str_entity:
            str_entity = str_entity.replace('\n', ', ')
        return html.escape(str_entity)

    def _find_os_family(self):
        """Work out which OSFamily and path separator to use from entities or file paths."""
        # Use OSFamily if any entities have this property set
        os_family_entities = [e for e in self.entities if 'OSFamily' in e]
        if os_family_entities:
            for os_entity in os_family_entities:
                if os_entity['OSFamily'] == 'Linux':
                    self.os_family = 'Linux'
                    self.path_separator = '/'
                    break
        else:
            # Otherwise try to infer from the file paths
            files = [e for e in self.entities if e['Type'] == 'file']
            if files:
                for file in files:
                    if 'Directory' in file and '/' in file['Directory']:
                        self.os_family = 'Linux'
                        self.path_separator = '/'
                        break
            else:
                for proc in [e for e in self.entities
                             if e['Type'] == 'process' and 'ImageFile' in e]:
                    file = proc['ImageFile']
                    if 'Directory' in file and '/' in file['Directory']:
                        self.os_family = 'Linux'
                        self.path_separator = '/'
                        break

    @staticmethod
    def _get_subscription_from_resource(resource_id):
        """Extract subscription Id from resource string."""
        sub_regex = r'^/subscriptions/([^/]+)/'
        sub_ids = re.findall(sub_regex, resource_id, re.RegexFlag.I)
        if sub_ids:
            return sub_ids[0]
