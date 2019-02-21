# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""default queries module ."""
from . query_defns import KqlQuery, DataFamily, DataEnvironment

# Module level variable that holds dictionary of queries
# indexed by name
# pylint: disable=C0103
query_definitions = dict()
# pylint: enable=C0103


KNOWN_PARAM_NAMES = ['table', 'query_project', 'start', 'end',
                     'system_alert_id', 'subscription_filter',
                     'host_filter_eq', 'host_filter_neq', 'host_name',
                     'account_name', 'process_name', 'process_id',
                     'logon_session_id', 'path_separator', 'commandline',
                     'source_ip_list', 'add_query_items']


def _add_query(kql_query):
    query_definitions[kql_query.name] = kql_query

# ------------------------------------------------------------------------
# Do Not edit above this line
# ------------------------------------------------------------------------


# Predefined queries
_add_query(KqlQuery(name='list_alerts_counts',
                    query='''
{table}
{query_project}
| where TimeGenerated >= datetime({start})
| where TimeGenerated <= datetime({end})
| summarize alertCount=count(), firstAlert=min(TimeGenerated),
    lastAlert=max(TimeGenerated) by AlertName
| order by alertCount desc
{add_query_items}
''',
                    description='Retrieves summary of current alerts',
                    data_source='security_alert',
                    data_families=[DataFamily.SecurityAlert],
                    data_environments=[DataEnvironment.LogAnalytics],
                    optional_params=['add_query_items']))

_add_query(KqlQuery(name='list_alerts',
                    query='''
{table}
{query_project}
| where TimeGenerated >= datetime({start})
| where TimeGenerated <= datetime({end})
| extend extendedProps = parse_json(ExtendedProperties)
| extend CompromisedEntity = tostring(extendedProps['Compromised Host'])
| project-away extendedProps
{add_query_items}
''',
                    description='Retrieves list of current alerts',
                    data_source='security_alert',
                    data_families=[DataFamily.SecurityAlert],
                    data_environments=[DataEnvironment.LogAnalytics],
                    optional_params=['add_query_items']))

_add_query(KqlQuery(name='get_alert',
                    query='''
{table}
{query_project}
| where TimeGenerated >= datetime({start})
| where TimeGenerated <= datetime({end})
| extend extendedProps = parse_json(ExtendedProperties)
| extend CompromisedEntity = tostring(extendedProps['Compromised Host'])
| project-away extendedProps
| where SystemAlertId == \'{system_alert_id}\'
{add_query_items}
''',
                    description='Retrieves an alert by alert Id',
                    data_source='security_alert',
                    data_families=[DataFamily.SecurityAlert],
                    data_environments=[DataEnvironment.LogAnalytics],
                    optional_params=['add_query_items']))

_add_query(KqlQuery(name='list_related_alerts',
                    query='''
let src_host = \'{host_name}\';
let src_acct = \'{account_name}\';
let src_proc = \'{process_name}\';
{table}
{query_project}
| where {subscription_filter}
| where TimeGenerated >= datetime({start})
| where TimeGenerated <= datetime({end})
| extend Computer = src_host
| extend src_hostname = tostring(split(src_host, '.')[0])
| extend src_accountname = iif(src_acct contains '\\\\',
                               tostring(split(src_acct, '\\\\')[-1]),
                               tostring(split(src_acct, '@')[0]))
| extend src_procname = tostring(split(src_proc, \'{path_separator}\')[-1])
| extend host_match = iif(isnotempty(src_host) and
    (Entities has src_hostname or Entities has src_host
     or ExtendedProperties has src_hostname
     or ExtendedProperties has src_host), true, false)
| extend acct_match = iif(isnotempty(src_acct)
     and (Entities has src_accountname or Entities has src_acct
     or ExtendedProperties has src_accountname
     or ExtendedProperties has src_acct), true, false)
| extend proc_match = iif(isnotempty(src_acct)
     and (Entities has src_procname or Entities has src_proc
     or ExtendedProperties has src_procname
     or ExtendedProperties has src_proc), true, false)
| where host_match or acct_match or proc_match
{add_query_items}
''',
                    description='Retrieves list of alerts with a common host, acount or process',
                    data_source='security_alert',
                    data_families=[DataFamily.SecurityAlert],
                    data_environments=[DataEnvironment.LogAnalytics],
                    optional_params=['process_name', 'account_name', 'add_query_items']))

_add_query(KqlQuery(name='list_related_ip_alerts',
                    query='''
let src_ips = \'{source_ip_list}\';
let src_ips_arr = split(src_ips, ',');
let IP_table = toscalar(range idx from 0 to array_length(src_ips_arr) - 1 step 1
| extend ip = trim(@'\\s*', tostring(src_ips_arr[idx]))
| project ip
| distinct ip
| summarize makeset(ip) );
let ip_extract = materialize(
{table}
{query_project}
| where {subscription_filter}
| where TimeGenerated >= datetime({start})
| where TimeGenerated <= datetime({end})
| project SystemAlertId, ExtendedProperties, Entities
| extend source_ips_str = extract("\\"Source IPs\\": \\"([^\\"]+)\\"", 1, ExtendedProperties)
| extend source_ips_1 = iif(isnotempty(source_ips_str), split(source_ips_str, ','), dynamic([]))
| extend source_ips_2 = extract_all("\\"Address\\": \\"([^\\"]+)\\"", dynamic([1]), Entities)
| mvexpand alert_ip_1 = source_ips_1 to typeof(string), alert_ip_2 = source_ips_2 to typeof(string)
| where isnotempty(alert_ip_1) or isnotempty(alert_ip_2)
| where alert_ip_1 in (IP_table) or alert_ip_2 in (IP_table)
| extend matching_ips = case(isnotempty(alert_ip_1) and isnotempty(alert_ip_2), strcat(alert_ip_1, ',', alert_ip_2),
                             isnotempty(alert_ip_1), alert_ip_1,
                             isnotempty(alert_ip_2), alert_ip_2,
                             '')
| extend MatchingIps = split(matching_ips, ',')
| project-away source_ips_str, source_ips_1, source_ips_2, alert_ip_1, alert_ip_2, matching_ips
);
{table}
{query_project}
| where {subscription_filter}
| where TimeGenerated >= datetime({start})
| where TimeGenerated <= datetime({end})
| join (ip_extract) on SystemAlertId
{add_query_items}
''',
                    description='Retrieves list of alerts with a common IP Address',
                    data_source='security_alert',
                    data_families=[DataFamily.SecurityAlert],
                    data_environments=[DataEnvironment.LogAnalytics],
                    optional_params=['add_query_items']))

_add_query(KqlQuery(name='get_process_tree',
                    query='''
let start = datetime({start});
let end = datetime({end});
let sourceProcessId = \'{process_id}\';
let sourceLogonId = \'{logon_session_id}\';
let sourceProcess =
materialize(
    {table}
    {query_project}
    | where {subscription_filter}
    | where {host_filter_eq}
    | where TimeGenerated >= start
    | where TimeGenerated <= end
    | where SubjectLogonId == sourceLogonId
    | where NewProcessId == sourceProcessId
    | where NewProcessName =~ \'{process_name}\'
    | extend NodeRole = 'source', Level = 0
    | top 1 by TimeCreatedUtc desc nulls last);
let sourceTimeCreatedUtc = toscalar(sourceProcess | project TimeCreatedUtc);
let sourceParentProcessId = toscalar(sourceProcess | project ProcessId);
let system_session_id = toscalar(sourceProcess
    | extend sys_session = iff(NewProcessName contains '/', '-1', '0x3e7')
    | project sys_session );
let parentProcess = // Parent Process
materialize(
    {table}
    {query_project}
    | where {subscription_filter}
    | where {host_filter_eq}
    | where TimeGenerated >= start - time(1d)
    | where TimeGenerated <= end
    | where TimeGenerated <= sourceTimeCreatedUtc
    | where (SubjectLogonId == sourceLogonId or TargetLogonId == sourceLogonId)
    | where NewProcessId == sourceParentProcessId
    | extend NodeRole = 'parent', Level = 1
    | top 1 by TimeCreatedUtc desc nulls last);
let parentLogonId = toscalar(sourceProcess | project SubjectLogonId);
let parentTimeCreated = toscalar(sourceProcess | project TimeCreatedUtc);
let childProcesses = // Child Process
materialize(
    {table}
    {query_project}
    | where {subscription_filter}
    | where {host_filter_eq}
    | where TimeGenerated >= start
    | where TimeGenerated <= end
    | where SubjectLogonId == sourceLogonId
    | where ProcessId == sourceProcessId
    | extend NodeRole = 'child', Level = 1);

sourceProcess
| union (parentProcess)
| union (childProcesses)
| union
(
    // GrandParent Process (we ignore this if this is the system logonId)
    {table}
    {query_project}
    | where {subscription_filter}
    | where {host_filter_eq}
    | where TimeGenerated >= start - time(1d)
    | where TimeGenerated <= end
    | where TimeGenerated <= parentTimeCreated
    | where (SubjectLogonId == parentLogonId or TargetLogonId == parentLogonId)
    | extend NodeRole = 'parent', Level = 2
    | join (parentProcess | project ProcessId) on $left.NewProcessId == $right.ProcessId
)
| union
(
    // GrandChild Process (we ignore this if this is the system logonId)
    {table}
    {query_project}
    | where {subscription_filter}
    | where {host_filter_eq}
    | where TimeGenerated >= start
    | where TimeGenerated <= end
    | where SubjectLogonId == sourceLogonId and SubjectLogonId != system_session_id
    | extend NodeRole = 'child', Level = 2
    | join (childProcesses | project NewProcessId) on $left.ProcessId == $right.NewProcessId
)
| union
(
    // Sibling Process
    {table}
    {query_project}
    | where {subscription_filter}
    | where {host_filter_eq}
    | where TimeGenerated >= start
    | where TimeGenerated <= end
    | where SubjectLogonId == sourceLogonId
    | where ProcessId == sourceParentProcessId
    | where NewProcessId != sourceProcessId
    | extend NodeRole = 'sibling', Level = 1
)
{add_query_items}
''',
                    description='Retrieves process tree for a process.',
                    data_source='process_create',
                    data_families=[DataFamily.WindowsSecurity,
                                   DataFamily.LinuxSecurity],
                    data_environments=[DataEnvironment.LogAnalytics],
                    optional_params=['add_query_items']))

_add_query(KqlQuery(name='list_processes',
                    query='''
let start = datetime({start});
let end = datetime({end});
{table}
{query_project}
| where {subscription_filter}
| where {host_filter_eq}
| where TimeGenerated >= start
| where TimeGenerated <= end
{add_query_items}
''',
                    description='Retrieves processes for a host.',
                    data_source='process_create',
                    data_families=[DataFamily.WindowsSecurity,
                                   DataFamily.LinuxSecurity],
                    data_environments=[DataEnvironment.LogAnalytics],
                    optional_params=['add_query_items']))

_add_query(KqlQuery(name='get_process_parent',
                    query='''
let start = datetime({start});
let end = datetime({end});
let sourceProcessId = \'{process_id}\';
let sourceLogonId = \'{logon_session_id}\';
let sourceProcess =
materialize(
    {table}
    {query_project}
    | where {subscription_filter}
    | where {host_filter_eq}
    | where TimeGenerated >= start
    | where TimeGenerated <= end
    | where SubjectLogonId == sourceLogonId
    | where NewProcessId == sourceProcessId
    | where NewProcessName =~ \'{process_name}\'
    | extend NodeRole = 'source', Level = 0
    | top 1 by TimeCreatedUtc desc nulls last);
let sourceTimeCreatedUtc = toscalar(sourceProcess | project TimeCreatedUtc );
let sourceParentProcessId = toscalar(sourceProcess | project ProcessId);
// Parent Process
{table}
{query_project}
| where {subscription_filter}
| where {host_filter_eq}
| where TimeGenerated >= start - time(2h)
| where TimeGenerated <= end
| where TimeGenerated <= sourceTimeCreatedUtc
| where (SubjectLogonId == sourceLogonId or TargetLogonId == sourceLogonId)
| where NewProcessId == sourceParentProcessId
| where NewProcessId == sourceParentProcessId
| extend NodeRole = 'parent', Level = 1
| top 1 by TimeCreatedUtc desc nulls last);
{add_query_items}
''',
                    description='Retrieves the parent process of a process process',
                    data_source='process_create',
                    data_families=[DataFamily.WindowsSecurity,
                                   DataFamily.LinuxSecurity],
                    data_environments=[DataEnvironment.LogAnalytics],
                    optional_params=['add_query_items']))

_add_query(KqlQuery(name='list_hosts_matching_commandline',
                    query='''
{table}
{query_project}
| where {subscription_filter}
| where {host_filter_neq}
| where TimeGenerated >= datetime({start})
| where TimeGenerated <= datetime({end})
| where NewProcessName endswith \'{process_name}\'
| where CommandLine =~ \'{commandline}\'
{add_query_items}
''',
                    description='Retrieves processes on other hosts with matching commandline',
                    data_source='process_create',
                    data_families=[DataFamily.WindowsSecurity,
                                   DataFamily.LinuxSecurity],
                    data_environments=[DataEnvironment.LogAnalytics],
                    optional_params=['add_query_items']))

_add_query(KqlQuery(name='list_processes_in_session',
                    query='''
{table}
{query_project}
| where {subscription_filter}
| where {host_filter_eq}
| where TimeGenerated >= datetime({start})
| where TimeGenerated <= datetime({end})
| where SubjectLogonId == \'{logon_session_id}\'
| extend processName = tostring(split(NewProcessName, \'{path_separator}\')[-1])
| extend commandlineparts = arraylength(split(CommandLine, ' '))
| extend commandlinelen = strlen(CommandLine)
{add_query_items}
''',
                    description='Retrieves all processes on the host for a logon session',
                    data_source='process_create',
                    data_families=[DataFamily.WindowsSecurity,
                                   DataFamily.LinuxSecurity],
                    data_environments=[DataEnvironment.LogAnalytics],
                    optional_params=['add_query_items']))

_add_query(KqlQuery(name='get_host_logon',
                    query='''
{table}
{query_project}
| where {subscription_filter}
| where {host_filter_eq}
| where TimeGenerated >= datetime({start})
| where TimeGenerated <= datetime({end})
| where TargetLogonId == \'{logon_session_id}\'
{add_query_items}
''',
                    description='Retrieves the logon event for the session id on the host.',
                    data_source='account_logon',
                    data_families=[DataFamily.WindowsSecurity,
                                   DataFamily.LinuxSecurity],
                    data_environments=[DataEnvironment.LogAnalytics],
                    optional_params=['add_query_items']))

_add_query(KqlQuery(name='list_host_logons',
                    query='''
{table}
{query_project}
| where {subscription_filter}
| where {host_filter_eq}
| where TimeGenerated >= datetime({start})
| where TimeGenerated <= datetime({end})
{add_query_items}
''',
                    description='Retrieves the logon events on the host.',
                    data_source='account_logon',
                    data_families=[DataFamily.WindowsSecurity,
                                   DataFamily.LinuxSecurity],
                    data_environments=[DataEnvironment.LogAnalytics],
                    optional_params=['add_query_items']))

_add_query(KqlQuery(name='list_host_logon_failures',
                    query='''
{table}
{query_project}
| where {subscription_filter}
| where {host_filter_eq}
| where TimeGenerated >= datetime({start})
| where TimeGenerated <= datetime({end})
{add_query_items}
''',
                    description='Retrieves the logon failure events on the host.',
                    data_source='account_logon_fail',
                    data_families=[DataFamily.WindowsSecurity,
                                   DataFamily.LinuxSecurity],
                    data_environments=[DataEnvironment.LogAnalytics],
                    optional_params=['add_query_items']))
