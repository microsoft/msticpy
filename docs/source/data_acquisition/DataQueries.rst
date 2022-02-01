Data Queries Reference
======================


Queries for Microsoft Sentinel
------------------------------

Data Environment identifier: MSSentinel

==================  ================================  ===========================================================================================================  ===============================================================================================================  ===========================
QueryGroup          Query                             Description                                                                                                  Req-Params                                                                                                       Table
==================  ================================  ===========================================================================================================  ===============================================================================================================  ===========================
Azure               get_vmcomputer_for_host           Gets latest VMComputer record for Host                                                                       host_name (str), start (datetime), end (datetime)                                                                VMComputer
Azure               get_vmcomputer_for_ip             Gets latest VMComputer record for IPAddress                                                                  start (datetime), ip_address (str), end (datetime)                                                               VMComputer
Azure               list_aad_signins_for_account      Lists Azure AD Signins for Account                                                                           start (datetime), account_name (str), end (datetime)                                                             SigninLogs
Azure               list_aad_signins_for_ip           Lists Azure AD Signins for an IP Address                                                                     start (datetime), ip_address_list (list), end (datetime)                                                         SigninLogs
Azure               list_all_signins_geo              Gets Signin data used by morph charts                                                                        start (datetime), end (datetime)                                                                                 SigninLogs
Azure               list_azure_activity_for_account   Lists Azure Activity for Account                                                                             start (datetime), account_name (str), end (datetime)                                                             AzureActivity
Azure               list_azure_activity_for_ip        Lists Azure Activity for Caller IP Address(es)                                                               start (datetime), ip_address_list (list), end (datetime)                                                         AzureActivity
Azure               list_azure_activity_for_resource  Lists Azure Activity for a Resource                                                                          start (datetime), resource_id (str), end (datetime)                                                              AzureActivity
Azure               list_storage_ops_for_hash         no description                                                                                               start (datetime), file_hash (str), end (datetime)                                                                StorageFileLogs
Azure               list_storage_ops_for_ip           no description                                                                                               start (datetime), ip_address (str), end (datetime)                                                               StorageFileLogs
AzureNetwork        az_net_analytics                  All Azure Network Analytics Data                                                                             start (datetime), end (datetime)                                                                                 AzureNetworkAnalytics_CL
AzureNetwork        dns_lookups_for_domain            Dns queries for a domain                                                                                     start (datetime), domain (str), end (datetime)                                                                   DnsEvents
AzureNetwork        dns_lookups_for_ip                Dns queries for a domain                                                                                     start (datetime), ip_address (str), end (datetime)                                                               DnsEvents
AzureNetwork        dns_lookups_from_ip               Dns queries for a domain                                                                                     start (datetime), ip_address (str), end (datetime)                                                               DnsEvents
AzureNetwork        get_heartbeat_for_host            Retrieves latest OMS Heartbeat event for host.                                                               host_name (str)                                                                                                  Heartbeat
AzureNetwork        get_heartbeat_for_ip              Retrieves latest OMS Heartbeat event for ip address.                                                         ip_address (str)                                                                                                 Heartbeat
AzureNetwork        get_host_for_ip                   Gets the latest AzureNetworkAnalytics interface event for a host.                                            ip_address (str)                                                                                                 AzureNetworkAnalytics_CL
AzureNetwork        get_ips_for_host                  Gets the latest AzureNetworkAnalytics interface event for a host.                                            host_name (str)                                                                                                  AzureNetworkAnalytics_CL
AzureNetwork        list_azure_network_flows_by_host  Retrieves Azure network analytics flow events.                                                               host_name (str), start (datetime), end (datetime)                                                                AzureNetworkAnalytics_CL
AzureNetwork        list_azure_network_flows_by_ip    Retrieves Azure network analytics flow events.                                                               end (datetime), start (datetime), ip_address_list (list)                                                         AzureNetworkAnalytics_CL
AzureSentinel       get_bookmark_by_id                Retrieves a single Bookmark by BookmarkId                                                                    start (datetime), end (datetime), bookmark_id (str)                                                              HuntingBookmark
AzureSentinel       get_bookmark_by_name              Retrieves one or more Bookmarks by Bookmark Name                                                             bookmark_name (str), start (datetime), end (datetime)                                                            HuntingBookmark
AzureSentinel       list_bookmarks                    Retrieves list of bookmarks                                                                                  start (datetime), end (datetime)                                                                                 HuntingBookmark
AzureSentinel       list_bookmarks_for_entity         Retrieves bookmarks for entity string                                                                        start (datetime), end (datetime)                                                                                 HuntingBookmark
AzureSentinel       list_bookmarks_for_tags           Retrieves Bookmark by one or mare Tags                                                                       start (datetime), bookmark_tags (list), end (datetime)                                                           HuntingBookmark
Heartbeat           get_heartbeat_for_host            Retrieves latest OMS Heartbeat event for host.                                                               host_name (str)                                                                                                  Heartbeat
Heartbeat           get_heartbeat_for_ip              Retrieves latest OMS Heartbeat event for ip address.                                                         ip_address (str)                                                                                                 Heartbeat
Heartbeat           get_info_by_hostname              Deprecated - use 'get_heartbeat_for_host'                                                                    host_name (str), start (datetime), end (datetime)                                                                Heartbeat
Heartbeat           get_info_by_ipaddress             Deprecated - use 'get_heartbeat_for_ip'                                                                      start (datetime), ip_address (str), end (datetime)                                                               Heartbeat
LinuxAudit          auditd_all                        Extract all audit messages grouped by mssg_id                                                                start (datetime), end (datetime)                                                                                 AuditLog_CL
LinuxSyslog         all_syslog                        Returns all syslog activity for a host                                                                       start (datetime), end (datetime)                                                                                 Syslog
LinuxSyslog         cron_activity                     All cron activity                                                                                            start (datetime), end (datetime)                                                                                 Syslog
LinuxSyslog         list_account_logon_failures       All failed user logon events from an IP address                                                              start (datetime), account_name (str), end (datetime)                                                             Syslog
LinuxSyslog         list_host_logon_failures          All failed user logon events on a host                                                                       host_name (str), start (datetime), end (datetime)                                                                Syslog
LinuxSyslog         list_ip_logon_failures            All failed user logon events from an IP address                                                              start (datetime), ip_address (str), end (datetime)                                                               Syslog
LinuxSyslog         list_logon_failures               All failed user logon events on any host                                                                     start (datetime), end (datetime)                                                                                 Syslog
LinuxSyslog         list_logons_for_account           All successful user logon events for account (all hosts)                                                     start (datetime), account_name (str), end (datetime)                                                             Syslog
LinuxSyslog         list_logons_for_host              All logon events on a host                                                                                   host_name (str), start (datetime), end (datetime)                                                                Syslog
LinuxSyslog         list_logons_for_source_ip         All successful user logon events for source IP (all hosts)                                                   start (datetime), ip_address (str), end (datetime)                                                               Syslog
LinuxSyslog         squid_activity                    All squid proxy activity                                                                                     host_name (str), start (datetime), end (datetime)                                                                Syslog
LinuxSyslog         sudo_activity                     All sudo activity                                                                                            start (datetime), end (datetime)                                                                                 Syslog
LinuxSyslog         user_group_activity               All user/group additions, deletions, and modifications                                                       start (datetime), end (datetime)                                                                                 Syslog
LinuxSyslog         user_logon                        All user logon events on a host                                                                              host_name (str), start (datetime), end (datetime)                                                                Syslog
MultiDataSource     get_timeseries_anomalies          Time Series filtered anomalies detected using built-in KQL time series function-series_decompose_anomalies   table (str), start (datetime), end (datetime)                                                                    na
MultiDataSource     get_timeseries_data               Retrieves TimeSeriesData prepared to use with built-in KQL time series functions                             table (str), start (datetime), end (datetime)                                                                    na
MultiDataSource     get_timeseries_decompose          Time Series decomposition and anomalies generated using built-in KQL time series function- series_decompose  table (str), start (datetime), end (datetime)                                                                    na
MultiDataSource     plot_timeseries_datawithbaseline  Plot timeseries data using built-in KQL time series decomposition using built-in KQL render method           table (str), start (datetime), end (datetime)                                                                    na
MultiDataSource     plot_timeseries_scoreanomolies    Plot timeseries anomaly score using built-in KQL render method                                               table (str), start (datetime), end (datetime)                                                                    na
Network             get_heartbeat_for_host            Retrieves latest OMS Heartbeat event for host.                                                               host_name (str)                                                                                                  Heartbeat
Network             get_heartbeat_for_ip              Retrieves latest OMS Heartbeat event for ip address.                                                         ip_address (str)                                                                                                 Heartbeat
Network             get_host_for_ip                   Gets the latest AzureNetworkAnalytics interface event for a host.                                            ip_address (str)                                                                                                 AzureNetworkAnalytics_CL
Network             get_ips_for_host                  Gets the latest AzureNetworkAnalytics interface event for a host.                                            host_name (str)                                                                                                  AzureNetworkAnalytics_CL
Network             list_azure_network_flows_by_host  Retrieves Azure network analytics flow events.                                                               host_name (str), start (datetime), end (datetime)                                                                AzureNetworkAnalytics_CL
Network             list_azure_network_flows_by_ip    Retrieves Azure network analytics flow events.                                                               end (datetime), start (datetime), ip_address_list (list)                                                         AzureNetworkAnalytics_CL
Office365           list_activity_for_account         Lists Office Activity for Account                                                                            start (datetime), account_name (str), end (datetime)                                                             OfficeActivity
Office365           list_activity_for_ip              Lists Office Activity for Caller IP Address(es)                                                              start (datetime), ip_address_list (list), end (datetime)                                                         OfficeActivity
Office365           list_activity_for_resource        Lists Office Activity for a Resource                                                                         start (datetime), resource_id (str), end (datetime)                                                              OfficeActivity
SecurityAlert       get_alert                         Retrieves a single alert by SystemAlertId                                                                    system_alert_id (str)                                                                                            SecurityAlert
SecurityAlert       list_alerts                       Retrieves list of alerts                                                                                     start (datetime), end (datetime)                                                                                 SecurityAlert
SecurityAlert       list_alerts_counts                Retrieves summary count of alerts by type                                                                    start (datetime), end (datetime)                                                                                 SecurityAlert
SecurityAlert       list_alerts_for_ip                Retrieves list of alerts with a common IP Address                                                            start (datetime), end (datetime), source_ip_list (str)                                                           SecurityAlert
SecurityAlert       list_related_alerts               Retrieves list of alerts with a common host, account or process                                              start (datetime), end (datetime)                                                                                 SecurityAlert
ThreatIntelligence  list_indicators                   Retrieves list of all current indicators.                                                                    start (datetime), end (datetime)                                                                                 ThreatIntelligenceIndicator
ThreatIntelligence  list_indicators_by_domain         Retrieves list of indicators by domain                                                                       start (datetime), domain_list (list), end (datetime)                                                             ThreatIntelligenceIndicator
ThreatIntelligence  list_indicators_by_email          Retrieves list of indicators by email address                                                                observables (list), start (datetime), end (datetime)                                                             ThreatIntelligenceIndicator
ThreatIntelligence  list_indicators_by_filepath       Retrieves list of indicators by file path                                                                    observables (list), start (datetime), end (datetime)                                                             ThreatIntelligenceIndicator
ThreatIntelligence  list_indicators_by_hash           Retrieves list of indicators by file hash                                                                    start (datetime), end (datetime), file_hash_list (list)                                                          ThreatIntelligenceIndicator
ThreatIntelligence  list_indicators_by_ip             Retrieves list of indicators by IP Address                                                                   start (datetime), ip_address_list (list), end (datetime)                                                         ThreatIntelligenceIndicator
ThreatIntelligence  list_indicators_by_url            Retrieves list of indicators by URL                                                                          url_list (list), start (datetime), end (datetime)                                                                ThreatIntelligenceIndicator
WindowsSecurity     get_host_logon                    Retrieves the logon event for the session id on the host                                                     host_name (str), logon_session_id (str), start (datetime), end (datetime)                                        SecurityEvent
WindowsSecurity     get_parent_process                Retrieves the parent process of a supplied process                                                           host_name (str), logon_session_id (str), start (datetime), process_name (str), end (datetime), process_id (str)  SecurityEvent
WindowsSecurity     get_process_tree                  Retrieves the process tree of a supplied process                                                             host_name (str), logon_session_id (str), start (datetime), process_name (str), end (datetime), process_id (str)  SecurityEvent
WindowsSecurity     list_all_logons_by_host           account all failed or successful logons to a host                                                            host_name (str), start (datetime), end (datetime)                                                                SecurityEvent
WindowsSecurity     list_events                       Retrieves list of all events                                                                                 start (datetime), end (datetime)                                                                                 SecurityEvent
WindowsSecurity     list_events_by_id                 Retrieves list of events on a host                                                                           start (datetime), event_list (list), end (datetime)                                                              SecurityEvent
WindowsSecurity     list_host_events                  Retrieves list of all events on a host                                                                       host_name (str), start (datetime), end (datetime)                                                                SecurityEvent
WindowsSecurity     list_host_events_by_id            Retrieves list of events on a host                                                                           host_name (str), start (datetime), end (datetime)                                                                SecurityEvent
WindowsSecurity     list_host_logon_failures          Retrieves the logon failure events on the host                                                               host_name (str), start (datetime), end (datetime)                                                                SecurityEvent
WindowsSecurity     list_host_logons                  Retrieves the logon events on the host                                                                       host_name (str), start (datetime), end (datetime)                                                                SecurityEvent
WindowsSecurity     list_host_processes               Retrieves list of processes on a host                                                                        host_name (str), start (datetime), end (datetime)                                                                SecurityEvent
WindowsSecurity     list_hosts_matching_commandline   Retrieves processes on hosts with matching commandline                                                       commandline (str), start (datetime), process_name (str), end (datetime)                                          SecurityEvent
WindowsSecurity     list_logon_attempts_by_account    Retrieves the logon events for an account                                                                    start (datetime), account_name (str), end (datetime)                                                             SecurityEvent
WindowsSecurity     list_logon_failures_by_account    Retrieves the logon failure events  for an account                                                           start (datetime), account_name (str), end (datetime)                                                             SecurityEvent
WindowsSecurity     list_logons_by_account            Retrieves the logon events for an account                                                                    start (datetime), account_name (str), end (datetime)                                                             SecurityEvent
WindowsSecurity     list_matching_processes           Retrieves list of processes matching process name                                                            start (datetime), process_name (str), end (datetime)                                                             SecurityEvent
WindowsSecurity     list_other_events                 Retrieves list of events other than logon and process on a host                                              host_name (str), start (datetime), end (datetime)                                                                SecurityEvent
WindowsSecurity     list_processes_in_session         Retrieves all processes on the host for a logon session                                                      host_name (str), logon_session_id (str), start (datetime), process_name (str), end (datetime), process_id (str)  SecurityEvent
==================  ================================  ===========================================================================================================  ===============================================================================================================  ===========================



Queries for Microsoft 365 Defender
----------------------------------

Data Environment identifier: M365D

============  ==========================  ==================================================================================================================================  ==================================================================  ===================
QueryGroup    Query                       Description                                                                                                                         Req-Params                                                          Table
============  ==========================  ==================================================================================================================================  ==================================================================  ===================
MDATP         file_path                   Lists all file events from files in a certain path                                                                                  start (datetime), path (str), end (datetime)                        DeviceProcessEvents
MDATP         host_alerts                 Lists alerts by for a specified hostname                                                                                            host_name (str), start (datetime), end (datetime)                   DeviceAlertEvents
MDATP         host_connections            Lists alerts by for a specified hostname                                                                                            host_name (str), start (datetime), end (datetime)                   DeviceNetworkEvents
MDATP         ip_alerts                   Lists alerts associated with a specified remote IP                                                                                  start (datetime), ip_address (str), end (datetime)                  DeviceAlertEvents
MDATP         ip_connections              Lists alerts associated with a specified remote IP                                                                                  start (datetime), ip_address (str), end (datetime)                  DeviceNetworkEvents
MDATP         list_alerts                 Retrieves list of alerts                                                                                                            start (datetime), end (datetime)                                    DeviceAlertEvents
MDATP         list_connections            Retrieves list of network connections for a host                                                                                    start (datetime), end (datetime)                                    DeviceNetworkEvents
MDATP         list_filehash               Lists all file events by hash                                                                                                       start (datetime), file_hash (str), end (datetime)                   DeviceProcessEvents
MDATP         list_files                  Lists all file events by filename                                                                                                   start (datetime), file_name (str), end (datetime)                   DeviceProcessEvents
MDATP         list_host_processes         Lists all process creations for a host                                                                                              host_name (str), start (datetime), end (datetime)                   DeviceProcessEvents
MDATP         process_cmd_line            Lists all processes with a command line containing a string                                                                         start (datetime), end (datetime), cmd_line (str)                    DeviceProcessEvents
MDATP         process_creations           Lists all processes created by name or hash                                                                                         process_identifier (str), start (datetime), end (datetime)          DeviceProcessEvents
MDATP         process_paths               Lists all processes created from a path                                                                                             file_path (str), start (datetime), end (datetime)                   DeviceProcessEvents
MDATP         protocol_connections        Lists alerts associated with a specified protocol                                                                                   start (datetime), protocol (str), end (datetime)                    DeviceNetworkEvents
MDATP         sha1_alerts                 Lists alerts associated with a specified SHA1 hash                                                                                  start (datetime), file_hash (str), end (datetime)                   DeviceAlertEvents
MDATP         url_alerts                  Lists alerts associated with a specified URL                                                                                        start (datetime), url (str), end (datetime)                         DeviceAlertEvents
MDATP         url_connections             Lists alerts associated with a specified URL                                                                                        start (datetime), url (str), end (datetime)                         DeviceNetworkEvents
MDATP         user_files                  Lists all files created by a user                                                                                                   account_name (str), start (datetime), end (datetime)                -
MDATP         user_logons                 Lists all user logons by user                                                                                                       account_name (str), start (datetime), end (datetime)                -
MDATP         user_network                Lists all network connections associated with a user                                                                                account_name (str), start (datetime), end (datetime)                -
MDATP         user_processes              Lists all processes created by a user                                                                                               account_name (str), start (datetime), end (datetime)                -
MDATPHunting  accessibility_persistence   This query looks for persistence or privilege escalation done using Windows Accessibility features.                                 start (datetime), end (datetime)                                    -
MDATPHunting  av_sites                    Pivot from downloads detected by Windows Defender Antivirus to other files downloaded from the same sites                           start (datetime), end (datetime)                                    -
MDATPHunting  b64_pe                      Finding base64 encoded PE files header seen in the command line parameters                                                          start (datetime), end (datetime)                                    -
MDATPHunting  brute_force                 Look for public IP addresses that failed to logon to a computer multiple times, using multiple accounts, and eventually succeeded.  start (datetime), end (datetime)                                    -
MDATPHunting  cve_2018_1000006l           Looks for CVE-2018-1000006 exploitation                                                                                             start (datetime), end (datetime)                                    -
MDATPHunting  cve_2018_1111               Looks for CVE-2018-1111 exploitation                                                                                                start (datetime), end (datetime)                                    -
MDATPHunting  cve_2018_4878               This query checks for specific processes and domain TLD used in the CVE-2018-4878                                                   start (datetime), end (datetime)                                    -
MDATPHunting  doc_with_link               Looks for a Word document attachment, from which a link was clicked, and after which there was a browser download.                  start (datetime), end (datetime)                                    -
MDATPHunting  dropbox_link                Looks for user content downloads from dropbox that originate from a link/redirect from a 3rd party site.                            start (datetime), end (datetime)                                    -
MDATPHunting  email_link                  Look for links opened from mail apps – if a detection occurred right afterwards                                                     start (datetime), end (datetime)                                    -
MDATPHunting  email_smartscreen           Look for links opened from outlook.exe, followed by a browser download and then a SmartScreen app warning                           start (datetime), end (datetime)                                    -
MDATPHunting  malware_recycle             Finding attackers hiding malware in the recycle bin.                                                                                start (datetime), end (datetime)                                    -
MDATPHunting  network_scans               Looking for high volume queries against a given RemoteIP, per ComputerName, RemotePort and Process                                  start (datetime), end (datetime)                                    -
MDATPHunting  powershell_downloads        Finds PowerShell execution events that could involve a download.                                                                    start (datetime), end (datetime)                                    -
MDATPHunting  service_account_powershell  Service Accounts Performing Remote PowerShell                                                                                       start (datetime), end (datetime)                                    -
MDATPHunting  smartscreen_ignored         Query for SmartScreen URL blocks, where the user has decided to run the malware nontheless.                                         start (datetime), end (datetime)                                    -
MDATPHunting  smb_discovery               Query for processes that accessed more than 10 IP addresses over port 445 (SMB) - possibly scanning for network shares.             start (datetime), end (datetime)                                    -
MDATPHunting  tor                         Looks for Tor client, or for a common Tor plugin called Meek.                                                                       start (datetime), end (datetime)                                    -
MDATPHunting  uncommon_powershell         Find which uncommon Powershell Cmdlets were executed on that machine in a certain time period.                                      host_name (str), start (datetime), timestamp (str), end (datetime)  -
MDATPHunting  user_enumeration            The query finds attempts to list users or groups using Net commands                                                                 start (datetime), end (datetime)                                    -
============  ==========================  ==================================================================================================================================  ==================================================================  ===================



Queries for Microsoft Graph
---------------------------

Data Environment identifier: SecurityGraph

==================  ====================  ====================================================  ==================================================  =======
QueryGroup          Query                 Description                                           Req-Params                                          Table
==================  ====================  ====================================================  ==================================================  =======
SecurityGraphAlert  get_alert             Retrieves a single alert by AlertId                   alert_id (str)                                      -
SecurityGraphAlert  list_alerts           Retrieves list of alerts                              start (datetime), end (datetime)                    -
SecurityGraphAlert  list_alerts_for_file  Retrieves list of alerts for file name, path or hash  start (datetime), end (datetime)                    -
SecurityGraphAlert  list_alerts_for_host  Retrieves list of alerts for a hostname or FQDN       host_name (str), start (datetime), end (datetime)   -
SecurityGraphAlert  list_alerts_for_ip    Retrieves list of alerts for a IP Address             start (datetime), ip_address (str), end (datetime)  -
SecurityGraphAlert  list_alerts_for_user  Retrieves list of alerts for a user account           start (datetime), end (datetime)                    -
SecurityGraphAlert  list_related_alerts   Retrieves list of alerts with a common entity         start (datetime), end (datetime)                    -
==================  ====================  ====================================================  ==================================================  =======



Queries for Splunk
------------------

Data Environment identifier: Splunk

==============  =========================  =============================================================  ====================================================  =======
QueryGroup      Query                      Description                                                    Req-Params                                            Table
==============  =========================  =============================================================  ====================================================  =======
Alerts          list_alerts                Retrieves list of alerts                                       start (datetime), end (datetime)                      -
Alerts          list_alerts_for_dest_ip    Retrieves list of alerts with a common destination IP Address  start (datetime), ip_address (str), end (datetime)    -
Alerts          list_alerts_for_src_ip     Retrieves list of alerts with a common source IP Address       start (datetime), ip_address (str), end (datetime)    -
Alerts          list_alerts_for_user       Retrieves list of alerts with a common username                start (datetime), user (str), end (datetime)          -
Alerts          list_all_alerts            Retrieves all configured alerts                                start (datetime), end (datetime)                      -
Authentication  list_logon_failures        All failed user logon events on any host                       start (datetime), end (datetime)                      -
Authentication  list_logons_for_account    All successful user logon events for account (all hosts)       start (datetime), account_name (str), end (datetime)  -
Authentication  list_logons_for_host       All logon events on a host                                     host_name (str), start (datetime), end (datetime)     -
Authentication  list_logons_for_source_ip  All successful user logon events for source IP (all hosts)     start (datetime), ip_address (str), end (datetime)    -
SplunkGeneral   get_events_parameterized   Generic parameterized query from index/source                  start (datetime), end (datetime)                      -
SplunkGeneral   list_all_datatypes         Summary of all events by index and sourcetype                  start (datetime), end (datetime)                      -
SplunkGeneral   list_all_savedsearches     Retrieves all saved searches                                   start (datetime), end (datetime)                      -
audittrail      list_all_audittrail        Retrieves all audit trail logs                                 start (datetime), end (datetime)                      -
==============  =========================  =============================================================  ====================================================  =======



Queries for Azure Resource Graph
--------------------------------

Data Environment identifier: ResourceGraph

=============  ==============================  ====================================================  ===================  =========
QueryGroup     Query                           Description                                           Req-Params           Table
=============  ==============================  ====================================================  ===================  =========
ResourceGraph  list_detailed_virtual_machines  Retrieves list of VMs with network details                                 resources
ResourceGraph  list_public_ips                 Retrieves list of resources with public IP addresses                       resources
ResourceGraph  list_resources                  Retrieves list of resources                                                resources
ResourceGraph  list_resources_by_api_version   Retrieves list of resources for each API version                           resources
ResourceGraph  list_resources_by_type          Retrieves list of resources by type                   resource_type (str)  resources
ResourceGraph  list_virtual_machines           Retrieves list of VM resources                                             resources
=============  ==============================  ====================================================  ===================  =========



Queries for Sumologic
---------------------

Data Environment identifier: Sumologic

================  ==================  =======================================  ================================  =======
QueryGroup        Query               Description                              Req-Params                        Table
================  ==================  =======================================  ================================  =======
SumologicGeneral  list_all_datatypes  Summary of all events by sourceCategory  start (datetime), end (datetime)  -
================  ==================  =======================================  ================================  =======



Queries for Local Data
----------------------

Data Environment identifier: LocalData

===============  ================================  ======================================  ============  =======
QueryGroup       Query                             Description                             Req-Params    Table
===============  ================================  ======================================  ============  =======
Azure            list_all_signins_geo              List all Azure AD logon events                        -
Network          list_azure_network_flows_by_host  List Azure Network flows by host name                 -
Network          list_azure_network_flows_by_ip    List Azure Network flows by IP address                -
SecurityAlert    list_alerts                       Retrieves list of alerts                              -
WindowsSecurity  get_process_tree                  Get process tree for a process                        -
WindowsSecurity  list_host_events                  List events failures on host                          -
WindowsSecurity  list_host_logon_failures          List logon failures on host                           -
WindowsSecurity  list_host_logons                  List logons on host                                   -
WindowsSecurity  list_host_processes               List processes on host                                -
===============  ================================  ======================================  ============  =======


