Data Queries Reference
======================

Queries for AzureSentinel
-------------------------

==================  ================================  ===========================================================================================================  ===============================================================================================================  ===========================
QueryGroup          Query                             Description                                                                                                  ReqdParams                                                                                                       Table
==================  ================================  ===========================================================================================================  ===============================================================================================================  ===========================
Azure               list_aad_signins_for_account      Lists Azure AD Signins for Account                                                                           account_name (str)                                                                                               SigninLogs
Azure               list_aad_signins_for_ip           Lists Azure AD Signins for an IP Address                                                                     ip_address_list (list)                                                                                           SigninLogs
Azure               list_all_signins_geo              Gets Signin data used by morph charts                                                                                                                                                                                         SigninLogs
Azure               list_azure_activity_for_account   Lists Azure Activity for Account                                                                             account_name (str)                                                                                               AzureActivity
Azure               list_azure_activity_for_ip        Lists Azure Activity for Caller IP Address(es)                                                               ip_address_list (list)                                                                                           AzureActivity
Azure               list_azure_activity_for_resource  Lists Azure Activity for a Resource                                                                          resource (str)                                                                                                   AzureActivity
AzureNetwork        az_net_analytics                  All Azure Network Analytics Data                                                                             start (datetime), end (datetime)                                                                                 AzureNetworkAnalytics_CL
AzureNetwork        get_heartbeat_for_host            Retrieves latest OMS Heartbeat event for host.                                                               host_name (str)                                                                                                  Heartbeat
AzureNetwork        get_heartbeat_for_ip              Retrieves latest OMS Heartbeat event for ip address.                                                         ip_address (str)                                                                                                 Heartbeat
AzureNetwork        get_host_for_ip                   Gets the latest AzureNetworkAnalytics interface event for a host.                                            ip_address (str)                                                                                                 AzureNetworkAnalytics_CL
AzureNetwork        get_ips_for_host                  Gets the latest AzureNetworkAnalytics interface event for a host.                                            host_name (str)                                                                                                  AzureNetworkAnalytics_CL
AzureNetwork        list_azure_network_flows_by_host  Retrieves Azure network analytics flow events.                                                               host_name (str), start (datetime), end (datetime)                                                                AzureNetworkAnalytics_CL
AzureNetwork        list_azure_network_flows_by_ip    Retrieves Azure network analytics flow events.                                                               ip_address_list (list), start (datetime), end (datetime)                                                         AzureNetworkAnalytics_CL
AzureSentinel       get_bookmark_by_id                Retrieves a single Bookmark by BookmarkId                                                                    bookmark_id (str)                                                                                                HuntingBookmark
AzureSentinel       get_bookmark_by_name              Retrieves one or more Bookmarks by Bookmark Name                                                             bookmark_name (str)                                                                                              HuntingBookmark
AzureSentinel       list_bookmarks                    Retrieves list of bookmarks                                                                                                                                                                                                   HuntingBookmark
AzureSentinel       list_bookmarks_for_entity         Retrieves bookmarks for entity string                                                                        entity_id (str)                                                                                                  HuntingBookmark
AzureSentinel       list_bookmarks_for_tags           Retrieves Bookmark by one or mare Tags                                                                       bookmark_tags (list)                                                                                             HuntingBookmark
Heartbeat           get_heartbeat_for_host            Retrieves latest OMS Heartbeat event for host.                                                               host_name (str)                                                                                                  Heartbeat
Heartbeat           get_heartbeat_for_ip              Retrieves latest OMS Heartbeat event for ip address.                                                         ip_address (str)                                                                                                 Heartbeat
Heartbeat           get_info_by_hostname              Retrieves Information by Hostname                                                                            start (datetime), end (datetime), host_name (str)                                                                Heartbeat
Heartbeat           get_info_by_ipaddress             Retrieves Information by IP address                                                                          start (datetime), end (datetime), ip_address (str)                                                               Heartbeat
LinuxAudit          auditd_all                        Extract all audit messages grouped by mssg_id                                                                start (datetime), end (datetime)                                                                                 AuditLog_CL
LinuxSyslog         all_syslog                        Returns all syslog activity for a host                                                                       start (datetime), end (datetime)                                                                                 Syslog
LinuxSyslog         cron_activity                     All cron activity                                                                                            start (datetime), end (datetime)                                                                                 Syslog
LinuxSyslog         list_host_logon_failures          All failed user logon events on a host                                                                       start (datetime), end (datetime), host_name (str)                                                                Syslog
LinuxSyslog         list_logon_failures               All failed user logon events on any host                                                                     start (datetime), end (datetime)                                                                                 Syslog
LinuxSyslog         list_logons_for_account           All successful user logon events for account (all hosts)                                                     start (datetime), end (datetime), account_name (str)                                                             Syslog
LinuxSyslog         list_logons_for_host              All logon events on a host                                                                                   start (datetime), end (datetime), host_name (str)                                                                Syslog
LinuxSyslog         list_logons_for_source_ip         All successful user logon events for source IP (all hosts)                                                   start (datetime), end (datetime), ip_address (str)                                                               Syslog
LinuxSyslog         squid_activity                    All squid proxy activity                                                                                     start (datetime), end (datetime), host_name (str)                                                                Syslog
LinuxSyslog         sudo_activity                     All sudo activity                                                                                            start (datetime), end (datetime)                                                                                 Syslog
LinuxSyslog         user_group_activity               All user/group additions, deletions, and modifications                                                       start (datetime), end (datetime)                                                                                 Syslog
LinuxSyslog         user_logon                        All user logon events on a host                                                                              start (datetime), end (datetime), host_name (str)                                                                Syslog
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
Network             list_azure_network_flows_by_ip    Retrieves Azure network analytics flow events.                                                               ip_address_list (list), start (datetime), end (datetime)                                                         AzureNetworkAnalytics_CL
Office365           list_activity_for_account         Lists Office Activity for Account                                                                            account_name (str)                                                                                               OfficeActivity
Office365           list_activity_for_ip              Lists Office Activity for Caller IP Address(es)                                                              ip_address_list (list)                                                                                           OfficeActivity
Office365           list_azure_activity_for_resource  Lists Office Activity for a Resource                                                                         resource (str)                                                                                                   OfficeActivity
SecurityAlert       get_alert                         Retrieves a single alert by SystemAlertId                                                                    system_alert_id (str)                                                                                            SecurityAlert
SecurityAlert       list_alerts                       Retrieves list of alerts                                                                                                                                                                                                      SecurityAlert
SecurityAlert       list_alerts_counts                Retrieves summary count of alerts by type                                                                                                                                                                                     SecurityAlert
SecurityAlert       list_alerts_for_ip                Retrieves list of alerts with a common IP Address                                                            start (datetime), end (datetime), source_ip_list (str)                                                           SecurityAlert
SecurityAlert       list_related_alerts               Retrieves list of alerts with a common host, account or process                                                                                                                                                               SecurityAlert
ThreatIntelligence  list_indicators                   Retrieves list of all current indicators.                                                                                                                                                                                     ThreatIntelligenceIndicator
ThreatIntelligence  list_indicators_by_domain         Retrieves list of indicators by domain                                                                       observables (list)                                                                                               ThreatIntelligenceIndicator
ThreatIntelligence  list_indicators_by_email          Retrieves list of indicators by email address                                                                observables (list)                                                                                               ThreatIntelligenceIndicator
ThreatIntelligence  list_indicators_by_filepath       Retrieves list of indicators by file path                                                                    observables (list)                                                                                               ThreatIntelligenceIndicator
ThreatIntelligence  list_indicators_by_hash           Retrieves list of indicators by file hash                                                                    observables (list)                                                                                               ThreatIntelligenceIndicator
ThreatIntelligence  list_indicators_by_ip             Retrieves list of indicators by IP Address                                                                   observables (list)                                                                                               ThreatIntelligenceIndicator
ThreatIntelligence  list_indicators_by_url            Retrieves list of indicators by URL                                                                          observables (list)                                                                                               ThreatIntelligenceIndicator
WindowsSecurity     get_host_logon                    Retrieves the logon event for the session id on the host                                                     start (datetime), end (datetime), host_name (str), logon_session_id (str)                                        SecurityEvent
WindowsSecurity     get_parent_process                Retrieves the parent process of a supplied process                                                           start (datetime), end (datetime), host_name (str), process_name (str), process_id (str), logon_session_id (str)  SecurityEvent
WindowsSecurity     get_process_tree                  Retrieves the process tree of a supplied process                                                             start (datetime), end (datetime), host_name (str), process_name (str), process_id (str), logon_session_id (str)  SecurityEvent
WindowsSecurity     list_all_logons_by_host           account all failed or successful logons to a host                                                            start (datetime), end (datetime), host_name (str)                                                                SecurityEvent
WindowsSecurity     list_events                       Retrieves list of all events                                                                                 start (datetime), end (datetime)                                                                                 SecurityEvent
WindowsSecurity     list_events_by_id                 Retrieves list of events on a host                                                                           start (datetime), end (datetime), event_list (list)                                                              SecurityEvent
WindowsSecurity     list_host_events                  Retrieves list of all events on a host                                                                       start (datetime), end (datetime), host_name (str)                                                                SecurityEvent
WindowsSecurity     list_host_events_by_id            Retrieves list of events on a host                                                                           start (datetime), end (datetime), host_name (str)                                                                SecurityEvent
WindowsSecurity     list_host_logon_failures          Retrieves the logon failure events on the host                                                               start (datetime), end (datetime), host_name (str)                                                                SecurityEvent
WindowsSecurity     list_host_logons                  Retrieves the logon events on the host                                                                       start (datetime), end (datetime), host_name (str)                                                                SecurityEvent
WindowsSecurity     list_host_processes               Retrieves list of processes on a host                                                                        start (datetime), end (datetime), host_name (str)                                                                SecurityEvent
WindowsSecurity     list_hosts_matching_commandline   Retrieves processes on hosts with matching commandline                                                       start (datetime), end (datetime), process_name (str), commandline (str)                                          SecurityEvent
WindowsSecurity     list_logon_attempts_by_account    Retrieves the logon events for an account                                                                    start (datetime), end (datetime), account_name (str)                                                             SecurityEvent
WindowsSecurity     list_logon_failures_by_account    Retrieves the logon failure events  for an account                                                           start (datetime), end (datetime), account_name (str)                                                             SecurityEvent
WindowsSecurity     list_logons_by_account            Retrieves the logon events for an account                                                                    start (datetime), end (datetime), account_name (str)                                                             SecurityEvent
WindowsSecurity     list_matching_processes           Retrieves list of processes matching process name                                                            start (datetime), end (datetime), process_name (str)                                                             SecurityEvent
WindowsSecurity     list_other_events                 Retrieves list of events other than logon and process on a host                                              start (datetime), end (datetime), host_name (str)                                                                SecurityEvent
WindowsSecurity     list_processes_in_session         Retrieves all processes on the host for a logon session                                                      start (datetime), end (datetime), host_name (str), process_name (str), process_id (str), logon_session_id (str)  SecurityEvent
==================  ================================  ===========================================================================================================  ===============================================================================================================  ===========================

Queries for LocalData
---------------------

===============  ================================  ======================================  ============  =======
QueryGroup       Query                             Description                             ReqdParams    Table
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

Queries for MDE
---------------

============  ==========================  ==================================================================================================================================  ===============================  ==========================
QueryGroup    Query                       Description                                                                                                                         ReqdParams                       Table
============  ==========================  ==================================================================================================================================  ===============================  ==========================
MDATP         file_path                   Lists all file events from files in a certain path                                                                                  path (str)                       ProcessCreationEvents
MDATP         host_alerts                 Lists alerts by for a specified hostname                                                                                            host_name (str)                  AlertEvents
MDATP         host_connections            Lists alerts by for a specified hostname                                                                                            hostname (str)                   NetworkCommunicationEvents
MDATP         ip_alerts                   Lists alerts associated with a specified remote IP                                                                                  ip_address (str)                 AlertEvents
MDATP         ip_connections              Lists alerts associated with a specified remote IP                                                                                  ip_address (str)                 NetworkCommunicationEvents
MDATP         list_alerts                 Retrieves list of alerts                                                                                                                                             AlertEvents
MDATP         list_connections            Retrieves list of network connections for a host                                                                                                                     NetworkCommunicationEvents
MDATP         list_filehash               Lists all file events by hash                                                                                                       file_hash (str)                  ProcessCreationEvents
MDATP         list_files                  Lists all file events by filename                                                                                                   file_name (str)                  ProcessCreationEvents
MDATP         list_host_processes         Lists all process creations for a host                                                                                              host_name (str)                  ProcessCreationEvents
MDATP         process_cmd_line            Lists all processes with a command line containing a string                                                                         cmd_line (str)                   ProcessCreationEvents
MDATP         process_creations           Lists all processes created by name or hash                                                                                         process_identifier (str)         ProcessCreationEvents
MDATP         process_paths               Lists all processes created from a path                                                                                             file_path (str)                  ProcessCreationEvents
MDATP         protocol_connections        Lists alerts associated with a specified protocol                                                                                   protocol (str)                   NetworkCommunicationEvents
MDATP         sha1_alerts                 Lists alerts associated with a specified SHA1 hash                                                                                  file_hash (str)                  AlertEvents
MDATP         url_alerts                  Lists alerts associated with a specified URL                                                                                        url (str)                        AlertEvents
MDATP         url_connections             Lists alerts associated with a specified URL                                                                                        url (str)                        NetworkCommunicationEvents
MDATP         user_files                  Lists all files created by a user                                                                                                   account_name (str)               -
MDATP         user_logons                 Lists all user logons by user                                                                                                       account_name (str)               -
MDATP         user_network                Lists all network connections associated with a user                                                                                account_name (str)               -
MDATP         user_processes              Lists all processes created by a user                                                                                               account_name (str)               -
MDATPHunting  accessibility_persistence   This query looks for persistence or privilege escalation done using Windows Accessibility features.                                                                  -
MDATPHunting  av_sites                    Pivot from downloads detected by Windows Defender Antivirus to other files downloaded from the same sites                                                            -
MDATPHunting  b64_pe                      Finding base64 encoded PE files header seen in the command line parameters                                                                                           -
MDATPHunting  brute_force                 Look for public IP addresses that failed to logon to a computer multiple times, using multiple accounts, and eventually succeeded.                                   -
MDATPHunting  cve_2018_1000006l           Looks for CVE-2018-1000006 exploitation                                                                                                                              -
MDATPHunting  cve_2018_1111               Looks for CVE-2018-1111 exploitation                                                                                                                                 -
MDATPHunting  cve_2018_4878               This query checks for specific processes and domain TLD used in the CVE-2018-4878                                                                                    -
MDATPHunting  doc_with_link               Looks for a Word document attachment, from which a link was clicked, and after which there was a browser download.                                                   -
MDATPHunting  dropbox_link                Looks for user content downloads from dropbox that originate from a link/redirect from a 3rd party site.                                                             -
MDATPHunting  email_link                  Look for links opened from mail apps – if a detection occurred right afterwards                                                                                      -
MDATPHunting  email_smartscreen           Look for links opened from outlook.exe, followed by a browser download and then a SmartScreen app warning                                                            -
MDATPHunting  malware_recycle             Finding attackers hiding malware in the recycle bin.                                                                                                                 -
MDATPHunting  network_scans               Looking for high volume queries against a given RemoteIP, per ComputerName, RemotePort and Process                                                                   -
MDATPHunting  powershell_downloads        Finds PowerShell execution events that could involve a download.                                                                                                     -
MDATPHunting  service_account_powershell  Service Accounts Performing Remote PowerShell                                                                                                                        -
MDATPHunting  smartscreen_ignored         Query for SmartScreen URL blocks, where the user has decided to run the malware nontheless.                                                                          -
MDATPHunting  smb_discovery               Query for processes that accessed more than 10 IP addresses over port 445 (SMB) - possibly scanning for network shares.                                              -
MDATPHunting  tor                         Looks for Tor client, or for a common Tor plugin called Meek.                                                                                                        -
MDATPHunting  uncommon_powershell         Find which uncommon Powershell Cmdlets were executed on that machine in a certain time period.                                      hostname (str), timestamp (str)  -
MDATPHunting  user_enumeration            The query finds attempts to list users or groups using Net commands                                                                                                  -
============  ==========================  ==================================================================================================================================  ===============================  ==========================

Queries for ResourceGraph
-------------------------

=============  ==============================  =====================================================  ============  =========
QueryGroup     Query                           Description                                            ReqdParams    Table
=============  ==============================  =====================================================  ============  =========
ResourceGraph  list_detailed_virtual_machines  Retrieves list of VMs with network details                           resources
ResourceGraph  list_public_ips                 Retrieves list of resources with public IP addresses                 resources
ResourceGraph  list_resources                  Retrieves list of resources                                          resources
ResourceGraph  list_resources_by_api_version   Retrieves list of resources for each API version                     resources
ResourceGraph  list_resources_by_type          Retrieves list of resources by type                                  resources
ResourceGraph  list_virtual_machines           Retrieves list of VM resources                                       resources
=============  ==============================  =====================================================  ============  =========

Queries for SecurityGraph
-------------------------

==================  ====================  ====================================================  ==================================================  =======
QueryGroup          Query                 Description                                           ReqdParams                                          Table
==================  ====================  ====================================================  ==================================================  =======
SecurityGraphAlert  get_alert             Retrieves a single alert by AlertId                   alert_id (str)                                      -
SecurityGraphAlert  list_alerts           Retrieves list of alerts                              start (datetime), end (datetime)                    -
SecurityGraphAlert  list_alerts_for_file  Retrieves list of alerts for file name, path or hash  start (datetime), end (datetime)                    -
SecurityGraphAlert  list_alerts_for_host  Retrieves list of alerts for a hostname or FQDN       start (datetime), end (datetime), host_name (str)   -
SecurityGraphAlert  list_alerts_for_ip    Retrieves list of alerts for a IP Address             start (datetime), end (datetime), ip_address (str)  -
SecurityGraphAlert  list_alerts_for_user  Retrieves list of alerts for a user account           start (datetime), end (datetime)                    -
SecurityGraphAlert  list_related_alerts   Retrieves list of alerts with a common entity         start (datetime), end (datetime)                    -
==================  ====================  ====================================================  ==================================================  =======

Queries for Splunk
------------------

=============  ========================  =============================================  ============  =======
QueryGroup     Query                     Description                                    ReqdParams    Table
=============  ========================  =============================================  ============  =======
Alerts         list_all_alerts           Retrieves all configured alerts                              -
SplunkGeneral  get_events_parameterized  Generic parameterized query from index/source                -
SplunkGeneral  list_all_datatypes        Summary of all events by index and sourcetype                -
SplunkGeneral  list_all_savedsearches    Retrieves all saved searches                                 -
audittrail     list_all_audittrail       Retrieves all audit trail logs                               -
=============  ========================  =============================================  ============  =======

