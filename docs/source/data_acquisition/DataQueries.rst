Data Queries Reference
======================


Queries for Microsoft Sentinel
------------------------------

Data Environment identifier: MSSentinel

==================  ================================  ===========================================================================================================  ===============================================================================================================  ===========================
QueryGroup          Query                             Description                                                                                                  Req-Params                                                                                                       Table
==================  ================================  ===========================================================================================================  ===============================================================================================================  ===========================
Azure               get_vmcomputer_for_host           Gets latest VMComputer record for Host                                                                       end (datetime), host_name (str), start (datetime)                                                                VMComputer
Azure               get_vmcomputer_for_ip             Gets latest VMComputer record for IPAddress                                                                  end (datetime), ip_address (str), start (datetime)                                                               VMComputer
Azure               list_aad_signins_for_account      Lists Azure AD Signins for Account                                                                           account_name (str), end (datetime), start (datetime)                                                             SigninLogs
Azure               list_aad_signins_for_ip           Lists Azure AD Signins for an IP Address                                                                     end (datetime), ip_address_list (list), start (datetime)                                                         SigninLogs
Azure               list_all_signins_geo              Gets Signin data used by morph charts                                                                        end (datetime), start (datetime)                                                                                 SigninLogs
Azure               list_azure_activity_for_account   Lists Azure Activity for Account                                                                             account_name (str), end (datetime), start (datetime)                                                             AzureActivity
Azure               list_azure_activity_for_ip        Lists Azure Activity for Caller IP Address(es)                                                               end (datetime), ip_address_list (list), start (datetime)                                                         AzureActivity
Azure               list_azure_activity_for_resource  Lists Azure Activity for a Resource                                                                          end (datetime), resource_id (str), start (datetime)                                                              AzureActivity
Azure               list_storage_ops_for_hash         no description                                                                                               end (datetime), file_hash (str), start (datetime)                                                                StorageFileLogs
Azure               list_storage_ops_for_ip           no description                                                                                               end (datetime), ip_address (str), start (datetime)                                                               StorageFileLogs
AzureNetwork        az_net_analytics                  All Azure Network Analytics Data                                                                             end (datetime), start (datetime)                                                                                 AzureNetworkAnalytics_CL
AzureNetwork        dns_lookups_for_domain            Dns queries for a domain                                                                                     domain (str), end (datetime), start (datetime)                                                                   DnsEvents
AzureNetwork        dns_lookups_for_ip                Dns queries for a domain                                                                                     end (datetime), ip_address (str), start (datetime)                                                               DnsEvents
AzureNetwork        dns_lookups_from_ip               Dns queries for a domain                                                                                     end (datetime), ip_address (str), start (datetime)                                                               DnsEvents
AzureNetwork        get_heartbeat_for_host            Retrieves latest OMS Heartbeat event for host.                                                               host_name (str)                                                                                                  Heartbeat
AzureNetwork        get_heartbeat_for_ip              Retrieves latest OMS Heartbeat event for ip address.                                                         ip_address (str)                                                                                                 Heartbeat
AzureNetwork        get_host_for_ip                   Gets the latest AzureNetworkAnalytics interface event for a host.                                            ip_address (str)                                                                                                 AzureNetworkAnalytics_CL
AzureNetwork        get_ips_for_host                  Gets the latest AzureNetworkAnalytics interface event for a host.                                            host_name (str)                                                                                                  AzureNetworkAnalytics_CL
AzureNetwork        list_azure_network_flows_by_host  Retrieves Azure network analytics flow events.                                                               end (datetime), host_name (str), start (datetime)                                                                AzureNetworkAnalytics_CL
AzureNetwork        list_azure_network_flows_by_ip    Retrieves Azure network analytics flow events.                                                               end (datetime), ip_address_list (list), start (datetime)                                                         AzureNetworkAnalytics_CL
AzureSentinel       get_bookmark_by_id                Retrieves a single Bookmark by BookmarkId                                                                    bookmark_id (str), end (datetime), start (datetime)                                                              HuntingBookmark
AzureSentinel       get_bookmark_by_name              Retrieves one or more Bookmarks by Bookmark Name                                                             bookmark_name (str), end (datetime), start (datetime)                                                            HuntingBookmark
AzureSentinel       list_bookmarks                    Retrieves list of bookmarks                                                                                  end (datetime), start (datetime)                                                                                 HuntingBookmark
AzureSentinel       list_bookmarks_for_entity         Retrieves bookmarks for entity string                                                                        end (datetime), start (datetime)                                                                                 HuntingBookmark
AzureSentinel       list_bookmarks_for_tags           Retrieves Bookmark by one or mare Tags                                                                       bookmark_tags (list), end (datetime), start (datetime)                                                           HuntingBookmark
Heartbeat           get_heartbeat_for_host            Retrieves latest OMS Heartbeat event for host.                                                               host_name (str)                                                                                                  Heartbeat
Heartbeat           get_heartbeat_for_ip              Retrieves latest OMS Heartbeat event for ip address.                                                         ip_address (str)                                                                                                 Heartbeat
Heartbeat           get_info_by_hostname              Deprecated - use 'get_heartbeat_for_host'                                                                    end (datetime), host_name (str), start (datetime)                                                                Heartbeat
Heartbeat           get_info_by_ipaddress             Deprecated - use 'get_heartbeat_for_ip'                                                                      end (datetime), ip_address (str), start (datetime)                                                               Heartbeat
LinuxAudit          auditd_all                        Extract all audit messages grouped by mssg_id                                                                end (datetime), start (datetime)                                                                                 AuditLog_CL
LinuxSyslog         all_syslog                        Returns all syslog activity for a host                                                                       end (datetime), start (datetime)                                                                                 Syslog
LinuxSyslog         cron_activity                     All cron activity                                                                                            end (datetime), start (datetime)                                                                                 Syslog
LinuxSyslog         list_account_logon_failures       All failed user logon events from an IP address                                                              account_name (str), end (datetime), start (datetime)                                                             Syslog
LinuxSyslog         list_host_logon_failures          All failed user logon events on a host                                                                       end (datetime), host_name (str), start (datetime)                                                                Syslog
LinuxSyslog         list_ip_logon_failures            All failed user logon events from an IP address                                                              end (datetime), ip_address (str), start (datetime)                                                               Syslog
LinuxSyslog         list_logon_failures               All failed user logon events on any host                                                                     end (datetime), start (datetime)                                                                                 Syslog
LinuxSyslog         list_logons_for_account           All successful user logon events for account (all hosts)                                                     account_name (str), end (datetime), start (datetime)                                                             Syslog
LinuxSyslog         list_logons_for_host              All logon events on a host                                                                                   end (datetime), host_name (str), start (datetime)                                                                Syslog
LinuxSyslog         list_logons_for_source_ip         All successful user logon events for source IP (all hosts)                                                   end (datetime), ip_address (str), start (datetime)                                                               Syslog
LinuxSyslog         squid_activity                    All squid proxy activity                                                                                     end (datetime), host_name (str), start (datetime)                                                                Syslog
LinuxSyslog         sudo_activity                     All sudo activity                                                                                            end (datetime), start (datetime)                                                                                 Syslog
LinuxSyslog         user_group_activity               All user/group additions, deletions, and modifications                                                       end (datetime), start (datetime)                                                                                 Syslog
LinuxSyslog         user_logon                        All user logon events on a host                                                                              end (datetime), host_name (str), start (datetime)                                                                Syslog
MultiDataSource     get_timeseries_anomalies          Time Series filtered anomalies detected using built-in KQL time series function-series_decompose_anomalies   end (datetime), start (datetime), table (str)                                                                    na
MultiDataSource     get_timeseries_data               Retrieves TimeSeriesData prepared to use with built-in KQL time series functions                             end (datetime), start (datetime), table (str)                                                                    na
MultiDataSource     get_timeseries_decompose          Time Series decomposition and anomalies generated using built-in KQL time series function- series_decompose  end (datetime), start (datetime), table (str)                                                                    na
MultiDataSource     plot_timeseries_datawithbaseline  Plot timeseries data using built-in KQL time series decomposition using built-in KQL render method           end (datetime), start (datetime), table (str)                                                                    na
MultiDataSource     plot_timeseries_scoreanomolies    Plot timeseries anomaly score using built-in KQL render method                                               end (datetime), start (datetime), table (str)                                                                    na
Network             get_heartbeat_for_host            Retrieves latest OMS Heartbeat event for host.                                                               host_name (str)                                                                                                  Heartbeat
Network             get_heartbeat_for_ip              Retrieves latest OMS Heartbeat event for ip address.                                                         ip_address (str)                                                                                                 Heartbeat
Network             get_host_for_ip                   Gets the latest AzureNetworkAnalytics interface event for a host.                                            ip_address (str)                                                                                                 AzureNetworkAnalytics_CL
Network             get_ips_for_host                  Gets the latest AzureNetworkAnalytics interface event for a host.                                            host_name (str)                                                                                                  AzureNetworkAnalytics_CL
Network             list_azure_network_flows_by_host  Retrieves Azure network analytics flow events.                                                               end (datetime), host_name (str), start (datetime)                                                                AzureNetworkAnalytics_CL
Network             list_azure_network_flows_by_ip    Retrieves Azure network analytics flow events.                                                               end (datetime), ip_address_list (list), start (datetime)                                                         AzureNetworkAnalytics_CL
Office365           list_activity_for_account         Lists Office Activity for Account                                                                            account_name (str), end (datetime), start (datetime)                                                             OfficeActivity
Office365           list_activity_for_ip              Lists Office Activity for Caller IP Address(es)                                                              end (datetime), ip_address_list (list), start (datetime)                                                         OfficeActivity
Office365           list_activity_for_resource        Lists Office Activity for a Resource                                                                         end (datetime), resource_id (str), start (datetime)                                                              OfficeActivity
SecurityAlert       get_alert                         Retrieves a single alert by SystemAlertId                                                                    system_alert_id (str)                                                                                            SecurityAlert
SecurityAlert       list_alerts                       Retrieves list of alerts                                                                                     end (datetime), start (datetime)                                                                                 SecurityAlert
SecurityAlert       list_alerts_counts                Retrieves summary count of alerts by type                                                                    end (datetime), start (datetime)                                                                                 SecurityAlert
SecurityAlert       list_alerts_for_ip                Retrieves list of alerts with a common IP Address                                                            end (datetime), source_ip_list (str), start (datetime)                                                           SecurityAlert
SecurityAlert       list_related_alerts               Retrieves list of alerts with a common host, account or process                                              end (datetime), start (datetime)                                                                                 SecurityAlert
ThreatIntelligence  list_indicators                   Retrieves list of all current indicators.                                                                    end (datetime), start (datetime)                                                                                 ThreatIntelligenceIndicator
ThreatIntelligence  list_indicators_by_domain         Retrieves list of indicators by domain                                                                       domain_list (list), end (datetime), start (datetime)                                                             ThreatIntelligenceIndicator
ThreatIntelligence  list_indicators_by_email          Retrieves list of indicators by email address                                                                end (datetime), observables (list), start (datetime)                                                             ThreatIntelligenceIndicator
ThreatIntelligence  list_indicators_by_filepath       Retrieves list of indicators by file path                                                                    end (datetime), observables (list), start (datetime)                                                             ThreatIntelligenceIndicator
ThreatIntelligence  list_indicators_by_hash           Retrieves list of indicators by file hash                                                                    end (datetime), file_hash_list (list), start (datetime)                                                          ThreatIntelligenceIndicator
ThreatIntelligence  list_indicators_by_ip             Retrieves list of indicators by IP Address                                                                   end (datetime), ip_address_list (list), start (datetime)                                                         ThreatIntelligenceIndicator
ThreatIntelligence  list_indicators_by_url            Retrieves list of indicators by URL                                                                          end (datetime), start (datetime), url_list (list)                                                                ThreatIntelligenceIndicator
WindowsSecurity     get_host_logon                    Retrieves the logon event for the session id on the host                                                     end (datetime), host_name (str), logon_session_id (str), start (datetime)                                        SecurityEvent
WindowsSecurity     get_parent_process                Retrieves the parent process of a supplied process                                                           end (datetime), host_name (str), logon_session_id (str), process_id (str), process_name (str), start (datetime)  SecurityEvent
WindowsSecurity     get_process_tree                  Retrieves the process tree of a supplied process                                                             end (datetime), host_name (str), logon_session_id (str), process_id (str), process_name (str), start (datetime)  SecurityEvent
WindowsSecurity     list_all_logons_by_host           account all failed or successful logons to a host                                                            end (datetime), host_name (str), start (datetime)                                                                SecurityEvent
WindowsSecurity     list_events                       Retrieves list of all events                                                                                 end (datetime), start (datetime)                                                                                 SecurityEvent
WindowsSecurity     list_events_by_id                 Retrieves list of events on a host                                                                           end (datetime), event_list (list), start (datetime)                                                              SecurityEvent
WindowsSecurity     list_host_events                  Retrieves list of all events on a host                                                                       end (datetime), host_name (str), start (datetime)                                                                SecurityEvent
WindowsSecurity     list_host_events_by_id            Retrieves list of events on a host                                                                           end (datetime), host_name (str), start (datetime)                                                                SecurityEvent
WindowsSecurity     list_host_logon_failures          Retrieves the logon failure events on the host                                                               end (datetime), host_name (str), start (datetime)                                                                SecurityEvent
WindowsSecurity     list_host_logons                  Retrieves the logon events on the host                                                                       end (datetime), host_name (str), start (datetime)                                                                SecurityEvent
WindowsSecurity     list_host_processes               Retrieves list of processes on a host                                                                        end (datetime), host_name (str), start (datetime)                                                                SecurityEvent
WindowsSecurity     list_hosts_matching_commandline   Retrieves processes on hosts with matching commandline                                                       commandline (str), end (datetime), process_name (str), start (datetime)                                          SecurityEvent
WindowsSecurity     list_logon_attempts_by_account    Retrieves the logon events for an account                                                                    account_name (str), end (datetime), start (datetime)                                                             SecurityEvent
WindowsSecurity     list_logon_failures_by_account    Retrieves the logon failure events  for an account                                                           account_name (str), end (datetime), start (datetime)                                                             SecurityEvent
WindowsSecurity     list_logons_by_account            Retrieves the logon events for an account                                                                    account_name (str), end (datetime), start (datetime)                                                             SecurityEvent
WindowsSecurity     list_matching_processes           Retrieves list of processes matching process name                                                            end (datetime), process_name (str), start (datetime)                                                             SecurityEvent
WindowsSecurity     list_other_events                 Retrieves list of events other than logon and process on a host                                              end (datetime), host_name (str), start (datetime)                                                                SecurityEvent
WindowsSecurity     list_processes_in_session         Retrieves all processes on the host for a logon session                                                      end (datetime), host_name (str), logon_session_id (str), process_id (str), process_name (str), start (datetime)  SecurityEvent
==================  ================================  ===========================================================================================================  ===============================================================================================================  ===========================



Queries for Microsoft 365 Defender
----------------------------------

Data Environment identifier: M365D

============  ==========================  ==================================================================================================================================  ==================================================================  ===================
QueryGroup    Query                       Description                                                                                                                         Req-Params                                                          Table
============  ==========================  ==================================================================================================================================  ==================================================================  ===================
MDATP         file_path                   Lists all file events from files in a certain path                                                                                  end (datetime), path (str), start (datetime)                        DeviceProcessEvents
MDATP         host_alerts                 Lists alerts by for a specified hostname                                                                                            end (datetime), host_name (str), start (datetime)                   DeviceAlertEvents
MDATP         host_connections            Lists alerts by for a specified hostname                                                                                            end (datetime), host_name (str), start (datetime)                   DeviceNetworkEvents
MDATP         ip_alerts                   Lists alerts associated with a specified remote IP                                                                                  end (datetime), ip_address (str), start (datetime)                  DeviceAlertEvents
MDATP         ip_connections              Lists alerts associated with a specified remote IP                                                                                  end (datetime), ip_address (str), start (datetime)                  DeviceNetworkEvents
MDATP         list_alerts                 Retrieves list of alerts                                                                                                            end (datetime), start (datetime)                                    DeviceAlertEvents
MDATP         list_connections            Retrieves list of network connections for a host                                                                                    end (datetime), start (datetime)                                    DeviceNetworkEvents
MDATP         list_filehash               Lists all file events by hash                                                                                                       end (datetime), file_hash (str), start (datetime)                   DeviceProcessEvents
MDATP         list_files                  Lists all file events by filename                                                                                                   end (datetime), file_name (str), start (datetime)                   DeviceProcessEvents
MDATP         list_host_processes         Lists all process creations for a host                                                                                              end (datetime), host_name (str), start (datetime)                   DeviceProcessEvents
MDATP         process_cmd_line            Lists all processes with a command line containing a string                                                                         cmd_line (str), end (datetime), start (datetime)                    DeviceProcessEvents
MDATP         process_creations           Lists all processes created by name or hash                                                                                         end (datetime), process_identifier (str), start (datetime)          DeviceProcessEvents
MDATP         process_paths               Lists all processes created from a path                                                                                             end (datetime), file_path (str), start (datetime)                   DeviceProcessEvents
MDATP         protocol_connections        Lists alerts associated with a specified protocol                                                                                   end (datetime), protocol (str), start (datetime)                    DeviceNetworkEvents
MDATP         sha1_alerts                 Lists alerts associated with a specified SHA1 hash                                                                                  end (datetime), file_hash (str), start (datetime)                   DeviceAlertEvents
MDATP         url_alerts                  Lists alerts associated with a specified URL                                                                                        end (datetime), start (datetime), url (str)                         DeviceAlertEvents
MDATP         url_connections             Lists alerts associated with a specified URL                                                                                        end (datetime), start (datetime), url (str)                         DeviceNetworkEvents
MDATP         user_files                  Lists all files created by a user                                                                                                   account_name (str), end (datetime), start (datetime)                -
MDATP         user_logons                 Lists all user logons by user                                                                                                       account_name (str), end (datetime), start (datetime)                -
MDATP         user_network                Lists all network connections associated with a user                                                                                account_name (str), end (datetime), start (datetime)                -
MDATP         user_processes              Lists all processes created by a user                                                                                               account_name (str), end (datetime), start (datetime)                -
MDATPHunting  accessibility_persistence   This query looks for persistence or privilege escalation done using Windows Accessibility features.                                 end (datetime), start (datetime)                                    -
MDATPHunting  av_sites                    Pivot from downloads detected by Windows Defender Antivirus to other files downloaded from the same sites                           end (datetime), start (datetime)                                    -
MDATPHunting  b64_pe                      Finding base64 encoded PE files header seen in the command line parameters                                                          end (datetime), start (datetime)                                    -
MDATPHunting  brute_force                 Look for public IP addresses that failed to logon to a computer multiple times, using multiple accounts, and eventually succeeded.  end (datetime), start (datetime)                                    -
MDATPHunting  cve_2018_1000006l           Looks for CVE-2018-1000006 exploitation                                                                                             end (datetime), start (datetime)                                    -
MDATPHunting  cve_2018_1111               Looks for CVE-2018-1111 exploitation                                                                                                end (datetime), start (datetime)                                    -
MDATPHunting  cve_2018_4878               This query checks for specific processes and domain TLD used in the CVE-2018-4878                                                   end (datetime), start (datetime)                                    -
MDATPHunting  doc_with_link               Looks for a Word document attachment, from which a link was clicked, and after which there was a browser download.                  end (datetime), start (datetime)                                    -
MDATPHunting  dropbox_link                Looks for user content downloads from dropbox that originate from a link/redirect from a 3rd party site.                            end (datetime), start (datetime)                                    -
MDATPHunting  email_link                  Look for links opened from mail apps – if a detection occurred right afterwards                                                     end (datetime), start (datetime)                                    -
MDATPHunting  email_smartscreen           Look for links opened from outlook.exe, followed by a browser download and then a SmartScreen app warning                           end (datetime), start (datetime)                                    -
MDATPHunting  malware_recycle             Finding attackers hiding malware in the recycle bin.                                                                                end (datetime), start (datetime)                                    -
MDATPHunting  network_scans               Looking for high volume queries against a given RemoteIP, per ComputerName, RemotePort and Process                                  end (datetime), start (datetime)                                    -
MDATPHunting  powershell_downloads        Finds PowerShell execution events that could involve a download.                                                                    end (datetime), start (datetime)                                    -
MDATPHunting  service_account_powershell  Service Accounts Performing Remote PowerShell                                                                                       end (datetime), start (datetime)                                    -
MDATPHunting  smartscreen_ignored         Query for SmartScreen URL blocks, where the user has decided to run the malware nontheless.                                         end (datetime), start (datetime)                                    -
MDATPHunting  smb_discovery               Query for processes that accessed more than 10 IP addresses over port 445 (SMB) - possibly scanning for network shares.             end (datetime), start (datetime)                                    -
MDATPHunting  tor                         Looks for Tor client, or for a common Tor plugin called Meek.                                                                       end (datetime), start (datetime)                                    -
MDATPHunting  uncommon_powershell         Find which uncommon Powershell Cmdlets were executed on that machine in a certain time period.                                      end (datetime), host_name (str), start (datetime), timestamp (str)  -
MDATPHunting  user_enumeration            The query finds attempts to list users or groups using Net commands                                                                 end (datetime), start (datetime)                                    -
============  ==========================  ==================================================================================================================================  ==================================================================  ===================



Queries for Microsoft Graph
---------------------------

Data Environment identifier: SecurityGraph

==================  ====================  ====================================================  ==================================================  =======
QueryGroup          Query                 Description                                           Req-Params                                          Table
==================  ====================  ====================================================  ==================================================  =======
SecurityGraphAlert  get_alert             Retrieves a single alert by AlertId                   alert_id (str)                                      -
SecurityGraphAlert  list_alerts           Retrieves list of alerts                              end (datetime), start (datetime)                    -
SecurityGraphAlert  list_alerts_for_file  Retrieves list of alerts for file name, path or hash  end (datetime), start (datetime)                    -
SecurityGraphAlert  list_alerts_for_host  Retrieves list of alerts for a hostname or FQDN       end (datetime), host_name (str), start (datetime)   -
SecurityGraphAlert  list_alerts_for_ip    Retrieves list of alerts for a IP Address             end (datetime), ip_address (str), start (datetime)  -
SecurityGraphAlert  list_alerts_for_user  Retrieves list of alerts for a user account           end (datetime), start (datetime)                    -
SecurityGraphAlert  list_related_alerts   Retrieves list of alerts with a common entity         end (datetime), start (datetime)                    -
==================  ====================  ====================================================  ==================================================  =======



Queries for Splunk
------------------

Data Environment identifier: Splunk

==============  =========================  =============================================================  ====================================================  =======
QueryGroup      Query                      Description                                                    Req-Params                                            Table
==============  =========================  =============================================================  ====================================================  =======
Alerts          list_alerts                Retrieves list of alerts                                       end (datetime), start (datetime)                      -
Alerts          list_alerts_for_dest_ip    Retrieves list of alerts with a common destination IP Address  end (datetime), ip_address (str), start (datetime)    -
Alerts          list_alerts_for_src_ip     Retrieves list of alerts with a common source IP Address       end (datetime), ip_address (str), start (datetime)    -
Alerts          list_alerts_for_user       Retrieves list of alerts with a common username                end (datetime), start (datetime), user (str)          -
Alerts          list_all_alerts            Retrieves all configured alerts                                end (datetime), start (datetime)                      -
Authentication  list_logon_failures        All failed user logon events on any host                       end (datetime), start (datetime)                      -
Authentication  list_logons_for_account    All successful user logon events for account (all hosts)       account_name (str), end (datetime), start (datetime)  -
Authentication  list_logons_for_host       All logon events on a host                                     end (datetime), host_name (str), start (datetime)     -
Authentication  list_logons_for_source_ip  All successful user logon events for source IP (all hosts)     end (datetime), ip_address (str), start (datetime)    -
SplunkGeneral   get_events_parameterized   Generic parameterized query from index/source                  end (datetime), start (datetime)                      -
SplunkGeneral   list_all_datatypes         Summary of all events by index and sourcetype                  end (datetime), start (datetime)                      -
SplunkGeneral   list_all_savedsearches     Retrieves all saved searches                                   end (datetime), start (datetime)                      -
audittrail      list_all_audittrail        Retrieves all audit trail logs                                 end (datetime), start (datetime)                      -
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
SumologicGeneral  list_all_datatypes  Summary of all events by sourceCategory  end (datetime), start (datetime)  -
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


