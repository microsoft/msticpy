# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
Contains a series of functions required to correct collect, parse and visualise linux syslog data. 
Specifically designed to support standard linux syslog for investigations where auditd is not avalaible.

It has the following functions:
syslog_host_picker - returns a list of hosts with syslog messages within the time period set
get_syslog_host_data - collects data on the host being investigated and returns it as a Host record
get_sucessful_logons_syslog - collects sucessful logon events, clusters them into sessions and returns the raw events and list of sessions
get_failed_logons_syslog - collects and returns all syslog messages associated with failed logon attempts
get_sudo_activity_syslog - collects data on all sudo activity on the host being investigated including sucessful and unsucessful sudo attempts, execution activity, and counts of commands executed via sudo
get_cron_activity_syslog - collects and returns data on cron execution and cron edit events
get_user_mods_syslog - collects and returns data on all user and group additions, deletions, and modifications
get_syslog_events - collects and retunrs all syslog messages for the host being investigated along with a list of the facilities present
syslog_volume_graph - displays a graph of syslog message volume over time
"""
from datetime import datetime
import pandas as pd
import numpy as np
from datetime import datetime
from ..nbtools.entityschema import GeoLocation, Host, IpAddress
from .geoip import GeoLiteLookup
from .eventcluster import dbcluster_events, add_process_features, _string_score
from IPython import get_ipython
from IPython.display import display, HTML, Markdown
import matplotlib.pyplot as plt
import collections
import ipywidgets as widgets
from typing import Mapping, Any, Tuple, Dict, List, Optional, Set
from pandas.plotting import register_matplotlib_converters


from .._version import VERSION
from ..nbtools.utility import export

__version__ = VERSION
__author__ = "Pete Bryan"

WIDGET_DEFAULTS = {
    "layout": widgets.Layout(width="95%"),
    "style": {"description_width": "initial"},
}
iplocation = GeoLiteLookup()
_ip = get_ipython()
register_matplotlib_converters()


class Error(Exception):
    """Base class for other exceptions"""

    pass


class KQLError(Error):
    """Raised whent there is an error related to KQL"""

    pass


class KQLDataError(KQLError):
    """Raised when there is an error related to the data returned by KQL"""

    pass


def convert_to_ip_entities(ip_str: str) -> Tuple[IpAddress]:
    """
    Takes in an IP Address string and converts it to an IP Entitity
    
    Parameters
    ----------
    ip_str : str
        The string containing the IP Address
    
    Returns
    ----------
    Tuple
        The populated IP entities including address and geo-location
    """
    ip_entities = []
    if ip_str:
        if "," in ip_str:
            addrs = ip_str.split(",")
        elif " " in ip_str:
            addrs = ip_str.split(" ")
        else:
            addrs = [ip_str]

        for addr in addrs:
            ip_entity = IpAddress()
            ip_entity.Address = addr.strip()
            iplocation.lookup_ip(ip_entity=ip_entity)
            ip_entities.append(ip_entity)
    return ip_entities


@export
def syslog_host_picker(time: int = 90) -> List[str]:
    """
    Returns a list of Computer names that have generated Syslog messages within the last 90 days

    Parameters
    ----------
    time: int
        The number of preceding days to check for hosts with syslog data in, default is 90

    Returns
    ----------
    items: List
        A list of Computer names

    Raises
    ----------
    KQLDataError
        There are no hosts which have generated syslog messages within the last the last 90 days.

    """
    syslog_hosts_query = f""" Syslog 
    | where TimeGenerated > ago({time}d) 
    | summarize SyslogCount=count(SyslogMessage) by Computer"""
    kql_raw_results = _ip.magic("kql -query syslog_hosts_query")
    if kql_raw_results.completion_query_info["StatusCode"] == 0:
        syslog_hosts = kql_raw_results.to_dataframe()

    if syslog_hosts.empty:
        raise KQLDataError(
            "There are no hosts which have generated syslog messages within the last the last 90 days."
        )

    else:
        display(
            Markdown(f"### Hosts With Syslog Generated Within the Previous {time} Days")
        )
        items = syslog_hosts["Computer"].values.tolist()

    return items


@export
def get_syslog_host_data(hostname: str, time: int, table_index: pd.DataFrame) -> Host:
    """
    Generate host_entity record for selected computer

    Parameters
    ----------
    hostname : str
        The Computer name to create the host_entity for
    time : int
        Number of days previous from which to collect host data from. Default is 90
    table_index : pd.DataFrame
        A Panda DataFrame containing tables avaliable for querying of data regarding the computer

    Returns
    ----------
    Host
        Details of the host data collected

    Raises
    ----------
    KQLDataError
        Could not find any data for the computer in the time window

    """
    host_syslog_check = f"""
    Syslog
    | where TimeGenerated > ago({time}d)
    | where Computer has '{hostname}'
    | top 1 by TimeGenerated desc nulls last
    """

    kql_raw_result = _ip.magic("kql -query host_syslog_check")
    if kql_raw_result.completion_query_info["StatusCode"] == 0:
        host_syslog_df = kql_raw_result.to_dataframe()

    if host_syslog_df.empty:
        raise KQLDataError(
            f"Could not find any Syslog events for {hostname} in the last {time} days"
        )
    else:
        host_entity = Host(src_event=host_syslog_df.iloc[0])

    if "Heartbeat" in table_index:
        heartbeat_query = f"""
            Heartbeat 
            | where Computer == '{hostname}' 
            | where TimeGenerated > ago({time}d)
            | top 1 by TimeGenerated desc nulls last
            """

        print("Getting host data...")
        kql_raw_result = _ip.magic("kql -query heartbeat_query")
        if kql_raw_result.completion_query_info["StatusCode"] == 0:
            host_hb = kql_raw_result.to_dataframe()

        if host_hb.empty:
            raise LookupError(
                f"Could find any heartbeat data for {hostname} in the last {time} days"
            )
        else:
            host_hb = host_hb.iloc[0]
            host_entity.SourceComputerId = host_hb["SourceComputerId"]
            host_entity.OSType = host_hb["OSType"]
            host_entity.OSName = host_hb["OSName"]
            host_entity.OSVMajorersion = host_hb["OSMajorVersion"]
            host_entity.OSVMinorVersion = host_hb["OSMinorVersion"]
            host_entity.ComputerEnvironment = host_hb["ComputerEnvironment"]
            host_entity.OmsSolutions = [
                sol.strip() for sol in host_hb["Solutions"].split(",")
            ]
            host_entity.VMUUID = host_hb["VMUUID"]
            ip_entity = IpAddress()
            ip_entity.Address = host_hb["ComputerIP"]
            geoloc_entity = GeoLocation()
            geoloc_entity.CountryName = host_hb["RemoteIPCountry"]
            geoloc_entity.Longitude = host_hb["RemoteIPLongitude"]
            geoloc_entity.Latitude = host_hb["RemoteIPLatitude"]
            ip_entity.Location = geoloc_entity
            host_entity.IPAddress = ip_entity

    if "AzureNetworkAnalytics_CL" in table_index:
        print("Looking for IP addresses in network flows...")
        aznet_query = f"""
            AzureNetworkAnalytics_CL
            | where TimeGenerated > ago({time}d)
            | where VirtualMachine_s has '{hostname}'
            | where ResourceType == 'NetworkInterface'
            | top 1 by TimeGenerated desc
            | project PrivateIPAddresses = PrivateIPAddresses_s, 
            PublicIPAddresses = PublicIPAddresses_s
            """
        kql_raw_result = _ip.magic("kql -query aznet_query")
        if kql_raw_result.completion_query_info["StatusCode"] == 0:
            az_net_df = kql_raw_result.to_dataframe()

        if len(az_net_df) == 1:
            priv_addr_str = az_net_df["PrivateIPAddresses"].loc[0]
            host_entity.properties["private_ips"] = convert_to_ip_entities(
                priv_addr_str
            )
            pub_addr_str = az_net_df["PublicIPAddresses"].loc[0]
            host_entity.properties["public_ips"] = convert_to_ip_entities(pub_addr_str)

        else:
            if "private_ips" not in host_entity.properties:
                host_entity.properties["private_ips"] = []
            if "public_ips" not in host_entity.properties:
                host_entity.properties["public_ips"] = []

    display(
        Markdown(
            "***Host Details***\n\n"
            f"**Hostname**: {host_entity.computer} \n\n"
            f"**OS**: {host_entity.OSType} {host_entity.OSName}\n\n"
            f"**IP Address**: {ip_entity.Address}\n\n"
            f"**Location**: {geoloc_entity.CountryName}\n\n"
        )
    )
    return host_entity


@export
def get_sucessful_logons_syslog(
    hostname: str, start: datetime, end: datetime
) -> Tuple[pd.DataFrame, List]:
    """
    Collects and returns data on all the sucessful logons to the computer defined the the time window specified

    Parameters
    ----------
    hostname : str
        The Computer name of the computer being investigated
    start : datetime
        The start of the time to query data regarding the computer for
    end : datetime
        The end of the time to query data regarding the computer for

    Returns
    ----------
    sucess_logons: namedTuple
        Contains:
                host_logons - Panda DataFrame containing key information regarding observed logons
                logon_items - List of individual logon sessions
     Raises
    ----------
    KQLDataError
        No Logon Events Found for Host
    """
    syslog_logon_query = f"""Syslog
    | where TimeGenerated >= datetime({start})
    | where TimeGenerated <= datetime({end})
    | extend StartTimeUtc = TimeGenerated
    | where Facility == "auth" or Facility == "authpriv"
    | where Computer == "{hostname}"
    | where SyslogMessage contains "Accepted" or (ProcessName == "su" and SyslogMessage contains "Successful")
    | extend User = extract("for ([[:alnum:]]+)",1,SyslogMessage), SourceIP = extract("from (([0-9]|[0-9][0-9]|[0-9][0-9][0-9])\\\.([0-9]|[0-9][0-9]|[0-9][0-9][0-9])\\\.([0-9]|[0-9][0-9]|[0-9][0-9][0-9])\\\.([0-9]|[0-9][0-9]|[0-9][0-9][0-9]))",1,SyslogMessage), SourcePort = extract("port ([0-9]+)",1,SyslogMessage),UID = extract("uid=([0-9]+)",1,SyslogMessage),SourceUser = extract("by ([[:alnum:]]+)$",1,SyslogMessage)
    """

    kql_raw_result = _ip.magic("kql -query syslog_logon_query")

    if kql_raw_result.completion_query_info["StatusCode"] == 0:
        host_logons = kql_raw_result.to_dataframe()

    if host_logons is not None and not host_logons.empty:
        logon_features = host_logons.copy()
        logon_features["AccountNum"] = host_logons.apply(
            lambda x: _string_score(x.User), axis=1
        )
        logon_features["TargetUserSid"] = host_logons.UID
        logon_features["SubjectUserSid"] = host_logons.UID
        logon_features["audit_user"] = host_logons.User
        logon_features["LogonType"] = host_logons.apply(
            lambda x: _string_score(x.ProcessName), axis=1
        )
        logon_features["LogonHour"] = host_logons.apply(
            lambda x: x.TimeGenerated.hour, axis=1
        )
        logon_features["TargetUserName"] = host_logons.User
        logon_features["SubjectUserName"] = host_logons.User
        logon_features["TargetDomainName"] = host_logons.Computer
        logon_features["SubjectDomainName"] = host_logons.Computer
        logon_features["TargetLogonId"] = 0
        logon_features["LogonProcessName"] = host_logons.ProcessName
        logon_features["UserNum"] = host_logons.apply(
            lambda x: _string_score(x.User), axis=1
        )
        logon_features["ProcessNum"] = host_logons.apply(
            lambda x: _string_score(x.ProcessName), axis=1
        )

        (clus_logons, _, _) = dbcluster_events(
            data=logon_features,
            time_column="TimeGenerated",
            cluster_columns=["UserNum", "ProcessNum"],
            max_cluster_distance=0.0001,
        )

        dist_logons = clus_logons.sort_values("TimeGenerated")[
            ["User", "TimeGenerated", "LastEventTime", "ProcessName", "ClusterSize"]
        ]
        logon_items = dist_logons.apply(
            lambda x: (
                f"{x.User}:    "
                f"(logontype={x.ProcessName})   "
                f"timerange={x.TimeGenerated} - {x.LastEventTime}    "
                f"count={x.ClusterSize}"
            ),
            axis=1,
        ).values.tolist()
        sucess_logons = collections.namedtuple(
            "sucess_logons", ["host_logons", "logon_items"]
        )
        return sucess_logons(host_logons, logon_items)

    else:
        raise KQLDataError("No Logon Events Found for Host")


@export
def cluster_syslog_logons(logon_events: pd.DataFrame) -> dict:
    """
    Clusters logon sessions observed in syslog by start and end time based on PAM events.
    Will return a LogonDataError if supplied dataframe does not contain complete logon sessions.

    Parameters:
    ----------
    logon_events: pd.DataFrame
        A DataFrame of all syslog logon events (can be generated with LinuxSyslog.user_logon query)
    
    Returns
    ----------
    logon_sessions: namedTuple
        A dictionary of logon sessions including start and end times and logged on user

     Raises
    ----------
    KQLDataError
        There are no logon sessions in the supplied data set
    """

    logon_sessions = []
    ses_close_time = datetime.now()
    ses_opened = 0
    ses_closed = 0
    logons_opened = (
        (
            logon_events[
                logon_events["SyslogMessage"].str.contains("pam_unix.+session opened")
            ]
        )
        .set_index("TimeGenerated")
        .sort_index(ascending=True)
    )
    logons_closed = (
        (
            logon_events[
                logon_events["SyslogMessage"].str.contains("pam_unix.+session closed")
            ]
        )
        .set_index("TimeGenerated")
        .sort_index(ascending=True)
    )
    if len(logons_opened.index) == 0 or len(logons_closed.index) == 0:
        raise KQLDataError("There are no logon sessions in the supplied data set")
    else:
        while ses_opened < len(logons_opened.index) and ses_closed < len(
            logons_closed.index
        ):
            ses_start = (logons_opened.iloc[ses_opened]).name
            user = (logons_opened.iloc[ses_opened]).User
            if ses_start.to_pydatetime() > ses_close_time or ses_opened == 0:
                pass
            else:
                ses_opened += 1
                continue
            ses_end = (logons_closed.iloc[ses_closed]).name
            if ses_end.to_pydatetime() < ses_start.to_pydatetime():
                ses_closed += 1
                continue
            logon_sessions.append({"start": ses_start, "end": ses_end, "user": user})
            ses_close_time = ses_end
            ses_closed = ses_closed + 1
            ses_opened = ses_opened + 1
        return logon_sessions


@export
def get_failed_logons_syslog(
    hostname: str, start: datetime, end: datetime
) -> Tuple[pd.DataFrame]:
    """
    Collects and returns data on unsucessful logon attempts to the computer defined the the time window specified
    
    Parameters
    ----------
    hostname : str
        The Computer name of the computer being investigated
    start : datetime
        The start of the time to query data regarding the computer for
    end : datetime
        The end of the time to query data regarding the computer for

    Returns
    ----------
    fail_logons: namedTuple
        Contains:
            failed_logons - Panda DataFrame of unsucessful logon attempts

     Raises
    ----------
    KQLDataError
        There are no logon failures

    """
    failed_Logons_query = f"""Syslog
    | where TimeGenerated >= datetime({start})
    | where TimeGenerated <= datetime({end})
    | extend StartTimeUtc = TimeGenerated
    | where Computer == "{hostname}"
    | where (Facility == "auth" and (SyslogMessage contains "failure" or SyslogMessage contains "invalid" or SyslogMessage contains "Uanble to negotiate" or SyslogMessage contains "Did not receive identification" or SyslogMessage contains " Bad protocol version identification" or SyslogMessage matches regex "^Connection closed .* [preauth]")) or (Facility == "authpriv" and ProcessName == "su" and SyslogMessage contains "FAILED" and SyslogMessage !contains "pam_") 
    | where ProcessName != "sudo"
    | extend User = extract("for ([[:alnum:]]+)",1,SyslogMessage), SourceIP = extract("(([0-9]|[0-9][0-9]|[0-9][0-9][0-9])\\\.([0-9]|[0-9][0-9]|[0-9][0-9][0-9])\\\.([0-9]|[0-9][0-9]|[0-9][0-9][0-9])\\\.([0-9]|[0-9][0-9]|[0-9][0-9][0-9]))",1,SyslogMessage), SourcePort = extract("port ([0-9]+)",1,SyslogMessage), SourceUser=extract("by ([[:alnum:]]+)$",1,SyslogMessage)
    """

    kql_raw_result = _ip.magic("kql -query failed_Logons_query")

    if kql_raw_result.completion_query_info["StatusCode"] == 0:
        failed_logons = kql_raw_result.to_dataframe()

    failed_logons.loc[failed_logons["User"] == "", "User"] = "Unknown"

    if failed_logons.empty:
        raise KQLDataError(
            f"No logon failures recorded for {hostname} between {start} and {end}"
        )
    else:
        fail_logons = collections.namedtuple("fail_logons", ["failed_logons"])
        return fail_logons(failed_logons)


@export
def get_sudo_activity_syslog(
    hostname: str, start: datetime, end: datetime
) -> Tuple[pd.DataFrame, pd.DataFrame, int, int, pd.DataFrame]:
    """
    Collects and returns sudo activity fromt the computer defined, in the timewindow specified
    Parameters
    ----------
    hostname : str
        The Computer name of the computer being investigated
    start : datetime
        The start of the time to query data regarding the computer for
    end : datetime
        The end of the time to query data regarding the computer for

    Returns
    ----------
    sudo_act: namedTuple
        contains:   
            sudo_activity_events: Panda DataFrame containing all syslog events relating to sucessful sudo activity
            failed_sudo_activity: Panda DataFrame containing all syslog events relating to unsucessful sudo activity
            sudo_activity_count_unique: int of the number of unique commands executed with sudo
            sudo_activity_count_total: int of the total number of commands exectured with sudo
            sudo_session_data: Panda DataFrame of all syslog events relating to opening or closing sudo sessions
    
    Raises
    ----------
    KQLDataError
        No sudo activity found

    """
    sudo_activity_query = f"""Syslog
    | where TimeGenerated >= datetime({start})
    | where TimeGenerated <= datetime({end})
    | extend StartTimeUtc = TimeGenerated
    | where Computer == "{hostname}"
    | where ProcessName == "sudo"
    | extend Sudoer = extract("by ([A-Z,a-z,0-9]+)",1,SyslogMessage), SudoTo = extract("for user ([A-Z,a-z,0-9]+)",1,SyslogMessage),Command=extract("COMMAND=(.*)$",1,SyslogMessage), CommandCall=extract("COMMAND=([[:graph:]]*)",1,SyslogMessage)
    """

    failed_sudo_query = f"""Syslog
    | where TimeGenerated >= datetime({start})
    | where TimeGenerated <= datetime({end})
    | extend StartTimeUtc = TimeGenerated
    | where Computer == "{hostname}"
    | where ProcessName == "sudo" and SyslogMessage contains "authentication failure"
    | extend Sudoer = extract("user=([[:alnum:]]+)",1,SyslogMessage)
    """

    kql_raw_result = _ip.magic("kql -query sudo_activity_query")

    if kql_raw_result.completion_query_info["StatusCode"] == 0:
        sudo_activity_events = kql_raw_result.to_dataframe()

    sudo_activity_events.loc[
        sudo_activity_events["CommandCall"] == "", "CommandCall"
    ] = np.nan
    sudo_activity_events.loc[sudo_activity_events["Command"] == "", "Command"] = np.nan
    sudo_session_data = sudo_activity_events[
        sudo_activity_events["SyslogMessage"].str.contains("session")
    ]
    sudo_activity_count_unique = len(sudo_activity_events["Command"].unique())
    sudo_activity_count_total = len(sudo_activity_events["Command"])

    kql_raw_result = _ip.magic("kql -query failed_sudo_query ")

    if kql_raw_result.completion_query_info["StatusCode"] == 0:
        failed_sudo_activity = kql_raw_result.to_dataframe()

    if sudo_activity_events.empty and failed_sudo_activity.empty:
        raise KQLDataError(f"No sudo activity for {hostname} between {start} and {end}")
    else:
        sudo_act = collections.namedtuple(
            "sudo_act",
            [
                "sudo_activity_events",
                "failed_sudo_activity",
                "sudo_activity_count_unique",
                "sudo_activity_count_total",
                "sudo_session_data",
            ],
        )
        return sudo_act(
            sudo_activity_events,
            failed_sudo_activity,
            sudo_activity_count_unique,
            sudo_activity_count_total,
            sudo_session_data,
        )


@export
def get_cron_activity_syslog(
    hostname: str, start: datetime, end: datetime
) -> Tuple[pd.DataFrame, pd.DataFrame, pd.DataFrame]:
    """
    Collects and returns data on syslog messages related to cron activity on the computer defined in the time window specified
    Parameters
    ----------
    hostname : str
        The Computer name of the computer being investigated
    start : datetime
        The start of the time to query data regarding the computer for
    end : datetime
        The end of the time to query data regarding the computer for

    Returns
    ----------
    cron_act: namedTuple
        Contains:
            all_cron_events - Panda DataFrame of all syslog messages related to cron activity
            cron_edits - Panda DataFrame of all syslog messages realted to cron file edits
            cron_executions - Panda DataFrame of all syslog messages related to cron executions
       
    Raises
    ----------
    KQLDataError
        No cron activity found
    """
    cron_activity_query = f"""Syslog
    | where TimeGenerated >= datetime({start})
    | where TimeGenerated <= datetime({end})
    | extend StartTimeUtc = TimeGenerated
    | where Computer == "{hostname}"
    | where ProcessName == "CRON" or Facility == "cron"
    | extend CMD=extract("CMD(.*)",1,SyslogMessage), User=extract("for user ([[:alpha:]]*)",1,SyslogMessage), CronUser=extract("^[(]([[:alpha:]]*)",1,SyslogMessage),EditStatus=extract("[A-Z]+ EDIT",0,SyslogMessage)
    """

    kql_raw_result = _ip.magic("kql -query cron_activity_query")

    if kql_raw_result.completion_query_info["StatusCode"] == 0:
        all_cron_events = kql_raw_result.to_dataframe()
    if all_cron_events.empty:
        raise KQLDataError(f"No cron activity for {hostname} between {start} and {end}")
    else:
        all_cron_events.loc[all_cron_events["User"] == "", "User"] = np.nan
        all_cron_events.loc[all_cron_events["CronUser"] == "", "CronUser"] = np.nan
        all_cron_events.loc[all_cron_events["CMD"] == "", "CMD"] = np.nan
        cron_edits = all_cron_events[
            all_cron_events["SyslogMessage"].str.contains("EDIT")
        ]
        cron_executions = all_cron_events[["TimeGenerated", "CMD", "CronUser"]].dropna()
        cron_act = collections.namedtuple(
            "cron_act", ["all_cron_events", "cron_edits", "cron_executions"]
        )
        return cron_act(all_cron_events, cron_edits, cron_executions)


@export
def get_user_mods_syslog(
    hostname: str, start: datetime, end: datetime
) -> Tuple[pd.DataFrame, pd.DataFrame, pd.DataFrame, List, List, List]:
    """
    Collects and returns data on all user and group creations, deletions and modifications on the computer defined, in the time window specified

    Parameters
    ----------
    hostname : str
        The Computer name of the computer being investigated
    start : datetime
        The start of the time to query data regarding the computer for
    end : datetime
        The end of the time to query data regarding the computer for

    Returns
    ----------
    user_mods: namedTuple
        Contains:
         add_events - Panda DataFrame of all syslog messages related to user or group additions
         del_events - Panda DataFrame of all syslog messages related to user or group deletions
         mod_events - Panda DataFrame of all syslog messages related to user or group modifications
         add_users - List of user or group names added
         del_users - List of user or group names deleted
         mod_users - List of user or group names modified   

    Raises
    ----------
    KQLDataError
        No user or group modifications found
    """
    add_query = f"""Syslog
    | where TimeGenerated >= datetime({start})
    | where TimeGenerated <= datetime({end})
    | extend StartTimeUtc = TimeGenerated
    | where Computer == "{hostname}"
    | where Facility == "authpriv"
    | where SyslogMessage contains "new"
    | extend User=extract("user: name=([[:alnum:]]+)",1,SyslogMessage), Group=extract("group: name=([[:alnum:]]+)",1,SyslogMessage), UID=extract("UID=([0-9]+)",1,SyslogMessage), GID=extract("GID=([0-9]+)",1,SyslogMessage)
    """

    del_query = f"""Syslog
    | where TimeGenerated >= datetime({start})
    | where TimeGenerated <= datetime({end})
    | extend StartTimeUtc = TimeGenerated
    | where Computer == "{hostname}"
    | where Facility == "authpriv"
    | where (SyslogMessage contains "delete" or SyslogMessage contains "removed") and ProcessName == "userdel"
    | extend User=extract("user '([[:alnum:]]+)",1,SyslogMessage),Group=extract("group '([[:alnum:]]+)",1,SyslogMessage)
    """

    mod_query = f"""Syslog
    | where TimeGenerated >= datetime({start})
    | where TimeGenerated <= datetime({end})
    | extend StartTimeUtc = TimeGenerated
    | where Computer == "{hostname}"
    | where Facility == "authpriv"
    | where ProcessName == "usermod"
    | extend User=extract("user '([[:alnum:]]+)",1,SyslogMessage)
    """

    kql_raw_result = _ip.magic("kql -query add_query")

    if kql_raw_result.completion_query_info["StatusCode"] == 0:
        add_events = kql_raw_result.to_dataframe()

    kql_raw_result = _ip.magic("kql -query del_query")

    if kql_raw_result.completion_query_info["StatusCode"] == 0:
        del_events = kql_raw_result.to_dataframe()

    kql_raw_result = _ip.magic("kql -query mod_query")

    if kql_raw_result.completion_query_info["StatusCode"] == 0:
        mod_events = kql_raw_result.to_dataframe()

    if add_events.empty and del_events.empty and mod_events.empty:
        raise KQLDataError(f"No user or group modications on {hostname}")
    else:
        if add_events.empty:
            add_users = []
            pass
        else:
            add_events.loc[add_events["User"] == "", "User"] = np.nan
            add_users = add_events["User"].dropna().unique()
            add_users = ", ".join(add_users)
        if del_events.empty:
            del_users = []
            pass

        else:
            del_events.loc[del_events["User"] == "", "User"] = np.nan
            del_users = del_events["User"].dropna().unique()
            del_users = ", ".join(del_users)
        if mod_events.empty:
            mod_users = []
            pass
        else:
            mod_events.loc[mod_events["User"] == "", "User"] = np.nan
            mod_users = del_events["User"].dropna().unique()
            mod_users = ", ".join(mod_users)
        user_mods = collections.namedtuple(
            "user_mods",
            [
                "add_events",
                "del_events",
                "mod_events",
                "add_users",
                "del_users",
                "mod_users",
            ],
        )
        return user_mods(
            add_events, del_events, mod_events, add_users, del_users, mod_users
        )


@export
def get_syslog_events(
    hostname: str, start: datetime, end: datetime
) -> Tuple[pd.DataFrame, pd.DataFrame, List]:
    """
    Collects and returns all syslog events, a count of the number of events returned and the syslog facilities observed

    Parameters
    ----------
    hostname : str
        The Computer name of the computer being investigated
    start : datetime
        The start of the time to query data regarding the computer for
    end : datetime
        The end of the time to query data regarding the computer for

    Returns
    ----------
    syslog_data: namedTuple
        Contains:
           all_syslog_events - Panda DataFrame containing all the syslog messages associated with the computer in the defined time window
           syslog_volume - Panda DataFrame containing details of syslog volumes over times
           syslog_facilities - List of syslog facilities present in the all_syslog_events dataset

    Raises
    ----------
    KQLDataError
        No syslog data found
    """
    all_syslog_query = f'''Syslog
    | where TimeGenerated >= datetime({start})
    | where TimeGenerated <= datetime({end})
    | extend StartTimeUtc = TimeGenerated
    | where Computer == "{hostname}"'''

    syslog_volume_query = f"""Syslog
    | where TimeGenerated >= datetime({start})
    | where TimeGenerated <= datetime({end})
    | extend StartTimeUtc = TimeGenerated
    | where Computer == "{hostname}"
    | summarize LogCount=count(SyslogMessage) by bin(TimeGenerated, 1h)"""

    kql_raw_result = _ip.magic("kql -query all_syslog_query")

    if kql_raw_result.completion_query_info["StatusCode"] == 0:
        all_syslog_events = kql_raw_result.to_dataframe()

    if all_syslog_events.empty:
        raise KQLDataError("No syslog data found")
    else:
        kql_raw_result = _ip.magic("kql -query syslog_volume_query")

        if kql_raw_result.completion_query_info["StatusCode"] == 0:
            syslog_volume = kql_raw_result.to_dataframe()

        syslog_facilities = all_syslog_events["Facility"].unique()
        syslog_data = collections.namedtuple(
            "syslog_data", ["all_syslog_events", "syslog_volume", "syslog_facilities"]
        )
        return syslog_data(all_syslog_events, syslog_volume, syslog_facilities)


@export
def syslog_volume_graph(syslog_volume: pd.DataFrame):
    """
    Plots a vertical bar graph showing syslog data volumes over time
    
    Parameters
    ----------
        syslog_volume: pd.DataFrame
            A Panda DataFrame containing a count of total Syslog Messages in 1 hour bins

    """
    time = pd.to_datetime(syslog_volume["TimeGenerated"]).dt.to_pydatetime()
    data = syslog_volume["LogCount"]
    display(Markdown("### Volume of Syslog Messages Generated Per Hour"))
    plt.rcParams["figure.figsize"] = (17, 4)
    ax = plt.subplot(111)
    ax.bar(time, data, width=0.025)
    ax.xaxis_date()
    ax.set_xlabel("Time Generated")
    ax.set_ylabel("Volume of Syslog Messages Generated")
    plt.show()
