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
import collections
import datetime as dt
import os
import re
from datetime import datetime
from typing import Any, Dict, List, Mapping, Optional, Set, Tuple

import ipywidgets as widgets
import matplotlib.pyplot as plt

import numpy as np
import pandas as pd
from IPython import get_ipython
from IPython.display import HTML, Markdown, display
from pandas.plotting import register_matplotlib_converters

from .._version import VERSION
from ..data.data_providers import QueryProvider
from ..nbtools.entityschema import GeoLocation, Host, IpAddress
from ..nbtools.utility import export
from .eventcluster import _string_score, add_process_features, dbcluster_events
from .geoip import GeoLiteLookup
from .base64unpack import unpack

__version__ = VERSION
__author__ = "Pete Bryan"

_DETECTIONS_DEF_DIR = "detections"

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
def create_host_record(syslog_df: pd.DataFrame, heartbeat_df: pd.DataFrame, az_net_df: pd.DataFrame = None) -> Host:
    # remove queries from function and push to notebook.
    """
    Generate host_entity record for selected computer

    Parameters
    ----------
    syslog_df : pd.DataFrame
        A dataframe of all syslog events for the host in the time window requried
    heartbeat_df : pd.DataFrame
        A dataframe of heartbeat data for the host
    az_net_df : pd.DataFrame
        Option dataframe of Azure network data for the host

    Returns
    ----------
    Host
        Details of the host data collected

    Raises
    ----------
    KQLDataError
        Could not find any data for the computer in the time window

    """
    
    if syslog_df.empty:
        raise KQLDataError(
            f"No syslog data provided"
        )
    else:
        host_entity = Host(src_event=syslog_df.iloc[0])

    if heartbeat_df.empty:
        raise KQLDataError(
            f"No heartbeat data provided"
        )
    else:
        host_hb = heartbeat_df.iloc[0]
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

    if az_net_df.empty:
        pass
    else:
        if len(az_net_df) == 1:
            priv_addr_str = az_net_df["PrivateIPAddresses"].loc[0]
            host_entity["private_ips"] = convert_to_ip_entities(priv_addr_str)
            pub_addr_str = az_net_df["PublicIPAddresses"].loc[0]
            host_entity["public_ips"] = convert_to_ip_entities(pub_addr_str)
        else:
            if "private_ips" not in host_entity:
                host_entity["private_ips"] = []
            if "public_ips" not in host_entity:
                host_entity["public_ips"] = []

    return host_entity

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
    logon_sessions: dict
        A dictionary of logon sessions including start and end times and logged on user

     Raises
    ----------
    KQLDataError
        There are no logon sessions in the supplied data set
    """

    logon_sessions = {}
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
            ses_end = (logons_closed.iloc[ses_closed]).name
            if "User" in logons_opened.columns:
                user = (logons_opened.iloc[ses_opened]).User
            elif "Sudoer" in logons_opened.columns:
                user = (logons_opened.iloc[ses_opened]).Sudoer
            else:
                user = "Unknown"
            if isinstance(ses_start, datetime) == False:
                ses_start = datetime.strptime(ses_start,"%Y-%m-%d %H:%M:%S")
                ses_end = datetime.strptime(ses_end,"%Y-%m-%d %H:%M:%S")
            else:
                pass
            if ses_start > ses_close_time or ses_opened == 0:
                pass
            else:
                ses_opened += 1
                continue
            if ses_end < ses_start:
                ses_closed += 1
                continue
            logon_string = f"Logged on user: {user} Session start time: {ses_start} Session end time: {ses_end}"
            logon_sessions[logon_string] = {
                "start": ses_start,
                "end": ses_end,
                "user": user,
            }
            ses_close_time = ses_end
            ses_closed = ses_closed + 1
            ses_opened = ses_opened + 1
        return logon_sessions


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
def sudo_risk_actions(sudo_events:pd.DataFrame, risky_stuff:str = os.path.join(os.path.join(os.path.dirname(__file__), _DETECTIONS_DEF_DIR), 'sudo_cmd_line.txt')):
    if 'Command' not in sudo_events.columns:
        #make exception
        print("DataFrame must contain column of commands run called 'Command'")
    else:
        pass
    risky_actions = []
    sudo_events['Command'].replace('', np.nan, inplace=True)
    sudo_actions = sudo_events['Command'].dropna().to_dict()
    risky_lines = [line.rstrip('\n') for line in open(risky_stuff)]
    for line in risky_lines:
        for action in sudo_actions:
            if re.match("(?P<b64>(?:[A-Za-z0-9+/]{4})*(?:[A-Za-z0-9+/]{2}==|[A-Za-z0-9+/]{3}=|[A-Za-z0-9+/]{4})$)", sudo_actions[action]):
                b64match = re.search("(?P<b64>(?:[A-Za-z0-9+/]{4})*(?:[A-Za-z0-9+/]{2}==|[A-Za-z0-9+/]{3}=|[A-Za-z0-9+/]{4})$)", sudo_actions[action])
                b64string = unpack(input_string=b64match[1])
                b64string = b64string[1]['decoded_string'].to_string()
                if re.match(line, sudo_actions[action]):
                    risky_actions.append(sudo_actions[action])
                else:
                    pass
            else:
                if re.match(line, sudo_actions[action]):
                    risky_actions.append(sudo_actions[action])
                else:
                    pass
    return risky_actions

@export
def sudo_actions_speed(sudo_events: pd.DataFrame, time: int = 5, events: int = 5):
    if 'Command' not in sudo_events.columns:
        #make exception
        print("DataFrame must contain column of commands run called 'Command'")
    elif isinstance(sudo_events["TimeGenerated"][0], datetime) == False:
        print("TimeGenerated is not a datetime format")
    else:
        pass
    suspicious_actions = []
    sudo_events['Command'].replace('', np.nan, inplace=True)
    sudo_actions = sudo_events.dropna(subset=['Command']).reset_index()
    df_len = len(sudo_actions.index) - (events+1)
    while df_len >= 0:
        if isinstance(sudo_actions['TimeGenerated'][(df_len+events)], datetime) == False:
            delta = datetime.strptime(sudo_events["TimeGenerated"][(df_len+events)],"%Y-%m-%dT%H:%M:%S.%fZ") - datetime.strptime(sudo_events["TimeGenerated"][(df_len)],"%Y-%m-%dT%H:%M:%S.%fZ")
        else:
            delta = sudo_actions['TimeGenerated'][(df_len+events)] - sudo_actions['TimeGenerated'][df_len]
        if delta < dt.timedelta(seconds = time):
            suspicious_actions.append({df_len:[sudo_actions[df_len:(df_len+events)],delta]})
        else:
            pass
        df_len = df_len-1
    return suspicious_actions
