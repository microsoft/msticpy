# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------

"""
Contains a series of functions required to correct collect, parse and visualise linux syslog data.

Specifically designed to support standard linux syslog for investigations where auditd is not avalaible.

"""
import datetime as dt
import os
import re
from typing import Tuple
import pytz

import ipywidgets as widgets

import numpy as np
import pandas as pd
from pandas.plotting import register_matplotlib_converters
from IPython import get_ipython

from .._version import VERSION
from ..nbtools.entityschema import GeoLocation, Host, IpAddress
from ..nbtools.utility import export
from .geoip import GeoLiteLookup
from .base64unpack import unpack

__version__ = VERSION
__author__ = "Pete Bryan"

_DETECTIONS_DEF_DIR = "detections"

WIDGET_DEFAULTS = {
    "layout": widgets.Layout(width="95%"),
    "style": {"description_width": "initial"},
}
IPLOCATION = GeoLiteLookup()
_IP = get_ipython()
register_matplotlib_converters()


class Error(Exception):
    """Base class for other exceptions."""


class DataError(Error):
    """Raised when thereis a data input error."""


class KQLError(Error):
    """Raised whent there is an error related to KQL."""


class KQLDataError(KQLError):
    """Raised when there is an error related to the data returned by KQL."""


def convert_to_ip_entities(ip_str: str) -> Tuple[IpAddress]:
    """
    Take in an IP Address string and converts it to an IP Entitity.

    Parameters
    ----------
    ip_str : str
        The string containing the IP Address

    Returns
    -------
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
            try:
                IPLOCATION.lookup_ip(ip_entity=ip_entity)
            except:
                pass
            ip_entities.append(ip_entity)
    return ip_entities


def create_host_record(
    syslog_df: pd.DataFrame, heartbeat_df: pd.DataFrame, az_net_df: pd.DataFrame = None
) -> Host:
    """
    Generate host_entity record for selected computer.

    Parameters
    ----------
    syslog_df : pd.DataFrame
        A dataframe of all syslog events for the host in the time window requried
    heartbeat_df : pd.DataFrame
        A dataframe of heartbeat data for the host
    az_net_df : pd.DataFrame
        Option dataframe of Azure network data for the host

    Returns
    -------
    Host
        Details of the host data collected

    Raises
    ------
    KQLDataError
        Could not find any data for the computer in the time window

    """
    if not isinstance(syslog_df, pd.DataFrame) or syslog_df.empty:
        raise KQLDataError(f"No syslog data provided")
    else:
        host_entity = Host(src_event=syslog_df.iloc[0])
        applications = []
        _apps = syslog_df["ProcessName"].unique().tolist()
        for app in _apps:
            if app not in (
                "CRON",
                "sudo",
                "snapd" "systemd-resolved",
                "systemd",
                "crontab",
                "systemd-timesyncd",
                "systemd-logind",
                "rsyslogd",
                "syslog-ng",
            ):
                applications.append(app)
            else:
                pass

    if not isinstance(heartbeat_df, pd.DataFrame) or heartbeat_df.empty:
        raise KQLDataError(f"No heartbeat data provided")

    host_hb = heartbeat_df.iloc[0]
    host_entity.SourceComputerId = host_hb["SourceComputerId"]
    host_entity.OSType = host_hb["OSType"]
    host_entity.OSName = host_hb["OSName"]
    host_entity.OSVMajorersion = host_hb["OSMajorVersion"]
    host_entity.OSVMinorVersion = host_hb["OSMinorVersion"]
    host_entity.ComputerEnvironment = host_hb["ComputerEnvironment"]
    host_entity.OmsSolutions = [sol.strip() for sol in host_hb["Solutions"].split(",")]
    host_entity.Applications = applications
    host_entity.VMUUID = host_hb["VMUUID"]
    ip_entity = IpAddress()
    ip_entity.Address = host_hb["ComputerIP"]
    geoloc_entity = GeoLocation()
    geoloc_entity.CountryName = host_hb["RemoteIPCountry"]
    geoloc_entity.Longitude = host_hb["RemoteIPLongitude"]
    geoloc_entity.Latitude = host_hb["RemoteIPLatitude"]
    ip_entity.Location = geoloc_entity
    host_entity.IPAddress = ip_entity

    if az_net_df is None or az_net_df.empty:
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
    Parameters
    ----------
    logon_events: pd.DataFrame
        A DataFrame of all syslog logon events (can be generated with LinuxSyslog.user_logon query)

    Returns
    -------
    logon_sessions: dict
        A dictionary of logon sessions including start and end times and logged on user

    Raises
    ------
    KQLDataError
        There are no logon sessions in the supplied data set

    """

    logon_sessions = {}
    ses_close_time = dt.datetime.now()
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
    if logons_opened.empty or logons_closed.empty:
        print("There are no logon sessions in the supplied data set")
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


def cluster_syslog_logons_df(logon_events: pd.DataFrame) -> dict:
    """
    Clusters logon sessions observed in syslog by start and end time based on PAM events.

    Will return a LogonDataError if supplied dataframe does not contain complete logon sessions.
    Parameters
    ----------
    logon_events: pd.DataFrame
        A DataFrame of all syslog logon events (can be generated with LinuxSyslog.user_logon query)

    Returns
    -------
    logon_sessions: dict
        A dictionary of logon sessions including start and end times and logged on user

    Raises
    ------
    KQLDataError
        There are no logon sessions in the supplied data set

    """

    logon_sessions = {}
    users = []
    starts = []
    ends = []
    ses_close_time = dt.datetime.now()
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
    if logons_opened.empty or logons_closed.empty:
        print("There are no logon sessions in the supplied data set")
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
            if ses_start > ses_close_time or ses_opened == 0:
                pass
            else:
                ses_opened += 1
                continue
            if ses_end < ses_start:
                ses_closed += 1
                continue
            users.append(user)
            starts.append(ses_start)
            ends.append(ses_end)
            logon_string = f"Logged on user: {user} Session start time: {ses_start} Session end time: {ses_end}"
            logon_sessions[logon_string] = {
                "start": ses_start,
                "end": ses_end,
                "user": user,
            }
            ses_close_time = ses_end
            ses_closed = ses_closed + 1
            ses_opened = ses_opened + 1
        logon_sessions_df = pd.DataFrame({"User": users, "Start": starts, "Ends": ends})
        return logon_sessions_df


def risky_actions(
    events: pd.DataFrame,
    risky_stuff: str = os.path.join(
        os.path.join(os.path.dirname(__file__), _DETECTIONS_DEF_DIR),
        "sudo_cmd_line.txt",
    ),
):
    risky_actions = {}
    if "SyslogMessage" not in events.columns:
        raise DataError("This function currently only supports Syslog")
    else:
        pass

    if "Command" in events.columns:
        events["Command"].replace("", np.nan, inplace=True)
        syslog_actions = (
            events[["TimeGenerated", "Command"]]
            .rename(columns={"Command": "Message"})
            .dropna()
            .set_index("TimeGenerated")
            .to_dict()
        )
    else:
        events["SyslogMessage"].replace("", np.nan, inplace=True)
        syslog_actions = (
            events[["TimeGenerated", "SyslogMessage"]]
            .rename(columns={"SyslogMessage": "Message"})
            .dropna()
            .set_index("TimeGenerated")
            .to_dict()
        )
    risky_lines = [line.rstrip("\n") for line in open(risky_stuff)]
    for line in risky_lines:
        for date, message in syslog_actions["Message"].items():
            # TODO regex compile
            if re.match(
                "(?P<b64>(?:[A-Za-z0-9+/]{4})*(?:[A-Za-z0-9+/]{2}==|[A-Za-z0-9+/]{3}=|[A-Za-z0-9+/]{4})$)",
                message,
            ):
                b64match = re.search(
                    "(?P<b64>(?:[A-Za-z0-9+/]{4})*(?:[A-Za-z0-9+/]{2}==|[A-Za-z0-9+/]{3}=|[A-Za-z0-9+/]{4})$)",
                    message,
                )
                b64string = unpack(input_string=b64match[1])
                b64string = b64string[1]["decoded_string"].to_string()
                if re.match(line, message):
                    risky_actions.update({date: message})
                else:
                    pass
            else:
                if re.match(line, message):
                    risky_actions.update({date: message})
                else:
                    pass
    return risky_actions


@export
def sudo_actions_speed(sudo_events: pd.DataFrame, time: int = 5, events: int = 5):
    if "Command" not in sudo_events.columns:
        raise DataError(
            "DataFrame must contain column of commands run called 'Command'"
        )
    elif isinstance(sudo_events["TimeGenerated"].iloc[0], dt.datetime) is False:
        print("TimeGenerated is not a datetime format")
    else:
        pass
    suspicious_actions = []
    sudo_events["Command"].replace("", np.nan, inplace=True)
    sudo_actions = sudo_events.dropna(subset=["Command"]).reset_index()
    df_len = len(sudo_actions.index) - (events + 1)
    while df_len >= 0:
        delta = (
            sudo_actions["TimeGenerated"][(df_len + events)]
            - sudo_actions["TimeGenerated"][df_len]
        )
        if delta < dt.timedelta(seconds=time):
            suspicious_actions.append(
                {df_len: [sudo_actions[df_len : (df_len + events)], delta]}
            )
        else:
            pass
        df_len = df_len - 1
    return suspicious_actions


@export
def risky_sudo_sessions(
    risky_actions: dict, sudo_sessions: dict, suspicious_events: list
):
    risky_sessions = {}
    if not risky_actions and not suspicious_events:
        raise DataError(f" No suspicious Sudo commands provided")
    elif risky_actions and not suspicious_events:
        for key, value in risky_actions.items():
            for sess_key, sess_val in sudo_sessions.items():
                if (
                    _normalize_to_utc(sess_val["start"])
                    <= _normalize_to_utc(key)
                    <= _normalize_to_utc(sess_val["end"])
                ):
                    risky_sessions.update({sess_key: value})
                else:
                    pass
    elif suspicious_events and not risky_actions:
        for event in suspicious_events:
            for key, value in event.items():
                for sess_key, sess_val in sudo_sessions.items():
                    if (
                        _normalize_to_utc(sess_val["start"])
                        <= _normalize_to_utc(value[0]["TimeGenerated"].iloc[1])
                        <= _normalize_to_utc(sess_val["end"])
                    ):
                        risky_sessions.update({sess_key: "Suspicious event pattern"})
                    else:
                        pass
    else:
        for key, value in risky_actions.items():
            for sess_key, sess_val in sudo_sessions.items():
                if (
                    _normalize_to_utc(sess_val["start"])
                    <= _normalize_to_utc(key)
                    <= _normalize_to_utc(sess_val["end"])
                ):
                    risky_sessions.update({sess_key: value})
                else:
                    pass
        for event in suspicious_events:
            for key, value in event.items():
                for sess_key, sess_val in sudo_sessions.items():
                    if (
                        _normalize_to_utc(sess_val["start"])
                        <= _normalize_to_utc(value[0]["TimeGenerated"].iloc[1])
                        <= _normalize_to_utc(sess_val["end"])
                    ):
                        risky_sessions.update({sess_key: "Suspicious event pattern"})
                    else:
                        pass
    return risky_sessions


def _normalize_to_utc(time_stamp: dt.datetime):
    if time_stamp.tzinfo is None or time_stamp.tzinfo.utcoffset(time_stamp) is None:
        time_stamp = time_stamp.replace(tzinfo=pytz.UTC)
        return time_stamp
    else:
        time_stamp = time_stamp.astimezone(pytz.utc)
        return time_stamp
