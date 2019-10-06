# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------

"""

Contains a series of functions required to correct collect, parse and
visualise linux syslog data.

Designed to support standard linux syslog for investigations where
auditd is not available.

"""
import datetime as dt
from typing import Tuple
import pytz

import ipywidgets as widgets
import pandas as pd
from pandas.plotting import register_matplotlib_converters
from IPython import get_ipython

from .._version import VERSION
from ..nbtools.entityschema import GeoLocation, Host, IpAddress
from ..nbtools.utility import export
from .geoip import GeoLiteLookup

__version__ = VERSION
__author__ = "Pete Bryan"

_DETECTIONS_DEF_DIR = "resources"

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


def convert_to_ip_entities(ip_str: str) -> Tuple[IpAddress]:
    """
    Take in an IP Address string and converts it to an IP Entitity.

    Parameters
    ----------
    ip_str : str
        The string of the IP Address

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
            except DataError:
                pass
            ip_entities.append(ip_entity)
    return ip_entities


@export
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

    """
    host_entity = Host(src_event=syslog_df.iloc[0])
    applications = []

    # Produce list of processes on the host that are not
    # part of a 'standard' linux distro
    _apps = syslog_df["ProcessName"].unique().tolist()
    for app in _apps:
        if app not in (
            "CRON",
            "sudo",
            "snapd",
            "systemd-resolved",
            "systemd",
            "crontab",
            "systemd-timesyncd",
            "systemd-logind",
            "rsyslogd",
            "syslog-ng",
        ):
            applications.append(app)

    # Produce host_entity record mapping linux heartbeat elements to host_entity fields
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

    # If Azure network data present add this to host record
    if az_net_df is not None and not az_net_df.empty:
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
def cluster_syslog_logons_df(logon_events: pd.DataFrame) -> pd.DataFrame:
    """
    Clusters logon sessions in syslog by start/end time based on PAM events.

    Will return a LogonDataError if supplied dataframe does not contain
    complete logon sessions.

    Parameters
    ----------
    logon_events: pd.DataFrame
        A DataFrame of all syslog logon events
        (can be generated with LinuxSyslog.user_logon query)

    Returns
    -------
    logon_sessions: pd.DataFrame
        A dictionary of logon sessions including start and end times
        and logged on user

    Raises
    ------
    DataError
        There are no logon sessions in the supplied data set

    """
    users = []
    starts = []
    ends = []
    ses_close_time = dt.datetime.now()
    ses_opened = 0
    ses_closed = 0
    # Extract logon session opened and logon session closed data.
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
        raise DataError("There are no logon sessions in the supplied data set")

    # For each session identify the likely start and end times
    while ses_opened < len(logons_opened.index) and ses_closed < len(
        logons_closed.index
    ):
        ses_start = (logons_opened.iloc[ses_opened]).name
        ses_end = (logons_closed.iloc[ses_closed]).name
        # If we can identify a user for the session add this to the details
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
        ses_close_time = ses_end
        ses_closed = ses_closed + 1
        ses_opened = ses_opened + 1
    logon_sessions_df = pd.DataFrame({"User": users, "Start": starts, "End": ends})
    return logon_sessions_df


@export
def risky_sudo_sessions(
    sudo_sessions: pd.DataFrame,
    risky_actions: dict = None,
    suspicious_actions: list = None,
) -> dict:
    """
    Detect if a sudo session occurs at the point of a suspicious event.

    Parameters
    ----------
    sudo_sessions: dict
        Dictionary of sudo sessions (as generated by cluster_syslog_logons)
    risky_actions: dict (Optional)
        Dictionary of risky sudo commands (as generated by cmd_line.risky_cmd_line)
    suspicious_actions: list (Optional)
        List of risky sudo commands (as generated by cmd_line.cmd_speed)

    Returns
    -------
    risky_sessions: dict
        A dictionary of sudo sessions with flags denoting risk

    """
    sessions = sudo_sessions[["User", "Start", "End"]].to_dict("index")

    if risky_actions is None and suspicious_actions is None:
        raise DataError(
            "At least one of risky_actions or suspicious_actions must be supplied"
        )

    risky_sessions = {}
    # Depending on whether we have risky or suspicious acitons or both
    # identify sessions which these actions occur in
    if risky_actions is not None and suspicious_actions is None:
        risky_sessions = _find_risky_sudo_session(
            risky_actions=risky_actions, sudo_sessions=sessions
        )
    elif suspicious_actions is not None and risky_actions is None:
        risky_sessions = _find_suspicious_sudo_session(
            suspicious_actions=suspicious_actions, sudo_sessions=sessions
        )
    else:
        risky_act_sessions = _find_risky_sudo_session(
            risky_actions=risky_actions, sudo_sessions=sessions
        )
        susp_sessions = _find_suspicious_sudo_session(
            suspicious_actions=suspicious_actions, sudo_sessions=sessions
        )
        risky_sessions = {**risky_act_sessions, **susp_sessions}
    return risky_sessions


def _normalize_to_utc(time_stamp: dt.datetime):
    # Normalize datetimes to UTC in case we have mixed timezones in datasets
    if time_stamp.tzinfo is None or time_stamp.tzinfo.utcoffset(time_stamp) is None:
        time_stamp = time_stamp.replace(tzinfo=pytz.UTC)
    else:
        time_stamp = time_stamp.astimezone(pytz.utc)
    return time_stamp


def _find_risky_sudo_session(risky_actions: dict, sudo_sessions: dict):
    risky_sessions = {}
    # Determine if risky event occurs during a session time window
    for key, value in risky_actions.items():
        for sess_key, sess_val in sudo_sessions.items():
            if (
                _normalize_to_utc(sess_val["Start"])
                <= _normalize_to_utc(key)
                <= _normalize_to_utc(sess_val["End"])
            ):
                risky_sessions.update({sess_key: value})
    return risky_sessions


def _find_suspicious_sudo_session(suspicious_actions: list, sudo_sessions: dict):
    risky_sessions = {}
    # Determine if suspicious event occurs during a session time window
    for event in suspicious_actions:
        for value in event.values():
            for sess_key, sess_val in sudo_sessions.items():
                if (
                    _normalize_to_utc(sess_val["Start"])
                    <= _normalize_to_utc(value[0]["TimeGenerated"].iloc[1])
                    <= _normalize_to_utc(sess_val["End"])
                ):
                    risky_sessions.update({sess_key: "Suspicious event pattern"})
    return risky_sessions
