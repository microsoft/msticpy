# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
ip_utils - IP Address functions.

Contains a series of functions required to manipulate and enrich IP Address data
to assist investigations.

Designed to support any data source containing IP address entity.

"""
import ipaddress as ip
from functools import lru_cache
from typing import List, Tuple, Callable

import pandas as pd
from ipwhois import IPWhois

from .._version import VERSION
from ..nbtools.entityschema import GeoLocation, IpAddress
from ..nbtools.utility import export
from .geoip import GeoLiteLookup

__version__ = VERSION
__author__ = "Ashwin Patil"


class Error(Exception):
    """Base class for other exceptions."""


class DataError(Error):
    """Raised when thereis a data input error."""


def _get_geolite_lookup() -> Callable:
    """Closure for instantiating GeoLiteLookup."""
    geo_ip = None

    def _get_geo_ip(**kwargs) -> GeoLiteLookup:
        nonlocal geo_ip
        if geo_ip is None:
            geo_ip = GeoLiteLookup(**kwargs)
        return geo_ip

    return _get_geo_ip


_GET_IP_LOOKUP = _get_geolite_lookup()


def convert_to_ip_entities(ip_str: str) -> List[IpAddress]:
    """
    Take in an IP Address string and converts it to an IP Entitity.

    Parameters
    ----------
    ip_str : str
        The string of the IP Address

    Returns
    -------
    List
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
                ip_lookup = _GET_IP_LOOKUP()
                ip_lookup.lookup_ip(ip_entity=ip_entity)
            except DataError:
                pass
            ip_entities.append(ip_entity)
    return ip_entities


@export
# pylint: disable=no-else-return
def get_ip_type(ip_str: str) -> str:
    """
    Validate value is an IP address and deteremine IPType category.

    (IPAddress category is e.g. Private/Public/Multicast).

    Parameters
    ----------
    ip_str : str
        The string of the IP Address

    Returns
    -------
    str
        Returns ip type string using ip address module

    """
    try:
        ip.ip_address(ip_str)
    except ValueError:
        print(f"{ip_str} does not appear to be an IPv4 or IPv6 address")
    else:
        if ip.ip_address(ip_str).is_private:
            ip_type = "Private"
        elif ip.ip_address(ip_str).is_multicast:
            ip_type = "Multicast"
        elif ip.ip_address(ip_str).is_unspecified:
            ip_type = "Unspecified"
        elif ip.ip_address(ip_str).is_reserved:
            ip_type = "Reserved"
        elif ip.ip_address(ip_str).is_loopback:
            ip_type = "Loopback"
        elif ip.ip_address(ip_str).is_global:
            ip_type = "Public"
        elif ip.ip_address(ip_str).is_link_local:
            ip_type = "Link Local"
    return ip_type


@lru_cache(maxsize=1024)
# pylint: disable=no-else-return
def get_whois_info(ip_str: str, show_progress=False) -> Tuple[str, dict]:
    """
    Retrieve whois ASN information for given IP address using IPWhois python package.

    Parameters
    ----------
    ip_str : str
        [description]
    show_progress : bool, optional
        [description], by default False

    Returns
    -------
    IP
        Details of the IP data collected

    """
    ip_type = get_ip_type(ip_str)
    if ip_type == "Public":
        whois = IPWhois(ip_str)
        whois_result = whois.lookup_whois()
        if show_progress:
            print(".", end="")
        return whois_result["asn_description"], whois_result
    else:
        return f"N ASN Information since IP address is of type: {ip_type}", {}


@export
def create_ip_record(
    heartbeat_df: pd.DataFrame, az_net_df: pd.DataFrame = None
) -> IpAddress:
    """
    Generate ip_entity record for provided IP value.

    Parameters
    ----------
    heartbeat_df : pd.DataFrame
        A dataframe of heartbeat data for the host
    az_net_df : pd.DataFrame
        Option dataframe of Azure network data for the host

    Returns
    -------
    IP
        Details of the IP data collected

    """
    ip_entity = IpAddress()

    # Produce ip_entity record using available dataframes
    ip_hb = heartbeat_df.iloc[0]
    ip_entity.Address = ip_hb["ComputerIP"]
    ip_entity.hostname = ip_hb["Computer"]  # type: ignore
    ip_entity.SourceComputerId = ip_hb["SourceComputerId"]  # type: ignore
    ip_entity.OSType = ip_hb["OSType"]  # type: ignore
    ip_entity.OSName = ip_hb["OSName"]  # type: ignore
    ip_entity.OSVMajorersion = ip_hb["OSMajorVersion"]  # type: ignore
    ip_entity.OSVMinorVersion = ip_hb["OSMinorVersion"]  # type: ignore
    ip_entity.ComputerEnvironment = ip_hb["ComputerEnvironment"]  # type: ignore
    ip_entity.OmsSolutions = [  # type: ignore
        sol.strip() for sol in ip_hb["Solutions"].split(",")
    ]
    ip_entity.VMUUID = ip_hb["VMUUID"]  # type: ignore
    ip_entity.SubscriptionId = ip_hb["SubscriptionId"]  # type: ignore
    geoloc_entity = GeoLocation()  # type: ignore
    geoloc_entity.CountryName = ip_hb["RemoteIPCountry"]  # type: ignore
    geoloc_entity.Longitude = ip_hb["RemoteIPLongitude"]  # type: ignore
    geoloc_entity.Latitude = ip_hb["RemoteIPLatitude"]  # type: ignore
    ip_entity.Location = geoloc_entity  # type: ignore

    # If Azure network data present add this to host record
    if az_net_df is not None and not az_net_df.empty:
        if len(az_net_df) == 1:
            priv_addr_str = az_net_df["PrivateIPAddresses"].loc[0]
            ip_entity["private_ips"] = convert_to_ip_entities(priv_addr_str)
            pub_addr_str = az_net_df["PublicIPAddresses"].loc[0]
            ip_entity["public_ips"] = convert_to_ip_entities(pub_addr_str)
        else:
            if "private_ips" not in ip_entity:
                ip_entity["private_ips"] = []
            if "public_ips" not in ip_entity:
                ip_entity["public_ips"] = []

    return ip_entity
