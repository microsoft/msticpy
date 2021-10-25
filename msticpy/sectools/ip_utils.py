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
import ipaddress
from functools import lru_cache
from typing import Callable, List, Optional, Set, Tuple

import pandas as pd
from deprecated.sphinx import deprecated
from ipwhois import (
    ASNRegistryError,
    HostLookupError,
    HTTPLookupError,
    HTTPRateLimitError,
    IPWhois,
    WhoisLookupError,
    WhoisRateLimitError,
)

from .._version import VERSION
from ..common.utility import arg_to_list, export
from ..datamodel.entities import GeoLocation, IpAddress
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


def convert_to_ip_entities(  # noqa: MC0001
    ip_str: Optional[str] = None,
    data: Optional[pd.DataFrame] = None,
    ip_col: Optional[str] = None,
    geo_lookup: bool = True,
) -> List[IpAddress]:
    """
    Take in an IP Address string and converts it to an IP Entity.

    Parameters
    ----------
    ip_str : str
        A string with a single IP Address or multiple addresses
        delimited by comma or space
    data : pd.DataFrame
        Use DataFrame as input
    ip_col : str
        Column containing IP addresses
    geo_lookup : bool
        If true, do geolocation lookup on IPs,
        by default, True

    Returns
    -------
    List
        The populated IP entities including address and geo-location

    Raises
    ------
    ValueError
        If neither ip_string or data/column provided as input

    """
    ip_entities: List[IpAddress] = []
    all_ips: Set[str] = set()

    if geo_lookup:
        ip_lookup = _GET_IP_LOOKUP()
    if ip_str:
        addrs = arg_to_list(ip_str)
    elif data is not None and ip_col:
        addrs = data[ip_col].values
    else:
        raise ValueError("Must specify either ip_str or data + ip_col parameters.")

    for addr in addrs:
        if isinstance(addr, list):
            ip_list = set(addr)
        elif isinstance(addr, str) and "," in addr:
            ip_list = {ip.strip() for ip in addr.split(",")}
        else:
            ip_list = {addr}
        ip_list = ip_list - all_ips  # remove IP addresses we've seen
        ip_entities.extend(IpAddress(Address=ip) for ip in ip_list)
        all_ips |= ip_list
        if geo_lookup:
            for ip_ent in ip_entities:
                try:
                    ip_lookup.lookup_ip(ip_entity=ip_ent)
                except DataError:
                    pass
    return ip_entities


@export  # noqa: MC0001
# pylint: disable=too-many-return-statements, invalid-name
def get_ip_type(ip: str = None, ip_str: str = None) -> str:  # noqa: MC0001
    """
    Validate value is an IP address and determine IPType category.

    (IPAddress category is e.g. Private/Public/Multicast).

    Parameters
    ----------
    ip : str
        The string of the IP Address
    ip_str : str
        The string of the IP Address - alias for `ip`

    Returns
    -------
    str
        Returns ip type string using ip address module

    """
    ip_str = ip or ip_str
    if not ip_str:
        raise ValueError("'ip' or 'ip_str' value must be specified")
    try:
        ipaddress.ip_address(ip_str)
    except ValueError:
        print(f"{ip_str} does not appear to be an IPv4 or IPv6 address")
    else:
        if ipaddress.ip_address(ip_str).is_multicast:
            return "Multicast"
        if ipaddress.ip_address(ip_str).is_global:
            return "Public"
        if ipaddress.ip_address(ip_str).is_loopback:
            return "Loopback"
        if ipaddress.ip_address(ip_str).is_link_local:
            return "Link Local"
        if ipaddress.ip_address(ip_str).is_unspecified:
            return "Unspecified"
        if ipaddress.ip_address(ip_str).is_private:
            return "Private"
        if ipaddress.ip_address(ip_str).is_reserved:
            return "Reserved"

    return "Unspecified"


# pylint: enable=too-many-return-statements


# pylint: disable=invalid-name
@lru_cache(maxsize=1024)
def get_whois_info(
    ip: str = None, show_progress: bool = False, **kwargs
) -> Tuple[str, dict]:
    """
    Retrieve whois ASN information for given IP address using IPWhois python package.

    Parameters
    ----------
    ip : str
        IP Address to look up.
    ip_str : str
        alias for `ip`.
    show_progress : bool, optional
        Show progress for each query, by default False

    Returns
    -------
    IP
        Details of the IP data collected

    Notes
    -----
    This function uses the Python functools lru_cache and
    will return answers from the cache for previously queried
    IP addresses.

    """
    ip_str = ip or kwargs.get("ip_str")
    if not ip_str:
        raise ValueError("'ip' or 'ip_str' value must be specified")
    ip_type = get_ip_type(ip_str)
    if ip_type == "Public":
        try:
            whois = IPWhois(ip_str)
            whois_result = whois.lookup_whois()
            if show_progress:
                print(".", end="")
            return whois_result["asn_description"], whois_result
        except (
            HTTPLookupError,
            HTTPRateLimitError,
            HostLookupError,
            WhoisLookupError,
            WhoisRateLimitError,
            ASNRegistryError,
        ) as err:
            return f"Error during lookup of {ip_str} {type(err)}", {}
    return f"No ASN Information for IP type: {ip_type}", {}


# pylint: enable=invalid-name


def get_whois_df(
    data: pd.DataFrame,
    ip_column: str,
    all_columns: bool = False,
    asn_col: str = "AsnDescription",
    whois_col: Optional[str] = None,
    show_progress: bool = False,
) -> pd.DataFrame:
    """
    Retrieve Whois ASN information for DataFrame of IP Addresses.

    Parameters
    ----------
    data : pd.DataFrame
        Input DataFrame
    ip_column : str
        Column name of IP Address to look up.
    all_columns:
        Expand all whois data to columns.
    asn_col : str, optional
        Name of the output column for ASN description,
        by default "ASNDescription".
        Ignored if `all_columns` is True.
    whois_col : str, optional
        Name of the output column for full whois data,
        by default "WhoIsData"
        Ignored if `all_columns` is True.
    show_progress : bool, optional
        Show progress for each query, by default False

    Returns
    -------
    pd.DataFrame
        Output DataFrame with results in added columns.

    """
    if all_columns:
        return data.apply(
            lambda x: get_whois_info(x[ip_column], show_progress=show_progress)[1],
            axis=1,
            result_type="expand",
        )
    data = data.copy()
    if whois_col is not None:
        data[[asn_col, whois_col]] = data.apply(
            lambda x: get_whois_info(x[ip_column], show_progress=show_progress),
            axis=1,
            result_type="expand",
        )
    else:
        data[asn_col] = data.apply(
            lambda x: get_whois_info(x[ip_column], show_progress=show_progress)[0],
            axis=1,
        )
    return data


# pylint: disable=too-few-public-methods
@pd.api.extensions.register_dataframe_accessor("mp_whois")
class IpWhoisAccessor:
    """Pandas api extension for IP Whois lookup."""

    def __init__(self, pandas_obj):
        """Instantiate pandas extension class."""
        self._df = pandas_obj

    def lookup(self, ip_column, **kwargs):
        """
        Extract IoCs from either a pandas DataFrame.

        Parameters
        ----------
        ip_column : str
            Column name of IP Address to look up.

        Other Parameters
        ----------------
        asn_col : str, optional
            Name of the output column for ASN description,
            by default "ASNDescription"
        whois_col : str, optional
            Name of the output column for full whois data,
            by default "WhoIsData"
        show_progress : bool, optional
            Show progress for each query, by default False

        Returns
        -------
        pd.DataFrame
            Output DataFrame with results in added columns.

        """
        return get_whois_df(data=self._df, ip_column=ip_column, **kwargs)


@deprecated("Will be removed in a future version", version="1.4.0")
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
