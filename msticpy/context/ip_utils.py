# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
ip_utils - IP Address functions.

Contains a series of functions required to manipulate and
enrich IP Address data to assist investigations.

Designed to support any data source containing IP address entity.

"""
from __future__ import annotations

import ipaddress
import logging
import re
import socket
import warnings
from dataclasses import dataclass, field
from functools import lru_cache
from time import sleep
from typing import Any, Callable

import httpx
import pandas as pd
from bs4 import BeautifulSoup
from deprecated.sphinx import deprecated
from typing_extensions import Self

from .._version import VERSION
from ..common.exceptions import MsticpyConnectionError, MsticpyException
from ..common.utility import arg_to_list, export
from ..datamodel.entities import GeoLocation, IpAddress

logger: logging.Logger = logging.getLogger(__name__)

__version__ = VERSION
__author__ = "Ashwin Patil"


_REGISTRIES: dict[str, dict[str, str]] = {
    "arin": {
        "url": "http://rdap.arin.net/registry/ip/",
    },
    "ripencc": {
        "url": "http://rdap.db.ripe.net/ip/",
    },
    "apnic": {
        "url": "http://rdap.apnic.net/ip/",
    },
    "lacnic": {
        "url": "http://rdap.lacnic.net/rdap/ip/",
    },
    "afrinic": {
        "url": "http://rdap.afrinic.net/rdap/ip/",
    },
}

_POTAROO_ASNS_URL = "https://bgp.potaroo.net/cidr/autnums.html"

RATE_LIMIT_THRESHOLD: int = 50


# Closure to cache ASN dictionary from Potaroo
def _fetch_asns() -> Callable[[], dict[str, str]]:
    """Create closure for ASN fetching."""
    asns_dict: dict[str, str] = {}

    def _get_asns_dict() -> dict[str, str]:
        """Return or fetch and return ASN Soup."""
        nonlocal asns_dict
        if not asns_dict:
            try:
                asns_resp: httpx.Response = httpx.get(_POTAROO_ASNS_URL)
            except httpx.ConnectError as err:
                err_msg: str = "Unable to get ASN details from potaroo.net"
                raise MsticpyConnectionError(err_msg) from err
            asns_soup = BeautifulSoup(asns_resp.content, features="lxml")
            asns_dict = {
                str(asn.next_element)
                .strip(): str(asn.next_element.next_element)
                .strip()
                for asn in asns_soup.find_all("a")
            }
        return asns_dict

    return _get_asns_dict


# Create the dictionary accessor from the fetch_asns wrapper
_ASNS_DICT: Callable[[], dict[str, str]] = _fetch_asns()


@export
def convert_to_ip_entities(
    ip_str: str | None = None,
    data: pd.DataFrame | None = None,
    ip_col: str | None = None,
    *,
    geo_lookup: bool = True,
) -> list[IpAddress]:
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
    # locally imported to prevent cyclic import
    # pylint: disable=import-outside-toplevel, cyclic-import
    from .geoip import GeoLiteLookup

    geo_lite_lookup: GeoLiteLookup = GeoLiteLookup()

    ip_entities: list[IpAddress] = []
    all_ips: set[str] = set()

    if ip_str:
        addrs: list[str] = arg_to_list(ip_str)
    elif data is not None and ip_col:
        addrs = data[ip_col].to_numpy().tolist()
    else:
        err_msg: str = "Must specify either ip_str or data + ip_col parameters."
        raise ValueError(err_msg)

    for addr in addrs:
        if isinstance(addr, list):
            ip_list: set[str] = set(addr)
        elif isinstance(addr, str) and "," in addr:
            ip_list = {ip.strip() for ip in addr.split(",")}
        else:
            ip_list = {addr}
        ip_list = ip_list - all_ips  # remove IP addresses we've seen
        ip_entities.extend(IpAddress(Address=ip) for ip in ip_list)
        all_ips |= ip_list
        if geo_lookup:
            for ip_ent in ip_entities:
                geo_lite_lookup.lookup_ip(ip_entity=ip_ent)
    return ip_entities


@deprecated("Will be removed in a version 2.2", version="1.4.0")
@export
def create_ip_record(
    heartbeat_df: pd.DataFrame,
    az_net_df: pd.DataFrame | None = None,
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
    ip_entity: IpAddress = IpAddress()

    # Produce ip_entity record using available dataframes
    ip_hb: pd.Series[str] = heartbeat_df.iloc[0]
    ip_entity.Address = ip_hb["ComputerIP"]
    ip_entity.hostname = ip_hb["Computer"]
    ip_entity.SourceComputerId = ip_hb["SourceComputerId"]
    ip_entity.OSType = ip_hb["OSType"]
    ip_entity.OSName = ip_hb["OSName"]
    ip_entity.OSVMajorVersion = ip_hb["OSMajorVersion"]
    ip_entity.OSVMinorVersion = ip_hb["OSMinorVersion"]
    ip_entity.ComputerEnvironment = ip_hb["ComputerEnvironment"]
    ip_entity.OmsSolutions = [sol.strip() for sol in ip_hb["Solutions"].split(",")]
    ip_entity.VMUUID = ip_hb["VMUUID"]
    ip_entity.SubscriptionId = ip_hb["SubscriptionId"]
    geoloc_entity: GeoLocation = GeoLocation()
    geoloc_entity.CountryOrRegionName = ip_hb["RemoteIPCountry"]
    geoloc_entity.Longitude = ip_hb["RemoteIPLongitude"]
    geoloc_entity.Latitude = ip_hb["RemoteIPLatitude"]
    ip_entity.Location = geoloc_entity

    # If Azure network data present add this to host record
    if az_net_df is not None and not az_net_df.empty:
        if len(az_net_df) == 1:
            priv_addr_str: str = az_net_df["PrivateIPAddresses"].loc[0]
            ip_entity["private_ips"] = convert_to_ip_entities(priv_addr_str)
            pub_addr_str: str = az_net_df["PublicIPAddresses"].loc[0]
            ip_entity["public_ips"] = convert_to_ip_entities(pub_addr_str)
        else:
            if "private_ips" not in ip_entity:
                ip_entity["private_ips"] = []
            if "public_ips" not in ip_entity:
                ip_entity["public_ips"] = []

    return ip_entity


@export
def get_ip_type(ip: str | None = None, ip_str: str | None = None) -> str:
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
        err_msg: str = "'ip' or 'ip_str' value must be specified"
        raise ValueError(err_msg)
    try:
        ip_obj: ipaddress.IPv4Address | ipaddress.IPv6Address = ipaddress.ip_address(
            ip_str,
        )
    except ValueError:
        logger.exception("%s does not appear to be an IPv4 or IPv6 address", ip_str)
    else:
        return_values: dict[str, str] = {
            "is_multicast": "Multicast",
            "is_global": "Public",
            "is_loopback": "Loopback",
            "is_link_local": "Link Local",
            "is_unspecified": "Unspecified",
            "is_private": "Private",
            "is_reserved": "Reserved",
        }
        for func, msg in return_values.items():
            if getattr(ip_obj, func):
                return msg
        return "Unspecified"

    return "Unspecified"


@deprecated("Replaced with ip_whois function", version="2.1.0")
@export
@lru_cache(maxsize=1024)
def get_whois_info(
    ip: str | None = None,
    *,
    show_progress: bool = False,
    ip_str: str | None = None,
) -> pd.DataFrame | _IpWhoIsResult:
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
    ip_str = ip or ip_str
    if not ip_str:
        err_msg: str = "'ip' or 'ip_str' value must be specified"
        raise ValueError(err_msg)
    ip_type: str = get_ip_type(ip_str)
    if ip_type == "Public":
        logger.info(ip_str)
        try:
            whois_result: pd.DataFrame | _IpWhoIsResult = ip_whois(ip_str)
        except MsticpyException as err:
            return _IpWhoIsResult(
                name=f"Error during lookup of {ip_str} {type(err)}",
                properties={},
            )
        if show_progress:
            logger.info(".")
        return whois_result
    return _IpWhoIsResult(
        name=f"No ASN Information for IP type: {ip_type}",
        properties={},
    )


@export
def get_whois_df(  # noqa: PLR0913
    data: pd.DataFrame,
    ip_column: str,
    *,
    all_columns: bool = True,
    asn_col: str = "AsnDescription",
    whois_col: str = "WhoIsData",
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
    del show_progress
    whois_data: pd.DataFrame | _IpWhoIsResult = ip_whois(
        data[ip_column].drop_duplicates(),
    )
    if (
        isinstance(whois_data, pd.DataFrame)
        and not whois_data.empty
        and "query" in whois_data.columns
    ):
        data = data.merge(
            whois_data,
            how="left",
            left_on=ip_column,
            right_on="query",
            suffixes=("", "_whois"),
        )
        data[whois_col] = data[whois_data.columns].apply(lambda x: x.to_dict(), axis=1)
        data[asn_col] = data["asn_description"]
        if not all_columns:
            return data.drop(columns=whois_data.columns)
        return data
    return data.assign(ASNDescription="No data returned")


@pd.api.extensions.register_dataframe_accessor("mp_whois")
@export
class IpWhoisAccessor:
    """Pandas api extension for IP Whois lookup."""

    def __init__(self: IpWhoisAccessor, pandas_obj: pd.DataFrame) -> None:
        """Instantiate pandas extension class."""
        self._df: pd.DataFrame = pandas_obj

    def lookup(
        self: Self,
        ip_column: str,
        *,
        asn_col: str = "ASNDescription",
        whois_col: str = "WhoIsData",
        show_progress: bool = False,
    ) -> pd.DataFrame:
        """
        Extract IoCs from either a pandas DataFrame.

        Parameters
        ----------
        ip_column : str
            Column name of IP Address to look up.
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
        warn_message = (
            "This accessor method has been deprecated.\n"
            "Please use IpAddress.util.whois() pivot function."
            "This will be removed in MSTICPy v2.2.0"
        )
        warnings.warn(
            warn_message,
            category=DeprecationWarning,
            stacklevel=1,
        )
        return get_whois_df(
            data=self._df,
            ip_column=ip_column,
            asn_col=asn_col,
            whois_col=whois_col,
            show_progress=show_progress,
        )


def ip_whois(
    ip: IpAddress | str | list | pd.Series | None = None,
    ip_address: IpAddress | str | list[str] | pd.Series | None = None,
    *,
    raw: bool = False,
    query_rate: float = 0.5,
    retry_count: int = 5,
) -> pd.DataFrame | _IpWhoIsResult:
    """
    Lookup IP Whois information.

    Parameters
    ----------
    ip : Union[IpAddress, str, List]
        An IP address or list of IP addresses to lookup.
    ip_address : Union[IpAddress, str, List]
        An IP address or list of IP addresses to lookup. Alias of `ip`.
    raw : bool, optional
        Set True if raw whois result wanted, by default False
    query_rate : float, optional
        Controls the rate at which queries are made, by default 0.5
    retry_count : int, optional
        The number of times to retry a query if it fails, default is 5

    Returns
    -------
    Union[pd.DataFrame, Dict]
        If a single IP Address provided result is a dictionary
        if multiple provided results are formatted as a dataframe.

    Raises
    ------
    ValueError:
        If neither `ip` nor `ip_address` is supplied.

    """
    ip = ip if ip is not None else ip_address
    if ip is None:
        err_msg: str = "One of ip or ip_address parameters must be supplied."
        raise ValueError(err_msg)
    if isinstance(ip, (list, pd.Series)):
        rate_limit: bool = len(ip) > RATE_LIMIT_THRESHOLD
        if rate_limit:
            logger.info("Large number of lookups, this may take some time.")
        whois_results: dict[str, Any] = {}
        for ip_addr in ip:
            if rate_limit:
                sleep(query_rate)
            whois_results[ip_addr] = _whois_lookup(
                ip_addr,
                raw=raw,
                retry_count=retry_count,
            ).properties
        return _whois_result_to_pandas(whois_results)
    if isinstance(ip, (str, IpAddress)):
        return _whois_lookup(ip, raw=raw)
    return pd.DataFrame()


def get_asn_details(asns: str | list[str]) -> pd.DataFrame | dict[str, Any]:
    """
    Get details about an ASN(s) from its number.

    Parameters
    ----------
    asns : Union[str, List]
        A single ASN or list of ASNs to lookup.

    Returns
    -------
    Union[pd.DataFrame, Dict]
        Details about the ASN(s) if a single ASN provided, result is a dictionary.
        If multiple provided, results are returned as a DataFrame.

    """
    if isinstance(asns, list):
        asn_detail_results: list[dict[str, Any]] = [
            _asn_results(str(asn)) for asn in asns
        ]
        return pd.DataFrame(asn_detail_results)
    return _asn_results(str(asns))


def get_asn_from_name(name: str) -> dict[str, Any]:
    """
    Get a list of ASNs that match a name.

    Parameters
    ----------
    name : str
        The name of the ASN to search for

    Returns
    -------
    Dict
        A list of ASNs that match the name.

    Raises
    ------
    MsticpyConnectionError
        If the list of all ASNs cannot be retrieved.
    MsticpyException
        If no matches found.

    """
    name = name.casefold()
    asns_dict: dict[str, str] = _ASNS_DICT()
    matches: dict[str, str] = {
        key: value for key, value in asns_dict.items() if name in value.casefold()
    }
    if len(matches.keys()) == 1:
        return next(iter(matches))  # type:ignore[arg-type]
    if len(matches.keys()) > 1:
        return matches

    err_msg: str = f"No ASNs found matching {name}"
    raise MsticpyException(err_msg)


def get_asn_from_ip(
    ip: str | IpAddress | None = None,
    ip_address: str | IpAddress | None = None,
) -> dict[str, Any]:
    """
    Get the ASN that an IP belongs to.

    Parameters
    ----------
    ip : Union[str, IpAddress]
        IP address to lookup.
    ip_address : Union[str, IpAddress]
        IP address to lookup (alias of `ip`).

    Returns
    -------
    dict
        Details of the ASN that the IP belongs to.

    """
    ip_param: str | IpAddress | None = ip or ip_address
    if not ip_param:
        return {}
    if isinstance(ip_param, IpAddress):
        ip_param = ip_param.Address
    ip_str: str = ip_param.strip()
    query: str = f" -v {ip_str}\r\n"
    ip_response: str = _cymru_query(query)
    keys: list[str] = ip_response.split("\n", maxsplit=1)[0].split("|")
    values: list[str] = ip_response.split("\n")[1].split("|")
    return {key.strip(): value.strip() for key, value in zip(keys, values)}


@dataclass
class _IpWhoIsResult:
    """Named tuple for IPWhoIs Result."""

    name: str | None = None
    properties: dict[str, Any] = field(default_factory=dict)


@lru_cache(maxsize=1024)
def _whois_lookup(
    ip_addr: str | IpAddress,
    *,
    raw: bool = False,
    retry_count: int = 5,
) -> _IpWhoIsResult:
    """Conduct lookup of IP Whois information."""
    if isinstance(ip_addr, IpAddress):
        ip_addr = ip_addr.Address
    asn_items: dict[str, Any] = get_asn_from_ip(ip_addr.strip())
    registry_url: str | None = None
    if asn_items and "Error: no ASN or IP match on line 1." not in asn_items:
        ipwhois_result: _IpWhoIsResult = _IpWhoIsResult(asn_items["AS Name"], {})
        ipwhois_result.properties["asn"] = asn_items["AS"]
        ipwhois_result.properties["query"] = asn_items["IP"]
        ipwhois_result.properties["asn_cidr"] = asn_items["BGP Prefix"]
        ipwhois_result.properties["asn_country_code"] = asn_items["CC"]
        ipwhois_result.properties["asn_registry"] = asn_items["Registry"]
        ipwhois_result.properties["asn_date"] = asn_items["Allocated"]
        ipwhois_result.properties["asn_description"] = asn_items["AS Name"]
        registry_url = _REGISTRIES.get(asn_items.get("Registry", ""), {}).get("url")
    if not asn_items or not registry_url:
        return _IpWhoIsResult(None)
    return _add_rdap_data(
        ipwhois_result=ipwhois_result,
        rdap_reg_url=f"{registry_url}{ip_addr}",
        retry_count=retry_count,
        raw=raw,
    )


def _add_rdap_data(
    ipwhois_result: _IpWhoIsResult,
    rdap_reg_url: str,
    retry_count: int,
    *,
    raw: bool,
) -> _IpWhoIsResult:
    """Add RDAP data to WhoIs result."""
    retries = 0
    while retries < retry_count:
        rdap_data: httpx.Response = _rdap_lookup(
            url=rdap_reg_url,
            retry_count=retry_count,
        )
        if rdap_data.is_success:
            rdap_data_content: dict[str, Any] = rdap_data.json()
            net: dict[str, Any] = _create_net(rdap_data_content)
            ipwhois_result.properties["nets"] = [net]
            for link in rdap_data_content["links"]:
                if link["rel"] == "up":
                    up_data_link: str = link["href"]
                    up_rdap_data: httpx.Response = httpx.get(up_data_link)
                    up_rdap_data_content: dict[str, Any] = up_rdap_data.json()
                    up_net: dict[str, Any] = _create_net(up_rdap_data_content)
                    ipwhois_result.properties["nets"].append(up_net)
            if raw:
                ipwhois_result.properties["raw"] = rdap_data
            break
        if rdap_data.status_code == httpx.codes.TOO_MANY_REQUESTS:
            sleep(3)
            retries += 1
            continue
        err_msg: str = f"Error code: {rdap_data.status_code}"
        raise MsticpyConnectionError(err_msg)
    return ipwhois_result


def _rdap_lookup(url: str, retry_count: int = 5) -> httpx.Response:
    """Perform RDAP lookup with retries."""
    rdap_data: httpx.Response | None = None
    while retry_count > 0 and not rdap_data:
        rdap_data = _run_rdap_query(url)
        retry_count -= 1
    if not rdap_data:
        err_msg: str = (
            "Rate limit exceeded - try adjusting query_rate parameter "
            "to slow down requests"
        )
        raise MsticpyException(err_msg)
    return rdap_data


def _run_rdap_query(url: str) -> httpx.Response | None:
    """Execute rdap query call and handle errors."""
    try:
        return httpx.get(url)
    except (httpx.WriteError, httpx.ReadError):
        return None


def _whois_result_to_pandas(results: str | list[str] | dict[str, Any]) -> pd.DataFrame:
    """Transform whois results to a Pandas DataFrame."""
    if isinstance(results, dict):
        return pd.DataFrame(
            [result or {"query": idx} for idx, result in results.items()],
        )
    err_msg: str = "Only dict type current supported for `results`."
    raise NotImplementedError(err_msg)


def _find_address(
    entity: dict,
) -> str | None:
    """Find an orgs address from an RDAP entity."""
    if "vcardArray" not in entity:
        return None
    for vcard in [vcard for vcard in entity["vcardArray"] if isinstance(vcard, list)]:
        for vcard_sub in vcard:
            if vcard_sub[0] == "adr" and "label" in vcard_sub[1]:
                return vcard_sub[1]["label"]
    return None


def _create_net(data: dict[str, Any]) -> dict[str, Any]:
    """Create a network object from RDAP data."""
    net_data: dict[str, Any] = data.get("cidr0_cidrs", [None])[0] or {}
    net_prefixes: set[str] = net_data.keys() & {"v4prefix", "v6prefix"}

    if not net_data or not net_prefixes:
        net_cidr: str = "No network data retrieved."
    else:
        net_cidr = " ".join(
            f"{net_data[net_prefix]}/{net_data.get('length', '')}"
            for net_prefix in net_prefixes
        )
    address: str | None = None
    created: str | None = None
    updated: str | None = None
    for item in data["events"]:
        created = item["eventDate"] if item["eventAction"] == "last changed" else None
        updated = item["eventDate"] if item["eventAction"] == "registration" else None
    for entity in data["entities"]:
        address = _find_address(entity)
    regex = r"[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9-]+(?:\.[a-zA-Z0-9-]+)*"
    emails: list[str] = re.findall(regex, str(data))
    return {
        "cidr": net_cidr,
        "handle": data["handle"],
        "name": data["name"],
        "startAddress": data["startAddress"],
        "endAddress": data["endAddress"],
        "created": created,
        "updated": updated,
        "address": address,
        "emails": emails,
    }


def _asn_whois_query(
    query: str,
    server: str,
    port: int = 43,
    retry_count: int = 5,
) -> str:
    """Connect to whois server and send query."""
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as conn:
        conn.connect((server, port))
        conn.send(query.encode())
        response: list[str] = []
        response_data: str | None = None
        while retry_count > 0 and not response_data:
            response_data = _run_asn_query(conn, response)
            retry_count -= 1
    return "".join(response)


def _run_asn_query(
    conn: socket.socket,
    response: list[str],
) -> str | None:
    """Execute asn query call and handle errors."""
    try:
        response_data: str = conn.recv(4096).decode()
    except (UnicodeDecodeError, ConnectionResetError):
        return None
    if "error" in response_data:
        err_msg: str = "An error occurred during lookup, please try again."
        raise MsticpyConnectionError(err_msg)
    if "rate limit exceeded" in response_data:
        err_msg = "Rate limit exceeded please wait and try again."
        raise MsticpyConnectionError(err_msg)
    response.append(response_data)
    return response_data


def _cymru_query(query: str) -> str:
    """Query Cymru for ASN information."""
    return _asn_whois_query(query, "whois.cymru.com")


def _radb_query(query: str) -> str:
    """Query RADB for ASN information."""
    return _asn_whois_query(query, "whois.radb.net")


def _parse_asn_response(response: str) -> dict[str, Any]:
    """Parse ASN response into a dictionary."""
    response_output: dict[str, Any] = {}
    for item in response.split("\n"):
        try:
            key: str = item.split(":")[0].strip()
            if key and key not in response_output:
                try:
                    response_output[key] = item.split(":")[1].strip()
                except IndexError:
                    continue
            elif key in response_output:
                if not isinstance(response_output[key], list):
                    response_output[key] = [response_output[key]]
                response_output[key].append(item.split(":")[1].strip())
        except IndexError:
            continue
    return response_output


def _asn_results(asn: str) -> dict[str, Any]:
    """Get ASN details from ASN number."""
    if not asn.startswith("AS"):
        asn = f"AS{asn}"
    query1: str = f" {asn}\r\n"
    asn_response: str = _radb_query(query1)
    asn_details: dict[str, Any] = _parse_asn_details(asn_response)
    query2: str = f" -i origin {asn}\r\n"
    asn_ranges_response: str = _radb_query(query2)
    asn_details["ranges"] = _parse_asn_ranges(asn_ranges_response)
    return asn_details


def _parse_asn_details(response: str) -> dict[str, Any]:
    """Parse ASN details response into a dictionary."""
    asn_keys: list[str] = ["aut-num", "as-name", "descr", "notify", "changed"]
    asn_output: dict[str, Any] = _parse_asn_response(response)
    asn_output_filtered: dict[str, Any] = {
        key: value for key, value in asn_output.items() if key in asn_keys
    }
    asn_output_filtered["Autonomous Number"] = asn_output_filtered.pop("aut-num", None)
    asn_output_filtered["AS Name"] = asn_output_filtered.pop("as-name", None)
    asn_output_filtered["Description"] = asn_output_filtered.pop("descr", None)
    asn_output_filtered["Contact"] = asn_output_filtered.pop("notify", None)
    asn_output_filtered["Last Updated"] = asn_output_filtered.pop("changed", None)
    return asn_output_filtered


def _parse_asn_ranges(response: str) -> list[str]:
    """Parse ASN ranges response into a list."""
    return [
        item.split(":   ")[1].strip()
        for item in response.split("\n")
        if item.split(":   ")[0].strip() == "route"
    ]
