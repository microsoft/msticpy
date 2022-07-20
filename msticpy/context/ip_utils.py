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
import re
import socket
import warnings
from functools import lru_cache
from time import sleep
from typing import Dict, List, Optional, Set, Tuple, Union

import httpx
import pandas as pd
from bs4 import BeautifulSoup
from deprecated.sphinx import deprecated

from .._version import VERSION
from ..common.exceptions import MsticpyConnectionError, MsticpyException
from ..common.utility import arg_to_list, export
from ..datamodel.entities import GeoLocation, IpAddress

__version__ = VERSION
__author__ = "Ashwin Patil"


_REGISTRIES = {
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
_ASNS = httpx.get("https://bgp.potaroo.net/cidr/autnums.html")
_ASNS_SOUP = BeautifulSoup(_ASNS.content)


@export  # noqa: MC0001
def convert_to_ip_entities(  # noqa: MC0001
    ip_str: Optional[str] = None,
    data: Optional[pd.DataFrame] = None,
    ip_col: Optional[str] = None,
    geo_lookup: bool = True,
) -> List[IpAddress]:  # noqa: MC0001
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

    geo_lite_lookup = GeoLiteLookup()

    ip_entities: List[IpAddress] = []
    all_ips: Set[str] = set()

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
                geo_lite_lookup.lookup_ip(ip_entity=ip_ent)
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
@deprecated("Replaced with ip_whois function", version="2.1.0")
@export
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
            whois_result = ip_whois(ip_str)
            if show_progress:
                print(".", end="")
            return whois_result["asn_description"], whois_result
        except (MsticpyException,) as err:
            return f"Error during lookup of {ip_str} {type(err)}", {}
    return f"No ASN Information for IP type: {ip_type}", {}


# pylint: enable=invalid-name


@export
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


@pd.api.extensions.register_dataframe_accessor("mp_whois")
@export
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
        warn_message = (
            "This accessor method has been deprecated.\n"
            "Please use IpAddress.util.whois() pivot function."
            "This will be removed in MSTICPy v2.2.0"
        )
        warnings.warn(warn_message, category=DeprecationWarning)
        return get_whois_df(data=self._df, ip_column=ip_column, **kwargs)


@deprecated("Will be removed in a version 2.2", version="1.4.0")
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
    ip_entity.OSVMajorVersion = ip_hb["OSMajorVersion"]  # type: ignore
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


def ip_whois(  # pylint: disable=inconsistent-return-statements
    ips: Union[IpAddress, str, List], raw=False
) -> Union[pd.DataFrame, Dict]:
    """
    Lookup IP Whois information.

    Parameters
    ----------
    ips : Union[IpAddress, str, List]
        An IP address or list of IP addresses to lookup.
    raw : bool, optional
        Set True if raw whois result wanted, by default False

    Returns
    -------
    Union[pd.DataFrame, Dict]
        If a single IP Address provided result is a dictionary
        if multipe provided results are formatted as a dataframe.

    """
    if isinstance(ips, list):
        whois_results = []
        for ip_address in ips:
            if len(ips) > 50:
                sleep(1)
            whois_results.append(_whois_lookup(ip_address, raw=raw))
        return _whois_result_to_pandas(whois_results)
    if isinstance(ips, (str, IpAddress)):
        return _whois_lookup(ips, raw=raw)


def _whois_lookup(ip_addr: Union[str, IpAddress], raw=False) -> Dict:
    """Conduct lookup of IP Whois information"""
    if isinstance(ip_addr, IpAddress):
        ip_addr = ip_addr.Address
    asn_details = get_asn_from_ip(ip_addr.strip())
    keys = asn_details.split("\n")[0].split("|")
    values = asn_details.split("\n")[1].split("|")
    asn_items = {key.strip(): value.strip() for key, value in zip(keys, values)}
    ipwhois_result = (asn_items["AS Name"], {})
    ipwhois_result[1]["asn"] = asn_items["AS"]
    ipwhois_result[1]["query"] = asn_items["IP"]
    ipwhois_result[1]["asn_cidr"] = asn_items["BGP Prefix"]
    ipwhois_result[1]["asn_country_code"] = asn_items["CC"]
    ipwhois_result[1]["asn_registry"] = asn_items["Registry"]
    ipwhois_result[1]["asn_date"] = asn_items["Allocated"]
    ipwhois_result[1]["asn_description"] = asn_items["AS Name"]
    registry_urls = _REGISTRIES[ipwhois_result[1]["asn_registry"]]
    rdap_data = httpx.get(registry_urls["url"] + str(ip_addr))
    if rdap_data.status_code == 200:
        rdap_data = rdap_data.json()
        net = _create_net(rdap_data)
        ipwhois_result[1]["nets"] = [net]
        for link in rdap_data["links"]:
            if link["rel"] == "up":
                up_data_link = link["href"]
                up_rdap_data = httpx.get(up_data_link)
                up_rdap_data = up_rdap_data.json()
                up_net = _create_net(up_rdap_data)
                ipwhois_result[1]["nets"].append(up_net)
        if raw:
            ipwhois_result[1]["raw"] = rdap_data
    elif rdap_data.status_code == 429:
        sleep(5)
        _whois_lookup(ip_addr)
    else:
        raise MsticpyConnectionError(f"Error code: {rdap_data.status_code}")
    return ipwhois_result


def _whois_result_to_pandas(results: Union[str, List]) -> pd.DataFrame:
    """Transfroms whois results to a Pandas DataFrame."""
    if isinstance(results, list):
        new_results = [results[1] for results in results]
    else:
        new_results = results[1]
    return pd.DataFrame(new_results)


def _find_address(
    entity: dict,
) -> str:  # pylint: disable=inconsistent-return-statements
    """Find an orgs address from an RDAP entity."""
    if "vcardArray" in entity:
        for vcard in entity["vcardArray"]:
            if isinstance(vcard, list):
                for vcard_sub in vcard:
                    if vcard_sub[0] == "adr":
                        return vcard_sub[1]["label"]
    return None


def _create_net(data: Dict) -> Dict:
    """Create a network object from RDAP data."""
    net_cidr = (
        str(data["cidr0_cidrs"][0]["v4prefix"])
        + "/"
        + str(data["cidr0_cidrs"][0]["length"])
        if "cidr0_cidrs" in data
        else None
    )
    address = ""
    for item in data["events"]:
        created = item["eventDate"] if item["eventAction"] == "last changed" else None
        updated = item["eventDate"] if item["eventAction"] == "registration" else None
    for entity in data["entities"]:
        address = _find_address(entity)
    regex = r"[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9-]+(?:\.[a-zA-Z0-9-]+)*"
    emails = re.findall(regex, str(data))
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


def get_asn_details(asns: Union[str, List]) -> Union[pd.DataFrame, Dict]:
    """
    Get details about an ASN(s) from its number.

    Parameters
    ----------
    asns : Union[str, List]
        A single ASN or list of ASNs to lookup.

    Returns
    -------
    Union[pd.DataFrame, Dict]
        Details about the ASN(s) if a single ASN provided result is a dictionary
        if multlete provided results are formatted as a DataFrame.

    """
    if isinstance(asns, list):
        asn_detail_results = [_asn_results(str(asn)) for asn in asns]
        return pd.DataFrame(asn_detail_results)
    return _asn_results(str(asns))


def get_asn_from_name(name: str) -> Dict:
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
    asns_dict = {}
    try:
        for asn in _ASNS_SOUP.find_all("a"):
            asns_dict[str(asn.next_element).strip()] = str(
                asn.next_element.next_element
            ).strip()
    except httpx.ConnectError as err:
        raise MsticpyConnectionError("Unable to get ASN details") from err
    matches = {
        key: value for key, value in asns_dict.items() if name in value.casefold()
    }
    if len(matches.keys()) == 1:
        return next(iter(matches))
    if len(matches.keys()) > 1:
        return matches

    raise MsticpyException(f"No ASNs found matching {name}")


def get_asn_from_ip(ip_addr: Union[str, IpAddress]) -> Dict:
    """
    Get the ASN that an IP belongs to.

    Parameters
    ----------
    ip : Union[str, IpAddress]
        IP address to lookup.

    Returns
    -------
    dict
        Details of the ASN that the IP belongs to.

    """
    if isinstance(ip_addr, IpAddress):
        ip_addr = ip_addr.Address
    ip_addr = ip_addr.strip()
    query = f" -v {ip_addr}\r\n"
    ip_response = _cymru_query(query)
    keys = ip_response.split("\n")[0].split("|")
    values = ip_response.split("\n")[1].split("|")
    return {key.strip(): value.strip() for key, value in zip(keys, values)}


def _asn_whois_query(query, server, port=43):
    """Connect to whois server and send query."""
    conn = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    conn.connect((server, port))
    conn.send(query.encode())
    response = ""
    while True:
        response_data = conn.recv(4096).decode()
        if "error" in response_data:
            raise MsticpyConnectionError(
                "An error occured during lookup, please try again."
            )
        if "rate limit exceeded" in response_data:
            raise MsticpyConnectionError(
                "Rate limit exceeded please wait and try again."
            )
        response += response_data
        if not response_data:
            break
    conn.close()
    return response


def _cymru_query(query):
    """Query Cymru for ASN information."""
    return _asn_whois_query(query, "whois.cymru.com")


def _radb_query(query):
    """Query RADB for ASN information."""
    return _asn_whois_query(query, "whois.radb.net")


def _parse_asn_response(response) -> dict:
    """Parse ASN response into a dictionary."""
    response_output = {}
    for item in response.split("\n"):
        try:
            key = item.split(":")[0].strip()
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


def _asn_results(asn: str) -> dict:
    """Get ASN details from ASN number."""
    if not asn.startswith("AS"):
        asn = f"AS{asn}"
    query1 = f" {asn}\r\n"
    asn_response = _radb_query(query1)
    asn_details = _parse_asn_details(asn_response)
    query2 = f" -i origin {asn}\r\n"
    asn_ranges_response = _radb_query(query2)
    asn_details["ranges"] = _parse_asn_ranges(asn_ranges_response)
    return asn_details


def _parse_asn_details(response):
    """Parse ASN details response into a dictionary."""
    asn_keys = ["aut-num", "as-name", "descr", "notify", "changed"]
    asn_output = _parse_asn_response(response)
    asn_output_filtered = {
        key: value for key, value in asn_output.items() if key in asn_keys
    }
    asn_output_filtered["Autonomous Number"] = asn_output_filtered.pop("aut-num", None)
    asn_output_filtered["AS Name"] = asn_output_filtered.pop("as-name", None)
    asn_output_filtered["Description"] = asn_output_filtered.pop("descr", None)
    asn_output_filtered["Contact"] = asn_output_filtered.pop("notify", None)
    asn_output_filtered["Last Updated"] = asn_output_filtered.pop("changed", None)
    return asn_output_filtered


def _parse_asn_ranges(response):
    """Parse ASN ranges response into a list."""
    ranges = []
    for item in response.split("\n"):
        if item.split(":   ")[0].strip() == "route":
            ranges.append(item.split(":   ")[1].strip())
    return ranges
