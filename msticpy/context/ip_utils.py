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
import ipaddress
import re
import socket
import warnings
from functools import lru_cache
from time import sleep
from typing import Any, Dict, List, NamedTuple, Optional, Set, Tuple, Union

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

_POTAROO_ASNS_URL = "https://bgp.potaroo.net/cidr/autnums.html"


# Closure to cache ASN dictionary from Potaroo
def _fetch_asns():
    """Create closure for ASN fetching."""
    asns_dict: Dict[str, str] = {}

    def _get_asns_dict() -> Dict[str, str]:
        """Return or fetch and return ASN Soup."""
        nonlocal asns_dict  # noqa
        if not asns_dict:
            try:
                asns_resp = httpx.get(_POTAROO_ASNS_URL)
            except httpx.ConnectError as err:
                raise MsticpyConnectionError(
                    "Unable to get ASN details from potaroo.net"
                ) from err
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
_ASNS_DICT = _fetch_asns()


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
            print(ip_str)
            whois_result = ip_whois(ip_str)
            if show_progress:
                print(".", end="")
            return whois_result  # type: ignore
        except MsticpyException as err:
            return f"Error during lookup of {ip_str} {type(err)}", {}
    return f"No ASN Information for IP type: {ip_type}", {}


# pylint: enable=invalid-name


@export
def get_whois_df(
    data: pd.DataFrame,
    ip_column: str,
    all_columns: bool = True,
    asn_col: str = "AsnDescription",
    whois_col: Optional[str] = "WhoIsData",
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
    whois_data = ip_whois(data[ip_column].drop_duplicates())
    if (
        isinstance(whois_data, pd.DataFrame)
        and not whois_data.empty
        and "query" in whois_data.columns
    ):
        data = data.merge(
            whois_data,  # type: ignore
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


# pylint: disable=inconsistent-return-statements, invalid-name
def ip_whois(
    ip: Union[IpAddress, str, List, pd.Series, None] = None,
    ip_address: Union[IpAddress, str, List, pd.Series, None] = None,
    raw=False,
    query_rate: float = 0.5,
    retry_count: int = 5,
) -> Union[pd.DataFrame, Tuple]:
    # sourcery skip: assign-if-exp, reintroduce-else
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
        raise ValueError("One of ip or ip_address parameters must be supplied.")
    if isinstance(ip, (list, pd.Series)):
        rate_limit = len(ip) > 50
        if rate_limit:
            print("Large number of lookups, this may take some time.")
        whois_results: Dict[str, Any] = {}
        for ip_addr in ip:
            if rate_limit:
                sleep(query_rate)
            whois_results[ip_addr] = _whois_lookup(
                ip_addr, raw=raw, retry_count=retry_count
            ).properties
        return _whois_result_to_pandas(whois_results)
    if isinstance(ip, (str, IpAddress)):
        return _whois_lookup(ip, raw=raw)
    return {}


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
        Details about the ASN(s) if a single ASN provided, result is a dictionary.
        If multiple provided, results are returned as a DataFrame.

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
    asns_dict = _ASNS_DICT()
    matches = {
        key: value for key, value in asns_dict.items() if name in value.casefold()
    }
    if len(matches.keys()) == 1:
        return next(iter(matches))  # type: ignore
    if len(matches.keys()) > 1:
        return matches

    raise MsticpyException(f"No ASNs found matching {name}")


def get_asn_from_ip(
    ip: Union[str, IpAddress, None] = None,
    ip_address: Union[str, IpAddress, None] = None,
) -> Dict:
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
    ip = ip or ip_address
    if not ip:
        return {}
    if isinstance(ip, IpAddress):
        ip = ip.Address
    ip = ip.strip()
    query = f" -v {ip}\r\n"
    ip_response = _cymru_query(query)
    keys = ip_response.split("\n", maxsplit=1)[0].split("|")
    values = ip_response.split("\n")[1].split("|")
    return {key.strip(): value.strip() for key, value in zip(keys, values)}


class _IpWhoIsResult(NamedTuple):
    """Named tuple for IPWhoIs Result."""

    name: Optional[str]
    properties: Dict[str, Any] = {}


@lru_cache(maxsize=1024)
def _whois_lookup(
    ip_addr: Union[str, IpAddress], raw: bool = False, retry_count: int = 5  # type: ignore
) -> _IpWhoIsResult:
    """Conduct lookup of IP Whois information."""
    if isinstance(ip_addr, IpAddress):  # type: ignore
        ip_addr = ip_addr.Address
    asn_items = get_asn_from_ip(ip_addr.strip())
    registry_url: Optional[str] = None
    if asn_items and "Error: no ASN or IP match on line 1." not in asn_items:
        ipwhois_result = _IpWhoIsResult(asn_items["AS Name"], {})  # type: ignore
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
        ipwhois_result=ipwhois_result,  # type: ignore
        rdap_reg_url=f"{registry_url}{ip_addr}",
        retry_count=retry_count,
        raw=raw,
    )


def _add_rdap_data(
    ipwhois_result: _IpWhoIsResult, rdap_reg_url: str, retry_count: int, raw: bool
) -> _IpWhoIsResult:
    """Add RDAP data to WhoIs result."""
    retries = 0
    while retries < retry_count:
        rdap_data = _rdap_lookup(url=rdap_reg_url, retry_count=retry_count)
        if rdap_data.status_code == 200:
            rdap_data_content = rdap_data.json()
            net = _create_net(rdap_data_content)
            ipwhois_result.properties["nets"] = [net]
            for link in rdap_data_content["links"]:
                if link["rel"] == "up":
                    up_data_link = link["href"]
                    up_rdap_data = httpx.get(up_data_link)
                    up_rdap_data_content = up_rdap_data.json()
                    up_net = _create_net(up_rdap_data_content)
                    ipwhois_result.properties["nets"].append(up_net)
            if raw:
                ipwhois_result.properties["raw"] = rdap_data
            break
        if rdap_data.status_code == 429:
            sleep(3)
            retries += 1
            continue
        raise MsticpyConnectionError(f"Error code: {rdap_data.status_code}")
    return ipwhois_result


def _rdap_lookup(url: str, retry_count: int = 5) -> httpx.Response:
    """Perform RDAP lookup with retries."""
    rdap_data = None
    while retry_count > 0 and not rdap_data:
        try:
            rdap_data = httpx.get(url)
        except (httpx.WriteError, httpx.ReadError):
            retry_count -= 1
    if not rdap_data:
        raise MsticpyException(
            "Rate limit exceeded - try adjusting query_rate parameter to slow down requests"
        )
    return rdap_data


def _whois_result_to_pandas(results: Union[str, List, Dict]) -> pd.DataFrame:
    """Transform whois results to a Pandas DataFrame."""
    if isinstance(results, dict):
        return pd.DataFrame(
            [result or {"query": idx} for idx, result in results.items()]
        )
    raise NotImplementedError("Only dict type current supported for `results`.")


def _find_address(
    entity: dict,
) -> Union[str, None]:  # pylint: disable=inconsistent-return-statements
    """Find an orgs address from an RDAP entity."""
    if "vcardArray" not in entity:
        return None
    for vcard in [vcard for vcard in entity["vcardArray"] if isinstance(vcard, list)]:
        for vcard_sub in vcard:
            if vcard_sub[0] == "adr":
                return vcard_sub[1]["label"]
    return None


def _create_net(data: Dict) -> Dict:
    """Create a network object from RDAP data."""
    net_data = data.get("cidr0_cidrs", [None])[0] or {}
    net_prefixes = net_data.keys() & {"v4prefix", "v6prefix"}

    if not net_data or not net_prefixes:
        net_cidr = "No network data retrieved."
    else:
        net_cidr = " ".join(
            f"{net_data[net_prefix]}/{net_data.get('length', '')}"
            for net_prefix in net_prefixes
        )
    address = ""
    created = updated = None
    for item in data["events"]:
        created = item["eventDate"] if item["eventAction"] == "last changed" else None
        updated = item["eventDate"] if item["eventAction"] == "registration" else None
    for entity in data["entities"]:
        address = _find_address(entity)  # type: ignore
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


def _asn_whois_query(query, server, port=43, retry_count=5) -> str:
    """Connect to whois server and send query."""
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as conn:
        conn.connect((server, port))
        conn.send(query.encode())
        response = []
        response_data = None
        while retry_count > 0 and not response_data:
            try:
                response_data = conn.recv(4096).decode()
                if "error" in response_data:
                    raise MsticpyConnectionError(
                        "An error occurred during lookup, please try again."
                    )
                if "rate limit exceeded" in response_data:
                    raise MsticpyConnectionError(
                        "Rate limit exceeded please wait and try again."
                    )
                response.append(response_data)
            except (UnicodeDecodeError, ConnectionResetError):
                retry_count -= 1
                response_data = None
    return "".join(response)


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
    return [
        item.split(":   ")[1].strip()
        for item in response.split("\n")
        if item.split(":   ")[0].strip() == "route"
    ]
