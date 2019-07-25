# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
Geoip Lookup module using IPStack and Maxmind GeoLite2.

Geographic location lookup for IP addresses. This module has two classes
for different services:

-  GeoLiteLookup - Maxmind Geolite (see https://www.maxmind.com)
-  IPStackLookup - IPStack (see https://ipstack.com) Both services offer
   a free tier for non-commercial use. However, a paid tier will
   normally get you more accuracy, more detail and a higher throughput
   rate. Maxmind geolite uses a downloadable database, while IPStack is
   an online lookup (API key required).

"""
# import gzip
from json import JSONDecodeError
import math
import os
from abc import ABCMeta, abstractmethod
from collections.abc import Iterable
from datetime import datetime, timedelta
from typing import Tuple, List, Dict
from IPython import get_ipython
from IPython.display import display, HTML

import pandas as pd
import requests

from geolite2 import geolite2

from ..nbtools.entityschema import GeoLocation, IpAddress
from ..nbtools.utility import export
from .._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"


class GeoIpLookup(metaclass=ABCMeta):
    """
    Abstract base class for GeoIP Lookup classes.

    See Also
    --------
    IPStackLookup : IPStack GeoIP Implementation
    GeoLiteLookup : MaxMind GeoIP Implementation

    """

    @abstractmethod
    def lookup_ip(
        self,
        ip_address: str = None,
        ip_addr_list: Iterable = None,
        ip_entity: IpAddress = None,
    ) -> Tuple[List[Tuple[Dict[str, str], int]], List[IpAddress]]:
        """
        Lookup IP location abstract method.

        Parameters
        ----------
        ip_address : str, optional
            a single address to look up (the default is None)
        ip_addr_list : Iterable, optional
            a collection of addresses to lookup (the default is None)
        ip_entity : IpAddress, optional
            an IpAddress entity (the default is None) - any existing
            data in the Location property will be overwritten

        Returns
        -------
        Tuple[List[Mapping[str, str]], List[IpAddress]]
            raw geolocation results and same results as IpAddress entities with
            populated Location property.

        """

    def df_lookup_ip(self, data: pd.DataFrame, column: str) -> pd.DataFrame:
        """
        Lookup Geolocation data from a pandas Dataframe.

        Parameters
        ----------
        data : pd.DataFrame
            pandas dataframe containing IpAddress column
        column : str
            the name of the dataframe column to use as a source

        Returns
        -------
        pd.DataFrame
            Copy of original dataframe with IP Location information columns
            appended (where a location lookup was successful)

        """
        ip_list = data[column].values
        _, entities = self.lookup_ip(ip_addr_list=ip_list)

        ip_dicts = [
            ent.Location.properties().update(IpAddress=ent.Address) for ent in entities
        ]
        df_out = pd.DataFrame(data=ip_dicts)
        return data.merge(df_out, how="left", left_on=column, right_on="IpAddress")


@export
class IPStackLookup(GeoIpLookup):
    """
    IPStack GeoIP Implementation.

    See Also
    --------
    GeoIpLookup : Abstract base class
    GeoLiteLookup : MaxMind GeoIP Implementation

    """

    _IPSTACK_API = "http://api.ipstack.com/{iplist}?access_key={access_key}&output=json"

    def __init__(self, api_key: str, bulk_lookup: bool = False):
        """
        Create a new instance of IPStackLookup.

        Parameters
        ----------
        api_key : str
            API Key from IPStack - see https://ipstack.com
        bulk_lookup : bool, optional
            For Professional and above tiers allowing you to
            submit multiple IPs in a single request.
            (the default is False, which submits a single request
            per address)

        """
        self._api_key = api_key
        self.bulk_lookup = bulk_lookup

    def lookup_ip(
        self,
        ip_address: str = None,
        ip_addr_list: Iterable = None,
        ip_entity: IpAddress = None,
    ) -> Tuple[List[Tuple[Dict[str, str], int]], List[IpAddress]]:
        """
        Lookup IP location from IPStack web service.

        Parameters
        ----------
        ip_address : str, optional
            a single address to look up (the default is None)
        ip_addr_list : Iterable, optional
            a collection of addresses to lookup (the default is None)
        ip_entity : IpAddress, optional
            an IpAddress entity (the default is None) - any existing
            data in the Location property will be overwritten

        Returns
        -------
        Tuple[List[Mapping[str, str]], List[IpAddress]]
            raw geolocation results and same results as IpAddress entities with
            populated Location property.

        Raises
        ------
        ConnectionError
            Invalid status returned from http request
        PermissionError
            Service refused request (e.g. requesting batch of addresses
            on free tier API key)

        """
        if ip_address and isinstance(ip_address, str):
            ip_list = [ip_address.strip()]
        elif ip_addr_list:
            ip_list = list((ip.strip() for ip in ip_addr_list))
        elif ip_entity:
            ip_list = [ip_entity.Address]
        else:
            raise ValueError("No valid ip addresses were passed as arguments.")

        results = self._submit_request(ip_list)
        output_entities = []
        for ip_loc, status in results:
            if status == 200:
                output_entities.append(self._create_ip_entity(ip_loc, ip_entity))

        return results, output_entities

    @staticmethod
    def _create_ip_entity(ip_loc: dict, ip_entity) -> IpAddress:
        if not ip_entity:
            ip_entity = IpAddress()
            ip_entity.Address = ip_loc["ip"]
        geo_entity = GeoLocation()
        geo_entity.CountryCode = ip_loc["country_code"]

        geo_entity.CountryName = ip_loc["country_name"]
        geo_entity.State = ip_loc["region_name"]
        geo_entity.City = ip_loc["city"]
        geo_entity.Longitude = ip_loc["longitude"]
        geo_entity.Latitude = ip_loc["latitude"]
        if "connection" in ip_loc:
            geo_entity.Asn = ip_loc["connection"]["asn"]
        ip_entity.Location = geo_entity
        return ip_entity

    def _submit_request(self, ip_list: List[str]) -> List[Tuple[Dict[str, str], int]]:
        """
        Submit the request to IPStack.

        Parameters
        ----------
        ip_list : List[str]
            String list of IPs to look up

        Returns
        -------
        List[Tuple[str, int]]
            List of response, status code pairs

        """
        if not self.bulk_lookup:
            return self._lookup_ip_list(ip_list)

        submit_url = self._IPSTACK_API.format(
            iplist=",".join(ip_list), access_key=self._api_key
        )
        response = requests.get(submit_url)

        if response.status_code == 200:
            results = response.json()
            # {"success":false,"error":{"code":303,"type":"batch_not_supported_on_plan",
            # "info":"Bulk requests are not supported on your plan.
            # Please upgrade your subscription."}}

            if "success" in results and not results["success"]:
                raise PermissionError(
                    "Service unable to complete request. Error: {}".format(
                        results["error"]
                    )
                )
            return [(item, response.status_code) for item in results]

        if response:
            try:
                return [(response.json(), response.status_code)]
            except JSONDecodeError:
                pass
        return [({}, response.status_code)]

    def _lookup_ip_list(self, ip_list: List[str]):
        """Lookup IP Addresses one-by-one."""
        ip_loc_results = []
        with requests.Session() as session:
            for ip_addr in ip_list:
                submit_url = self._IPSTACK_API.format(
                    iplist=ip_addr, access_key=self._api_key
                )
                response = session.get(submit_url)
                if response.status_code == 200:
                    ip_loc_results.append((response.json(), response.status_code))
                else:
                    if response:
                        try:
                            ip_loc_results.append(
                                (response.json(), response.status_code)
                            )
                            continue
                        except JSONDecodeError:
                            ip_loc_results.append((None, response.status_code))
                    else:
                        print("Unknown response from IPStack request.")
                        ip_loc_results.append((None, -1))
        return ip_loc_results


@export
class GeoLiteLookup(GeoIpLookup):
    """
    GeoIP Lookup using MaxMindDB database.

    See Also
    --------
    GeoIpLookup : Abstract base class
    IPStackLookup : IPStack GeoIP Implementation

    """

    _MAXMIND_DOWNLOAD = "https://dev.maxmind.com/geoip/geoip2/geolite2/#Downloads"
    _DB_FILE = "GeoLite2-City.mmdb"

    # Check for out of date file
    _last_mod_time = datetime.fromtimestamp(os.path.getmtime(geolite2.filename))
    _db_age = datetime.utcnow() - _last_mod_time
    if _db_age > timedelta(40):
        print(f"{_DB_FILE} is over one month old. Update the maxminddb package")
        print(f"to refresh or download a new version from {_MAXMIND_DOWNLOAD}")

    def __init__(self):
        """Return new instance of GeoLiteLookup class."""
        self._reader = geolite2.reader()

    def lookup_ip(
        self,
        ip_address: str = None,
        ip_addr_list: Iterable = None,
        ip_entity: IpAddress = None,
    ) -> Tuple[List[Tuple[Dict[str, str], int]], List[IpAddress]]:
        """
        Lookup IP location from IPStack web service.

        Parameters
        ----------
        ip_address : str, optional
            a single address to look up (the default is None)
        ip_addr_list : Iterable, optional
            a collection of addresses to lookup (the default is None)
        ip_entity : IpAddress, optional
            an IpAddress entity (the default is None) - any existing
            data in the Location property will be overwritten

        Returns
        -------
        Tuple[List[Mapping[str, str]], List[IpAddress]]
            raw geolocation results and same results as IpAddress entities with
            populated Location property.

        """
        if ip_address and isinstance(ip_address, str):
            ip_list = [ip_address.strip()]
        elif ip_addr_list:
            ip_list = list((ip.strip() for ip in ip_addr_list))
        elif ip_entity:
            ip_list = [ip_entity.Address]
        else:
            raise ValueError("No valid ip addresses were passed as arguments.")

        output_raw = []
        output_entities = []
        for ip_input in ip_list:
            geo_match = self._reader.get(ip_input)
            if geo_match:
                output_raw.append(geo_match)
                output_entities.append(
                    self._create_ip_entity(ip_input, geo_match, ip_entity)
                )

        return output_raw, output_entities

    @staticmethod
    def _create_ip_entity(ip_address, geo_match: dict, ip_entity) -> IpAddress:
        if not ip_entity:
            ip_entity = IpAddress()
            ip_entity.Address = ip_address
        geo_entity = GeoLocation()
        geo_entity.CountryCode = geo_match.get("country", {}).get("iso_code", None)
        geo_entity.CountryName = (
            geo_match.get("country", {}).get("names", {}).get("en", None)
        )
        subdivs = geo_match.get("subdivisions", [])
        if subdivs:
            geo_entity.State = subdivs[0].get("names", {}).get("en", None)
        geo_entity.City = geo_match.get("city", {}).get("names", {}).get("en", None)
        geo_entity.Longitude = geo_match.get("location", {}).get("longitude", None)
        geo_entity.Latitude = geo_match.get("location", {}).get("latitude", None)
        ip_entity.Location = geo_entity
        return ip_entity


_MM_LICENSE_HTML = """
This product includes GeoLite2 data created by MaxMind, available from
<a href="https://www.maxmind.com">https://www.maxmind.com</a>.
"""
_MM_LICENSE_TXT = """
This product includes GeoLite2 data created by MaxMind, available from
https://www.maxmind.com.
"""
_IPSTACK_LICENSE_HTML = """
This library uses services provided by ipstack.
<a href="https://ipstack.com">https://ipstack.com</a>"""

_IPSTACK_LICENSE_TXT = """
This library uses services provided by ipstack (https://ipstack.com)"""

if not get_ipython():
    print(_MM_LICENSE_TXT)
    print(_IPSTACK_LICENSE_TXT)
else:
    display(HTML(_MM_LICENSE_HTML))
    display(HTML(_IPSTACK_LICENSE_HTML))


@export
def entity_distance(ip_src: IpAddress, ip_dest: IpAddress) -> float:
    """
    Return distance between two IP Entities.

    Parameters
    ----------
    ip_src : IpAddress
        Source/Origin IpAddress Entity
    ip_dest : IpAddress
        Destination IpAddress Entity

    Returns
    -------
    float
        Distance in kilometers.

    Raises
    ------
        AttributeError
            If either entity has no location information

    """
    if not ip_src.Location or not ip_dest.Location:
        raise AttributeError(
            "Source and destination entities must have defined Location properties."
        )

    return geo_distance(
        origin=(ip_src.Location.Latitude, ip_src.Location.Longitude),
        destination=(ip_dest.Location.Latitude, ip_dest.Location.Longitude),
    )


_EARTH_RADIUS_KM = 6371  # km


@export
def geo_distance(
    origin: Tuple[float, float], destination: Tuple[float, float]
) -> float:
    """
    Calculate the Haversine distance.

    Parameters
    ----------
    origin : Tuple[float, float]
        Latitude, Longitude of origin of distance measurement.
    destination : Tuple[float, float]
        Latitude, Longitude of origin of distance measurement.

    Returns
    -------
    float
        Distance in kilometers.

    Examples
    --------
    >>> origin = (48.1372, 11.5756)  # Munich
    >>> destination = (52.5186, 13.4083)  # Berlin
    >>> round(geo_distance(origin, destination), 1)
    504.2

    Notes
    -----
        Author: Martin Thoma - stackoverflow

    """
    orig_lat, orig_lon = origin
    dest_lat, dest_lon = destination

    ang_dist_lat = math.radians(dest_lat - orig_lat)
    ang_dist_lon = math.radians(dest_lon - orig_lon)
    hav_a = (math.sin(ang_dist_lat / 2) * math.sin(ang_dist_lat / 2)) + (
        math.cos(math.radians(orig_lat))
        * math.cos(math.radians(dest_lat))
        * math.sin(ang_dist_lon / 2)
        * math.sin(ang_dist_lon / 2)
    )
    hav_c = 2 * math.atan2(math.sqrt(hav_a), math.sqrt(1 - hav_a))
    return _EARTH_RADIUS_KM * hav_c
