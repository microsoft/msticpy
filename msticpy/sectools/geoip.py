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
import gzip
import math
import os
from pathlib import Path
import shutil
import warnings
from abc import ABCMeta, abstractmethod
from collections.abc import Iterable
from datetime import datetime, timedelta
from json import JSONDecodeError
from typing import Dict, List, Tuple, Optional

import pandas as pd
import requests
from requests.exceptions import HTTPError
from IPython import get_ipython
from IPython.display import HTML, display
import geoip2.database  # type: ignore
from geoip2.errors import AddressNotFoundError  # type: ignore

from .._version import VERSION
from ..nbtools.entityschema import GeoLocation, IpAddress  # type: ignore
from ..nbtools.utility import export

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
        geo_entity.CountryCode = ip_loc["country_code"]  # type: ignore

        geo_entity.CountryName = ip_loc["country_name"]  # type: ignore
        geo_entity.State = ip_loc["region_name"]  # type: ignore
        geo_entity.City = ip_loc["city"]  # type: ignore
        geo_entity.Longitude = ip_loc["longitude"]  # type: ignore
        geo_entity.Latitude = ip_loc["latitude"]  # type: ignore
        if "connection" in ip_loc:
            geo_entity.Asn = ip_loc["connection"]["asn"]  # type: ignore
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

    _MAXMIND_DOWNLOAD = (
        "https://geolite.maxmind.com/download/geoip/database/GeoLite2-City.mmdb.gz"
    )
    _DB_HOME = os.path.join(os.path.expanduser('~'), ".msticpy")
    _DB_ARCHIVE = "GeoLite2-City.mmdb.gz"
    _DB_FILE = "GeoLite2-City.mmdb"

    def __init__(
        self,
        db_folder: str = None,
        force_update: bool = False,
        auto_update: bool = True,
    ):
        r"""
        Return new instance of GeoLiteLookup class.

        Parameters
        ----------
        db_folder: str, optional
            Provide absolute path to the folder containing MMDB file
            (e.g. '/usr/home' or 'C:\maxmind').
            If no path provided, it is set to download to .msticpy dir under user`s home directory.
        force_update : bool, optional
            Force update can be set to true or false. depending on it,
            new download request will be initiated.

        """
        if db_folder is None:
            db_folder = self._DB_HOME
        self._force_update = force_update
        self._auto_update = auto_update
        self._check_and_update_db(db_folder, self._force_update, self._auto_update)
        self._dbpath = self._get_geoip_dbpath(db_folder)
        if not self._dbpath:
            raise RuntimeError("No usable GeoIP Database could be found.")
        self._reader = geoip2.database.Reader(self._dbpath)

    def _download_and_extract_gzip(self, url: str = None, db_folder: str = None):
        r"""
        Download file from the given URL and extract if it is archive.

        Parameters
        ----------
        url : str
            Web URL location to the Maxmind city Database. (the default is None)
        db_folder: str, optional
            Provide absolute path to the folder containing MMDB file
            (e.g. '/usr/home' or 'C:\maxmind').
            If no path provided, it is set to download to .msticpy dir under
            user`s home directory.(the default is None)

        """
        if url is None:
            url = self._MAXMIND_DOWNLOAD

        if db_folder is None:
            db_folder = self._DB_HOME

        if not os.path.exists(db_folder):
            os.mkdir(db_folder)
        db_archive_path = os.path.join(db_folder, self._DB_ARCHIVE)
        db_file_path = os.path.join(db_folder, self._DB_FILE)

        try:
            response = requests.get(url, stream=True)
            response.raise_for_status()
        except HTTPError as http_err:
            warnings.warn(
                f"HTTP error occurred trying to download GeoLite DB: {http_err}",
                RuntimeWarning,
            )
        # pylint: disable=broad-except
        except Exception as err:
            warnings.warn(
                f"Other error occurred trying to download GeoLite DB: {err}",
                RuntimeWarning,
            )
        # pylint: enable=broad-except
        else:
            print("Downloading GeoLite DB archive from MaxMind....")
            with open(db_archive_path, "wb") as file_hdl:
                for chunk in response.iter_content(chunk_size=10000):
                    file_hdl.write(chunk)
            try:
                with gzip.open(db_archive_path, "rb") as f_in:
                    print(f"Extracting city database...")
                    with open(db_file_path, "wb") as f_out:
                        shutil.copyfileobj(f_in, f_out)
                        print(
                            "Extraction complete. Local Maxmind city DB:",
                            f"{db_file_path}",
                        )
            except IOError as err:
                warnings.warn(f"Error writing GeoIP DB file: {db_archive_path} - {err}")

    @staticmethod
    def _get_geoip_dbpath(db_folder: str = None) -> Optional[str]:
        r"""
        Get the correct path containing GeoLite City Database.

        Parameters
        ----------
        db_folder: str, optional
            Provide absolute path to the folder containing MMDB file
            (e.g. '/usr/home' or 'C:\maxmind').
            If no path provided, it is set to download to .msticpy dir under
            user`s home directory.

        Returns
        -------
        Optional[str]
            Returns the absolute path of local maxmind geolite city
            database after control flow logic.

        """
        if not db_folder:
            db_folder = "."
        list_of_db_paths = [str(db) for db in Path(db_folder).glob("*.mmdb")]

        if len(list_of_db_paths) > 1:
            latest_db_path = max(list_of_db_paths, key=os.path.getmtime)
        elif len(list_of_db_paths) == 1:
            latest_db_path = list_of_db_paths[0]
        else:
            return None

        return latest_db_path

    def _check_and_update_db(
        self,
        db_folder: str = None,
        force_update: bool = False,
        auto_update: bool = True,
    ):
        r"""
        Check the age of geo ip database file and download if it older than 30 days.

        User can set auto_update or force_update to True or False to
        override auto-download behavior.

        Parameters
        ----------
        db_folder: str, optional
            Provide absolute path to the folder containing MMDB file
            (e.g. '/usr/home' or 'C:\maxmind').
            If no path provided, it is set to download to .msticpy dir under
            user`s home directory.
        force_update : bool, optional
            Force update can be set to true or false. depending on it,
            new download request will be initiated, overriding age criteria.
        auto_update : bool, optional
            Auto update can be set to true or false. depending on it,
            new download request will be initiated if age criteria is matched.

        """
        geoip_db_path = self._get_geoip_dbpath(db_folder)

        if geoip_db_path is None:
            print(
                "No local Maxmind City Database found. ",
                f"Attempting to downloading new database to {db_folder}",
            )
            self._download_and_extract_gzip(self._MAXMIND_DOWNLOAD, db_folder)
        else:
            # Create a reader object to retrive db info and build date
            # to check age from build_epoch property.
            reader = geoip2.database.Reader(geoip_db_path)
            last_mod_time = datetime.utcfromtimestamp(reader.metadata().build_epoch)
            # Check for out of date DB file according to db_age
            db_age = datetime.utcnow() - last_mod_time
            if db_age > timedelta(30) and auto_update:
                print(
                    "Latest local Maxmind City Database present is older than 30 days.",
                    f"Attempting to download new database to {db_folder}",
                )
                self._download_and_extract_gzip(self._MAXMIND_DOWNLOAD, db_folder)
            elif force_update and auto_update:
                print(
                    "force_update is set to True.",
                    f"Attempting to download new database to {db_folder}",
                )
                self._download_and_extract_gzip(self._MAXMIND_DOWNLOAD, db_folder)

    def lookup_ip(
        self,
        ip_address: str = None,
        ip_addr_list: Iterable = None,
        ip_entity: IpAddress = None,
    ) -> Tuple[List[Tuple[Dict[str, str], int]], List[IpAddress]]:
        """
        Lookup IP location from GeoLite2 data created by MaxMind.

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
            geo_match = None
            try:
                geo_match = self._reader.city(ip_input).raw
            except (AddressNotFoundError, AttributeError):
                continue
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
        geo_entity.CountryCode = geo_match.get("country", {}).get(  # type: ignore
            "iso_code", None
        )
        geo_entity.CountryName = (  # type: ignore
            geo_match.get("country", {}).get("names", {}).get("en", None)
        )
        subdivs = geo_match.get("subdivisions", [])
        if subdivs:
            geo_entity.State = (  # type: ignore
                subdivs[0].get("names", {}).get("en", None)
            )
        geo_entity.City = (  # type: ignore
            geo_match.get("city", {}).get("names", {}).get("en", None)
        )
        geo_entity.Longitude = geo_match.get("location", {}).get(  # type: ignore
            "longitude", None
        )
        geo_entity.Latitude = geo_match.get("location", {}).get(  # type: ignore
            "latitude", None
        )
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
