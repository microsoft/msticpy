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
-  IPStackLookup - IPStack (see https://ipstack.com)

Both services offer
a free tier for non-commercial use. However, a paid tier will
normally get you more accuracy, more detail and a higher throughput
rate. Maxmind geolite uses a downloadable database, while IPStack is
an online lookup (API key required).

"""
import math
import os
import random
import tarfile
import warnings
from abc import ABCMeta, abstractmethod
from collections.abc import Iterable
from datetime import datetime, timedelta
from json import JSONDecodeError
from pathlib import Path
from time import sleep
from typing import Any, Dict, List, Mapping, Optional, Tuple

import geoip2.database  # type: ignore
import pandas as pd
import requests
from geoip2.errors import AddressNotFoundError  # type: ignore
from IPython import get_ipython
from IPython.display import HTML, display
from requests.exceptions import HTTPError

from .._version import VERSION
from ..common.exceptions import MsticpyUserConfigError
from ..common.provider_settings import ProviderSettings, get_provider_settings
from ..common.pkg_config import current_config_path
from ..common.utility import export
from ..datamodel.entities import GeoLocation, IpAddress

__version__ = VERSION
__author__ = "Ian Hellen"


class GeoIPDatabaseException(Exception):
    """Exception when GeoIP database cannot be found."""


class GeoIpLookup(metaclass=ABCMeta):
    """
    Abstract base class for GeoIP Lookup classes.

    See Also
    --------
    IPStackLookup : IPStack GeoIP Implementation
    GeoLiteLookup : MaxMind GeoIP Implementation

    """

    _LICENSE_TXT: Optional[str] = None
    _LICENSE_HTML: Optional[str] = None
    _license_shown: bool = False

    def __init__(self):
        """Initialize instance of GeoIpLookup class."""
        self._print_license()

    @abstractmethod
    def lookup_ip(
        self,
        ip_address: str = None,
        ip_addr_list: Iterable = None,
        ip_entity: IpAddress = None,
    ) -> Tuple[List[Any], List[IpAddress]]:
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
        Tuple[List[Any], List[IpAddress]]:
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
        return data.merge(
            self.lookup_ips(data, column),
            how="left",
            left_on=column,
            right_on="IpAddress",
        )

    def lookup_ips(self, data: pd.DataFrame, column: str) -> pd.DataFrame:
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
            IpLookup results as DataFrame.

        """
        ip_list = list(data[column].values)
        _, entities = self.lookup_ip(ip_addr_list=ip_list)

        ip_dicts = [
            {**ent.Location.properties, "IpAddress": ent.Address}
            for ent in entities
            if ent.Location is not None
        ]
        return pd.DataFrame(data=ip_dicts)

    # pylint: disable=protected-access
    def _print_license(self):
        if self.__class__._license_shown:
            return
        if self._LICENSE_HTML and get_ipython():
            display(HTML(self._LICENSE_HTML))
        elif self._LICENSE_TXT:
            print(self._LICENSE_TXT)
        self.__class__._license_shown = True

    # pylint: enable=protected-access


@export
class IPStackLookup(GeoIpLookup):
    """
    IPStack GeoIP Implementation.

    See Also
    --------
    GeoIpLookup : Abstract base class
    GeoLiteLookup : MaxMind GeoIP Implementation

    """

    _LICENSE_HTML = """
This library uses services provided by ipstack.
<a href="https://ipstack.com">https://ipstack.com</a>"""

    _LICENSE_TXT = """
This library uses services provided by ipstack (https://ipstack.com)"""

    _IPSTACK_API = "http://api.ipstack.com/{iplist}?access_key={access_key}&output=json"

    _NO_API_KEY_MSSG = """
No API Key was found to access the IPStack service.
If you do not have an account, go here to create one and obtain and API key.

Add this API key to your msticpyconfig.yaml
After adding the key run the following commands to reload your settings and retry:
    import msticpy
    msticpy.settings.refresh_config()

Alternatively, you can pass this to the IPStackLookup class when creating it:
>>> iplookup = IPStackLookup(api_key="your_api_key")
"""

    def __init__(self, api_key: Optional[str] = None, bulk_lookup: bool = False):
        """
        Create a new instance of IPStackLookup.

        Parameters
        ----------
        api_key : str, optional
            API Key from IPStack - see https://ipstack.com
            default is None - obtain key from msticpyconfig.yaml
        bulk_lookup : bool, optional
            For Professional and above tiers allowing you to
            submit multiple IPs in a single request.
            (the default is False, which submits a single request
            per address)

        """
        super().__init__()

        self.settings = _get_geoip_provider_settings("IPStack")
        self._api_key = api_key or self.settings.args.get("AuthKey")
        if not self._api_key:
            raise MsticpyUserConfigError(
                self._NO_API_KEY_MSSG,
                help_uri=(
                    "https://msticpy.readthedocs.io/en/latest/data_acquisition/"
                    + "GeoIPLookups.html#ipstack-geo-lookup-class"
                ),
                service_uri="https://ipstack.com/product",
                title="IPStack API key not found",
            )
        self.bulk_lookup = bulk_lookup

    def lookup_ip(
        self,
        ip_address: str = None,
        ip_addr_list: Iterable = None,
        ip_entity: IpAddress = None,
    ) -> Tuple[List[Any], List[IpAddress]]:
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
        Tuple[List[Any], List[IpAddress]]:
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
            ip_list = [ip.strip() for ip in ip_addr_list]
        elif ip_entity:
            ip_list = [ip_entity.Address]
        else:
            raise ValueError("No valid ip addresses were passed as arguments.")

        results = self._submit_request(ip_list)
        output_raw = []
        output_entities = []
        for ip_loc, status in results:
            if status == 200 and "error" not in ip_loc:
                output_entities.append(self._create_ip_entity(ip_loc, ip_entity))
            output_raw.append((ip_loc, status))
        return output_raw, output_entities

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
                    f"Service unable to complete request. Error: {results['error']}"
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
                elif response:
                    try:
                        ip_loc_results.append((response.json(), response.status_code))
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
        "https://download.maxmind.com/app/geoip_download?"
        + "edition_id=GeoLite2-City&license_key={license_key}&suffix=tar.gz"
    )

    _DB_HOME = str(Path.joinpath(Path("~").expanduser(), ".msticpy", "GeoLite2"))
    _DB_ARCHIVE = "GeoLite2-City.mmdb.{rand}.tar.gz"
    _DB_FILE = "GeoLite2-City.mmdb"

    _LICENSE_HTML = """
This product includes GeoLite2 data created by MaxMind, available from
<a href="https://www.maxmind.com">https://www.maxmind.com</a>.
"""

    _LICENSE_TXT = """
This product includes GeoLite2 data created by MaxMind, available from
https://www.maxmind.com.
"""

    _NO_API_KEY_MSSG = """
No API Key was found to download the Maxmind GeoIPLite database.
If you do not have an account, go here to create one and obtain and API key.
https://www.maxmind.com/en/geolite2/signup

Add this API key to your msticpyconfig.yaml
https://msticpy.readthedocs.io/en/latest/data_acquisition/GeoIPLookups.html#maxmind-geo-ip-lite-lookup-class.
Alternatively, you can pass this to the GeoLiteLookup class when creating it:
>>> iplookup = GeoLiteLookup(api_key="your_api_key")
"""

    def __init__(
        self,
        api_key: Optional[str] = None,
        db_folder: Optional[str] = None,
        force_update: bool = False,
        auto_update: bool = True,
        debug: bool = False,
    ):
        r"""
        Return new instance of GeoLiteLookup class.

        Parameters
        ----------
        api_key : str, optional
            Default is None - use configuration value from msticpyconfig.yaml.
            API Key from MaxMind -
            Read more about GeoLite2 : https://dev.maxmind.com/geoip/geoip2/geolite2/
            Sign up for a MaxMind account:
            https://www.maxmind.com/en/geolite2/signup
            Set your password and create a license key:
            https://www.maxmind.com/en/accounts/current/license-key
        db_folder: str, optional
            Provide absolute path to the folder containing MMDB file
            (e.g. '/usr/home' or 'C:/maxmind').
            If no path provided, it is set to download to .msticpy/GeoLite2
            under user`s home directory.
        force_update : bool, optional
            Force update can be set to true or false. depending on it,
            new download request will be initiated.
        auto_update: bool, optional
            Auto update can be set to true or false. depending on it,
            new download request will be initiated if age criteria is matched.
        debug : bool, optional
            Print additional debugging information, default is False.

        """
        super().__init__()

        self._debug = debug
        if self._debug:
            dbg_api_key = (
                "None" if api_key is None else api_key[:4] + "*" * (len(api_key) - 4)
            )
            self._pr_debug(f"__init__ params: api_key={dbg_api_key}")
            self._pr_debug(f"    db_folder={db_folder}")
            self._pr_debug(f"    force_update={force_update}")
            self._pr_debug(f"    auto_update={auto_update}")
        self.settings = _get_geoip_provider_settings("GeoIPLite")
        self._api_key = api_key or self.settings.args.get("AuthKey")

        self._dbfolder = db_folder
        if self._dbfolder is None:
            self._dbfolder = self.settings.args.get("DBFolder", self._DB_HOME)

        self._dbfolder = str(Path(self._dbfolder).expanduser())  # type: ignore
        self._force_update = force_update
        self._auto_update = auto_update
        self._check_and_update_db(self._dbfolder, self._force_update, self._auto_update)
        self._dbpath = self._get_geoip_dbpath(self._dbfolder)
        if self._debug:
            dbg_api_key = (
                "None"
                if self._api_key is None
                else self._api_key[:4] + "*" * (len(self._api_key) - 4)
            )
            self._pr_debug(f"__init__ values (inc settings): api_key={dbg_api_key}")
            self._pr_debug(f"    db_folder={self._dbfolder}")
            self._pr_debug(f"    force_update={self._force_update}")
            self._pr_debug(f"    auto_update={self._auto_update}")
            self._pr_debug(f"    dbpath={self._dbpath}")
            self._pr_debug(f"Using config file: {current_config_path()}")

        if not self._dbpath:
            raise MsticpyUserConfigError(
                "No usable GeoIP Database could be found.",
                (
                    "Check that you have correctly configured the Maxmind API key in "
                    "msticpyconfig.yaml."
                ),
                (
                    "If you are using a custom DBFolder setting in your config, "
                    + f"check that this is a valid path: {self._dbfolder}."
                ),
                (
                    "If you edit your msticpyconfig to change this setting run the "
                    "following commands to reload your settings and retry:"
                    "    import msticpy"
                    "    msticpy.settings.refresh_config()"
                ),
                help_uri=(
                    "https://msticpy.readthedocs.io/en/latest/data_acquisition/"
                    + "GeoIPLookups.html#maxmind-geo-ip-lite-lookup-class"
                ),
                service_uri="https://www.maxmind.com/en/geolite2/signup",
                title="Maxmind GeoIP database not found",
            )
        self._reader = geoip2.database.Reader(self._dbpath)

    def close(self):
        """Close an open GeoIP DB."""
        if self._reader:
            try:
                self._reader.close()
            except Exception as err:  # pylint: disable=broad-except
                print(f"Exception when trying to close GeoIP DB {err}")

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
            If no path provided, it is set to download to .msticpy\GeoLite2 dir under
            user`s home directory.
        force_update : bool, optional
            Force update can be set to true or false. depending on it,
            new download request will be initiated, overriding age criteria.
        auto_update : bool, optional
            Auto update can be set to true or false. depending on it,
            new download request will be initiated if age criteria is matched.

        """
        geoip_db_path = self._get_geoip_dbpath(db_folder)
        url = self._MAXMIND_DOWNLOAD.format(license_key=self._api_key)
        self._pr_debug(f"Checking geoip DB {geoip_db_path}")
        self._pr_debug(f"Download URL is {self._MAXMIND_DOWNLOAD}")
        if geoip_db_path is None:
            print(
                "No local Maxmind City Database found. ",
                f"Attempting to downloading new database to {db_folder}",
            )
            self._download_and_extract_archive(url, db_folder)
        else:
            # Create a reader object to retrive db info and build date
            # to check age from build_epoch property.
            with geoip2.database.Reader(geoip_db_path) as reader:
                last_mod_time = datetime.utcfromtimestamp(reader.metadata().build_epoch)

            # Check for out of date DB file according to db_age
            db_age = datetime.utcnow() - last_mod_time
            db_updated = True
            if db_age > timedelta(30) and auto_update:
                print(
                    "Latest local Maxmind City Database present is older than 30 days.",
                    f"Attempting to download new database to {db_folder}",
                )
                if not self._download_and_extract_archive(url, db_folder):
                    self._pr_debug("DB download failed")
                    self._geolite_warn("DB download failed")
                    db_updated = False
            elif force_update:
                print(
                    "force_update is set to True.",
                    f"Attempting to download new database to {db_folder}",
                )
                if not self._download_and_extract_archive(url, db_folder):
                    self._pr_debug("DB download failed")
                    self._geolite_warn("DB download failed")
                    db_updated = False
            if not db_updated:
                self._pr_debug("Continuing with cached database.")
                self._geolite_warn(
                    "Continuing with cached database. Results may inaccurate."
                )

    # pylint: disable=too-many-branches
    def _download_and_extract_archive(  # noqa: MC0001
        self, url: str = None, db_folder: str = None
    ) -> bool:
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

        Returns
        -------
        bool :
            True if download_success

        """
        if not self._api_key:
            return False
        if url is None:
            url = self._MAXMIND_DOWNLOAD.format(license_key=self._api_key)

        if db_folder is None:
            db_folder = self._DB_HOME

        if not Path(db_folder).exists():
            # using makedirs to create intermediate-level dirs to contain the leaf dir
            Path(db_folder).mkdir(exist_ok=True, parents=True)
        rand_int = random.randint(10000, 99999)  # nosec
        db_archive_path = Path(db_folder).joinpath(
            self._DB_ARCHIVE.format(rand=rand_int)
        )
        self._pr_debug(f"Downloading GeoLite DB: {db_archive_path}")
        try:
            # wait a small rand amount of time in case multiple procs try
            # to download simultaneously
            sleep(rand_int / 500000)
            if list(Path(db_folder).glob(self._DB_ARCHIVE.format(rand="*"))):
                # Some other process is downloading
                return True
            with requests.get(url, stream=True) as response:
                response = requests.get(url, stream=True)
                response.raise_for_status()
                print("Downloading and extracting GeoLite DB archive from MaxMind....")
                with open(db_archive_path, "wb") as file_hdl:
                    for chunk in response.iter_content(chunk_size=10000):
                        file_hdl.write(chunk)
                        file_hdl.flush()
            self._pr_debug(f"Downloaded GeoLite DB: {db_archive_path}")
        except HTTPError as http_err:
            self._pr_debug(
                f"HTTP error occurred trying to download GeoLite DB: {http_err}"
            )
            self._geolite_warn(
                f"HTTP error occurred trying to download GeoLite DB: {http_err}"
            )
        # pylint: disable=broad-except
        except Exception as err:
            self._pr_debug(f"Other error occurred trying to download GeoLite DB: {err}")
            self._geolite_warn(
                f"Other error occurred trying to download GeoLite DB: {err}"
            )
        # pylint: enable=broad-except
        else:
            try:
                self._extract_to_folder(db_archive_path, db_folder)
                print(
                    "Extraction complete. Local Maxmind city DB:", f"{db_archive_path}"
                )
                return True
            except PermissionError as err:
                self._pr_debug(
                    f"Error writing GeoIP DB file: {db_archive_path} - {err}"
                )
                self._geolite_warn(
                    f"Cannot overwrite GeoIP DB file: {db_archive_path}."
                    + " The file may be in use or you do not have"
                    + f" permission to overwrite.\n - {err}"
                )
            except Exception as err:  # pylint: disable=broad-except
                # There are several exception types that might come from
                # unpacking a tar.gz
                self._pr_debug(
                    f"Error writing GeoIP DB file: {db_archive_path} - {err}"
                )
                self._geolite_warn(
                    f"Error writing GeoIP DB file: {db_archive_path} - {err}"
                )
        finally:
            if db_archive_path.is_file():
                self._pr_debug(f"Removing temp file {db_archive_path}")
                db_archive_path.unlink()
        return False

    # pylint: enable=too-many-branches

    def _extract_to_folder(self, db_archive_path, db_folder):
        self._pr_debug(f"Extracting tarfile {db_archive_path}")
        with tarfile.open(db_archive_path) as tar_archive:
            for member in tar_archive.getmembers():
                if not member.isreg():
                    continue
                # Will skip the dirs to extract only file objects
                self._pr_debug(f"extracting {member} to {db_folder}")
                tar_archive.extract(member, db_folder)
                # The files are extract to a subfolder (with a date in the name)
                # We want to move these into the main folder above this.
                targetname = Path(member.name).name
                if targetname != member.name:
                    curr_file = Path(db_folder).joinpath(member.name)
                    extr_folder = Path(db_folder).joinpath(member.name).parent
                    curr_file.replace(Path(db_folder).joinpath(targetname))
                    self._pr_debug(f"Moving to {Path(db_folder).joinpath(targetname)}")
                    # if the folder is empty, remove it
                    if not list(extr_folder.glob("*")):
                        extr_folder.rmdir()

    @staticmethod
    def _get_geoip_dbpath(db_folder: str = None) -> Optional[str]:
        r"""
        Get the correct path containing GeoLite City Database.

        Parameters
        ----------
        db_folder: str, optional
            Provide absolute path to the folder containing MMDB file
            (e.g. '/usr/home' or 'C:\maxmind').
            If no path provided, it is set to download to .msticpy\GeoLite2 dir under
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

    def lookup_ip(
        self,
        ip_address: str = None,
        ip_addr_list: Iterable = None,
        ip_entity: IpAddress = None,
    ) -> Tuple[List[Any], List[IpAddress]]:
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
        Tuple[List[Any], List[IpAddress]]
            raw geolocation results and same results as IpAddress entities with
            populated Location property.

        """
        if ip_address and isinstance(ip_address, str):
            ip_list = [ip_address.strip()]
        elif ip_addr_list:
            ip_list = [ip.strip() for ip in ip_addr_list]
        elif ip_entity:
            ip_list = [ip_entity.Address]
        else:
            raise ValueError("No valid ip addresses were passed as arguments.")

        output_raw = []
        output_entities = []
        ip_cache: Dict[str, Any] = {}
        for ip_input in ip_list:
            geo_match = None
            try:
                geo_match = ip_cache.get(ip_input, self._reader.city(ip_input).raw)
            except (AddressNotFoundError, AttributeError, ValueError):
                continue
            if geo_match:
                output_raw.append(geo_match)
                output_entities.append(
                    self._create_ip_entity(ip_input, geo_match, ip_entity)
                )

        return output_raw, output_entities

    @staticmethod
    def _create_ip_entity(
        ip_address: str, geo_match: Mapping[str, Any], ip_entity: IpAddress = None
    ) -> IpAddress:
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

    def _pr_debug(self, *args):
        """Print out debug info."""
        if self._debug:
            print(*args)

    @staticmethod
    def _geolite_warn(mssg):
        warnings.warn(
            f"GeoIpLookup: {mssg}",
            UserWarning,
        )


def _get_geoip_provider_settings(provider_name: str) -> ProviderSettings:
    """
    Return settings for a provider.

    Parameters
    ----------
    provider_name : str
        Name of the provider.

    Returns
    -------
    ProviderSettings
        Settings for the provider.

    """
    settings = get_provider_settings(config_section="OtherProviders")
    if provider_name in settings:
        return settings[provider_name]
    return ProviderSettings(name=provider_name, description="Not found.")


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
