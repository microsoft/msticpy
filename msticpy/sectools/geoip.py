
# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""geoip module using ipstack."""
# import gzip
from json import JSONDecodeError
import math
import os
from abc import ABC, abstractmethod
from collections.abc import Iterable
from datetime import datetime, timedelta
from typing import Tuple
from IPython import get_ipython
from IPython.display import display, HTML

from geolite2 import geolite2
import pandas as pd
import requests

from .. nbtools.entityschema import GeoLocation, IpAddress
from .. nbtools.utility import export
from .. _version import VERSION

__version__ = VERSION
__author__ = 'Ian Hellen'


class GeoIpLookup(ABC):
    """Abstract base class for GeoIP Lookup classes."""

    @abstractmethod
    def lookup_ip(self, ip_address: str = None, ip_addr_list: Iterable = None,
                  ip_entity: IpAddress = None):
        """
        Lookup IP location.

        Keyword Arguments:
            ip_address {str} -- a single address to look up (default: {None})
            ip_addr_list {Iterable} -- a collection of addresses to lookup (default: {None})
            ip_entity {IpAddress} -- an IpAddress entity

        Returns:
            tuple(list{dict}, list{entity}) -- returns raw geolocation results and
                same results as IP/Geolocation entities

        """

    def df_lookup_ip(self, data: pd.DataFrame, column: str):
        """
        Lookup Geolocation data from a pandas Dataframe.

        Keyword Arguments:
            data {pd.DataFrame} -- pandas dataframe containing IpAddress column
            column {str} -- the name of the dataframe column to use as a source
        """
        ip_list = data[column].values
        _, entities = self.lookup_ip(ip_addr_list=ip_list)

        ip_dicts = [ent.Location.properties().update(IpAddress=ent.Address)
                    for ent in entities]
        df_out = pd.DataFrame(data=ip_dicts)
        return data.merge(df_out, left_on=column, right_on='IpAddress')


@export
class IPStackLookup(GeoIpLookup):
    """
    GeoIP Lookup using IPStack web service.

    Raises:
        ConnectionError -- Invalid status returned from http request
        PermissionError -- Service refused request (e.g. requesting batch of addresses
                on free tier API key)

    """

    _IPSTACK_API = 'http://api.ipstack.com/{iplist}?access_key={access_key}'

    def __init__(self, api_key):
        """
        Create a new instance of IPStackLookup.

        Arguments:
            api_key {str} -- API Key from IPStack - see https://ipstack.com
        """
        self._api_key = api_key

    def lookup_ip(self, ip_address: str = None, ip_addr_list: Iterable = None,
                  ip_entity: IpAddress = None):
        """
        Lookup IP location from IPStack web service.

        Keyword Arguments:
            ip_address {str} -- a single address to look up (default: {None})
            ip_addr_list {Iterable} -- a collection of addresses to lookup (default: {None})
            ip_entity {IpAddress} -- an IpAddress entity

        Raises:
            ConnectionError -- Invalid status returned from http request
            PermissionError -- Service refused request (e.g. requesting batch of addresses
                on free tier API key)

        Returns:
            tuple(list{dict}, list{entity}) -- returns raw geolocation results and
                same results as IP/Geolocation entities

        """
        if ip_address and isinstance(ip_address, str):
            ip_list = [ip_address.strip()]
        elif ip_addr_list:
            ip_list = (ip.strip() for ip in ip_addr_list)
        elif ip_entity:
            ip_list = [ip_entity.Address]
        else:
            raise ValueError('No valid ip addresses were passed as arguments.')

        results, status = self._submit_request(ip_list)
        if status != 200:
            raise ConnectionError('Error reponse from server: ' + str(status))

        # {"success":false,"error":{"code":303,"type":"batch_not_supported_on_plan",
        # "info":"Bulk requests are not supported on your plan.
        # Please upgrade your subscription."}}

        if 'success' in results and not results["success"]:
            raise PermissionError('Service unable to complete request. Error: {}'
                                  .format(results['error']))

        output_raw = list(results)
        output_entities = []
        if isinstance(results, list):
            for ip_loc in results:
                output_entities.append(self._create_ip_entity(ip_loc, ip_entity))
        else:
            output_entities.append(self._create_ip_entity(results, ip_entity))

        return output_raw, output_entities

    def _create_ip_entity(self, ip_loc: dict, ip_entity):
        if not ip_entity:
            ip_entity = IpAddress()
            ip_entity.Address = ip_loc['ip']
        geo_entity = GeoLocation()
        geo_entity.CountryCode = ip_loc['country_code']

        geo_entity.CountryName = ip_loc['country_name']
        geo_entity.State = ip_loc['region_name']
        geo_entity.City = ip_loc['city']
        geo_entity.Longitude = ip_loc['longitude']
        geo_entity.Latitude = ip_loc['latitude']
        if 'connection' in ip_loc:
            geo_entity.Asn = ip_loc['connection']['asn']
        ip_entity.Location = geo_entity
        return ip_entity

    def _submit_request(self, ip_list):
        """
        Submit the request to IPStack.

            :param ip_list: Comma-separated string list of IPs to look up
        """
        submit_url = self._IPSTACK_API.format(iplist=ip_list, access_key=self._api_key)
        response = requests.get(submit_url)

        if response.status_code == 200:
            return response.json(), response.status_code
        else:
            if response:
                try:
                    return response.json(), response.status_code
                except JSONDecodeError:
                    pass
            return None, response.status_code


@export
class GeoLiteLookup(GeoIpLookup):
    """
    GeoIP Lookup using MaxMindDB database.

    Raises:
        ConnectionError -- Invalid status returned from http request
        PermissionError -- Service refused request (e.g. requesting batch of addresses
                on free tier API key)

    """

    _MAXMIND_DOWNLOAD = 'https://dev.maxmind.com/geoip/geoip2/geolite2/#Downloads'
    _DB_FILE = 'GeoLite2-City.mmdb'

    def __init__(self):
        """Return new instance of GeoLiteLookup class."""
        self._reader = geolite2.reader()
        last_mod_time = datetime.fromtimestamp(os.path.getmtime(geolite2.filename))
        db_age = datetime.utcnow() - last_mod_time
        if db_age > timedelta(40):
            print(f'{self._DB_FILE} is over one month old. Update the maxminddb package')
            print(f'to refresh or download a new version from {self._MAXMIND_DOWNLOAD}')

    def lookup_ip(self, ip_address: str = None, ip_addr_list: Iterable = None,
                  ip_entity: IpAddress = None):
        """
        Lookup IP location from IPStack web service.

        Keyword Arguments:
            ip_address {str} -- a single address to look up (default: {None})
            ip_addr_list {Iterable} -- a collection of addresses to lookup (default: {None})

        Returns:
            tuple(list{dict}, list{entity}) -- returns raw geolocation results and
                same results as IP/Geolocation entities

        """
        if ip_address and isinstance(ip_address, str):
            ip_list = [ip_address.strip()]
        elif ip_addr_list:
            ip_list = (ip.strip() for ip in ip_addr_list)
        elif ip_entity:
            ip_list = [ip_entity.Address]
        else:
            raise ValueError('No valid ip addresses were passed as arguments.')

        output_raw = []
        output_entities = []
        for ip_input in ip_list:
            geo_match = self._reader.get(ip_input)
            if geo_match:
                output_raw.append(geo_match)
                output_entities.append(self._create_ip_entity(ip_input, geo_match, ip_entity))

        return output_raw, output_entities

    def _create_ip_entity(self, ip_address, geo_match: dict, ip_entity):
        if not ip_entity:
            ip_entity = IpAddress()
            ip_entity.Address = ip_address
        geo_entity = GeoLocation()
        geo_entity.CountryCode = geo_match.get('country', {}).get('iso_code', None)
        geo_entity.CountryName = geo_match.get('country', {}).get('names', {}).get('en', None)
        subdivs = geo_match.get('subdivisions', [])
        if subdivs:
            geo_entity.State = subdivs[0].get('names', {}).get('en', None)
        geo_entity.City = geo_match.get('city', {}).get('names', {}).get('en', None)
        geo_entity.Longitude = geo_match.get('location', {}).get('longitude', None)
        geo_entity.Latitude = geo_match.get('location', {}).get('latitude', None)
        ip_entity.Location = geo_entity
        return ip_entity


_MM_LICENSE_HTML = '''
This product includes GeoLite2 data created by MaxMind, available from
<a href="https://www.maxmind.com">https://www.maxmind.com</a>.
'''
_MM_LICENSE_TXT = '''
This product includes GeoLite2 data created by MaxMind, available from
https://www.maxmind.com.
'''
_IPSTACK_LICENSE_HTML = '''
This library uses services provided by ipstack.
<a href="https://ipstack.com">https://ipstack.com</a>'''

_IPSTACK_LICENSE_TXT = 'This library uses services provided by ipstack (https://ipstack.com)'

if not get_ipython():
    print(_MM_LICENSE_TXT)
    print(_IPSTACK_LICENSE_TXT)
else:
    display(HTML(_MM_LICENSE_HTML))
    display(HTML(_IPSTACK_LICENSE_HTML))


def geo_distance(origin: Tuple[float, float],
                 destination: Tuple[float, float]) -> float:
    """
    Calculate the Haversine distance.

    Author: Martin Thoma - stackoverflow

    Parameters
    ----------
    origin : tuple of float
        (lat, long)
    destination : tuple of float
        (lat, long)

    Returns
    -------
    distance_in_km : float

    Examples
    --------
    >>> origin = (48.1372, 11.5756)  # Munich
    >>> destination = (52.5186, 13.4083)  # Berlin
    >>> round(distance(origin, destination), 1)
    504.2

    """
    orig_lat, orig_lon = origin
    dest_lat, dest_lon = destination
    EARTH_RADIUS_KM = 6371  # km

    ang_dist_lat = math.radians(dest_lat - orig_lat)
    ang_dist_lon = math.radians(dest_lon - orig_lon)
    hav_a = (math.sin(ang_dist_lat / 2) * math.sin(ang_dist_lat / 2) +
             math.cos(math.radians(orig_lat)) * math.cos(math.radians(dest_lat)) *
             math.sin(ang_dist_lon / 2) * math.sin(ang_dist_lon / 2))
    hav_c = 2 * math.atan2(math.sqrt(hav_a), math.sqrt(1 - hav_a))
    return EARTH_RADIUS_KM * hav_c
