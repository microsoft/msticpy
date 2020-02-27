GeoIP Lookup
============

Introduction
------------

This :py:mod:`module<msticpy.sectools.geoip>` contains two classes
that allow you to look up the Geolocation of IP Addresses.

MaxMind GeoIPLite
^^^^^^^^^^^^^^^^^

This product includes GeoLite2 data created by MaxMind, available from
https://www.maxmind.com.

This uses a local database which is downloaded first time when class
object is instantiated. It gives very fast lookups but you need to
download updates regularly. Maxmind offers a free tier of this database,
updated monthly. For greater accuracy and more detailed information they
have varying levels of paid service. Please check out their site for
more details.

The geoip module uses the official Maxmind PyPi package - geoip2.

IPStack
^^^^^^^


This library uses services provided by ipstack. https://ipstack.com

IPStack is an online service and also offers a free tier of their
service. Again, the paid tiers offer greater accuracy, more detailed
information and higher throughput. Please check out their site for more
details.

Importing the GeoIP classes
---------------------------

.. code:: ipython3

    # Imports
    import sys
    MIN_REQ_PYTHON = (3,6)
    if sys.version_info < MIN_REQ_PYTHON:
        print('Check the Kernel->Change Kernel menu and ensure that Python 3.6')
        print('or later is selected as the active kernel.')
        sys.exit("Python %s.%s or later is required.\n" % MIN_REQ_PYTHON)


    from IPython.display import display
    import pandas as pd

    import msticpy.sectools as sectools
    from msticpy.nbtools import *
    from msticpy.nbtools.entityschema import IpAddress, GeoLocation
    from msticpy.sectools.geoip import GeoLiteLookup, IPStackLookup




Maxmind Geo-IP Lite Lookup Class
--------------------------------

See :py:class:`GeoLiteLookup<msticpy.sectools.geoip.GeoLiteLookup>`

.. note:: Maxmind now require an API Key to download database
   updates. You can create a free account or opt for a paid tier,
   which gives you greater accuracy and more features.


Setting GeoIPLite configuration options
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

You can supply options for the GeoLiteLookup class within the
`msticpyconfig.yaml` configuration file (see
:doc:`../getting_started/msticpyconfig`) or when instantiating the
GeoLiteLookup class.

The example shown here shows part of the ``OtherProviders`` section of
msticpyconfig.yaml. You can specify an API key in the ``AuthKey`` setting.
For example, ``AuthKey: abcd123456789`` or use a reference to an
environment variable holding the key value.
The API key you need to specify in the ``AuthKey`` setting is you MaxMind
License Key that can be found on the MaxMind website under Account > Services.

The DBFolder setting specifies a folder where the downloaded Maxmind
database files will be stored and referenced from. Thefolder path
can be prefixed with "~" to specify a path relative to the current
users home directory (this works cross-platform).

.. note:: You can specify the MaxMind API key value as an environment
  variable as follows:

.. code:: yaml

    ...
    OtherProviders:
      GeoIPLite:
        Args:
          AuthKey:
            EnvironmentVar: "MAXMIND_AUTH"
          DBFolder: "~/.msticpy"
        Provider: "GeoLiteLookup"

.. note:: Alternatively you can specify it directly in the config file
  in AuthKey:

.. code:: yaml

    ...
    OtherProviders:
      GeoIPLite:
        Args:
          AuthKey: "your_maxmind_key"
          DBFolder: "~/.msticpy"
        Provider: "GeoLiteLookup"

You can also specify the API key and folder options when creating an
instance of the GeoLiteLookup class. In this case the folder path
must be either an absolute or relative path - expansion of "~" will
not work reliably cross-platform.


.. code:: ipython3

    iplocation = GeoLiteLookup(api_key="mykey", db_folder="/tmp/mmdb")


Usage
^^^^^

Creating an instance of the GeoLiteLookup class
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: ipython3

    iplocation = GeoLiteLookup()

You can also supply options to customize the behavior of the
local maxmind database.

* ``api_key``: described above
* ``db_folder`` : Specify custom path containing local Maxmind city
  database. If not specified, download to .msticpy dir under user's home
  directory.
*  ``force_update`` : Set to ``True`` to force
   update without an age-check the current database.
*  ``auto_update`` : ``True`` (default) will check the age of the Maxmind
   city database if the current database is older than 30 days. Setting
   to ``False`` to skip age checking.
   ``force_update=True`` will override this setting.


Lookup IP location from GeoLite2 database
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

You can pass a single IP Address, a list of IPAddresses or an IpAddress
entity (see :py:class:`IpAddress<msticpy.nbtools.entityschema.IpAddress>`)


.. code:: ipython3

    iplocation = GeoLiteLookup()
    loc_result, ip_entity = iplocation.lookup_ip(ip_address='90.156.201.97')

    print('Raw result')
    display(loc_result)

    print('IP Address Entity')
    display(ip_entity[0])


.. parsed-literal::

    No local Maxmind City Database found.  Attempting to downloading new database to /home/nbuser/.msticpy
    Downloading GeoLite DB archive from MaxMind....
    Extracting city database...
    Extraction complete. Local Maxmind city DB: /home/nbuser/.msticpy/GeoLite2-City.mmdb
    Raw result



.. parsed-literal::

    [{'continent': {'code': 'EU',
       'geoname_id': 6255148,
       'names': {'de': 'Europa',
        'en': 'Europe',
        'es': 'Europa',
        'fr': 'Europe',
        'ja': 'ヨーロッパ',
        'pt-BR': 'Europa',
        'ru': 'Европа',
        'zh-CN': '欧洲'}},
      'country': {'geoname_id': 2017370,
       'iso_code': 'RU',
       'names': {'de': 'Russland',
        'en': 'Russia',
        'es': 'Rusia',
        'fr': 'Russie',
        'ja': 'ロシア',
        'pt-BR': 'Rússia',
        'ru': 'Россия',
        'zh-CN': '俄罗斯'}},
      'location': {'accuracy_radius': 1000,
       'latitude': 55.7386,
       'longitude': 37.6068,
       'time_zone': 'Europe/Moscow'},
      'registered_country': {'geoname_id': 2017370,
       'iso_code': 'RU',
       'names': {'de': 'Russland',
        'en': 'Russia',
        'es': 'Rusia',
        'fr': 'Russie',
        'ja': 'ロシア',
        'pt-BR': 'Rússia',
        'ru': 'Россия',
        'zh-CN': '俄罗斯'}},
      'traits': {'ip_address': '90.156.201.97'}}]


.. parsed-literal::

    IP Address Entity

    IpAddress(Type=ipaddress, Address=90.156.201.97, Location={ 'AdditionalData': {},
      'Count...)


Looking up a list of IP Addresses
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


.. code:: ipython3

    import socket
    socket_info = socket.getaddrinfo("pypi.org",0,0,0,0)

    ips = [res[4][0] for res in socket_info]
    print(ips)

    _, ip_entities = iplocation.lookup_ip(ip_addr_list=ips)
    display(ip_entities)


.. parsed-literal::

    ['151.101.0.223', '151.101.0.223', '151.101.0.223', '151.101.128.223', '151.101.128.223', '151.101.128.223', '151.101.64.223', '151.101.64.223', '151.101.64.223', '151.101.192.223', '151.101.192.223', '151.101.192.223', '2a04:4e42::223', '2a04:4e42::223', '2a04:4e42::223', '2a04:4e42:600::223', '2a04:4e42:600::223', '2a04:4e42:600::223', '2a04:4e42:400::223', '2a04:4e42:400::223', '2a04:4e42:400::223', '2a04:4e42:200::223', '2a04:4e42:200::223', '2a04:4e42:200::223']



.. parsed-literal::

    [IpAddress(Type=ipaddress, Address=151.101.0.223, Location={ 'AdditionalData': {},
       'Count...),
     IpAddress(Type=ipaddress, Address=151.101.0.223, Location={ 'AdditionalData': {},
       'Count...),
     IpAddress(Type=ipaddress, Address=151.101.0.223, Location={ 'AdditionalData': {},
       'Count...),
     IpAddress(Type=ipaddress, Address=151.101.128.223, Location={ 'AdditionalData': {},
       'Cou...),
     IpAddress(Type=ipaddress, Address=151.101.128.223, Location={ 'AdditionalData': {},
       'Cou...),
     IpAddress(Type=ipaddress, Address=151.101.128.223, Location={ 'AdditionalData': {},
       'Cou...),
     IpAddress(Type=ipaddress, Address=151.101.64.223, Location={ 'AdditionalData': {},
       'Coun...),
     IpAddress(Type=ipaddress, Address=151.101.64.223, Location={ 'AdditionalData': {},
       'Coun...),
     IpAddress(Type=ipaddress, Address=151.101.64.223, Location={ 'AdditionalData': {},
       'Coun...),
     IpAddress(Type=ipaddress, Address=151.101.192.223, Location={ 'AdditionalData': {},
       'Cou...),
     IpAddress(Type=ipaddress, Address=151.101.192.223, Location={ 'AdditionalData': {},
       'Cou...),
     IpAddress(Type=ipaddress, Address=151.101.192.223, Location={ 'AdditionalData': {},
       'Cou...),
     IpAddress(Type=ipaddress, Address=2a04:4e42::223, Location={'AdditionalData': {}, 'Latitud...),
     IpAddress(Type=ipaddress, Address=2a04:4e42::223, Location={'AdditionalData': {}, 'Latitud...),
     IpAddress(Type=ipaddress, Address=2a04:4e42::223, Location={'AdditionalData': {}, 'Latitud...),
     IpAddress(Type=ipaddress, Address=2a04:4e42:600::223, Location={'AdditionalData': {}, 'Lat...),
     IpAddress(Type=ipaddress, Address=2a04:4e42:600::223, Location={'AdditionalData': {}, 'Lat...),
     IpAddress(Type=ipaddress, Address=2a04:4e42:600::223, Location={'AdditionalData': {}, 'Lat...),
     IpAddress(Type=ipaddress, Address=2a04:4e42:400::223, Location={'AdditionalData': {}, 'Lat...),
     IpAddress(Type=ipaddress, Address=2a04:4e42:400::223, Location={'AdditionalData': {}, 'Lat...),
     IpAddress(Type=ipaddress, Address=2a04:4e42:400::223, Location={'AdditionalData': {}, 'Lat...),
     IpAddress(Type=ipaddress, Address=2a04:4e42:200::223, Location={'AdditionalData': {}, 'Lat...),
     IpAddress(Type=ipaddress, Address=2a04:4e42:200::223, Location={'AdditionalData': {}, 'Lat...),
     IpAddress(Type=ipaddress, Address=2a04:4e42:200::223, Location={'AdditionalData': {}, 'Lat...)]


IPStack Geo-lookup Class
------------------------

See :py:class:`IPStackLookup<msticpy.sectools.geoip.IPStackLookup>`


.. note:: IPStack requires an IPStack API Key.
   If you have a paid tier service with IPStack you should enable
   the bulk lookup option when instantiating the class. This
   allows more efficient batching when querying multiple IP Addresses.
   Trying to use option with the free tier will result in the
   request being rejected.

Setting IPStack configuration options
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

You can supply options for the IPStack class within the
`msticpyconfig.yaml` configuration file (see
:doc:`../getting_started/msticpyconfig`) or when instantiating the
class.

The example shown here shows part of the ``OtherProviders`` section of
msticpyconfig.yaml. You can specify an API key in the ``AuthKey`` setting.
For example, ``AuthKey: abcd123456789`` or use a reference to an
environment variable holding the key value, as shown in the example.

.. code:: yaml

    ...
    OtherProviders:
      IPStack:
        Args:
          AuthKey: "987654321-222"
        Provider: "IPStackLookup"


Usage
^^^^^

Manually Entering the IPStack Key
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: ipython3

    # Enter your IPStack Key here
    ips_key = nbwidgets.GetEnvironmentKey(env_var='IPSTACK_API_KEY',
                               help_str='To obtain an API key sign up here https://www.ipstack.com/',
                               prompt='IPStack API key:')
    iplocation = IPStackLookup(api_key=ips_key.value)


Lookup IP location from IPStack
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.. code:: ipython3

    # Assumes that you have configured the AuthKey value in msticpyconfig.yaml
    iplocation = IPStackLookup()
    loc_result, ip_entity = iplocation.lookup_ip(ip_address='90.156.201.97')
    print('Raw result')
    display(loc_result)

    print('IP Address Entity')
    display(ip_entity[0])


.. parsed-literal::

    Raw result



.. parsed-literal::

    [({'ip': '90.156.201.97',
       'type': 'ipv4',
       'continent_code': 'EU',
       'continent_name': 'Europe',
       'country_code': 'RU',
       'country_name': 'Russia',
       'region_code': None,
       'region_name': None,
       'city': None,
       'zip': None,
       'latitude': 55.7386,
       'longitude': 37.6068,
       'location': {'geoname_id': None,
        'capital': 'Moscow',
        'languages': [{'code': 'ru', 'name': 'Russian', 'native': 'Русский'}],
        'country_flag': 'http://assets.ipstack.com/flags/ru.svg',
        'country_flag_emoji': '🇷🇺',
        'country_flag_emoji_unicode': 'U+1F1F7 U+1F1FA',
        'calling_code': '7',
        'is_eu': False}},
      200)]


.. parsed-literal::

    IP Address Entity


.. parsed-literal::

    {"Address": "90.156.201.97", "Location": {"CountryCode": "RU", "CountryName": "Russia", "Longitude": 37.6068, "Latitude": 55.7386, "Type": "geolocation"}, "Type": "ipaddress"}


Looking up a list of IP Addresses
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: ipython3

    loc_result, ip_entities = iplocation.lookup_ip(ip_addr_list=ips)

    display(ip_entities)


.. parsed-literal::

    [{"Address": "2a04:4e42:400::223", "Location": {"Longitude": 8, "Latitude": 47, "Type": "geolocation"}, "Type": "ipaddress"},
     {"Address": "2a04:4e42:200::223", "Location": {"Longitude": 8, "Latitude": 47, "Type": "geolocation"}, "Type": "ipaddress"},
     {"Address": "2a04:4e42:600::223", "Location": {"Longitude": 8, "Latitude": 47, "Type": "geolocation"}, "Type": "ipaddress"},
     {"Address": "2a04:4e42::223", "Location": {"Longitude": 8, "Latitude": 47, "Type": "geolocation"}, "Type": "ipaddress"},
     {"Address": "151.101.64.223", "Location": {"CountryCode": "US", "CountryName": "United States", "Longitude": -97.822, "Latitude": 37.751, "Type": "geolocation"}, "Type": "ipaddress"},
     {"Address": "151.101.0.223", "Location": {"CountryCode": "US", "CountryName": "United States", "Longitude": -97.822, "Latitude": 37.751, "Type": "geolocation"}, "Type": "ipaddress"},
     {"Address": "151.101.192.223", "Location": {"CountryCode": "US", "CountryName": "United States", "Longitude": -97.822, "Latitude": 37.751, "Type": "geolocation"}, "Type": "ipaddress"},
     {"Address": "151.101.128.223", "Location": {"CountryCode": "US", "CountryName": "United States", "Longitude": -97.822, "Latitude": 37.751, "Type": "geolocation"}, "Type": "ipaddress"}]


Taking input from a pandas DataFrame
------------------------------------

See :py:meth:`df_lookup_ip<msticpy.sectools.geoip.GeoIpLookup.df_lookup_ip>`

The base class for both implementations has a method that sources the ip
addresses from a dataframe column and returns a new dataframe with the
location information merged with the input frame.

Pass the input DataFrame using the ``data`` parameter and specify a
column name containing the IPAddresses with the ``column`` parameter.




Creating a Custom GeopIP Lookup Class
-------------------------------------

You can derive a class that implements the same operations to use with a
different GeoIP service by subclassing the GeoIpLookup class.

See :py:class:`GeoIpLookup<msticpy.sectools.geoip.GeoIpLookup>`

You should override the lookup_ip method implementing your own method of
geoip lookup.


Calculating Geographical Distances
----------------------------------

Use the :py:func:`geo_distance<msticpy.sectools.geoip.geo_distance>` function from
msticpy.sectools.geoip to calculate distances between two locations.

I am indebted to Martin Thoma who
posted this solution (which I’ve modified slightly) on Stackoverflow.


.. code:: ipython3

    from msticpy.sectools.geoip import geo_distance
    _, ip_entity1 = iplocation.lookup_ip(ip_address='90.156.201.97')
    _, ip_entity2 = iplocation.lookup_ip(ip_address='151.101.64.223')

    print(ip_entity1[0])
    print(ip_entity2[0])
    dist = geo_distance(origin=(ip_entity1[0].Location.Latitude, ip_entity1[0].Location.Longitude),
                        destination=(ip_entity2[0].Location.Latitude, ip_entity2[0].Location.Longitude))
    print(f'\nDistance between IP Locations = {round(dist, 1)}km')


.. parsed-literal::

    { 'Address': '90.156.201.97',
      'Location': { 'CountryCode': 'RU',
                    'CountryName': 'Russia',
                    'Latitude': 55.7386,
                    'Longitude': 37.6068,
                    'Type': 'geolocation'},
      'Type': 'ipaddress'}
    { 'Address': '151.101.64.223',
      'Location': { 'CountryCode': 'US',
                    'CountryName': 'United States',
                    'Latitude': 37.751,
                    'Longitude': -97.822,
                    'Type': 'geolocation'},
      'Type': 'ipaddress'}

    Distance between IP Locations = 8796.8km


.. code:: ipython3

    dist2 = entity_distance(ip_entity1[0],ip_entity2[0])
    print(f'\nDistance between IP Entity Locations = {round(dist2, 1)}km')


.. parsed-literal::


    Distance between IP Locations = 8796.8km


See also
--------

:doc:`../visualization/FoliumMap`