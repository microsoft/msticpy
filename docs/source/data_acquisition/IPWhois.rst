IP Whois Enrichment
===================

MSTICPy supports enriching IP address information with data from open source Whois services.
Lookups are possible against IPs and ASNs (Autonomous System Number).

IP Lookups
----------

Whois lookups can be performed against a single IP address or as a bulk lookup against a list or
DataFrame column.

WhoIs Lookup of single IP Address
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The :py:func:`ip_whois<msticpy.context.ip_utils.ip_whois>` function looks up an single IP Address and returns a results
as a Python dictionary.

.. code:: ipython3

    >>> from msticpy.context.ip_utils import ip_whois
    >>> ip_whois("65.55.44.109")

.. parsed-literal::

    ('MICROSOFT-CORP-MSN-AS-BLOCK, US',
    {'asn': '8075',
    'query': '65.55.44.109',
    'asn_cidr': '65.52.0.0/14',
    'asn_country_code': 'US',
    'asn_registry': 'arin',
    'asn_date': '2001-02-14',
    'asn_description': 'MICROSOFT-CORP-MSN-AS-BLOCK, US',
    'nets': [{'cidr': '65.52.0.0/14',
        'handle': 'NET-65-52-0-0-1',
        'name': 'MICROSOFT-1BLK',
        'startAddress': '65.52.0.0',
        'endAddress': '65.55.255.255',
        'created': None,
    ...

You can also lookup a single IP Address using the ``IpAddress.whois`` function. This
returns results as a pandas DataFrame.

.. code:: ipython3

    >>> IpAddress.whois(["123.1.2.3", "124.5.6.7"])

WhoIs Lookup of multiple IP Addresses
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

If a list of IP addresses (or a pandas series) is passed to ``ip_whois``
then the data is returned as a DataFrame.

This same feature can be accessed using the ``mp`` pandas accessor or via the
``IpAddress.whois`` pivot function.

Using the ``mp`` pandas accessor:

.. code:: ipython3

    >>> df.mp.whois(ip_column="IPAddress")


Using the whois pivot function:

.. code:: ipython3

    >>> IpAddress.whois(["123.1.2.3", "124.5.6.7"])
    >>> IpAddress.whois(data=df, column="IP")

ASN Lookups
-----------

ASN Lookup by IP
^^^^^^^^^^^^^^^^

It is also possible to lookup details of the ASN that an IP address belongs to.
This is done with the :py:func:`get_asn_from_ip<msticpy.context.ip_utils.get_asn_from_ip>` function.

.. code:: ipython3

    >>> from msticpy.context.ip_utils import get_asn_from_ip
    >>> get_asn_from_ip("65.55.44.109")

.. parsed-literal::

    {'AS': '8075',
    'IP': '65.55.44.109',
    'BGP Prefix': '65.52.0.0/14',
    'CC': 'US',
    'Registry': 'arin',
    'Allocated': '2001-02-14',
    'AS Name': 'MICROSOFT-CORP-MSN-AS-BLOCK, US'}

The same function is also accessible via the ``IpAddress.whois_asn`` pivot function:

.. code:: ipython3

    >>> IpAddress.whois_asn("65.55.44.109")

This function can accepts a single IP, an iterable of IPs or a DataFrame (
in the latter case specify the dataframe via the ``data`` parameter and the
IP column via the ``column`` parameter).


ASN Lookup by Number or Name
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

You can get details of a specific to look up against an ASN.
:py:func:`get_asn_details<msticpy.context.ip_utils.get_asn_details>` can
be used to get details based on an ASN, along with details of the IP ranges
belonging to that ASN.

.. code:: ipython3

    >>> from msticpy.context.ip_utils import get_asn_details
    >>> get_asn_details("AS3598")

.. parsed-literal::

    {'Autonomous Number': 'AS3598',
    'AS Name': 'MICROSOFT',
    'Description': 'MICROSOFT',
    'Contact': 'radb@microsoft.com',
    'Last Updated': 'mkasten@microsoft.com 20180125',
    'ranges': ['167.220.204.0/22',
    '157.57.0.0/16',
    '157.58.0.0/16',
    '157.58.31.0/24',
    '157.58.192.0/19',
    '157.59.0.0/16',
    ...

It is also possible to search ASNs based on the AS Name. For example, you can search for "Microsoft"
to see a list of all ASNs that are associated with Microsoft with
:py:func:`get_asns_from_name<msticpy.context.ip_utils.get_asns_from_name>`.

.. code:: ipython3

    >>> get_asn_from_name("Microsoft")

.. parsed-literal::

    {'AS3598': 'MICROSOFT-CORP-AS, US',
    'AS5761': 'MICROSOFT-CORP-MSN-AS-SATURN, US',
    'AS6182': 'MICROSOFT-CORP-MSN-AS-4, US',
    'AS6291': 'MICROSOFT-CORP-MSN-AS, US',
    'AS6584': 'MICROSOFT-GP-AS, US',
    ...

