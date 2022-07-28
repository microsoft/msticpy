IP Whois Enrichment
===================

MSTICPy supports enriching IP address information with data from open source Whois services.
Lookups are possible against IPs and ASNs.

IP Lookups
^^^^^^^^^^

Whois lookups can be performed against a single IP address or a as a bulk lookup against a list or
DataFrame column.
A single IP can be looked up with the `ip_whois` method.

.. code:: ipython3

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

If a list of IP addressed is passed to `ip_whois` then the data is returned as a DataFrame.
This same feature can be accessed as a Pandas accessor with `.mp_whois.lookup`

.. code:: ipython3

    >>> df.mp_whois.lookup("IPAddress")

As well as Whois data it is also possible to lookup details of the ASN an IP address belongs to.
This is done with the `get_asn_from_ip` function.

.. code:: ipython3

    >>> get_asn_from_ip("65.55.44.109")

.. parsed-literal::

    {'AS': '8075',
    'IP': '65.55.44.109',
    'BGP Prefix': '65.52.0.0/14',
    'CC': 'US',
    'Registry': 'arin',
    'Allocated': '2001-02-14',
    'AS Name': 'MICROSOFT-CORP-MSN-AS-BLOCK, US'}

ASN Lookups
^^^^^^^^^^^

In addition to lookups against an IP address it’s possible to look up against an ASN.
`get_asn_details` can be used to get details based on an ASN, along with details of the IP ranges
belonging to that ASN.

.. code:: ipython3

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

It is also possible to search of ASNs based on the AS Name. For example, you can search for "Microsoft"
to see a list of all ASNs that are associated with Microsoft with `get_asns_from_name`.

.. code:: ipython3

    >>> get_asn_from_name("Microsoft")

.. parsed-literal::

    {'AS3598': 'MICROSOFT-CORP-AS, US',
    'AS5761': 'MICROSOFT-CORP-MSN-AS-SATURN, US',
    'AS6182': 'MICROSOFT-CORP-MSN-AS-4, US',
    'AS6291': 'MICROSOFT-CORP-MSN-AS, US',
    'AS6584': 'MICROSOFT-GP-AS, US',
    ...

