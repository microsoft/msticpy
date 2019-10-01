
msticpy Threat Intel Lookup
===========================

This document describes the use of the Threat Intelligence lookup class
in msticpy. The class allows lookup of individual or multiple IoCs from
one or more TI providers.

TILookup is also extensible - you can subclass TIProvider to implement
your own custom lookups. You can also subclass the HTTPProvider or
KqlProvider classes, which provide support for querying a REST endpoint
or Log Analytics table respectively.

Notebook
--------
`TIProvider Usage Notebook <https://github.com/microsoft/msticpy/blob/master/docs/notebooks/TIProviders.ipynb>`__

Table of Contents
-----------------

* TILookup class

  * Constructor
  * Methods
  * Available Providers

* Looking up IoCs

  * lookup_ioc
  * Lookup an IoC from a single provider
  * Convert result to a DataFrame and let pandas do the display work…
  * Lookup using all primary providers

* Provider Usage

  * Use to do a passive DNS lookup

* Inferring IoC Type vs. Specifying explicity

* Looking up Multiple IoCs

  * lookup_iocs
  * Multiple IoCs using all providers

* Specifying Time Ranges



TILookup class
--------------

Input can be a single IoC observable or a pandas DataFrame containing
multiple observables. Processing may require a an API key and processing
performance may be limited to a specific number of requests per minute
for the account type that you have.


Constructor
~~~~~~~~~~~


.. parsed-literal::


            Initialize TILookup instance.

            Parameters
            ----------
            primary_providers : Optional[List[TIProvider]], optional
                Primary TI Providers, by default None
            secondary_providers : Optional[List[TIProvider]], optional
                Secondary TI Providers, by default None


Methods
~~~~~~~

* *add_provider()* - Add a TI provider to the current collection.
* *available_providers()* - Return a list of builtin providers.
* *list_available_providers()* - Print a list of builtin providers with
  optional usage.
* *loaded_providers()* - Return dictionary of loaded providers.
* *lookup_ioc()* - Lookup single IoC in active providers.
* *lookup_iocs()* - Lookup a collection of IoCs.
* *provider_status()* - Return loaded provider status.
* *provider_usage()* - Print usage of loaded providers.
* *reload_provider_settings()* - Reload provider settings from config.
* *reload_providers()* - Reload providers based on currret settings in config.
* *result_to_df()* - Return DataFrame representation of IoC Lookup response.


Available Providers
-------------------

The **msticpy** TI Provider library can lookup IoCs in multiple
providers.

The list below shows the current set of providers.

.. code:: ipython3

    ti_lookup = TILookup()
    # List available providers
    ti_lookup.available_providers


.. parsed-literal::

    ['AzSTI', 'OTX', 'VirusTotal', 'XForce']



Configuration File
------------------

You **must** have a correctly configured ``msticpyconfig.yaml`` in
order to use the TILookup module. In this file you specify the
providers you want to load, any API keys that the provider services
require. You can configure primary and secondary providers.

Primary providers are those used by default, when you query for IoCs
without specifying any specific provider names. You may want to
add some providers as secondary so that they are not used for every
query but are available if you want to search more widely.

You will usually need to supply an authorization (API) key and in some cases a
user ID for each provider.

For LogAnalytics/Azure Sentinel providers, you will need the workspace
ID and tenant ID and will need to authenticate in order to access the
data (although if you have an existing authenticated connection with the
same workspace/tenant, this connection will be re-used).

The configuration file is read from the current directory.

Alternatively, you can specify a location for this file in an
environment variable ``MSTICPYCONFIG``.

If you need to create a config file, use the content shown below.
The ``Provider`` key must correspond to an available Python class.
The names of the built-in provider classes are shown in the
``ti_lookup.available_providers`` property.

Delete any provider entries from the example below that you do not want
to use and add the missing parameters for your providers.
Save the file as ``msticpyconfig.yaml``.


.. note:: If you have your Azure Sentinel workspace and tenant IDs configured
  either in a config.json file or in the `AzureSentinel` configuration section
  of the `msticpyconfig.yaml` you do not need to set these values for the
  provider here. They will be inherited from the global configuration.


.. code:: yaml

    QueryDefinitions:

    TIProviders:
      OTX:
        Args:
          AuthKey: "your-otx-key"
        Primary: True
        Provider: "OTX" # Explicitly name provider to override
      VirusTotal:
        Args:
          AuthKey: "your-vt-key"
        Primary: True
        Provider: "VirusTotal"
      XForce:
        Args:
          ApiID: "your-xforce-id"
          AuthKey: "your-xforce-key"
        Primary: True
        Provider: "XForce"
      AzureSentinel:
        Args:
          WorkspaceID: "your-azure-sentinel-workspace-id"
          TenantID: "your-azure-sentinel-tenant-id"
        Primary: True
        Provider: "AzSTI"


.. note:: You can also specify that the Args values as environment
  variables as follows:


.. code:: yaml

      XForce:
          Args:
            ApiID:
              EnvironmentVar: "XFORCE_ID"
            AuthKey:
              EnvironmentVar: "XFORCE_KEY"
          Primary: False
          Provider: "XForce"



When you have made a configuration change you can reload the
providers and check the status like this.


.. code:: ipython3

    ti_lookup.reload_providers()
    ti_lookup.provider_status


.. parsed-literal::

    ['OTX - AlientVault OTX Lookup. (primary)',
     'VirusTotal - VirusTotal Lookup. (primary)',
     'XForce - IBM XForce Lookup. (primary)',
     'AzSTI - Azure Sentinel TI provider class. (primary)']



Looking up IoCs
---------------

lookup_ioc
~~~~~~~~~~

To lookup a single IoC.

::

   ti_lookup.lookup_ioc(
       observable: str = None,
       ioc_type: str = None,
       ioc_query_type: str = None,
       providers: List[str] = None,
       prov_scope: str = 'primary',
       **kwargs,
   ) -> Tuple[bool, List[Tuple[str, msticpy.sectools.tiproviders.ti_provider_base.LookupResult]]]

   Lookup single IoC in active providers.

   Parameters
   ----------
   observable : str
       IoC observable
       (`ioc` is also an alias for observable)
   ioc_type : str, optional
       One of IoCExtract.IoCType, by default None
       If none, the IoC type will be inferred
   ioc_query_type: str, optional
       The ioc query type (e.g. rep, info, malware)
   providers: List[str]
       Explicit list of providers to use
   prov_scope : str, optional
       Use primary, secondary or all providers, by default "primary"
   kwargs :
       Additional arguments passed to the underlying provider(s)

   Returns
   -------
   Tuple[bool, List[Tuple[str, LookupResult]]]
       The result returned as a tuple(bool, list):
       bool indicates whether a TI record was found in any provider
       list has an entry for each provider result


Lookup an IoC from a single provider
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

And show the output

.. code:: ipython3

    result, details = ti_lookup.lookup_ioc(observable="38.75.137.9", providers=["OTX"])

    print("Positive" if result else "Negative")
    # the details is a list (since there could be multiple responses for an IoC)
    for provider, detail in details:
        print(provider)
        detail.summary
        print("\nRaw Results")
        detail.raw_result_fmt



.. parsed-literal::

    Positive
    OTX
    ioc: 38.75.137.9 ( ipv4 )
    result: True
    {   'names': ['Underminer EK'],
        'pulse_count': 1,
        'references': [   [   'https://blog.malwarebytes.com/threat-analysis/2019/07/exploit-kits-summer-2019-review/']],
        'tags': [[]]}
    reference:  https://otx.alienvault.com/api/v1/indicators/IPv4/38.75.137.9/general

    Raw Results
    { 'area_code': 0,
      'asn': 'AS63023 GTHost',
      'base_indicator': { 'access_reason': '',
                          'access_type': 'public',
                          'content': '',
                          'description': '',
                          'id': 2127020821,
                          'indicator': '38.75.137.9',
                          'title': '',
                          'type': 'IPv4'},
      'charset': 0,
      'city': 'Los Angeles',
      'city_data': True,
      'continent_code': 'NA',
      'country_code': 'US',
      'country_code3': 'USA',
      'country_name': 'United States',
      'dma_code': 803,
      'flag_title': 'United States',
      'flag_url': '/assets/images/flags/us.png',
      'indicator': '38.75.137.9',
      'latitude': 34.0584,
      'longitude': -118.278,
      'postal_code': '90017',
      'pulse_info': { 'count': 1,
                      'pulses': [ { 'TLP': 'white',
                                    'adversary': '',
                                    'attack_ids': [],
                                    'author': { 'avatar_url': 'https://otx.alienvault.com/assets/images/default-avatar.png',
                                                'id': '79520',
                                                'is_following': False,
                                                'is_subscribed': False,
                                                'username': 'mattvittitoe'},
                                    'cloned_from': None,
                                    'comment_count': 0,
                                    'created': '2019-07-31T18:01:29.744000',
                                    'description': '',
                                    'downvotes_count': 0,
                                    'export_count': 0,
                                    'follower_count': 0,
                                    'groups': [ { 'id': 614,
                                                  'name': 'DCT Security Team'}],
                                    'id': '5d41d77901a2f8c6e9b650e9',
                                    'in_group': True,
                                    'indicator_count': 24,
                                    'indicator_type_counts': { 'FileHash-MD5': 5,
                                                               'IPv4': 3,
                                                               'URL': 16},
                                    'industries': [],
                                    'is_author': False,
                                    'is_modified': False,
                                    'is_subscribing': None,
                                    'locked': 0,
                                    'malware_families': [],
                                    'modified': '2019-07-31T18:01:29.744000',
                                    'modified_text': '19 days ago ',
                                    'name': 'Underminer EK',
                                    'public': 1,
                                    'pulse_source': 'web',
                                    'references': [ 'https://blog.malwarebytes.com/threat-analysis/2019/07/exploit-kits-summer-2019-review/'],
                                    'subscriber_count': 10,
                                    'tags': [],
                                    'targeted_countries': [],
                                    'threat_hunter_scannable': True,
                                    'upvotes_count': 0,
                                    'validator_count': 0,
                                    'vote': 0,
                                    'votes_count': 0}],
                      'references': [ 'https://blog.malwarebytes.com/threat-analysis/2019/07/exploit-kits-summer-2019-review/']},
      'region': 'CA',
      'reputation': 0,
      'sections': [ 'general',
                    'geo',
                    'reputation',
                    'url_list',
                    'passive_dns',
                    'malware',
                    'nids_list',
                    'http_scans'],
      'type': 'IPv4',
      'type_title': 'IPv4',
      'whois': 'http://whois.domaintools.com/38.75.137.9'}


Or convert result to a DataFrame and let pandas do the display work…
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. code:: ipython3

    result = ti_lookup.lookup_ioc(observable="38.75.137.9", providers=["OTX"])
    ti_lookup.result_to_df(result).T


.. raw:: html

    <div>
    <style scoped>
        .dataframe tbody tr th:only-of-type {
            vertical-align: middle;
        }

        .dataframe tbody tr th {
            vertical-align: top;
        }

        .dataframe thead th {
            text-align: right;
        }
    </style>
    <table border="1" class="dataframe">
      <thead>
        <tr style="text-align: right;">
          <th></th>
          <th>OTX</th>
        </tr>
      </thead>
      <tbody>
        <tr>
          <th>IoC</th>
          <td>38.75.137.9</td>
        </tr>
        <tr>
          <th>IoCType</th>
          <td>ipv4</td>
        </tr>
        <tr>
          <th>QuerySubtype</th>
          <td>None</td>
        </tr>
        <tr>
          <th>Result</th>
          <td>True</td>
        </tr>
        <tr>
          <th>Details</th>
          <td>{'pulse_count': 1, 'names': ['Underminer EK'], 'tags': [[]], 'references': [['https://blog.malwa...</td>
        </tr>
        <tr>
          <th>RawResult</th>
          <td>{'sections': ['general', 'geo', 'reputation', 'url_list', 'passive_dns', 'malware', 'nids_list',...</td>
        </tr>
        <tr>
          <th>Reference</th>
          <td>https://otx.alienvault.com/api/v1/indicators/IPv4/38.75.137.9/general</td>
        </tr>
        <tr>
          <th>Status</th>
          <td>200</td>
        </tr>
      </tbody>
    </table>
    </div>



.. code:: ipython3

    # Extract a single field (RawResult) from the dataframe (.iloc[0] is to select the row)
    ti_lookup.result_to_df(result)["RawResult"].iloc[0]




.. parsed-literal::

    {'sections': ['general',
      'geo',
      'reputation',
      'url_list',
      'passive_dns',
      'malware',
      'nids_list',
      'http_scans'],
     'city': 'Los Angeles',
     'area_code': 0,
     'pulse_info': {'count': 1,
      'references': ['https://blog.malwarebytes.com/threat-analysis/2019/07/exploit-kits-summer-2019-review/'],
      'pulses': [{'indicator_type_counts': {'URL': 16,
         'FileHash-MD5': 5,
         'IPv4': 3},
        'pulse_source': 'web',
        'TLP': 'white',
        'description': '',
        ...



Lookup using all primary providers
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: ipython3

    result = ti_lookup.lookup_ioc(observable="38.75.137.9")
    ti_lookup.result_to_df(result)



.. raw:: html

    <div>
    <style scoped>
        .dataframe tbody tr th:only-of-type {
            vertical-align: middle;
        }

        .dataframe tbody tr th {
            vertical-align: top;
        }

        .dataframe thead th {
            text-align: right;
        }
    </style>
    <table border="1" class="dataframe">
      <thead>
        <tr style="text-align: right;">
          <th></th>
          <th>IoC</th>
          <th>IoCType</th>
          <th>QuerySubtype</th>
          <th>Result</th>
          <th>Details</th>
          <th>RawResult</th>
          <th>Reference</th>
          <th>Status</th>
        </tr>
      </thead>
      <tbody>
        <tr>
          <th>OTX</th>
          <td>38.75.137.9</td>
          <td>ipv4</td>
          <td>None</td>
          <td>True</td>
          <td>{'pulse_count': 1, 'names': ['Underminer EK'], 'tags': [[]], 'references': [['https://blog.malwa...</td>
          <td>{'sections': ['general', 'geo', 'reputation', 'url_list', 'passive_dns', 'malware', 'nids_list',...</td>
          <td>https://otx.alienvault.com/api/v1/indicators/IPv4/38.75.137.9/general</td>
          <td>200</td>
        </tr>
        <tr>
          <th>VirusTotal</th>
          <td>38.75.137.9</td>
          <td>ipv4</td>
          <td>None</td>
          <td>True</td>
          <td>{'verbose_msg': 'IP address in dataset', 'response_code': 1, 'detected_urls': ['http://38.75.137...</td>
          <td>{'asn': 63023, 'undetected_urls': [['http://38.75.137.9:9088/', '3d5edb0e0bb726e414a9b76dac619c1...</td>
          <td>https://www.virustotal.com/vtapi/v2/ip-address/report</td>
          <td>200</td>
        </tr>
        <tr>
          <th>XForce</th>
          <td>38.75.137.9</td>
          <td>ipv4</td>
          <td>None</td>
          <td>True</td>
          <td>{'score': 1, 'cats': {}, 'categoryDescriptions': {}, 'reason': 'Regional Internet Registry', 're...</td>
          <td>{'ip': '38.75.137.9', 'history': [{'created': '2012-03-22T07:26:00.000Z', 'reason': 'Regional In...</td>
          <td>https://api.xforce.ibmcloud.com/ipr/38.75.137.9</td>
          <td>200</td>
        </tr>
        <tr>
          <th>AzSTI</th>
          <td>38.75.137.9</td>
          <td>ipv4</td>
          <td>None</td>
          <td>False</td>
          <td>0 rows returned.</td>
          <td>None</td>
          <td>None</td>
          <td>-1</td>
        </tr>
      </tbody>
    </table>
    </div>



Provider Usage
--------------

This shows the supported IoC Types.

In some cases an IoC type will also support special types of sub-query
such as geo-ip and passive-dns

.. code:: ipython3

    ti_lookup.provider_usage()


.. parsed-literal::

    Primary providers
    -----------------

    Provider class: OTX
    AlientVault OTX Lookup. Supported query types:
      ioc_type=dns
      ioc_type=dns, ioc_query_type=geo
      ioc_type=dns, ioc_query_type=passivedns
      ioc_type=file_hash
      ioc_type=hostname
      ioc_type=ipv4
      ioc_type=ipv4, ioc_query_type=geo
      ioc_type=ipv4, ioc_query_type=passivedns
      ioc_type=ipv6
      ioc_type=ipv6, ioc_query_type=geo
      ioc_type=ipv6, ioc_query_type=passivedns
      ioc_type=md5_hash
      ioc_type=sha1_hash
      ioc_type=sha256_hash
      ioc_type=url

    Provider class: VirusTotal
    VirusTotal Lookup. Supported query types:
      ioc_type=dns
      ioc_type=file_hash
      ioc_type=ipv4
      ioc_type=md5_hash
      ioc_type=sha1_hash
      ioc_type=sha256_hash
      ioc_type=url

    Provider class: XForce
    IBM XForce Lookup. Supported query types:
      ioc_type=dns, ioc_query_type=info
      ioc_type=dns, ioc_query_type=passivedns
      ioc_type=dns, ioc_query_type=whois
      ioc_type=file_hash
      ioc_type=hostname, ioc_query_type=whois
      ioc_type=ipv4
      ioc_type=ipv4, ioc_query_type=malware
      ioc_type=ipv4, ioc_query_type=passivedns
      ioc_type=ipv4, ioc_query_type=rep
      ioc_type=ipv4, ioc_query_type=whois
      ioc_type=ipv6
      ioc_type=ipv6, ioc_query_type=malware
      ioc_type=ipv6, ioc_query_type=passivedns
      ioc_type=ipv6, ioc_query_type=rep
      ioc_type=ipv6, ioc_query_type=whois
      ioc_type=md5_hash
      ioc_type=sha1_hash
      ioc_type=sha256_hash
      ioc_type=url
      ioc_type=url, ioc_query_type=malware

    Provider class: AzSTI
    Azure Sentinel TI provider class. Supported query types:
      ioc_type=dns
      ioc_type=file_hash
      ioc_type=hostname
      ioc_type=ipv4
      ioc_type=ipv6
      ioc_type=linux_path
      ioc_type=md5_hash
      ioc_type=sha1_hash
      ioc_type=sha256_hash
      ioc_type=url
      ioc_type=windows_path

    Secondary providers
    -------------------
    none


Use to do a passive DNS lookup
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: ipython3

    result = ti_lookup.lookup_ioc(observable="38.75.137.9", ico_type="ipv4", ioc_query_type="passivedns", providers=["XForce"])
    result




.. parsed-literal::

    (True,
     [('XForce',
       LookupResult(ioc='38.75.137.9', ioc_type='ipv4', query_subtype='passivedns', result=True, details={'records': 1}, raw_result={'Passive': {'query': '0x00000000000000000000ffff264b8909', 'records': []}, 'RDNS': ['9-137-75-38.clients.gthost.com'], 'total_rows': 1}, reference='https://api.xforce.ibmcloud.com/resolve/38.75.137.9', status=200))])



Inferring IoC Type vs. Specifying explicity
-------------------------------------------

If you do a lookup without specifying a type, TILookup will try to infer
the type by matching regexes. There are patterns for all supported types
but there are some caveats:

-  The match is not 100% foolproof - e.g. some URLs and hash types may
   be misidentified.
-  The inference adds an overhead to each lookup.

If you know the type that you want to look up, it is always better to
explicitly include it. - For single IoC lookup, use the ``ioc_type``
parameter. - For multiple IoC lookups (see below), supply either: - a
DataFrame with a column that specifies the type for each entry - a
dictionary of the form ``{ioc_observable: ioc_type}``

Looking up Multiple IoCs
------------------------

lookup_iocs
~~~~~~~~~~~

::

   Signature:
   ti_lookup.lookup_iocs(
       data: Union[pandas.core.frame.DataFrame, Mapping[str, str], Iterable[str]],
       obs_col: str = None,
       ioc_type_col: str = None,
       ioc_query_type: str = None,
       providers: List[str] = None,
       prov_scope: str = 'primary',
       **kwargs,
   ) -> pandas.core.frame.DataFrame

   Lookup a collection of IoCs.

   Parameters
   ----------
   data : Union[pd.DataFrame, Mapping[str, str], Iterable[str]]
       Data input in one of three formats:
       1. Pandas dataframe (you must supply the column name in
       `obs_col` parameter)
       2. Mapping (e.g. a dict) of [observable, IoCType]
       3. Iterable of observables - IoCTypes will be inferred
   obs_col : str, optional
       DataFrame column to use for observables, by default None
   ioc_type_col : str, optional
       DataFrame column to use for IoCTypes, by default None
   ioc_query_type: str, optional
       The ioc query type (e.g. rep, info, malware)
   providers: List[str]
       Explicit list of providers to use
   prov_scope : str, optional
       Use primary, secondary or all providers, by default "primary"
   kwargs :
       Additional arguments passed to the underlying provider(s)

   Returns
   -------
   pd.DataFrame
       DataFrame of results

.. code:: ipython3

    # View the docstring (as above)
    ti_lookup.lookup_iocs?

.. code:: ipython3

    ioc_ips = [
        "185.92.220.35",
        "213.159.214.86",
        "77.222.54.202",
        "91.219.29.81",
        "193.9.28.254",
        "89.108.83.196",
        "91.219.28.44",
        "188.127.231.124",
        "192.42.116.41",
        "91.219.31.18",
        "46.4.239.76",
        "188.166.168.250",
        "195.154.241.208",
        "51.255.172.55",
        "93.170.169.52",
        "104.215.148.63",
        "13.77.161.179",
        "40.76.4.15",
        "40.112.72.205",
        "40.113.200.201",
    ]

    ti_lookup.lookup_iocs(data=ioc_ips, providers="AzSTI")


.. raw:: html

    <div>
    <style scoped>
        .dataframe tbody tr th:only-of-type {
            vertical-align: middle;
        }

        .dataframe tbody tr th {
            vertical-align: top;
        }

        .dataframe thead th {
            text-align: right;
        }
    </style>
    <table border="1" class="dataframe">
      <thead>
        <tr style="text-align: right;">
          <th></th>
          <th>IoC</th>
          <th>IoCType</th>
          <th>QuerySubtype</th>
          <th>Reference</th>
          <th>Result</th>
          <th>Status</th>
          <th>Details</th>
          <th>RawResult</th>
          <th>Provider</th>
        </tr>
      </thead>
      <tbody>
        <tr>
          <th>0</th>
          <td>213.159.214.86</td>
          <td>ipv4</td>
          <td>None</td>
          <td>ThreatIntelligenceIndicator  | where TimeGenerated &gt;= datetime(2019-07-21T17:30:32.934234Z) | w...</td>
          <td>True</td>
          <td>0.0</td>
          <td>{'Action': 'alert', 'ThreatType': 'Malware', 'ThreatSeverity': nan, 'Active': True, 'Description...</td>
          <td>{'IndicatorId': '0164ADB4A6CB7A79FBAE7BE90A43050B090A18364E3855048AC86B9DA5E0A92B', 'TimeGenerat...</td>
          <td>AzSTI</td>
        </tr>
        <tr>
          <th>1</th>
          <td>40.113.200.201</td>
          <td>ipv4</td>
          <td>None</td>
          <td>ThreatIntelligenceIndicator  | where TimeGenerated &gt;= datetime(2019-07-21T17:30:32.934234Z) | w...</td>
          <td>False</td>
          <td>-1.0</td>
          <td>0 rows returned.</td>
          <td>NaN</td>
          <td>AzSTI</td>
        </tr>
        <tr>
          <th>2</th>
          <td>91.219.29.81</td>
          <td>ipv4</td>
          <td>None</td>
          <td>ThreatIntelligenceIndicator  | where TimeGenerated &gt;= datetime(2019-07-21T17:30:32.934234Z) | w...</td>
          <td>True</td>
          <td>0.0</td>
          <td>{'Action': 'alert', 'ThreatType': 'Malware', 'ThreatSeverity': nan, 'Active': True, 'Description...</td>
          <td>{'IndicatorId': '3F458D91A21866C9037B99D997379A6906573766C0C2F8FB45327A6A15676A0D', 'TimeGenerat...</td>
          <td>AzSTI</td>
        </tr>
        <tr>
          <th>3</th>
          <td>89.108.83.196</td>
          <td>ipv4</td>
          <td>None</td>
          <td>ThreatIntelligenceIndicator  | where TimeGenerated &gt;= datetime(2019-07-21T17:30:32.934234Z) | w...</td>
          <td>True</td>
          <td>0.0</td>
          <td>{'Action': 'alert', 'ThreatType': 'Malware', 'ThreatSeverity': nan, 'Active': True, 'Description...</td>
          <td>{'IndicatorId': 'C3CA82D5B30A34F4BD6188C9DCFAD9E46D3C0CC45CC4FD969DA3A398DC34B1AE', 'TimeGenerat...</td>
          <td>AzSTI</td>
        </tr>
        <tr>
          <th>4</th>
          <td>192.42.116.41</td>
          <td>ipv4</td>
          <td>None</td>
          <td>ThreatIntelligenceIndicator  | where TimeGenerated &gt;= datetime(2019-07-21T17:30:32.934234Z) | w...</td>
          <td>True</td>
          <td>0.0</td>
          <td>{'Action': 'alert', 'ThreatType': 'Malware', 'ThreatSeverity': nan, 'Active': True, 'Description...</td>
          <td>{'IndicatorId': '2F321C9D2593B6EF59DEB64B6CB209F375529C429F0DF463D639784E7353AA5D', 'TimeGenerat...</td>
          <td>AzSTI</td>
        </tr>
      </tbody>
    </table>
    </div>



Multiple IoCs using all providers
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Output sorted by IoC

.. code:: ipython3

    ioc_urls = [
        "http://cheapshirts.us/zVnMrG.php",
        "http://chinasymbolic.com/i9jnrc",
        "http://cetidawabi.com/468fd",
        "http://append.pl/srh9xsz",
        "http://aiccard.co.th/dvja1te",
        "http://ajaraheritage.ge/g7cberv",
        "http://cic-integration.com/hjy93JNBasdas",
        "https://google.com",  # benign
        "https://microsoft.com",  # benign
        "https://python.org",  # benign
    ]
    results = ti_lookup.lookup_iocs(data=ioc_urls)
    results.sort_values("IoC")


.. raw:: html

    <div>
    <style scoped>
        .dataframe tbody tr th:only-of-type {
            vertical-align: middle;
        }

        .dataframe tbody tr th {
            vertical-align: top;
        }

        .dataframe thead th {
            text-align: right;
        }
    </style>
    <table border="1" class="dataframe">
      <thead>
        <tr style="text-align: right;">
          <th></th>
          <th>IoC</th>
          <th>IoCType</th>
          <th>QuerySubtype</th>
          <th>Result</th>
          <th>Details</th>
          <th>RawResult</th>
          <th>Reference</th>
          <th>Provider</th>
          <th>Status</th>
        </tr>
      </thead>
      <tbody>
        <tr>
          <th>0</th>
          <td>http://aiccard.co.th/dvja1te</td>
          <td>url</td>
          <td>None</td>
          <td>True</td>
          <td>{'Action': 'alert', 'ThreatType': 'Malware', 'ThreatSeverity': nan, 'Active': True, 'Description...</td>
          <td>{'IndicatorId': 'FAE39C007D6554822504A1E0BDFD788E27DDC748ED63B258651DE52E4FA6D511', 'TimeGenerat...</td>
          <td>ThreatIntelligenceIndicator  | where TimeGenerated &gt;= datetime(2019-07-21T17:30:41.900764Z) | w...</td>
          <td>AzSTI</td>
          <td>0.0</td>
        </tr>
        <tr>
          <th>4</th>
          <td>http://aiccard.co.th/dvja1te</td>
          <td>url</td>
          <td>None</td>
          <td>True</td>
          <td>{'cats': None, 'categoryDescriptions': None}</td>
          <td>{'result': {'url': 'aiccard.co.th', 'cats': {}, 'score': None, 'categoryDescriptions': {}}, 'ass...</td>
          <td>https://api.xforce.ibmcloud.com/url/http://aiccard.co.th/dvja1te</td>
          <td>XForce</td>
          <td>NaN</td>
        </tr>
        <tr>
          <th>4</th>
          <td>http://aiccard.co.th/dvja1te</td>
          <td>url</td>
          <td>None</td>
          <td>True</td>
          <td>{'pulse_count': 3, 'names': ['Locky Ransomware Distribution Sites URL blocklist (LY_DS_URLBL)', ...</td>
          <td>{'indicator': 'http://aiccard.co.th/dvja1te', 'alexa': 'http://www.alexa.com/siteinfo/aiccard.co...</td>
          <td>https://otx.alienvault.com/api/v1/indicators/url/http://aiccard.co.th/dvja1te/general</td>
          <td>OTX</td>
          <td>NaN</td>
        </tr>
        <tr>
          <th>4</th>
          <td>http://aiccard.co.th/dvja1te</td>
          <td>url</td>
          <td>None</td>
          <td>False</td>
          <td>No response from provider.</td>
          <td>&lt;Response [403]&gt;</td>
          <td>https://www.virustotal.com/vtapi/v2/url/report</td>
          <td>VirusTotal</td>
          <td>NaN</td>
        </tr>
        <tr>
          <th>5</th>
          <td>http://ajaraheritage.ge/g7cberv</td>
          <td>url</td>
          <td>None</td>
          <td>True</td>
          <td>{'cats': None, 'categoryDescriptions': None}</td>
          <td>{'result': {'url': 'ajaraheritage.ge', 'cats': {}, 'score': None, 'categoryDescriptions': {}}, '...</td>
          <td>https://api.xforce.ibmcloud.com/url/http://ajaraheritage.ge/g7cberv</td>
          <td>XForce</td>
          <td>NaN</td>
        </tr>
      </tbody>
    </table>
    </div>



Specifying Time Ranges
----------------------

Some providers (currently only AzSTI) support time ranges
so that you can specify specific periods to search for.

If a provider does not support time ranges, the parameters will be
ignored

.. code:: ipython3

    from datetime import datetime
    search_origin = datetime(2019, 8, 5)
    q_times = nbwidgets.QueryTime(units="hour", auto_display=True, origin_time=search_origin, max_after=24, max_before=24)

    # Using this data range returned no results
    ti_lookup.lookup_iocs(data=ioc_ips, providers="AzSTI", start=q_times.start, end=q_times.end).head()


.. raw:: html

    <div>
    <style scoped>
        .dataframe tbody tr th:only-of-type {
            vertical-align: middle;
        }

        .dataframe tbody tr th {
            vertical-align: top;
        }

        .dataframe thead th {
            text-align: right;
        }
    </style>
    <table border="1" class="dataframe">
      <thead>
        <tr style="text-align: right;">
          <th></th>
          <th>IoC</th>
          <th>IoCType</th>
          <th>QuerySubtype</th>
          <th>Reference</th>
          <th>Result</th>
          <th>Details</th>
          <th>Status</th>
          <th>Provider</th>
        </tr>
      </thead>
      <tbody>
        <tr>
          <th>0</th>
          <td>213.159.214.86</td>
          <td>ipv4</td>
          <td>None</td>
          <td>ThreatIntelligenceIndicator  | where TimeGenerated &gt;= datetime(2019-08-04T00:00:00Z) | where Ti...</td>
          <td>False</td>
          <td>0 rows returned.</td>
          <td>-1</td>
          <td>AzSTI</td>
        </tr>
        <tr>
          <th>1</th>
          <td>40.113.200.201</td>
          <td>ipv4</td>
          <td>None</td>
          <td>ThreatIntelligenceIndicator  | where TimeGenerated &gt;= datetime(2019-08-04T00:00:00Z) | where Ti...</td>
          <td>False</td>
          <td>0 rows returned.</td>
          <td>-1</td>
          <td>AzSTI</td>
        </tr>
        <tr>
          <th>2</th>
          <td>91.219.29.81</td>
          <td>ipv4</td>
          <td>None</td>
          <td>ThreatIntelligenceIndicator  | where TimeGenerated &gt;= datetime(2019-08-04T00:00:00Z) | where Ti...</td>
          <td>False</td>
          <td>0 rows returned.</td>
          <td>-1</td>
          <td>AzSTI</td>
        </tr>
        <tr>
          <th>3</th>
          <td>89.108.83.196</td>
          <td>ipv4</td>
          <td>None</td>
          <td>ThreatIntelligenceIndicator  | where TimeGenerated &gt;= datetime(2019-08-04T00:00:00Z) | where Ti...</td>
          <td>False</td>
          <td>0 rows returned.</td>
          <td>-1</td>
          <td>AzSTI</td>
        </tr>
        <tr>
          <th>4</th>
          <td>192.42.116.41</td>
          <td>ipv4</td>
          <td>None</td>
          <td>ThreatIntelligenceIndicator  | where TimeGenerated &gt;= datetime(2019-08-04T00:00:00Z) | where Ti...</td>
          <td>False</td>
          <td>0 rows returned.</td>
          <td>-1</td>
          <td>AzSTI</td>
        </tr>
      </tbody>
    </table>
    </div>



.. code:: ipython3

    from datetime import datetime
    search_origin = datetime(2019, 8, 5)
    q_times = nbwidgets.QueryTime(units="day", auto_display=True, origin_time=search_origin, max_after=24, max_before=24)

    # Using a wider ranges produces results
    ti_lookup.lookup_iocs(data=ioc_ips, providers="AzSTI", start=q_times.start, end=q_times.end)




.. raw:: html

    <div>
    <style scoped>
        .dataframe tbody tr th:only-of-type {
            vertical-align: middle;
        }

        .dataframe tbody tr th {
            vertical-align: top;
        }

        .dataframe thead th {
            text-align: right;
        }
    </style>
    <table border="1" class="dataframe">
      <thead>
        <tr style="text-align: right;">
          <th></th>
          <th>IoC</th>
          <th>IoCType</th>
          <th>QuerySubtype</th>
          <th>Reference</th>
          <th>Result</th>
          <th>Status</th>
          <th>Details</th>
          <th>RawResult</th>
          <th>Provider</th>
        </tr>
      </thead>
      <tbody>
        <tr>
          <th>0</th>
          <td>213.159.214.86</td>
          <td>ipv4</td>
          <td>None</td>
          <td>ThreatIntelligenceIndicator  | where TimeGenerated &gt;= datetime(2019-07-12T00:00:00Z) | where Ti...</td>
          <td>True</td>
          <td>0.0</td>
          <td>{'Action': 'alert', 'ThreatType': 'Malware', 'ThreatSeverity': nan, 'Active': True, 'Description...</td>
          <td>{'IndicatorId': '0164ADB4A6CB7A79FBAE7BE90A43050B090A18364E3855048AC86B9DA5E0A92B', 'TimeGenerat...</td>
          <td>AzSTI</td>
        </tr>
        <tr>
          <th>1</th>
          <td>40.113.200.201</td>
          <td>ipv4</td>
          <td>None</td>
          <td>ThreatIntelligenceIndicator  | where TimeGenerated &gt;= datetime(2019-07-12T00:00:00Z) | where Ti...</td>
          <td>False</td>
          <td>-1.0</td>
          <td>0 rows returned.</td>
          <td>NaN</td>
          <td>AzSTI</td>
        </tr>
        <tr>
          <th>2</th>
          <td>91.219.29.81</td>
          <td>ipv4</td>
          <td>None</td>
          <td>ThreatIntelligenceIndicator  | where TimeGenerated &gt;= datetime(2019-07-12T00:00:00Z) | where Ti...</td>
          <td>True</td>
          <td>0.0</td>
          <td>{'Action': 'alert', 'ThreatType': 'Malware', 'ThreatSeverity': nan, 'Active': True, 'Description...</td>
          <td>{'IndicatorId': '3F458D91A21866C9037B99D997379A6906573766C0C2F8FB45327A6A15676A0D', 'TimeGenerat...</td>
          <td>AzSTI</td>
        </tr>
        <tr>
          <th>3</th>
          <td>89.108.83.196</td>
          <td>ipv4</td>
          <td>None</td>
          <td>ThreatIntelligenceIndicator  | where TimeGenerated &gt;= datetime(2019-07-12T00:00:00Z) | where Ti...</td>
          <td>True</td>
          <td>0.0</td>
          <td>{'Action': 'alert', 'ThreatType': 'Malware', 'ThreatSeverity': nan, 'Active': True, 'Description...</td>
          <td>{'IndicatorId': 'C3CA82D5B30A34F4BD6188C9DCFAD9E46D3C0CC45CC4FD969DA3A398DC34B1AE', 'TimeGenerat...</td>
          <td>AzSTI</td>
        </tr>
        <tr>
          <th>4</th>
          <td>192.42.116.41</td>
          <td>ipv4</td>
          <td>None</td>
          <td>ThreatIntelligenceIndicator  | where TimeGenerated &gt;= datetime(2019-07-12T00:00:00Z) | where Ti...</td>
          <td>True</td>
          <td>0.0</td>
          <td>{'Action': 'alert', 'ThreatType': 'Malware', 'ThreatSeverity': nan, 'Active': True, 'Description...</td>
          <td>{'IndicatorId': '2F321C9D2593B6EF59DEB64B6CB209F375529C429F0DF463D639784E7353AA5D', 'TimeGenerat...</td>
          <td>AzSTI</td>
        </tr>
      </tbody>
    </table>
    </div>
