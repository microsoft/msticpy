Cybereason Provider
===================

This driver provides functions to allow you to query a Cybereason instance.

Cybereason Configuration
------------------------

Authentication for the Cybereason data provider is handled by specifying
credentials directly in the connect call or specifying the credentials
in MSTICPy config file.

For more information on how to create new user with approapriate roles
and permissions, follow the product documentation:
`User Roles and Permissions <https://nest.cybereason.com/documentation/product-documentation/181/manage-user-roles-and-permissions>`__
and
`API Guide <https://nest.cybereason.com/documentation/api-documentation/all-versions/cybereason-api-guide>`__.

Cybereason Configuration in MSTICPy
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Once you created user account with the appropriate roles, you will
require the following details to specify while connecting:
- TenantId = "instance". As this is a cloud-based solution, each customer has its dedicated instance. FQDN will be formatted as: "<instance>.cybereason.net"
- ClientId = "account" (account to connect to Cybereason instance)
- ClientSecret = "yoursecret" (secret for the client specified in ClientId)

Once you have details, you can specify it in ``msticpyconfig.yaml`` as
shown in below example

::

  Cybereason:
    Args:
      TenantId: instance
      ClientId: account
      ClientSecret: yoursecret

Loading and Connecting a QueryProvider for Cybereason
-----------------------------------------------------

You can instantiate a data provider for Cybereason by specifying the
credentials in connect or if left blank details from your MSTICPy config file will be used.
If the details are correct and authentication is successful, it will show connected.

.. code:: ipython3

    cybereason_prov = QueryProvider('Cybereason')
    cybereason_prov.connect(TenantId=<instance>, ClientId=<id>, ClientSecret=<secret>)


.. parsed-literal::

    connected


Listing available queries
-------------------------

Upon connecting to the Cybereason data environment, we can take a look what
query options available to us by running
``QUERY_PROVIDER.list_queries()``

For more information, see
:ref:`data_acquisition/dataproviders:getting help for a query`.

This will display all the saved searches from the connected cybereason
instance and also pre-built custom queries to do common operations such
as list datatypes, list saved searches, alerts, audittrail informaion.

.. code:: ipython3

    cybereason_prov.list_queries()




.. parsed-literal::

  ['Connection.list_connections_from_process',
  'Host.find_hosts',
  'Process.find_process_by_commandLine',
  'Process.find_process_by_pid',
  'Process.find_process_by_suspicions']



In order to get help for specific query , you can execute
``QUERY_PROVIDER.<QueryName>?`` .

For more information, see
`Getting Help for a query <DataProviders:getting-help-for-a-query>`

.. code:: ipython3

    cybereason_prov.Connection.list_connections_from_process?


.. parsed-literal::

    Query:  list_connections_from_process
    Data source:  Cybereason
    Search for process with a specific suspicion

    Parameters
    ----------
    customFields: list (optional)
        List of fields to output
        (default value is: ['elementDisplayName', 'direction', 'ownerMachine', 'ownerProcess',
          'serverPort', 'serverAddress','portType', 'aggregatedReceivedBytesCount',
          'aggregatedTransmittedBytesCount', 'remoteAddressCountryName', 'dnsQuery',
          'accessedByMalwareEvidence', 'domainName', 'isExternalConnection',
          'remoteAddressInternalExternalLocal', 'calculatedCreationTime', 'endTime'
        ])
    end: datetime (optional)
        Query end time
    hostname: list
        Hostname where the process is running
    pid: list
        Command to search for
    start: datetime (optional)
        Query start time
        (default value is: -7)
    timeFeatureId: str (optional)
        Time boundary
        (default value is: startFeatureId)
    timefield: str (optional)
        Field to use for time
        (default value is: creationTime)


If you want to print the query prior to executing, pass ‘print’ as an
argument

.. code:: ipython3

    cybereason_prov.Connection.list_connections_from_process('print', hostname="hostname", pid=42)


.. parsed-literal::

    '{
        "queryPath" : [
            {
                "requestedType": "Process",
                "filters":[
                    {
                        "facetName": "applicablePid",
                        "values":[ 42 ],
                        "filterType":"Equals"
                    },
                    {
                        "facetName": "ownerMachine",
                        "values":[ "hostname" ],
                        "filterType":"Equals"
                    },
                    {
                        "facetName": "creationTime",
                        "values": [ 1643011155594, 1643615955594 ],
                        "filterType":"Between"
                    }
                ],
                "connectionFeature": {
                  "elementInstanceType": "Process",
                  "featureName": "connections"
                }
            },
            {
                "requestedType": "Connection",
                "filters":[],
                "isResult": true
            }
        ],
        "customFields": [
          "elementDisplayName","direction","ownerMachine","ownerProcess",
          "serverPort","serverAddress","portType","aggregatedReceivedBytesCount",
          "aggregatedTransmittedBytesCount","remoteAddressCountryName","dnsQuery",
          "accessedByMalwareEvidence","domainName","isExternalConnection",
          "remoteAddressInternalExternalLocal","calculatedCreationTime","endTime"
        ]
      }'


If you have set the arguments and then would like to validate the query,
use below example

.. code:: ipython3

    cybereason_prov.Connection.list_connections_from_process('print',
        hostname="hostname",
        pid=42
        start=-10,
        end=-2
    )




.. parsed-literal::

    ' {
        "queryPath" : [
            {
                "requestedType": "Process",
                "filters":[
                    {
                        "facetName": "applicablePid",
                        "values":[ 42 ],
                        "filterType":"Equals"
                    },
                    {
                        "facetName": "ownerMachine",
                        "values":[ "hostname" ],
                        "filterType":"Equals"
                    },
                    {
                        "facetName": "creationTime",
                        "values": [ 1642752424307, 1643443624308 ],
                        "filterType":"Between"
                    }
                ],
                "connectionFeature": {
                  "elementInstanceType": "Process",
                  "featureName": "connections"
                }
            },
            {
                "requestedType": "Connection",
                "filters":[],
                "isResult": true
            }
        ],
        "customFields": ["elementDisplayName","direction","ownerMachine","ownerProcess",
          "serverPort","serverAddress","portType","aggregatedReceivedBytesCount",
          "aggregatedTransmittedBytesCount","remoteAddressCountryName","dnsQuery",
          "accessedByMalwareEvidence","domainName","isExternalConnection",
          "remoteAddressInternalExternalLocal","calculatedCreationTime","endTime"
        ]
      }'



Running pre-defined queries
---------------------------

In order to run pre-defined query, call the provider followed by the query name.
Pre-defined queries can be run with either values specified as arguments
or run with default arguments.

For more information , refer to the documentation
:ref:`Running a pre-defined query <data_acquisition/dataproviders:running a pre-defined query>`

.. code:: ipython3

    cybereason_prov.Connection.list_connections_from_process('print',
        hostname="hostname",
        pid=42
        start=-10,
        end=-2
    )




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
          <th>remoteAddressCountryName</th>
          <th>aggregatedReceivedBytesCount</th>
          <th>endTime</th>
          <th>portType</th>
          <th>accessedByMalwareEvidence</th>
          <th>group</th>
          <th>elementDisplayName</th>
          <th>aggregatedTransmittedBytesCount</th>
          <th>isExternalConnection</th>
          <th>serverAddress</th>
          <th>serverPort</th>
          <th>calculatedCreationTime</th>
          <th>direction</th>
          <th>ownerMachine.Machine</th>
          <th>ownerMachine.Process</th>
          <th>dnsQuery.DnsQueryResolvedDomainToIp</th>
        </tr>
      </thead>
      <tbody>
        <tr>
          <th>0</th>
          <td>France</td>
          <td>1235</td>
          <td>2021-12-20 07:01:21</td>
          <td>SERVICE_HTTP</td>
          <td>false</td>
          <td>6d0da6b2-e909-411a-95b7-3869f9147919</td>
          <td>10.11.12.13:53154 > 1.2.3.4:80</td>
          <td>314</td>
          <td>false</td>
          <td>> 1.2.3.4</td>
          <td>80</td>
          <td>2021-12-20 07:01:20</td>
          <td>OUTGOING</td>
          <td>hostname</td>
          <td>process.exe</td>
          <td>external.domain.tld > 1.2.3.4</td>
        </tr>
      </tbody>
    </table>
    <p>1 row × 16 columns</p>
    </div>



Running an ad-hoc Cybereason query
----------------------------------

You can also create your own query and run it via the Cybereason
provider using this syntax:
``QUERY_PROVIDER.exec_query(<query_text>)``

For more information, check documentation :ref:`data_acquisition/dataproviders:running an ad hoc query`

.. code:: ipython3

    cybereason_query = '''
      {
        "queryPath" : [
          {
            "requestedType": "Connection",
            "filters":[],
            "isResult": true
          }
        ]
      }
    '''
    df = cybereason_prov.exec_query(cybereason_query)
    df.head()



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
          <th>remoteAddressCountryName</th>
          <th>aggregatedReceivedBytesCount</th>
          <th>endTime</th>
          <th>portType</th>
          <th>accessedByMalwareEvidence</th>
          <th>group</th>
          <th>elementDisplayName</th>
          <th>aggregatedTransmittedBytesCount</th>
          <th>isExternalConnection</th>
          <th>serverAddress</th>
          <th>serverPort</th>
          <th>calculatedCreationTime</th>
          <th>direction</th>
          <th>ownerMachine.Machine</th>
          <th>ownerMachine.Process</th>
          <th>dnsQuery.DnsQueryResolvedDomainToIp</th>
        </tr>
      </thead>
      <tbody>
        <tr>
          <th>0</th>
          <td>France</td>
          <td>1235</td>
          <td>2021-12-20 07:01:21</td>
          <td>SERVICE_HTTP</td>
          <td>false</td>
          <td>6d0da6b2-e909-411a-95b7-3869f9147919</td>
          <td>10.11.12.13:53154 > 1.2.3.4:80</td>
          <td>314</td>
          <td>false</td>
          <td>> 1.2.3.4</td>
          <td>80</td>
          <td>2021-12-20 07:01:20</td>
          <td>OUTGOING</td>
          <td>hostname</td>
          <td>process.exe</td>
          <td>external.domain.tld > 1.2.3.4</td>
        </tr>
      </tbody>
    </table>
    </div>

|

Other Cybereason Documentation
------------------------------

-  `Cybereason Documentation
   <https://nest.cybereason.com/documentation/product-documentation/202/cybereason-202-documentation>`__
-  `Cybereason API Documentation <https://nest.cybereason.com/documentation/api-documentation/all-versions/cybereason-api-guide>`__
-  `Cybereason Tips for the API
   <https://nest.cybereason.com/documentation/api-documentation/all-versions/tips-using-api-documentation#tips-for-using-the-api-documentation>`__
