Splunk - Data Connector
=======================

Description
-----------

The data provider module of msticpy provides functions to allow for the
defining of data sources, connectors to them and queries for them as
well as the ability to return query result from the defined data
sources.

For more information on Data Propviders, check the documentation :doc:`DataProviders`

In this notebooks we will demonstrate Splunk data connector feature of
msticpy. This feature is built on-top of the [Splunk Enterprise SDK for
Python]
(https://dev.splunk.com/enterprise/docs/devtools/python/sdk-python/)
with some customizations and enhancements.

Installation
~~~~~~~~~~~~

.. code:: ipython3

    # Only run first time to install/upgrade msticpy to latest version
    #!pip install --upgrade msticpy[splunk]

Authentication
~~~~~~~~~~~~~~

Authentication for the Splunk data provider is handled by specifying
credentials directly in the connect call or specifying the credentials
in msticpy config file.

For more information on how to create new user with approapriate roles
and permissions, follow the Splunk documents:
`Addandeditusers <https://docs.splunk.com/Documentation/Splunk/8.0.5/Security/Addandeditusers>`__
and
`Aboutusersandroles <https://docs.splunk.com/Documentation/Splunk/8.0.5/Security/Aboutusersandroles>`__.
The user should have permission to at least run its own searches or more
depending upon the actions to be performed by user.

Once you created user account with the appropriate roles, you will
require the following details to specify while connecting - host =
“localhost”(Splunk server FQDN hostname to connect, for locally
installed splunk, you can specify localhost) - port = 8089 (Splunk REST
API ) - username = “admin” (username to connect to Splunk instance) -
password = “yourpassword” (password of the userspecified in username)

Once you have details, you can specify it in ``msticpyconfig.yaml`` as
shown in below example

::

   SplunkApp:
     Args:
       host: "{Splunk server FQDN or localhost}"
       port: "{default 8089}"
       username: "{username with search permissions to connect}"
       password: "{password of the user specified}"

.. code:: ipython3

    #Check we are running Python 3.6
    import sys
    MIN_REQ_PYTHON = (3,6)
    if sys.version_info < MIN_REQ_PYTHON:
        print('Check the Kernel->Change Kernel menu and ensure that Python 3.6')
        print('or later is selected as the active kernel.')
        sys.exit("Python %s.%s or later is required.\n" % MIN_REQ_PYTHON)

    #imports
    import pandas as pd
    import msticpy.nbtools as nbtools

    #data library imports
    from msticpy.data.data_providers import QueryProvider

    print('Imports Complete')


.. parsed-literal::

    Imports Complete


Instantiating a query provider
------------------------------

You can instantiate a data provider for Splunk by specifying the
credentials in connect or in msticpy config file. If the details are
correct and authentication is successful, it will show connected.

.. code:: ipython3

    splunk_prov = QueryProvider('Splunk')
    splunk_prov.connect(host=<hostname>, username=<username>, password=<password>)


.. parsed-literal::

    connected


Listing available queries
-------------------------

Upon connecting to the Splunk data environment, we can take a look what
query options available to us by running
``QUERY_PROVIDER.list_queries()``

For more information, see
:ref:`data_acquisition/dataproviders:getting help for a query`.

This will display all the saved searches from the connected splunk
instance and also pre-built custom queries to do common operations such
as list datatypes, list saved searches, alerts, audittrail informaion.

.. code:: ipython3

    splunk_prov.list_queries()




.. parsed-literal::

    ['Alerts.list_all_alerts',
     'SavedSearches.Errors_in_the_last_24_hours',
     'SavedSearches.Errors_in_the_last_hour',
     'SavedSearches.License_Usage_Data_Cube',
     'SavedSearches.Load_sample_User_Agreements',
     'SavedSearches.Messages_by_minute_last_3_hours',
     'SavedSearches.Orphaned_scheduled_searches',
     'SavedSearches.Score-Base',
     'SavedSearches.Splunk_errors_last_24_hours',
     'SavedSearches.Website_Performance_Problem',
     'SavedSearches.inoperable_sites_rangemap',
     'SavedSearches.slow_sites_avg_rangemap',
     'SavedSearches.slow_sites_rangemap',
     'SavedSearches.web_ping_inputs_lookup_gen',
     'SavedSearches.website_availability_overview',
     'SavedSearches.website_performance_problems',
     'SplunkGeneral.get_events_parameterized',
     'SplunkGeneral.list_all_datatypes',
     'SplunkGeneral.list_all_savedsearches',
     'audittrail.list_all_audittrail']



In order to get help for specific query , you can execute
``QUERY_PROVIDER.<QueryName>?`` .

For more information, see
`Getting Help for a query <DataProviders:getting-help-for-a-query>`

.. code:: ipython3

    splunk_prov.SplunkGeneral.get_events_parameterized?


.. parsed-literal::

    Query:  get_events_parameterized
    Data source:  Splunk
    Generic parameterized query from index/source

    Parameters
    ----------
    add_query_items: str (optional)
        Additional query clauses
        (default value is: | head 100)
    end: datetime (optional)
        Query end time
        (default value is: 08/26/2017:00:00:00)
    index: str (optional)
        Splunk index name
        (default value is: \*)
    project_fields: str (optional)
        Project Field names
        (default value is: | table TimeCreated, host, EventID, EventDescripti...)
    source: str (optional)
        Splunk source type
        (default value is: \*)
    start: datetime (optional)
        Query start time
        (default value is: 08/25/2017:00:00:00)
    timeformat: str (optional)
        Datetime format to use in Splunk query
        (default value is: "%Y-%m-%d %H:%M:%S.%6N")
    Query:
     search index={index} source={source} timeformat={timeformat} earliest={start} latest={end} {project_fields} {add_query_items}


If you want to print the query prior to executing, pass ‘print’ as an
argument

.. code:: ipython3

    splunk_prov.SplunkGeneral.get_events_parameterized('print')




.. parsed-literal::

    ' search index=* source=* timeformat="%Y-%m-%d %H:%M:%S.%6N" earliest="2020-08-15 19:15:47.466710" latest="2020-08-15 19:15:47.466938" | table TimeCreated, host, EventID, EventDescription, User, process, cmdline, Image, parent_process, ParentCommandLine, dest, Hashes | head 100'



If you have set the arguments and then would like to validate the query,
use below example

.. code:: ipython3

    splunk_prov.SplunkGeneral.get_events_parameterized('print',
        index="botsv2",
        source="WinEventLog:Microsoft-Windows-Sysmon/Operational",
        timeformat="%Y-%m-%d %H:%M:%S",
        start="2017-08-25 00:00:00",
        end="2017-08-25 10:00:00"
    )




.. parsed-literal::

    ' search index=botsv2 source=WinEventLog:Microsoft-Windows-Sysmon/Operational
      timeformat=%Y-%m-%d %H:%M:%S earliest="2017-08-25 00:00:00" latest="2017-08-25 10:00:00"
      | table TimeCreated, host, EventID, EventDescription, User, process, cmdline, Image,
      parent_process, ParentCommandLine, dest, Hashes | head 100'



Running pre-defined queries
---------------------------

In order to run pre-defined query , execute with the name either by
setting values for arguments if available or run with default arguments.

For more information , refer to the documentation
:ref:`Running a pre-defined query <data_acquisition/dataproviders:running a pre-defined query>`

.. code:: ipython3

    splunk_prov.SplunkGeneral.get_events_parameterized(
        index="botsv2",
        source="WinEventLog:Microsoft-Windows-Sysmon/Operational",
        start="2017-08-25 00:00:00.000000",
        end="2017-08-25 10:00:00.000000"
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
          <th>TimeCreated</th>
          <th>host</th>
          <th>EventID</th>
          <th>EventDescription</th>
          <th>User</th>
          <th>process</th>
        </tr>
      </thead>
      <tbody>
        <tr>
          <th>0</th>
          <td>2017-08-25T04:57:45.512440700Z</td>
          <td>venus</td>
          <td>3</td>
          <td>Network Connect</td>
          <td>NT AUTHORITY\SYSTEM</td>
          <td>powershell.exe</td>
        </tr>
        <tr>
          <th>1</th>
          <td>2017-08-25T04:57:45.213738500Z</td>
          <td>wrk-aturing</td>
          <td>5</td>
          <td>Process Terminate</td>
          <td>NaN</td>
          <td>conhost.exe</td>
        </tr>
        <tr>
          <th>2</th>
          <td>2017-08-25T04:57:45.213738500Z</td>
          <td>wrk-aturing</td>
          <td>5</td>
          <td>Process Terminate</td>
          <td>NaN</td>
          <td>cscript.exe</td>
        </tr>
        <tr>
          <th>3</th>
          <td>2017-08-25T04:57:45.088941700Z</td>
          <td>wrk-aturing</td>
          <td>1</td>
          <td>Process Create</td>
          <td>NT AUTHORITY\SYSTEM</td>
          <td>conhost.exe</td>
        </tr>
        <tr>
          <th>4</th>
          <td>2017-08-25T04:57:45.088941700Z</td>
          <td>wrk-aturing</td>
          <td>1</td>
          <td>Process Create</td>
          <td>NT AUTHORITY\SYSTEM</td>
          <td>cscript.exe</td>
        </tr>
        <tr>
          <th>...</th>
          <td>...</td>
          <td>...</td>
          <td>...</td>
          <td>...</td>
          <td>...</td>
          <td>...</td>
        </tr>
        <tr>
          <th>95</th>
          <td>2017-08-25T04:57:02.003800000Z</td>
          <td>wrk-ghoppy</td>
          <td>1</td>
          <td>Process Create</td>
          <td>NT AUTHORITY\SYSTEM</td>
          <td>splunk-powershell.exe</td>
        </tr>
        <tr>
          <th>96</th>
          <td>2017-08-25T04:57:01.170335100Z</td>
          <td>venus</td>
          <td>3</td>
          <td>Network Connect</td>
          <td>NT AUTHORITY\SYSTEM</td>
          <td>powershell.exe</td>
        </tr>
        <tr>
          <th>97</th>
          <td>2017-08-25T04:57:01.941402000Z</td>
          <td>wrk-ghoppy</td>
          <td>5</td>
          <td>Process Terminate</td>
          <td>NaN</td>
          <td>splunk-winprintmon.exe</td>
        </tr>
        <tr>
          <th>98</th>
          <td>2017-08-25T04:57:01.863404500Z</td>
          <td>wrk-ghoppy</td>
          <td>1</td>
          <td>Process Create</td>
          <td>NT AUTHORITY\SYSTEM</td>
          <td>splunk-netmon.exe</td>
        </tr>
        <tr>
          <th>99</th>
          <td>2017-08-25T04:57:01.754208000Z</td>
          <td>wrk-ghoppy</td>
          <td>5</td>
          <td>Process Terminate</td>
          <td>NaN</td>
          <td>splunk-powershell.exe</td>
        </tr>
      </tbody>
    </table>
    <p>100 rows × 12 columns</p>
    </div>

|

By-default, splunk query results are limited to 100. you can specify
``count=0`` argument to return all the results. Default value for
``add_query_items`` argument is set to ``| head 100`` which you can
reset as shown in below example while retrieving all results.

.. code:: ipython3

    splunk_prov.SplunkGeneral.get_events_parameterized(
        index="botsv2",
        source="WinEventLog:Microsoft-Windows-Sysmon/Operational",
        start="2017-08-25 00:00:00.000000",
        end="2017-08-25 10:00:00.000000",
        add_query_items='',
        count=0
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
          <th>TimeCreated</th>
          <th>host</th>
          <th>EventID</th>
          <th>EventDescription</th>
          <th>User</th>
          <th>process</th>
        </tr>
      </thead>
      <tbody>
        <tr>
          <th>0</th>
          <td>2017-08-25T04:57:45.512440700Z</td>
          <td>venus</td>
          <td>3</td>
          <td>Network Connect</td>
          <td>NT AUTHORITY\SYSTEM</td>
          <td>powershell.exe</td>
        </tr>
        <tr>
          <th>1</th>
          <td>2017-08-25T04:57:45.213738500Z</td>
          <td>wrk-aturing</td>
          <td>5</td>
          <td>Process Terminate</td>
          <td>NaN</td>
          <td>conhost.exe</td>
        </tr>
        <tr>
          <th>2</th>
          <td>2017-08-25T04:57:45.213738500Z</td>
          <td>wrk-aturing</td>
          <td>5</td>
          <td>Process Terminate</td>
          <td>NaN</td>
          <td>cscript.exe</td>
        </tr>
        <tr>
          <th>3</th>
          <td>2017-08-25T04:57:45.088941700Z</td>
          <td>wrk-aturing</td>
          <td>1</td>
          <td>Process Create</td>
          <td>NT AUTHORITY\SYSTEM</td>
          <td>conhost.exe</td>
        </tr>
        <tr>
          <th>4</th>
          <td>2017-08-25T04:57:45.088941700Z</td>
          <td>wrk-aturing</td>
          <td>1</td>
          <td>Process Create</td>
          <td>NT AUTHORITY\SYSTEM</td>
          <td>cscript.exe</td>
        </tr>
        <tr>
          <th>...</th>
          <td>...</td>
          <td>...</td>
          <td>...</td>
          <td>...</td>
          <td>...</td>
          <td>...</td>
        </tr>
        <tr>
          <th>7923</th>
          <td>2017-08-25T04:57:46.758125600Z</td>
          <td>wrk-klagerf</td>
          <td>1</td>
          <td>Process Create</td>
          <td>NT AUTHORITY\SYSTEM</td>
          <td>splunk-admon.exe</td>
        </tr>
        <tr>
          <th>7924</th>
          <td>2017-08-25T04:57:46.695728800Z</td>
          <td>wrk-klagerf</td>
          <td>5</td>
          <td>Process Terminate</td>
          <td>NaN</td>
          <td>splunk-MonitorNoHandle.exe</td>
        </tr>
        <tr>
          <th>7925</th>
          <td>2017-08-25T04:57:46.570935200Z</td>
          <td>wrk-klagerf</td>
          <td>1</td>
          <td>Process Create</td>
          <td>NT AUTHORITY\SYSTEM</td>
          <td>splunk-MonitorNoHandle.exe</td>
        </tr>
        <tr>
          <th>7926</th>
          <td>2017-08-25T04:57:46.539736800Z</td>
          <td>wrk-klagerf</td>
          <td>5</td>
          <td>Process Terminate</td>
          <td>NaN</td>
          <td>splunk-powershell.exe</td>
        </tr>
        <tr>
          <th>7927</th>
          <td>2017-08-25T04:57:46.430542400Z</td>
          <td>wrk-klagerf</td>
          <td>1</td>
          <td>Process Create</td>
          <td>NT AUTHORITY\SYSTEM</td>
          <td>splunk-powershell.exe</td>
        </tr>
      </tbody>
    </table>
    <p>7928 rows × 12 columns</p>
    </div>



Running an ad hoc Splunk query
------------------------------

You can also create your own query and run it via the Splunk
provider using this syntax:
``QUERY_PROVIDER.exec_query(<query_text>)``

For more information, check documentation :ref:`data_acquisition/dataproviders:running an ad hoc query`

.. code:: ipython3

    splunk_query = '''
    search index="blackhat" sourcetype="network" earliest=0
    | table TimeGenerated, TotalBytesSent
    '''
    df = splunk_prov.exec_query(splunk_query)
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
          <th>TimeGenerated</th>
          <th>TotalBytesSent</th>
        </tr>
      </thead>
      <tbody>
        <tr>
          <th>0</th>
          <td>2020-07-02T10:00:00Z</td>
          <td>27055</td>
        </tr>
        <tr>
          <th>1</th>
          <td>2020-07-02T09:00:00Z</td>
          <td>33777</td>
        </tr>
        <tr>
          <th>2</th>
          <td>2020-07-02T08:00:00Z</td>
          <td>27355</td>
        </tr>
        <tr>
          <th>3</th>
          <td>2020-07-02T07:00:00Z</td>
          <td>25544</td>
        </tr>
        <tr>
          <th>4</th>
          <td>2020-07-02T06:00:00Z</td>
          <td>11771</td>
        </tr>
      </tbody>
    </table>
    </div>

|

References
----------

-  `Splunk Enterprise SDK for Python
   <https://dev.splunk.com/enterprise/docs/devtools/python/sdk-python/>`__
-  `Splunk Community
   <https://community.splunk.com/t5/Community/ct-p/en-us>`__
-  `Splunk Documentation <https://docs.splunk.com/Documentation>`__
