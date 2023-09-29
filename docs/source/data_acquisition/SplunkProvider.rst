Splunk Provider
===============

In this document we describe the Splunk data connector feature of
MSTICPy. This feature is built on-top of the
`Splunk Enterprise SDK for Python <https://dev.splunk.com/enterprise/docs/devtools/python/sdk-python/>`__
with some customizations and enhancements.

Splunk Configuration
--------------------

Splunk SDK Installation
~~~~~~~~~~~~~~~~~~~~~~~

The Splunk SDK is an optional dependency of MSTICPy. To install it,
run the following:

.. code:: ipython3

    # Only run first time to install/upgrade msticpy to latest version
    %pip install --upgrade msticpy[splunk]

Splunk Configuration in MSTICPy
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

You can store your connection details in *msticpyconfig.yaml*.

For more information on using and configuring *msticpyconfig.yaml* see
:doc:`msticpy Package Configuration <../getting_started/msticpyconfig>`
and :doc:`MSTICPy Settings Editor<../getting_started/SettingsEditor>`

The settings in the file should look like the following:

.. code:: yaml

    DataProviders:
      ...
      Splunk:
          Args:
            host: splunk_host
            port: '8089'
            username: splunk_user
            password: [PLACEHOLDER]


We strongly recommend storing the password secret value
in Azure Key Vault. You can replace the text value with a referenced
to a Key Vault secret using the MSTICPy configuration editor.

.. code:: yaml

    DataProviders:
      ...
      Splunk:
          Args:
            host: splunk_host
            port: '8089'
            username: splunk_user
            password:
              KeyVault:

Required connection parameters:

===========  ===========================================================================================================================
Parameter    Description
===========  ===========================================================================================================================
host         (string) The host name (the default is 'localhost').
username     (string) The Splunk account username, which is used to authenticate the Splunk instance.
password     (string) The password for the Splunk account.
splunkToken  (string) The Authorization Bearer Token <JWT> created in the Splunk.
===========  ===========================================================================================================================

The username and password are needed for user account authentication.
On the other hand, splunkToken is needed for Token authentication.
The user auth method has a priority to token auth method if both username and splunkToken are set.


Optional configuration parameters:

===========  ===========================================================================================================================
Parameter    Description
===========  ===========================================================================================================================
port         (integer) The port number (the default is 8089).
http_scheme  ('https' or 'http') The scheme for accessing the service (the default is 'https').
verify       (Boolean) Enable (True) or disable (False) SSL verification for https connections. (optional, the default is True)
owner        (string) The owner context of the namespace (optional).
app          (string) The app context of the namespace (optional).
sharing      ('global', 'system', 'app', or 'user') The sharing mode for the namespace (the default is 'user').
token        (string) The current session token (optional). Session tokens can be shared across multiple service instances.
cookie       (string) A session cookie. When provided, you don’t need to call login(). This parameter is only supported for Splunk 6.2+.
autologin    (boolean) When True, automatically tries to log in again if the session terminates.
===========  ===========================================================================================================================



Loading a QueryProvider for Splunk
----------------------------------

.. code:: ipython3

        qry_prov = QueryProvider("Splunk")


Connecting to Splunk
--------------------

Authentication for the Splunk data provider is handled by specifying
credentials directly in the connect call or specifying the credentials
in msticpy config file.

For more information on how to create new user with appropriate roles
and permissions, follow the Splunk documents:

`Securing the Spunk platform <https://docs.splunk.com/Documentation/Splunk/9.1.1/Security/Addandeditusers>`__

and

`About users and roles <https://docs.splunk.com/Documentation/Splunk/9.1.1/Security/Aboutusersandroles>`__

The user should have permission to at least run its own searches or more
depending upon the actions to be performed by user.

Once you created user account with the appropriate roles, you will
require the following details to specify while connecting:

- host = "localhost" (Splunk server FQDN hostname to connect, for locally
  installed splunk, you can specify localhost)
- port = "8089" (Splunk REST API)
- username = "admin" (username to connect to Splunk instance)
- password = "yourpassword" (password of the user specified in username)

On the other hand, you can use the authentification token to connect.

`Create authentication token <https://docs.splunk.com/Documentation/Splunk/9.1.1/Security/CreateAuthTokens>`__

- host = "localhost" (Splunk server FQDN hostname to connect, for locally
  installed splunk, you can specify localhost)
- port = "8089" (Splunk REST API)
- splunkToken = "<Authorization Bearer Token>" (token can be used instead of username/password)


Once you have details, you can specify it in ``msticpyconfig.yaml`` as
described earlier.

Authenticate using the following, if you have stored your configuration
in *msticpyconfig.yaml*

.. code:: ipython3

    qry_prov.connect()

Or provide connection parameters explicitly. You can also have some
of the required parameters stored in your configuration and
specify others (e.g. password) at connect time, as a parameter to
connect.

You can also use any of the optional parameters described earlier
as parameters to connect.

.. code:: ipython3

    qry_prov.connect(host=<hostname>, username=<username>, password=<password>)

OR

.. code:: ipython3

    qry_prov.connect(host=<hostname>, splunkToken=<token_string>)


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
        (default value is: current time + 1 day)
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
        (default value is: current time - 1 day)
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


==== ==============================  ===========  =========  ==================  ====================  ======================
0    TimeCreated                     host         EventID    EventDescription    User                  process
==== ==============================  ===========  =========  ==================  ====================  ======================
0    2017-08-25T04:57:45.512440700Z  venus        3          Network Connect     NT AUTHORITY\\SYSTEM  powershell.exe
1    2017-08-25T04:57:45.213738500Z  wrk-aturing  5          Process Terminate   nan                   conhost.exe
2    2017-08-25T04:57:45.213738500Z  wrk-aturing  5          Process Terminate   nan                   cscript.exe
3    2017-08-25T04:57:45.088941700Z  wrk-aturing  1          Process Create      NT AUTHORITY\\SYSTEM  conhost.exe
4    2017-08-25T04:57:45.088941700Z  wrk-aturing  1          Process Create      NT AUTHORITY\\SYSTEM  cscript.exe
...  ...                             ...          ...        ...                 ...                   ...
95   2017-08-25T04:57:02.003800000Z  wrk-ghoppy   1          Process Create      NT AUTHORITY\\SYSTEM  splunk-powershell.exe
96   2017-08-25T04:57:01.170335100Z  venus        3          Network Connect     NT AUTHORITY\\SYSTEM  powershell.exe
97   2017-08-25T04:57:01.941402000Z  wrk-ghoppy   5          Process Terminate   nan                   splunk-winprintmon.exe
98   2017-08-25T04:57:01.863404500Z  wrk-ghoppy   1          Process Create      NT AUTHORITY\\SYSTEM  splunk-netmon.exe
99   2017-08-25T04:57:01.754208000Z  wrk-ghoppy   5          Process Terminate   nan                   splunk-powershell.exe
==== ==============================  ===========  =========  ==================  ====================  ======================

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


==== ==============================  ===========  =========  ==================  ====================  ======================
0    TimeCreated                     host         EventID    EventDescription    User                  process
==== ==============================  ===========  =========  ==================  ====================  ======================
0    2017-08-25T04:57:45.512440700Z  venus        3          Network Connect     NT AUTHORITY\\SYSTEM  powershell.exe
1    2017-08-25T04:57:45.213738500Z  wrk-aturing  5          Process Terminate   nan                   conhost.exe
2    2017-08-25T04:57:45.213738500Z  wrk-aturing  5          Process Terminate   nan                   cscript.exe
3    2017-08-25T04:57:45.088941700Z  wrk-aturing  1          Process Create      NT AUTHORITY\\SYSTEM  conhost.exe
4    2017-08-25T04:57:45.088941700Z  wrk-aturing  1          Process Create      NT AUTHORITY\\SYSTEM  cscript.exe
...  ...                             ...          ...        ...                 ...                   ...
95   2017-08-25T04:57:02.003800000Z  wrk-ghoppy   1          Process Create      NT AUTHORITY\\SYSTEM  splunk-powershell.exe
96   2017-08-25T04:57:01.170335100Z  venus        3          Network Connect     NT AUTHORITY\\SYSTEM  powershell.exe
97   2017-08-25T04:57:01.941402000Z  wrk-ghoppy   5          Process Terminate   nan                   splunk-winprintmon.exe
98   2017-08-25T04:57:01.863404500Z  wrk-ghoppy   1          Process Create      NT AUTHORITY\\SYSTEM  splunk-netmon.exe
99   2017-08-25T04:57:01.754208000Z  wrk-ghoppy   5          Process Terminate   nan                   splunk-powershell.exe
==== ==============================  ===========  =========  ==================  ====================  ======================



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

====  ====================  ================
0     TimeGenerated           TotalBytesSent
====  ====================  ================
   0  2020-07-02T10:00:00Z             27055
   1  2020-07-02T09:00:00Z             33777
   2  2020-07-02T08:00:00Z             27355
   3  2020-07-02T07:00:00Z             25544
   4  2020-07-02T06:00:00Z             11771
====  ====================  ================

|

Other Splunk Documentation
--------------------------


Built-in :ref:`data_acquisition/DataQueries:Queries for Splunk`.

:py:mod:`Splunk driver API documentation<msticpy.data.drivers.splunk_driver>`


-  `Splunk Enterprise SDK for Python
   <https://dev.splunk.com/enterprise/docs/devtools/python/sdk-python/>`__
-  `Splunk Community
   <https://community.splunk.com/t5/Community/ct-p/en-us>`__
-  `Splunk Documentation <https://docs.splunk.com/Documentation>`__
