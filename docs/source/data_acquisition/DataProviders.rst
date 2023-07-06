Query Providers Usage (common to all data sources)
==================================================

Query providers allow you query data from diverse data sources.
They support built-in templated queries as well as ad-hoc
queries. The data is typically returned as a pandas DataFrame.

The package currently support several data drivers giving
access to environments such as Microsoft Sentinel, Microsoft Defender,
Splunk and several more.

The Query providers documentation is split between this document,
which describes usage an functionality common to all of the
data source drivers. Each provider (like Sentinel and Splunk)
also has documentation covering specific usage of that
data environment. These can be found in
:ref:`DataAcquisition:Individual Data Environments`.


Creating a Query Provider
-------------------------

In order to connect to and query a
data source we need to create a ``QueryProvider`` instance and tell it
the Data Environment that we want to connect to and query.
To view the options available you can call
QueryProvider.list_data_environments() which will return a list of all
the available options.

.. code:: ipython3

    QueryProvider.list_data_environments()

.. parsed-literal::

    ['AzureSentinel',
    'LogAnalytics',
    'MSSentinel',
    'Kusto',
    'AzureSecurityCenter',
    'SecurityGraph',
    'MDE',
    'MDATP',
    'LocalData',
    'Splunk',
    'Mordor',
    'ResourceGraph',
    'Sumologic',
    'M365D',
    'Cybereason']

.. note:: New providers are being added regularly so this list
    may look a little different. Also some items in this list
    are aliases (e.g. AzureSentinel and LogAnalytics are aliases
    of MSSentinel)

After selecting a Data Environment we can initialize our Query Provider
by calling QueryProvider(*data_environment*), where
*data_environment* is a string. This will load the relevant
driver for connecting to the data environment we have selected as well
as loading any built-in queries available for that environment.

.. code:: ipython3

    qry_prov = QueryProvider(
        data_environment=DATA_ENVIRONMENT,
    )

There are two other optional parameters we can pass when initializing
our Query Providers to further customize it:

1. You can also chose to
initialize our Query Provider with a driver other than the default one
for the chosen environment with:

.. code:: ipython3

    qry_prov = QueryProvider(
        data_environment=DATA_ENVIRONMENT,
        driver=DRIVER_CLASS_NAME,
    )

TThe class must be imported before it can be used in this way. You
would only use this parameter if you were building your own
data driver backend, which is not common.

2. You can choose to import additional queries from a custom
query directory (see :doc:`../extending/Queries` for more
details) with:

.. code:: ipython3

    qry_prov = QueryProvider(
        data_environment=DATA_ENVIRONMENT,
        query_paths=QUERY_DIRECTORY_PATH
    )


For more details see :py:class:`QueryProvider API <msticpy.data.data_providers.QueryProvider>`.


Connecting to a Data Environment
--------------------------------

Once we have instantiated the query
provider and loaded the relevant driver we can connect to the Data
Environment. This is done by calling the connect() function of the Query
Provider we just initialized and passing it a connection string
or authentication parameters to use.

Documentation string

::

   connect(self, connection_str: str, **kwargs):

       Connect to data source.

       Parameters
       ----------
       connection_string : str
           Connection string for the data source


Example

.. code:: ipython3

    la_connection_string = f'loganalytics://code().tenant("{ten_id}").workspace("{ws_id}")'
    qry_prov.connect(connection_str=la_connection_string)


The format of the parameters supplied to the ``connect`` function varies
by the environment/driver you are trying to use. Please check
the details for the environment you are using in the
:ref:`DataAcquisition:Individual Data Environments` section.

List of current built-in queries
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This page contains a list of current built-in queries
:doc:`MSTICPy built-in queries <DataQueries>`






Listing available queries
-------------------------

Upon connecting to the relevant Data
Environment we need to look at what query options we have available to
us. In order to do this we can call

    *query_provider*.list_queries().

This will return a list all queries in our store.

.. note:: An individual query may be listed multiple times if it was
    added to multiple data families.

The results returned show the data family the query belongs to and the
name of the specific query.

::

   list_queries(self):

       Return list of family.query in the store.

       Returns
       -------
       Iterable[str]
           List of queries

.. code:: ipython3

    qry_prov.list_queries()

.. parsed-literal::

    LinuxSyslog.all_syslog
    LinuxSyslog.cron_activity
    LinuxSyslog.squid_activity
    LinuxSyslog.sudo_activity
    LinuxSyslog.user_group_activity
    LinuxSyslog.user_logon
    SecurityAlert.get_alert
    SecurityAlert.list_alerts
    SecurityAlert.list_alerts_counts
    SecurityAlert.list_alerts_for_ip
    SecurityAlert.list_related_alerts
    WindowsSecurity.get_host_logon
    WindowsSecurity.get_parent_process
    WindowsSecurity.get_process_tree
    WindowsSecurity.list_host_logon_failures
    WindowsSecurity.list_host_logons
    WindowsSecurity.list_host_processes
    WindowsSecurity.list_hosts_matching_commandline
    WindowsSecurity.list_matching_processes
    WindowsSecurity.list_processes_in_session


Each of these items is a callable function that will return results
as a pandas DataFrame.

Getting Help for a query
~~~~~~~~~~~~~~~~~~~~~~~~

To get further details on a specific query call:

qry_prov.{*query_group*}.{*query_name*}("?") or

qry_prov.{*query_group*}.{*query_name*}("help")

or you can use the builtin Python help:

help(qry_prov.{*query_group*}.{*query_name*})

``qry_prov`` is the name of your query provider object.


This will display:

-  Query Name
-  What Data Environment it is designed for
-  A short description of what the query does
-  What parameters the query can be passed
-  The raw (un-parameterized) query that will be run


.. code:: ipython3

    qry_prov.SecurityAlert.list_alerts('?')


.. parsed-literal::

    Query:  list_alerts
    Data source:  LogAnalytics
    Retrieves list of alerts

    Parameters
    ----------
    add_query_items: str (optional)
        Additional query clauses
    end: datetime
        Query end time
    path_separator: str (optional)
        Path separator
        (default value is: \\)
    query_project: str (optional)
        Column project statement
        (default value is:  | project-rename StartTimeUtc = StartTime, EndTim...)
    start: datetime
        Query start time
    subscription_filter: str (optional)
        Optional subscription/tenant filter expression
        (default value is: true)
    table: str (optional)
        Table name
        (default value is: SecurityAlert)
    Query:
     {table} {query_project}
     | where {subscription_filter}
     | where TimeGenerated >= datetime({start})
     | where TimeGenerated <= datetime({end})
     | extend extendedProps = parse_json(ExtendedProperties)
     | extend CompromisedEntity = tostring(extendedProps["Compromised Host"])
     | project-away extendedProps {add_query_items}


Searching for a query
---------------------

The data providers have a simple search capability letting you search
over the names or properties of queries. It takes four parameters:

- search - search terms to look for in the
  query name, description, parameter names, table and query text.
- table - search terms to match on the target table of the query.
  (note: not all queries have the table parameter defined in their
  metadata)
- param - search terms to match on a parameter name
- case - boolean to force case-sensitive matching (default is case-sensitive).

The first three parameters can be a simple string or an iterable (e.g. list, tuple)
of search terms. The search terms are treated as regular expressions. This
means that a the search terms are treated as substrings (if no other
regular expression syntax is included).

Find all queries that have the term "syslog" in their properties

.. code:: ipython3

    qry_prov.search("syslog")
    # equivalent to qry_prov.search(search="syslog")

.. parsed-literal::

    ['LinuxSyslog.all_syslog',
    'LinuxSyslog.cron_activity',
    'LinuxSyslog.list_account_logon_failures',
    'LinuxSyslog.list_host_logon_failures',
    'LinuxSyslog.list_ip_logon_failures',
    'LinuxSyslog.list_logon_failures',
    ...

Other examples:

.. code:: ipython3

    # Find queries that target the "syslog" table and have the term "logon"
    qry_prov.search("logon", table="Syslog")

.. parsed-literal::

    ['LinuxSyslog.list_account_logon_failures',
    'LinuxSyslog.list_host_logon_failures',
    'LinuxSyslog.list_ip_logon_failures',
    'LinuxSyslog.list_logon_failures',
    'LinuxSyslog.list_logons_for_account',
    ...

.. code:: ipython3

    # Queries with the term "Azure" and a parameter beginning with "ip"
    qry_prov.search("Azure", param="ip.*")

    # Table name contains "sign" and has a parameter matching "ip..."
    qry_prov.search(table="sign", param="ip.*")


Running a pre-defined query
---------------------------

To run a query from our query store we
again call qry_prov.{*query_group*}.{*query_name*}(``**kwargs``) but this time
we simply pass required parameters for that query as key word arguments.

This will return a Pandas DataFrame of the results with the columns
determined by the query parameters. Should the query fail for some
reason an exception will be raised.

.. code:: ipython3

    alerts = qry_prov.SecurityAlert.list_alerts(
        start='2019-07-21 23:43:18.274492',
        end='2019-07-27 23:43:18.274492'
    )
    alerts.head()


===================  =================================================  ==========  =================================================  =========================  ============  ================================================  ==========================================
TimeGenerated        AlertDisplayName                                   Severity    Description                                        ProviderName               VendorName    ExtendedProperties                                Entities
===================  =================================================  ==========  =================================================  =========================  ============  ================================================  ==========================================
2019-07-22 06:35:13  Suspicious authentication activity                 Medium      Although none of them succeeded, some of them ...  Detection                  Microsoft     {\r\n "Activity start time (UTC)": "2019/07/2...  [\r\n {\r\n "$id": "4",\r\n "HostName":...
2019-07-22 06:35:13  Suspicious authentication activity                 Medium      Although none of them succeeded, some of them ...  Detection                  Microsoft     {\r\n "Activity start time (UTC)": "2019/07/2...  [\r\n {\r\n "$id": "4",\r\n "HostName":...
2019-07-22 07:02:42  Traffic from unrecommended IP addresses was de...  Low         Azure security center has detected incoming tr...  AdaptiveNetworkHardenings  Microsoft     {\r\n "Destination Port": "3389",\r\n "Proto...   [\r\n {\r\n "$id": "4",\r\n "ResourceId...
2019-07-26 06:03:16  Traffic from unrecommended IP addresses was de...  Low         Azure security center has detected incoming tr...  AdaptiveNetworkHardenings  Microsoft     {\r\n "Destination Port": "22",\r\n "Protoco...   [\r\n {\r\n "$id": "4",\r\n "ResourceId...
2019-07-23 06:42:01  Traffic from unrecommended IP addresses was de...  Low         Azure security center has detected incoming tr...  AdaptiveNetworkHardenings  Microsoft     {\r\n "Destination Port": "3389",\r\n "Proto...   [\r\n {\r\n "$id": "4",\r\n "ResourceId...
===================  =================================================  ==========  =================================================  =========================  ============  ================================================  ==========================================


It is also possible to pass queries objects as arguments before defining
keyword arguments. For example if I wanted to define query times as an
object rather than defining a start and end via keyword arguments I
could simply pass a querytimes object to the pre-defined query.

.. code:: ipython3

    query_times = mas.nbwidgets.QueryTime(
        units='day', max_before=40, max_after=1, before=5
    )
    query_times.display()

Running the above cell will display an interactive data range selector. You
can use that when running a query to automatically supply the ``start`` and
``end`` parameters for the query

.. code:: ipython3

    qry_prov.SecurityAlert.list_alerts(query_times)


===================  =================================================  ==========  =================================================  ================================================  ==========================================  ==============
TimeGenerated        AlertDisplayName                                   Severity    Description                                        ExtendedProperties                                Entities                                    SourceSystem
===================  =================================================  ==========  =================================================  ================================================  ==========================================  ==============
2019-07-26 06:03:16  Traffic from unrecommended IP addresses was de...  Low         Azure security center has detected incoming tr...  {\r\n "Destination Port": "22",\r\n "Protoco...   [\r\n {\r\n "$id": "4",\r\n "ResourceId...  Detection
2019-07-23 06:42:01  Traffic from unrecommended IP addresses was de...  Low         Azure security center has detected incoming tr...  {\r\n "Destination Port": "3389",\r\n "Proto...   [\r\n {\r\n "$id": "4",\r\n "ResourceId...  Detection
2019-07-22 06:35:13  Suspicious authentication activity                 Medium      Although none of them succeeded, some of them ...  {\r\n "Activity start time (UTC)": "2019/07/2...  [\r\n {\r\n "$id": "4",\r\n "HostName":...  Detection
2019-07-22 06:35:13  Suspicious authentication activity                 Medium      Although none of them succeeded, some of them ...  {\r\n "Activity start time (UTC)": "2019/07/2...  [\r\n {\r\n "$id": "4",\r\n "HostName":...  Detection
2019-07-22 07:02:42  Traffic from unrecommended IP addresses was de...  Low         Azure security center has detected incoming tr...  {\r\n "Destination Port": "3389",\r\n "Proto...   [\r\n {\r\n "$id": "4",\r\n "ResourceId...  Detection
===================  =================================================  ==========  =================================================  ================================================  ==========================================  ==============

|

Running an ad hoc query
-----------------------


It is also possible to run ad hoc queries
via a similar method. Rather than calling a named query from the Query
Provider query store, we can pass a query directly to our Query Provider
with:

    *query_provider*.exec\_query(query= *query_string*)

This will execute
the query string passed in the parameters with the driver contained in
the Query Provider and return data in a Pandas DataFrame. As with
predefined queries an exception will be raised should the query fail to
execute.

::

   query(self, query: str) -> Union[pd.DataFrame, Any]:
       Execute query string and return DataFrame of results.

       Parameters
       ----------
       query : str
           The kql query to execute

       Returns
       -------
       Union[pd.DataFrame, results.ResultSet]
           A DataFrame (if successful) or
           Kql ResultSet if an error.

.. code:: ipython3

    test_query = '''
        SecurityAlert
        | take 5
        '''

    query_test = qry_prov.exec_query(query=test_query)
    query_test.head()

===================  =================================================  ==========  =================================================  ================================================  ==========================================  ==============
TimeGenerated        AlertDisplayName                                   Severity    Description                                        ExtendedProperties                                Entities                                    SourceSystem
===================  =================================================  ==========  =================================================  ================================================  ==========================================  ==============
2019-07-26 06:03:16  Traffic from unrecommended IP addresses was de...  Low         Azure security center has detected incoming tr...  {\r\n "Destination Port": "22",\r\n "Protoco...   [\r\n {\r\n "$id": "4",\r\n "ResourceId...  Detection
2019-07-23 06:42:01  Traffic from unrecommended IP addresses was de...  Low         Azure security center has detected incoming tr...  {\r\n "Destination Port": "3389",\r\n "Proto...   [\r\n {\r\n "$id": "4",\r\n "ResourceId...  Detection
2019-07-22 06:35:13  Suspicious authentication activity                 Medium      Although none of them succeeded, some of them ...  {\r\n "Activity start time (UTC)": "2019/07/2...  [\r\n {\r\n "$id": "4",\r\n "HostName":...  Detection
2019-07-22 06:35:13  Suspicious authentication activity                 Medium      Although none of them succeeded, some of them ...  {\r\n "Activity start time (UTC)": "2019/07/2...  [\r\n {\r\n "$id": "4",\r\n "HostName":...  Detection
2019-07-22 07:02:42  Traffic from unrecommended IP addresses was de...  Low         Azure security center has detected incoming tr...  {\r\n "Destination Port": "3389",\r\n "Proto...   [\r\n {\r\n "$id": "4",\r\n "ResourceId...  Detection
===================  =================================================  ==========  =================================================  ================================================  ==========================================  ==============

.. _multiple_connections:

Running a query across multiple connections
-------------------------------------------

It is common for data services to be spread across multiple tenants or
workloads. For example, you may have multiple Sentinel workspaces,
Microsoft Defender subscriptions or Splunk instances. You can use the
``QueryProvider`` to run a query across multiple connections and return
the results in a single DataFrame.

.. note:: This feature only works for multiple instances using the same
    ``DataEnvironment`` (e.g. "MSSentinel", "Splunk", etc.)

To create a multi-instance provider you first need to create an
instance of a QueryProvider for your data source and execute
the ``connect()`` method to connect to the first instance of your
data service. Which instance you choose is not important.
Then use the
:py:meth:`add_connection() <msticpy.data.core.query_provider_connections_mixin.QueryProviderConnectionsMixin.add_connection>`
method. This takes the same parameters as the
:py:meth:`connect() <msticpy.data.core.data_providers.QueryProvider.connect>`
method (the parameters for this method vary by data provider).

``add_connection()`` also supports an ``alias`` parameter to allow
you to refer to the connection by a friendly name. Otherwise, the
connection is just assigned an index number in the order that it was
added.

Use the
:py:meth:`list_connections() <msticpy.data.core.query_provider_connections_mixin.QueryProviderConnectionsMixin.list_connections>`
to see all of the current connections.

.. code:: ipython3

    qry_prov = QueryProvider("MSSentinel")
    qry_prov.connect(workspace="Workspace1")
    qry_prov.add_connection(workspace="Workspace2, alias="Workspace2")
    qry_prov.list_connections()

When you now run a query for this provider, the query will be run on
all of the connections and the results will be returned as a single
dataframe.

.. code:: ipython3

    test_query = '''
        SecurityAlert
        | take 5
        '''

    query_test = qry_prov.exec_query(query=test_query)
    query_test.head()

Some of the MSTICPy drivers support asynchronous execution of queries
against multiple instances, so that the time taken to run the query is
much reduced compared to running the queries sequentially. Drivers
that support asynchronous queries will use this automatically.

By default, the queries will use at most 4 concurrent threads. You can
override this by initializing the QueryProvider with the
``max_threads`` parameter to set it to the number of threads you want.

.. code:: ipython3

    qry_prov = QueryProvider("MSSentinel", max_threads=10)


.. _splitting_query_execution:

Splitting Query Execution into Chunks
-------------------------------------

Some queries return too much data or take too long to execute in a
single request. The MSTICPy data providers have an option to
split a query into time ranges. Each sub-range is run as an independent
query and the results are combined before being returned as a
DataFrame.

.. note:: Some data drivers support running queries asynchronously.
    This means that the time taken to run all chunks of the query is much reduced
    compared to running these sequentially. Drivers that support
    asynchronous queries will use this automatically.

To use this feature you must specify the keyword parameter ``split_query_by``
when executing the query function. The value to this parameter is a
string that specifies a time period. The time range specified by the
``start`` and ``end`` parameters to the query is split into sub-ranges
each of which are the length of the split time period. For example, if you
specify ``split_query_by="1H"`` the query will be split into one hour
chunks.

.. note:: The final chunk may cover a time period larger or smaller
   than the split period that you specified in the *split_query_by*
   parameter. This can happen if *start* and *end* are not aligned
   exactly on time boundaries (e.g. if you used a one hour split period
   and *end* is 10 hours 15 min after *start*). The query split logic
   will create a larger final slice if *end* is close to the final time
   range or it will insert an extra time range to ensure that the full
   *start** to *end* time range is covered.

The sub-ranges are used to generate a query for each time range. The
queries are then executed in sequence and the results concatenated into
a single DataFrame before being returned.

The values acceptable for the *split_query_by* parameter have the format:

::

    {N}{TimeUnit}

where N is the number of units and TimeUnit is a mnemonic of the unit, e.g.
H = hour, D = day, etc. For the full list of these see the documentation
for Timedelta in the
`pandas documentation <https://pandas.pydata.org/docs>`__

.. warning:: There are some important caveats to this feature.

   1. It currently only works with pre-defined queries (including ones
      that you may create and add yourself, see :doc:`../extending/Queries`
      below). It does not work with `Running an ad hoc query`_
   2. If the query contains joins, the joins will only happen within
      the time ranges of each subquery.
   3. It only supports queries that have *start* and *end* parameters.
   4. Very large queries may return results that can exhaust the memory
      on the Python client machine.
   5. Duplicate records are possible at the time boundaries. The code
      tries to avoid returning duplicate records occurring
      exactly on the time boundaries but some data sources may not use
      granular enough time stamps to avoid this.

Dynamically adding new queries
------------------------------

You can use the :py:meth:`msticpy.data.core.data_providers.QueryProvider.add_query`
to add parameterized queries from a notebook or script. This
let you use temporary parameterized queries without having to
add them to a YAML file (as described in :doc:`../extending/Queries`).

get_host_events

.. code:: python

    # initialize a query provider
    qry_prov = mp.QueryProvider("MSSentinel")

    # define a query
    query = """
    SecurityEvent
    | where EventID == {event_id}
    | where TimeGenerated between (datetime({start}) .. datetime({end}))
    | where Computer has "{host_name}"
    """
    # define the query parameters
    # (these can also be passed as a list of raw tuples)
    qp_host = qry_prov.create_param("host_name", "str", "Name of Host")
    qp_start = qry_prov.create_param("start", "datetime")
    qp_end = qry_prov.create_param("end", "datetime")
    qp_evt = qry_prov.create_param("event_id", "int", None, 4688)

    # add the query
    qry_prov.add_custom_query(
        name="get_host_events",
        query=query,
        family="Custom",
        parameters=[qp_host, qp_start, qp_end, qp_evt]
    )

    # query is now available as
    qry_prov.Custom.get_host_events(host_name="MyPC"....)

Adding queries to the MSTICPy query library
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

You can also add permanent parameterized queries to your data providers.
Read :doc:`../extending/Queries` for information on how to do this.
