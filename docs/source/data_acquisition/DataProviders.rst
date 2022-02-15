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
query directory (see `Creating new queries`_ for more
details) with:

.. code:: ipython3

    qry_prov = QueryProvider(
        data_environment=DATA_ENVIRONMENT,
        query_paths=QUERY_DIRECTORY_PATH
    )


For more details see :py:class:`QueryProvider API<class:msticpy.data.data_providers.QueryProvider>`.


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
`Individual Data Environments/Drivers`__ section.

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

qry_prov.{*query_group*}.{*query_name*}(‘?’) or

qry_prov.{*query_group*}.{*query_name*}(‘help’)

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


Splitting Query Execution into Chunks
-------------------------------------

Some queries return too much data or take too long to execute in a
single request. The MSTICPy data providers have an option to
split a query into time ranges. Each sub-range is run as an independent
query and the results are combined before being returned as a
DataFrame.

To use this feature you must specify the keyword parameter ``split_queries_by``
when executing the query function. The value to this parameter is a
string that specifies a time period. The time range specified by the
``start`` and ``end`` parameters to the query is split into sub-ranges
each of which are the length of the split time period. For example, if you
specify ``split_queries_by="1H"`` the query will be split into one hour
chunks.

.. note:: The final chunk may cover a time period larger or smaller
   than the split period that you specified in the *split_queries_by*
   parameter. This can happen if *start* and *end* are not aligned
   exactly on time boundaries (e.g. if you used a one hour split period
   and *end* is 10 hours 15 min after *start*. The query split logic
   will create a larger final slice if *end* is close to the final time
   range or it will insert an extra time range to ensure that the full
   *start** to *end* time range is covered.

The sub-ranges are used to generate a query for each time range. The
queries are then executed in sequence and the results concatenated into
a single DataFrame before being returned.

The values acceptable for the *split_queries_by* parameter have the format:

::

    {N}{TimeUnit}

where N is the number of units and TimeUnit is a mnemonic of the unit, e.g.
H = hour, D = day, etc. For the full list of these see the documentation
for Timedelta in the
`pandas documentation <https://pandas.pydata.org/pandas-docs>`__

.. warning:: There are some important caveats to this feature.

   1. It currently only works with pre-defined queries (including ones
      that you may create and add yourself, see `Creating new queries`_
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

Creating new queries
--------------------

*msticpy* provides a number of
pre-defined queries to call with using the data package. You can also
add in additional queries to be imported and used by your Query
Provider, these are defined in YAML format files and examples of these
files can be found at the msticpy GitHub site
https://github.com/microsoft/msticpy/tree/master/msticpy/data/queries.

The required structure of these query definition files is as follows.

At the top level the file has the following keys:
- **metadata**
- **defaults**
- **sources**

These are described in the following sections.

The metadata section
~~~~~~~~~~~~~~~~~~~~

- **version**: The version number of the definition file
- **description**: A description of the purpose of this collection of query
  definitions
- **data_environments** []: A list of the Data Environments that
  the defined queries can be run against (1 or more)
- **data_families** []: A list of Data Families the defined queries related
  to, these families are defined as part of msticpy.data.query_defns but
  you can add custom ones.
- **tags** []: A list of tags to help manage definition files (this is not
  currently used


The defaults section
~~~~~~~~~~~~~~~~~~~~

A set of defaults that apply to all queries in the file. You
can use this section to define parameters that are common to all
of the queries in the file. Child keys of the ``defaults`` section
are inherited by the query definitions in the file.

- **metadata**: Metadata regarding a query
  - **data_source**: The data source to be used for the query
- **parameters**: parameter defaults for the queries (the format of
  the parameters section is the same as described in
  the sources section.


The sources section
~~~~~~~~~~~~~~~~~~~

Each key in the sources section defines a new query. The name of
the key is the query name and must be unique and a valid Python identifier.
Each query key has the following structure:

- **description**: this is used to display help text for the query.
- **metadata**: (optional) - if you want to override the global metadata
  for this query
- **args**: The primary item here is the query text.

  - **query**: usually a multi-line string that will be passed to the
    data provider. The string is usually parameterized, the parameters
    being denoted by surrounding them with single braces ({}). If
    you need to include literal braces in the query, type two braces.
    For example::
    "this {{literal_string}}" ->> "this {literal_string}"
    Surround your query string with single quotes.
  - **uri**: this is currently not used.
- **parameters**: The parameters section defines the name, data type and
  optional default value for each parameter that will be substituted into
  the query before being passed to the data provider. Each parameter
  must have a unique name (for each query, not globally). All parameters
  specified in the query text must have an entry here or in the file
  defauls section. The parameter subsection has the following sub-keys:

  - **description**: A description of what the parameter is (used for generating
    documentation strings.
  - **type**: The data type of the parameter. Valid types include: "str", "int",
    "float", "list" and "datetime". The list and datetime types cause additional
    formatting to be applied (such as converting from a datestring)
  - **default**: (optional) the default value for that parameter. Any parameter
    that does not have a default value (here or in the file defaults section)
    must be supplied at query time.

Some common parameters used in the queries are:

- **table**: making this a substitutable parameter allows you to use the same
  query with different data sets. More commonly, you can add additional
  filtering statements here, for example:

.. code:: yaml

    table:
        description: The table name
        type: str
        default: SecurityEvent | where EventID == 4624

- **add_query_items**: This is a useful way of extending queries by adding
  ad hoc statements to the end of the query (e.g. additional filtering order
  summarization).

Using yaml aliases and macros in your queries
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

You can use standard yaml aliasing to define substitutable strings in your
query definitions. E.g. you might have a parameter default that is a long
string expression. Define an alias in the ``aliases`` key of the file
metadata section. An alias is defined by prefixing the name with "&".
The alias is referenced (and inserted) by using the alias name prefixed
with "*"

.. code:: yaml

    metadata:
        ...
        aliases:
            - &azure_network_project '| project TenantId, TimeGenerated,
                FlowStartTime = FlowStartTime_t,
                FlowEndTime = FlowEndTime_t,
                FlowIntervalEndTime = FlowIntervalEndTime_t,
                FlowType = FlowType_s,
                ResourceGroup = split(VM_s, "/")[0],
                VMName = split(VM_s, "/")[1],
                VMIPAddress = VMIP_s'
        ...
    sources:
        list_azure_network_flows_by_host:
            description: Retrieves Azure network analytics flow events.
            ...
            parameters:
                ...
                query_project:
                    description: Column project statement
                    type: str
                    default: *azure_network_project


You can also use *macros*, which work like parameters but are substituted
into the query before any parameter substitution is carried out. This
allows you to, for example, use a single base query but with different
filter and summarization clauses defined as macros. The macro text is
substituted into the main query.

Macros are added to the ``query_macros`` subkey of a query. They have
two subkeys: description and value. value defines the text to be inserted.
The key name is the name of the macro.

In the query, you denote the substitution point by surrounding the macro name
with "$<" and ">$". This is show in the example below.

.. code:: yaml

    - query: '
        {table}
        | where SubType_s == "FlowLog"
        | where FlowStartTime_t >= datetime({start})
        | where FlowEndTime_t <= datetime({end})
        $<query_condition>$
        | where (AllowedOutFlows_d > 0 or AllowedInFlows_d > 0)
        {query_project}
        | extend AllExtIPs = iif(isempty(PublicIPs), pack_array(ExtIP),
                         iif(isempty(ExtIP), PublicIPs, array_concat(PublicIPs, pack_array(ExtIP)))
                         )
        | project-away ExtIP
        | mvexpand AllExtIPs
        {add_query_items}'

Macros are particularly useful when combined with yaml aliases. You can, for
example, define a base query (using a yaml alias) with a macro reference in the
query body. Then in each query definition you can have different macro values
for the macro to be substituted. For example:

.. code:: yaml

    metadata:
        ...
        aliases:
            - &azure_network_base_query '
                {table}
                | where SubType_s == "FlowLog"
                | where FlowStartTime_t >= datetime({start})
                | where FlowEndTime_t <= datetime({end})
                $<query_condition>$
                | where (AllowedOutFlows_d > 0 or AllowedInFlows_d > 0)
                {query_project}
                | extend AllExtIPs = iif(isempty(PublicIPs), pack_array(ExtIP),
                                iif(isempty(ExtIP), PublicIPs, array_concat(PublicIPs, pack_array(ExtIP)))
                                )
                | project-away ExtIP
                | mvexpand AllExtIPs
                {add_query_items}'
        ...
    sources:
        list_azure_network_flows_by_ip:
            description: Retrieves Azure network analytics flow events.
        args:
            query: *azure_network_base_query
        parameters:
            query_project:
                ...
            end:
                description: Query end time
                type: datetime
        query_macros:
            query_condition:
                description: Query-specific where clause
                value: '| where (VMIP_s in ({ip_address_list})
                or SrcIP_s in ({ip_address_list})
                or DestIP_s in ({ip_address_list})
                )'

This allows you define a series of related queries that have the
same basic logic but have different filter clauses. This is extremely useful
where the query is complex and allows you to keep a single copy.

.. note:: Using aliases and macros complicates the logic for anyone
    trying to read the query file, so use this sparingly.


Guidelines for creating and debugging queries
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It is often helpful to start with a working version of a query without
using any parameters. Just paste in a query that you know is working. Once
you have verified that this works and returns data as expected you can
start to parameterize it.

As you add parameters you can expect to find escaping and quoting
issues with the parameter values. To see what the parameterized version
of the query (without submitting it to the data provider) run the query
with the first parameter "print". This will return the parameterized version
of the query as a string:

.. code:: ipython3

    qry_prov.SecurityEvents.my_new_query(
        "print",
        start=start_dt,
        end=end_dt,
        account="ian",
    )


There are also a number of tools within the package to assist in
validating new query definition files once created.

::

   data_query_reader.find_yaml_files

       Return iterable of yaml files found in `source_path`.

       Parameters
       ----------
       source_path : str
           The source path to search in.
       recursive : bool, optional
           Whether to recurse through subfolders.
           By default False

       Returns
       -------
       Iterable[str]
           File paths of yaml files found.

    data_query_reader.validate_query_defs

        Validate content of query definition.

       Parameters
       ----------
       query_def_dict : dict
           Dictionary of query definition yaml file contents.

       Returns
       -------
       bool
           True if validation succeeds.

       Raises
       ------
       ValueError
           The validation failure reason is returned in the
           exception message (arg[0])

validate_query_defs() does not perform comprehensive checks on the file
but does check key elements required in the file are present.

.. code:: ipython3

    for file in QueryReader.find_yaml_files(source_path="C:\\queries"):
        with open(file) as f_handle:
            yaml_file = yaml.safe_load(f_handle)
            if QueryReader.validate_query_defs(query_def_dict = yaml_file) == True:
                print(f' {file} is a valid query definition')
            else:
                print(f'There is an error with {file}')


.. parsed-literal::

     C:\queries\example.yaml is a valid query definition


Adding a new set of queries and running them
--------------------------------------------

Once you are happy with
a query definition file then you import it with

    *query_provider*.import_query_file(query_file= *path_to_query_file*)

This will load the query file into the Query Provider’s Query Store from
where it can be called.

.. code:: ipython3

    qry_prov.import_query_file(query_file='C:\\queries\\example.yaml')

Once imported the queries in the files appear in the Query Provider’s
Query Store alongside the others and can be called in the same manner as
pre-defined queries.

If you have created a large number of query definition files and you
want to have the automatically imported into a Query Provider’s query
store at initialization you can specify a directory containing these
queries in the msticpyconfig.yaml file under QueryDefinitions: Custom:

For example if I have a folder at C:\\queries I will set the
config file to:

.. code:: yaml

    QueryDefinitions:
        Custom:
            - C:\queries


Having the Custom field populated will mean the Query Provider will
automatically enumerate all the YAML files in the directory provided and
automatically import he relevant queries into the query store at
initialization alongside the default queries. Custom queries with the
same name as default queries will overwrite default queries.

.. code:: ipython3

    queries = qry_prov.list_queries()
    for query in queries:
        print(query)


.. parsed-literal::

    LinuxSyslog.all_syslog
    LinuxSyslog.cron_activity
    LinuxSyslog.squid_activity
    LinuxSyslog.sudo_activity
    LinuxSyslog.syslog_example
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


.. code:: ipython3

    qry_prov.LinuxSyslog.syslog_example('?')


.. parsed-literal::

    Query:  syslog_example
    Data source:  LogAnalytics
    Example query

    Parameters
    ----------
    add_query_items: str (optional)
        Additional query clauses
    end: datetime
        Query end time
    host_name: str
        Hostname to query for
    query_project: str (optional)
        Column project statement
        (default value is:  | project TenantId, Computer, Facility, TimeGener...)
    start: datetime
        Query start time
    subscription_filter: str (optional)
        Optional subscription/tenant filter expression
        (default value is: true)
    table: str (optional)
        Table name
        (default value is: Syslog)
    Query:
     {table} | where {subscription_filter}
     | where TimeGenerated >= datetime({start})
     | where TimeGenerated <= datetime({end})
     | where Computer == "{host_name}" | take 5


.. code:: ipython3

    qry_prov.LinuxSyslog.syslog_example(
        start='2019-07-21 23:43:18.274492',
        end='2019-07-27 23:43:18.274492',
        host_name='UbuntuDevEnv'
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
          <th>TenantId</th>
          <th>SourceSystem</th>
          <th>TimeGenerated</th>
          <th>Computer</th>
          <th>EventTime</th>
          <th>Facility</th>
          <th>HostName</th>
          <th>SeverityLevel</th>
          <th>SyslogMessage</th>
          <th>ProcessID</th>
          <th>HostIP</th>
          <th>ProcessName</th>
          <th>MG</th>
          <th>Type</th>
          <th>_ResourceId</th>
        </tr>
      </thead>
      <tbody>
        <tr>
          <th>0</th>
          <td>b1315f05-4a7a-45b4-811f-73e715f7c122</td>
          <td>Linux</td>
          <td>2019-07-25 15:15:37.213</td>
          <td>UbuntuDevEnv</td>
          <td>2019-07-25 15:15:37</td>
          <td>authpriv</td>
          <td>UbuntuDevEnv</td>
          <td>notice</td>
          <td>omsagent : TTY=unknown   PWD=/opt/microsoft/om...</td>
          <td>NaN</td>
          <td>10.0.1.4</td>
          <td>sudo</td>
          <td>00000000-0000-0000-0000-000000000002</td>
          <td>Syslog</td>
          <td>/subscriptions/3b701f84-d04b-4479-89b1-fa8827e...</td>
        </tr>
        <tr>
          <th>1</th>
          <td>b1315f05-4a7a-45b4-811f-73e715f7c122</td>
          <td>Linux</td>
          <td>2019-07-25 15:15:37.313</td>
          <td>UbuntuDevEnv</td>
          <td>2019-07-25 15:15:37</td>
          <td>authpriv</td>
          <td>UbuntuDevEnv</td>
          <td>info</td>
          <td>pam_unix(sudo:session): session opened for use...</td>
          <td>NaN</td>
          <td>10.0.1.4</td>
          <td>sudo</td>
          <td>00000000-0000-0000-0000-000000000002</td>
          <td>Syslog</td>
          <td>/subscriptions/3b701f84-d04b-4479-89b1-fa8827e...</td>
        </tr>
        <tr>
          <th>2</th>
          <td>b1315f05-4a7a-45b4-811f-73e715f7c122</td>
          <td>Linux</td>
          <td>2019-07-25 15:15:37.917</td>
          <td>UbuntuDevEnv</td>
          <td>2019-07-25 15:15:37</td>
          <td>authpriv</td>
          <td>UbuntuDevEnv</td>
          <td>info</td>
          <td>pam_unix(sudo:session): session closed for use...</td>
          <td>NaN</td>
          <td>10.0.1.4</td>
          <td>sudo</td>
          <td>00000000-0000-0000-0000-000000000002</td>
          <td>Syslog</td>
          <td>/subscriptions/3b701f84-d04b-4479-89b1-fa8827e...</td>
        </tr>
        <tr>
          <th>3</th>
          <td>b1315f05-4a7a-45b4-811f-73e715f7c122</td>
          <td>Linux</td>
          <td>2019-07-25 15:15:50.793</td>
          <td>UbuntuDevEnv</td>
          <td>2019-07-25 15:15:50</td>
          <td>authpriv</td>
          <td>UbuntuDevEnv</td>
          <td>info</td>
          <td>pam_unix(cron:session): session closed for use...</td>
          <td>29486.0</td>
          <td>10.0.1.4</td>
          <td>CRON</td>
          <td>00000000-0000-0000-0000-000000000002</td>
          <td>Syslog</td>
          <td>/subscriptions/3b701f84-d04b-4479-89b1-fa8827e...</td>
        </tr>
        <tr>
          <th>4</th>
          <td>b1315f05-4a7a-45b4-811f-73e715f7c122</td>
          <td>Linux</td>
          <td>2019-07-25 15:16:01.800</td>
          <td>UbuntuDevEnv</td>
          <td>2019-07-25 15:16:01</td>
          <td>authpriv</td>
          <td>UbuntuDevEnv</td>
          <td>info</td>
          <td>pam_unix(cron:session): session opened for use...</td>
          <td>29844.0</td>
          <td>10.0.1.4</td>
          <td>CRON</td>
          <td>00000000-0000-0000-0000-000000000002</td>
          <td>Syslog</td>
          <td>/subscriptions/3b701f84-d04b-4479-89b1-fa8827e...</td>
        </tr>
      </tbody>
    </table>
    </div>

|

If you are having difficulties with a defined query and it is not
producing the expected results it can be useful to see the raw query
exactly as it is passed to the Data Environment. If you call a query
with ‘print’ and the parameters required by that query it will construct
and print out the query string to be run.

.. code:: ipython3

    qry_prov.LinuxSyslog.syslog_example(
        'print',
        start='2019-07-21 23:43:18.274492',
        end='2019-07-27 23:43:18.274492',
        host_name='UbuntuDevEnv'
    )




.. parsed-literal::

    'Syslog
        | where true
        | where TimeGenerated >= datetime(2019-07-21 23:43:18.274492)
        | where TimeGenerated <= datetime(2019-07-27 23:43:18.274492)
        | where Computer == "UbuntuDevEnv"
        | take 5'


