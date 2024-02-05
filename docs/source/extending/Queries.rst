Adding Queries to MSTICPy
=========================

See :doc:`../data_acquisition/DataProviders` for more details on use
of data queries.

See :doc:`QueryFileEditor` for more details on the notebook Query Editor.

*MSTICPy* provides a number of
pre-defined queries used by the different query providers. You can also
add you own queries for use by the built-in providers or custom
data providers.

Queries are grouped into query template files that use the YAML format.
You can see some examples of these files in the
`MSTICPy GitHub repo queries folder <https://github.com/microsoft/msticpy/tree/master/msticpy/data/queries>`_.

Here is an example of a query definition file:

.. code:: yaml

    metadata:
        version: 1
        description: Kql Sentinel Windows Logon Event Queries
        data_environments: [MSSentinel]
        data_families: [WindowsSecurity]
        tags: ["process", "windows", "processtree", "session"]
    defaults:
        parameters:
            start:
                description: Query start time
                type: datetime
            end:
                description: Query end time
                type: datetime
            table:
                description: Table name
                type: str
                default: "SecurityEvent"
    sources:
        get_host_logon:
            description: Retrieves the logon event for the session id on the host
            metadata:
            args:
                query: '
                    {table}
                    | where EventID == 4624
                    | where Computer has "{host_name}"
                    | where TimeGenerated >= datetime({start})
                    | where TimeGenerated <= datetime({end})
                    | where TargetLogonId == "{logon_session_id}"
                    {add_query_items}'
            parameters:
                host_name:
                    description: Name of host
                    type: str
                logon_session_id:
                    description: The logon session ID of the source process
                    type: str
        list_host_logons:
            description: Retrieves the logon events on the host
            metadata:
            args:
                query: '
                    {table}
                    | where EventID == 4624
                    | where Computer has "{host_name}"
                    | where TimeGenerated >= datetime({start})
                    | where TimeGenerated <= datetime({end})
                    {add_query_items}'
            parameters:
                host_name:
                    description: Name of host
                    type: str


The required structure of these query definition files is as follows.

At the top level the file has the following keys:

- **metadata** - global parameters affecting all queries in the file
- **defaults** - default values for parameters used in the queries
- **sources** - the individual query definitions

These are described in the following sections.

The metadata section
--------------------

.. code:: yaml

    metadata:
        version: 1
        description: Kql Sentinel Windows Logon Event Queries
        data_environments: [MSSentinel]
        data_families: [WindowsSecurity]
        tags: ["process", "windows", "processtree", "session"]

- **version**: The version number of the definition file
- **description**: A description of the purpose of this collection of query
  definitions
- **data_environments** []: A list of the Data Environments that
  the defined queries can be run against (1 or more). This value defines
  which QueryProvider instances the queries will be attached to.
- **data_families** []: A list of Data Families the defined queries related
  to. These are just strings that allow you to group related
  queries in the same subcontainer (e.g. queries with a data family "Logons"
  will appear as ``qry_prov.Logons.query_name()``. You can you add
  more than one `data_family` causing the query to be added to each group
  (sub-container). A data family can be a dotted string, causing queries
  to be added to a hierarchy (e.g. "Logons.AAD", "Logons.Linux").
- **tags** []: A list of tags to help manage definition files (this is not
  currently used)

.. note:: You can override metadata items for individual queries.

The defaults section
--------------------

.. code:: yaml

    defaults:
        parameters:
            start:
                description: Query start time
                type: datetime
            end:
                description: Query end time
                type: datetime

This section contains defaults that apply to all queries in the file. The most
common use for this section is to define parameters that are common to all
or many queries in the file. Child keys of the ``defaults`` section
are inherited by the individual query definitions in the file.

.. note:: queries that do not make use parameters defined in defaults
    will just ignore them. That is, if you have a parameter ``xyz`` defined
    here, it will only be used if the query text contains ``{xyz}``.

- **parameters**: parameter defaults for the queries (the format of
  the parameters section is the same as described in
  the sources section.)


The sources section
-------------------

.. code:: yaml

    sources:
        get_host_logon:
            description: Retrieves the logon event for the session id on the host
            metadata:
            args:
                query: '
                    {table}
                    | where EventID == 4624
                    | where Computer has "{host_name}"
                    | where TimeGenerated >= datetime({start})
                    | where TimeGenerated <= datetime({end})
                    | where TargetLogonId == "{logon_session_id}"
                    {add_query_items}'
            parameters:
                host_name:
                    description: Name of host
                    type: str
                logon_session_id:
                    description: The logon session ID of the source process
                    type: str

Each key in the sources section defines a new query (``get_host_logon`` in
the example above). The name of
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
  specified in the query text must have an entry here or in the
  **defaults** section.
  Each parameter entry has the following sub-keys:

  - **description**: A description of what the parameter is (used for generating
    documentation strings).
  - **type**: The data type of the parameter. Valid types include: "str", "int",
    "float", "list" and "datetime". The list and datetime types cause additional
    formatting to be applied (such as converting from a date string)
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

Using known parameter names
---------------------------

Try to use standard names for common entities and other parameter values.
This makes things easier for users of the queries and, in some cases,
enables functionality such as automatic insertion of times.

Always use these names for common parameters

=================  =================================  ============= ===============
Query Parameter    Description                        type          default
=================  =================================  ============= ===============
start              The start datetime for the query   datetime      N/A
end                The end datetime for the query     datetime      N/A
table              The name of the main table (opt)   str           the table name
add_query_items    Placeholder for additional query   str           ""
=================  =================================  ============= ===============

Entity names
For entities such as IP address, host name, account name, process, domain, etc.,
always use one of the standard names - these are used by pivot functions to
map queries to the correct entity.

For the current set of names see the following section in the Pivot Functions
documentation - :ref:`data_analysis/PivotFunctions:How are queries assigned to specific entities?`

How parameter substitution works
--------------------------------

For simple text queries, such as KQL or SQL, parameters are substituted
into the query using string replace. Some parameter types may
have additional formatting applied (e.g. lists and datetimes) to
match the expected format of these types.

You define a replacement parameter token by surrounding it with
single braces ("{" and "}").

.. note:: if you need to include a literal brace in the query
    use a double brace - "{{" and "}}"

For query providers that use JSON queries, the substitution process
is more complex but follows a similar pattern.

Using yaml aliases and macros in your queries
---------------------------------------------

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
---------------------------------------------

It is often helpful to start with a working version of a query without
using any parameters. Just paste in a query that you know is working. Once
you have verified that this works and returns data as expected you can
start to parameterize it.

As you add parameters you can expect to find escaping and quoting
issues with the parameter values. To see what the parameterized version
of the query (without submitting it to the data provider) run the query
with the first parameter "print". This will return the parameterized version
of the query as a string:

.. code:: python3

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

.. code:: python3

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
a query definition file then you import it with:

    *query_provider*.import_query_file(query_file= *path_to_query_file*)

Where *query_provider* is your QueryProvider instance.

This will load the query file into the Query Provider's Query Store from
where it can be called.

.. code:: python3

    qry_prov.import_query_file(query_file='C:\\queries\\example.yaml')

You can also put the file into a folder and load the queries
when you create your query provider:

.. code::python3

    qry_prov = mp.QueryProvider("Splunk", query_paths=["~/home/mp_queries"])

.. note:: ``query_paths``` is a list of strings, so make sure that you
    surround a single path with Python list brackets.

Once imported the queries in the files appear in the Query Provider's
Query Store alongside the others and can be called in the same manner as
pre-defined queries.

If you have created a large number of query definition files and you
want to have the automatically imported into a Query Provider's query
store at initialization you can specify a directory containing these
queries in the msticpyconfig.yaml file under QueryDefinitions: Custom:

For example, if you have two folders with queries in each that
you want to load, add entries for each to your msticpyconfig.yaml file.

Example:

.. code:: yaml

    QueryDefinitions:
        Custom:
            - /home/ian/mp_queries
            - /home/ian/mp_queries_common


Having the ``Custom`` field populated will mean the Query Provider will
automatically enumerate all the YAML files in the directory provided and
automatically import he relevant queries into the query store at
initialization alongside the default queries. Custom queries with the
same name as default queries will overwrite default queries.

.. code:: python3

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


.. code:: python3

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


.. code:: python3

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
with "print" and the parameters required by that query it will construct
and print out the query string to be run.

.. code:: python3

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

