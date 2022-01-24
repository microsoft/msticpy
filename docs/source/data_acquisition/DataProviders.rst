Data Provider Library
=====================

Description:
------------

This package provides functions to allow for the defining of data
sources, connectors to them, and queries for them as well as the ability
to call these elements to return query result from the defined data
sources. The package currently support connections to Log
Analytics/Azure Sentinel/Azure Security Center, and the Microsoft
Security Graph.

The first step in using this package is to install the msticpy package.

.. code:: ipython3

    !pip install msticpy --upgrade --user


.. parsed-literal::

    Collecting git+https://github.com/microsoft/msticpy
    Building wheels for collected packages: msticpy
      Building wheel for msticpy (setup.py): started
      Building wheel for msticpy (setup.py): finished with status 'done'
    Successfully built msticpy
    Installing collected packages: msticpy
    Successfully installed msticpy-0.2.1



.. code:: ipython3

    #Check we are running Python 3.6
    import sys
    MIN_REQ_PYTHON = (3,6)
    if sys.version_info < MIN_REQ_PYTHON:
        print('Check the Kernel->Change Kernel menu and ensure that Python 3.6')
        print('or later is selected as the active kernel.')
        sys.exit("Python %s.%s or later is required.\n" % MIN_REQ_PYTHON)

    #imports
    import yaml
    import msticpy.nbtools as nbtools

    #data library imports
    from msticpy.data.data_providers import QueryProvider
    import msticpy.data.data_query_reader as QueryReader
    from msticpy.data.param_extractor import extract_query_params
    import msticpy.nbtools as mas

    print('Imports Complete')



.. parsed-literal::

    Imports Complete


Instantiating a Query Provider
------------------------------

In order to connect to and query a
data source we need to define what sort of Data Environment we want to
connect to and query (in this Notebook we will use Log Analytics as an
example). To view the options available you can call
QueryProvider.list_data_environments() which will return a list of all
the available options.

After selecting a Data Environment we can initialize our Query Provider
by calling QueryProvider(DATA_ENVIRONMENT). This will load the relevant
driver for connecting to the data environment we have selected as well
as provisioning a query store for us and adding queries from our default
query directory.

There are two other optional parameters we can pass when initializing
our Query Providers to further customize it: \* We can also chose to
initialize our Query Provider with a driver other than the default one
with:

.. code:: ipython3

    QueryProvider(
        data_environment=DATA_ENVIRONMENT,
        driver=QUERY_DRIVER
    )

\* We can choose to import queries from a custom
query directory (see `Creating new queries`_ for more
details) with:

.. code:: ipython3

    QueryProvider(
        data_environment=DATA_ENVIRONMENT,
        driver=QUERY_DRIVER,
        query_path=QUERY_DIRECTORY_PATH
    )


For now we will simply create a Query Provider with default values.

::

    Query provider interface to queries.

    Parameters
    ----------
    data_environment : Union[str, DataEnvironment]
        Name or Enum of environment for the QueryProvider
    driver : DriverBase, optional
        Override the built-in driver (query execution class)
        and use your own driver (must inherit from
        `DriverBase`)

.. code:: ipython3

    # List the data environments available
    data_environments = QueryProvider.list_data_environments()
    print(data_environments)

    # Create a query provider for Azure Sentinel/Log Analytics
    qry_prov = QueryProvider(data_environment='LogAnalytics')


.. parsed-literal::

    ['LogAnalytics', 'Kusto', 'AzureSecurityCenter', 'SecurityGraph']
    Please wait. Loading Kqlmagic extension...



.. raw:: html

    <html>
        <head>
        <style>
        .kql-magic-banner {
            display: flex;
            background-color: #d9edf7;
        }
        .kql-magic-banner > div {
            margin: 10px;
            padding: 20px;
            color: #3a87ad;
            font-size: 13px;
        }
        </style>
        </head>
        <body>
            <div class='kql-magic-banner'>
                <div><img src='data:image/png;base64,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'></div>
                <div>
                    <p>Kql Query Language, aka kql, is the query language for advanced analytics on Azure Monitor resources. The current supported data sources are
                    Azure Data Explorer (Kusto), Log Analytics and Application Insights. To get more information execute '%kql --help "kql"'</p>
                    <p>   &bull; kql reference: Click on 'Help' tab > and Select 'kql reference' or execute '%kql --help "kql"'<br>
                        &bull; Kqlmagic configuration: execute '%config Kqlmagic'<br>
                        &bull; Kqlmagic usage: execute '%kql --usage'<br>
                </div>
            </div>
        </body>
    </html>


Connecting to a Data Environment
--------------------------------

Once we have instantiated the query
provider and loaded the relevant driver we can connect to the Data
Environment. This is done by calling the connect() function of the Query
Provider we just initialized and passing it a connection string to use.

For Log Analytics/Azure Sentinel the connection string is in the format
of loganalytics://code().tenant(“TENANT_ID”).workspace(“WORKSPACE_ID”).
Other Data Environments will have different connection string formats.

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

    ws_id = input('Workspace ID')
    ten_id = input('Tenant ID')
    la_connection_string = f'loganalytics://code().tenant("{ten_id}").workspace("{ws_id}")'
    qry_prov.connect(connection_str=f'{la_connection_string}')


.. parsed-literal::

    Workspace ID xxxxxxxxxxxxxxxxxxxxxxxxxxx
    Tenant ID xxxxxxxxxxxxxxxxxxxxxxxxxxx


.. note::

     The KQL provider now supports authentication via the `Azure CLI <https://docs.microsoft.com/en-us/cli/azure/authenticate-azure-cli>`_ and `Managed System Identities <https://docs.microsoft.com/en-us/azure/active-directory/managed-identities-azure-resources/overview>_`.
     To use these authentication methods, pass local() with either the `cli` or `msi` keyword arguements:
     ``la_connection_string = f'loganalytics://code().tenant("{ten_id}").workspace("{ws_id}")'
     qry_prov.connect(connection_str=f'{la_connection_string}', cli=locals())``


List of current built-in queries
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

See this document :doc:`MSTICPy built-in queries <DataQueries>`

Connecting to an Azure Sentinel Workspace
-----------------------------------------


The previous example showed making a connection to an Azure Sentinel workspace
by manually creating a connection string. *msticpy* has functions to build
this connection string for you and some flexible configuration options
allowing you to store and manage the settings for multiple workspaces.

Configuration in *config.json*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When you load a notebook from the Azure Sentinel UI (either in Azure Notebooks
or in an Azure Machine Learning Workspace) a configuration file *config.json*
is provisioned for you with the details of the source workspace populated in
the file. An example is shown here.

.. code:: json

    {
        "tenant_id": "335b56ab-67a2-4118-ac14-6eb454f350af",
        "subscription_id": "b8f250f8-1ba5-4b2c-8e74-f7ea4a1df8a6",
        "resource_group": "ExampleWorkspaceRG",
        "workspace_id": "271f17d3-5457-4237-9131-ae98a6f55c37",
        "workspace_name": "ExampleWorkspace"
    }

*msticpy* will automatically look for a *config.json* file in the current
directory. If not found here, it will search the parent directory and in all
its subdirectories. It will use the first *config.json* file found.

The class that searches for and loads your config.json is ``WorkspaceConfig``.
See :py:mod:`WorkspaceConfig API documentation<msticpy.common.wsconfig>`

``WorkspaceConfig`` also works with workspace configuration stored in *msticpyconfig.yaml*
(see next section).

To use ``WorkspaceConfig``, simple create an instance of it. It will automatically build
your connection string for use with the query provider library.

.. code:: IPython

    >>> ws_config = WorkspaceConfig()
    >>> ws_config.code_connect_str

    "loganalytics://code().tenant('335b56ab-67a2-4118-ac14-6eb454f350af').workspace('271f17d3-5457-4237-9131-ae98a6f55c37')"

You can use this connection string in the call to ``QueryProvider.connect()``

.. code:: IPython

    qry_prov.connect(connection_str=ws_config.code_connect_str)

If you need use a specific instance of a config.json you can specify a full
path to the file you want to use when you create your ``WorkspaceConfig``
instance.

.. code:: IPython

    ws_config = WorkspaceConfig(config_file="~/myworkspaces/ws123-config.json")
    ws_config.code_connect_str


Configuration in *msticpyconfig.yaml*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

You can also store workspace details in your *msticpyconfig.yaml* file. This
has some advantages over using a *config.json*:

- you can store multiple workspace definitions
- you can use an environment variable to specify its location

You likely need to use a *msticpyconfig.yaml* anyway. If you are using other
*msticpy* features such as Threat Intelligence Providers, GeoIP Lookup, Azure Data,
etc., these all have their own configuration settings, so using a single
configuration file makes managing your settings easier. The one downside to using
msticpyconfig.yaml is that you have to populate the workspace settings manually.

For more information on using and configuring *msticpyconfig.yaml* see
:doc:`msticpy Package Configuration <../getting_started/msticpyconfig>`

The Azure Sentinel connection settings are stored in the
``AzureSentinel\\Workspaces`` section of the file.

.. code:: yaml

    AzureSentinel:
      Workspaces:
        # Workspace used if you don't explicitly name a workspace when creating WorkspaceConfig
        # Specifying values here overrides config.json settings unless you explicitly load
        # WorkspaceConfig with config_file parameter (WorkspaceConfig(config_file="../config.json")
        Default:
          WorkspaceId: "271f17d3-5457-4237-9131-ae98a6f55c37"
          TenantId: "335b56ab-67a2-4118-ac14-6eb454f350af"
        # To use these launch with an explicit name - WorkspaceConfig(workspace_name="Workspace2")
        Workspace2:
          WorkspaceId: "c88dd3c2-d657-4eb3-b913-58d58d811a41"
          TenantId: "335b56ab-67a2-4118-ac14-6eb454f350af"
        Workspace3:
          WorkspaceId: "17e64332-19c9-472e-afd7-3629f299300c"
          TenantId: "4ea41beb-4546-4fba-890b-55553ce6003a"

If you only use a single workspace, you only need to create a ``Default`` entry and
add the values for your *WorkspaceID* and *TenantID*. You can add other entries here,
for example, SubscriptionID, ResourceGroup but these are not currently used by
*msticpy*.

.. note:: The property names are spelled differently to the values in the
   *config.json* so be sure to enter these as shown in the example. These
   names are case-sensitive, so they should be entered as shown.

If you use multiple workspaces, you can add further entries here. Each
workspace entry is normally the name of the Azure Sentinel workspace but
you can use any name you prefer.

To see which workspaces are configured in your *msticpyconfig.yaml* use
the ``list_workspaces()`` function.

.. tip:: ``list_workspaces`` is a class function, so you do not need to
   instantiate a WorkspaceConfig to call this function.

.. code:: IPython

    >>> WorkspaceConfig.list_workspaces()

    {'Default': {'WorkspaceId': '271f17d3-5457-4237-9131-ae98a6f55c37',
      'TenantId': '335b56ab-67a2-4118-ac14-6eb454f350af'},
     'Workspace2': {'WorkspaceId': 'c88dd3c2-d657-4eb3-b913-58d58d811a41',
       'TenantId': '335b56ab-67a2-4118-ac14-6eb454f350af'},
     'Workspace3': {'WorkspaceId': '17e64332-19c9-472e-afd7-3629f299300c',
       'TenantId': '4ea41beb-4546-4fba-890b-55553ce6003a'}}

If you run ``WorkspaceConfig`` with no parameters it will
try to load values from the "Default" entry in *msticpyconfig.yaml*.
If this fails it will fall back to searching for a *config.json* as
described in the previous section.

.. tip:: You can duplicate the settings as the ``Default`` entry in another
   named entry so that you can load it by name.

To load settings for a specific workspace use the ``workspace_name``
parameter to specify the workspace that you want to connect to.
``workspace_name`` is the name of the workspace entry that you created in
the msticpyconfig section added under ``AzureSentinel\\Workspaces``
- not necessarily that actual name of the workspace.

.. code:: IPython

    ws_config = WorkspaceConfig(workspace_name="Workspace2")

Entries in msticpyconfig always take precedence over settings in your
config.json. If you want to force use of the config.json, specify the path
to the config.json file in the ``config_file`` parameter to ``WorkspaceConfig``.

.. warning:: Although msticpy allows you to configure multiple entries for
   workspaces in different tenants, you cannot currently authenticate to workspaces
   that span multiple tenants in the same notebook. If you need to do this, you
   should investigate
   `Azure Lighthouse <https://azure.microsoft.com/services/azure-lighthouse/>`__.
   This allows delegated access to workspaces in multiple tenants from a single
   tenant.


Connecting to an OData Source
-----------------------------
:py:mod:`OData driver API documentation<msticpy.data.drivers.odata_driver>`

You can also connect to OData based data sources such as the Microsoft Defender,
Microsoft Defender for Endpoint, and the Microsoft Graph APIs.
These connections often rely on having a
dedicated Azure AD app (plus app secret) for handling the authentication process.

Microsoft 365 Defender
~~~~~~~~~~~~~~~~~~~~~~
:py:mod:`MDATP driver API documentation<msticpy.data.drivers.mdatp_driver>`

.. note:: This section applies to both Microsoft 365 Defender and Microsoft Defender
    for Endpoint (MDE). The former supersedes and is a subset of the the latter
    but both are still available to use.

Details on registering an Azure AD application for MS 365 Defender can be found
`here <https://docs.microsoft.com/en-us/windows/security/threat-protection/microsoft-defender-atp/exposed-apis-create-app-webapp>`__.
Once you have registered the application you can use it to connect to
the MS Defender API.

The parameters required for connection to Defender can be passed in
a number of ways. The simplest is to pass these as
keyword arguments (although it's better to add these to your configuration
if you are going to be using them directly - see the following section).

You need to both load the Microsoft Defender data provider and call
the connect function to authenticate.

Call the QueryProvider with "M365D" as the environment parameter.
To use the Defender for Endpoint API you can pass "MDE" as the parameter.

If you are specifying connection parameters in the function call (rather
than using configuration settings), the required parameters are:

* tenant_id -- The tenant ID of the Defender workspace to connect to.
* client_id -- The ID of the application registered for MS Defender.
* client_secret -- The secret used for by the application.


.. code:: ipython3

        ten_id = input('Tenant ID')
        client_id = input('Client ID')
        client_secret = input('Client Secret')
        md_prov = QueryProvider('M365D')
        md_prov.connect(tenant_id=ten_id, client_id=client_id, client_secret=client_secret)

.. note:: You can also specify these parameters as a connection string:
    "tenant_id='my_tenant'; client_id='my_appid'; client_secret='my_secret'"

Alternatively you can store these details in the msticpyconfig.yaml
file. Details should be included in the following format:

.. code:: yaml

      MicrosoftDefender:
          Args:
            ClientId: "CLIENT ID"
            ClientSecret: "CLIENT SECRET"
            TenantId: "TENANT ID"


We strongly recommend storing the client secret value
in Azure Key Vault. You can replace the text value with a referenced
to a Key Vault secret using the MSTICPy configuration editor.
See :doc:`msticpy Settings Editor <../getting_started/SettingsEditor>`)

Your configuration when using Key Vault should look like the following:

.. code:: yaml

      MicrosoftDefender:
          Args:
            ClientId: "CLIENT ID"
            ClientSecret:
                KeyVault:
            TenantId: "TENANT ID"


To use the configured values, call ``connect`` with no parameters.

.. code:: ipython3

        mdatp_prov = QueryProvider('M365D')
        mdatp_prov.connect()

For examples of using the MS Defender provider, see the sample MDATPQuery Notebook.


Security Graph API
~~~~~~~~~~~~~~~~~~
:py:mod:`Security Graph driver API documentation<msticpy.data.drivers.security_graph_driver>`

Connecting to the Security Graph API follows the same format as MS Defender
connections with connection variables passed to the function in the
same way. The configuration format is also identical to that specified in the
previous section.



Details for registering an application for the Security Graph API can
be found `here <https://docs.microsoft.com/en-us/graph/auth-register-app-v2?context=graph%2Fapi%2F1.0&view=graph-rest-1.0>`__.

.. code:: ipython3

        mdatp_prov = QueryProvider('SecurityGraph')
        mdatp_prov.connect()


Using Local Data - the LocalData provider
-----------------------------------------

:py:mod:`LocalData driver documentation<msticpy.data.drivers.local_data_driver>`

The ``LocalData`` data provider is intended primarily for testing or demonstrations
where you may not be able to connect to an online data source reliably.

The data backing this driver can be in the form of a pickled pandas DataFrame
or a CSV file. In either case the data is converted to a DataFrame to be returned
from the query. Usage of this driver is a little different to most other drivers:

* You will need to provide a path to your data files when initializing
  the query provider (by default it will search in the current folder).
* You will also need to provide a query definition file (see following
  example) that maps the data file names that you are using to
  query names. The path to search for this is specified in the ``query_paths``
  parameter (see code examples below).
* Parameters to queries are ignored.

To define the queries you need to create a query definition file.
This is an example of a LocalData yaml query file. It is similar to the query
definition files for other providers but simpler. It only requires a
description, the data family that the query should be grouped under and
the name of the file containing the data.

.. code:: yaml

    metadata:
        version: 1
        description: Local Data Alert Queries
        data_environments: [LocalData]
        data_families: [SecurityAlert, WindowsSecurity, Network]
        tags: ['alert', 'securityalert', 'process', 'account', 'network']
    defaults:
    sources:
        list_alerts:
            description: Retrieves list of alerts
            metadata:
                data_families: [SecurityAlert]
            args:
                query: alerts_list.pkl
            parameters:
        list_host_logons:
            description: List logons on host
            metadata:
                data_families: [WindowsSecurity]
            args:
                query: host_logons.csv
            parameters:
        list_network_alerts:
            description: List network alerts.
            args:
                query: net_alerts.csv
            parameters:


In this example the value for the "query" is just the file name.
If the queries in your file are a mix of data from different data families,
you can group them by specifying one or more values for ``data_families``.
If this isn't specified for an individual query, it will inherit the setting
for ``data_families`` in the global ``metadata`` section at the top of the file.
Specifying more than one value for ``data_families``
will add links to the query under each data family grouping. This is to allow
for cases where a query may be relevant to multiple categories.
The ``data_families`` control only how the queries appear in query provider and
don't affect any other aspects of the query operation.

In the example shown, the ``list_alerts`` query has been added to the ``SecurityAlert``
attribute of the query provider, while ``list_host_logons`` is member of
``WindowsSecurity``. The entry for ``list_network_alerts`` had no ``data_families``
attribute so inherits the values from the file's ``metadata``. Since this has multiple
values, the query is added to all three families.

.. code:: ipython3

    # Structure of the query attributes added to the query provider
    qry_prov.list_queries()

.. parsed-literal::

    Network.list_host_logons
    Network.list_network_alerts
            ...<other queries>
    SecurityAlert.list_alerts
    SecurityAlert.list_network_alerts
            ...<other queries>
    WindowsSecurity.list_host_logons
    WindowsSecurity.list_network_alerts

For more details about the query definition file structure see
`Creating new queries`_.


To use the ``LocalData`` provider:

1. Collect your data files into one or more directories or directory trees
   (the default location to search for data file is the current directory).
   Subdirectories are searched for ".pkl" and ".csv" files but only file
   names matching your query definitions will loaded.
2. Create one or more query definition yaml files (following the pattern above)
   and place these in a directory (this can be the same as the data files).
   The query provider will load and merge definitions from multiple YAML files.

QueryProvider defaults to searching for data files in the current directory
and subdirectories. The default paths for query definition files are a) the
built-in package queries path (msticpy/data/queries) and b) any custom
paths that you have added to msticpyconfig.yaml (see
:doc:`msticpy Package Configuration <../getting_started/msticpyconfig>`).

.. note:: The query definition files must have a ``.yaml`` extension.


.. code:: ipython3

    # Creating a query provider with "LocalData" parameter
    qry_prov = QueryProvider("LocalData")

    # list the queries loaded
    print(qry_prov.list_queries())

    # run a query
    my_alerts = qry_prov.SecurityAlert.list_alerts()

    # Specify path to look for data files
    data_path = "./my_data"
    qry_prov = QueryProvider("LocalData", data_paths=[data_path])

    # Show the schema of the data files read in
    print(qry_prov.schema)

    # Specify both data and query locations
    data_path = "./my_data"
    query_path = "./myqueries"
    qry_prov = QueryProvider("LocalData", data_paths=[data_path], query_paths=[query_path])

    host_logons_df = qry_prov.WindowsSecurity.list_host_logons()

    # parameters are accepted but ignored
    host_logons_df = qry_prov.WindowsSecurity.list_host_logons(
        start=st_date,
        end=end_date,
        host_name="myhost.com",
    )


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
-  The raw (unparameterized) query that will be run


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
          <th>TimeGenerated</th>
          <th>AlertDisplayName</th>
          <th>AlertName</th>
          <th>Severity</th>
          <th>Description</th>
          <th>ProviderName</th>
          <th>VendorName</th>
          <th>VendorOriginalId</th>
          <th>SystemAlertId</th>
          <th>...</th>
          <th>ExtendedProperties</th>
          <th>Entities</th>
          <th>SourceSystem</th>
          <th>WorkspaceSubscriptionId</th>
          <th>WorkspaceResourceGroup</th>
          <th>ExtendedLinks</th>
          <th>ProductName</th>
          <th>ProductComponentName</th>
          <th>Type</th>
          <th>CompromisedEntity</th>
        </tr>
      </thead>
      <tbody>
        <tr>
          <th>0</th>
          <td>b1315f05-4a7a-45b4-811f-73e715f7c122</td>
          <td>2019-07-22 06:35:13</td>
          <td>Suspicious authentication activity</td>
          <td>Suspicious authentication activity</td>
          <td>Medium</td>
          <td>Although none of them succeeded, some of them ...</td>
          <td>Detection</td>
          <td>Microsoft</td>
          <td>8af9954d-f28d-40ff-a079-d9d4cc5a5268</td>
          <td>2518385291989119899_8af9954d-f28d-40ff-a079-d9...</td>
          <td>...</td>
          <td>{\r\n  "Activity start time (UTC)": "2019/07/2...</td>
          <td>[\r\n  {\r\n    "$id": "4",\r\n    "HostName":...</td>
          <td>Detection</td>
          <td>3b701f84-d04b-4479-89b1-fa8827eb537e</td>
          <td>sentineltest</td>
          <td>[\r\n  {\r\n    "Href": "https://interflowwebp...</td>
          <td>Azure Security Center</td>
          <td></td>
          <td>SecurityAlert</td>
          <td></td>
        </tr>
        <tr>
          <th>1</th>
          <td>b1315f05-4a7a-45b4-811f-73e715f7c122</td>
          <td>2019-07-22 06:35:13</td>
          <td>Suspicious authentication activity</td>
          <td>Suspicious authentication activity</td>
          <td>Medium</td>
          <td>Although none of them succeeded, some of them ...</td>
          <td>Detection</td>
          <td>Microsoft</td>
          <td>8af9954d-f28d-40ff-a079-d9d4cc5a5268</td>
          <td>5d60fff6-7dd2-4474-a4d0-4c8e3fa6fad6</td>
          <td>...</td>
          <td>{\r\n  "Activity start time (UTC)": "2019/07/2...</td>
          <td>[\r\n  {\r\n    "$id": "4",\r\n    "HostName":...</td>
          <td>Detection</td>
          <td>3b701f84-d04b-4479-89b1-fa8827eb537e</td>
          <td>sentineltest</td>
          <td>[\r\n  {\r\n    "Href": "https://interflowwebp...</td>
          <td>Azure Security Center</td>
          <td></td>
          <td>SecurityAlert</td>
          <td></td>
        </tr>
        <tr>
          <th>2</th>
          <td>b1315f05-4a7a-45b4-811f-73e715f7c122</td>
          <td>2019-07-22 07:02:42</td>
          <td>Traffic from unrecommended IP addresses was de...</td>
          <td>Traffic from unrecommended IP addresses was de...</td>
          <td>Low</td>
          <td>Azure security center has detected incoming tr...</td>
          <td>AdaptiveNetworkHardenings</td>
          <td>Microsoft</td>
          <td>ba07c315-0af5-4568-9ecd-6c788f9267ae</td>
          <td>b7adb73b-0778-4929-b46a-c0ed642bc61f</td>
          <td>...</td>
          <td>{\r\n  "Destination Port": "3389",\r\n  "Proto...</td>
          <td>[\r\n  {\r\n    "$id": "4",\r\n    "ResourceId...</td>
          <td>Detection</td>
          <td></td>
          <td></td>
          <td>[\r\n  {\r\n    "DetailBladeInputs": "protecte...</td>
          <td>Azure Security Center</td>
          <td></td>
          <td>SecurityAlert</td>
          <td></td>
        </tr>
        <tr>
          <th>3</th>
          <td>b1315f05-4a7a-45b4-811f-73e715f7c122</td>
          <td>2019-07-26 06:03:16</td>
          <td>Traffic from unrecommended IP addresses was de...</td>
          <td>Traffic from unrecommended IP addresses was de...</td>
          <td>Low</td>
          <td>Azure security center has detected incoming tr...</td>
          <td>AdaptiveNetworkHardenings</td>
          <td>Microsoft</td>
          <td>c3144593-9bae-448e-87dd-b2d3c47de571</td>
          <td>d89ad3b2-f7a7-4cff-b8a4-3f6fa58b4760</td>
          <td>...</td>
          <td>{\r\n  "Destination Port": "22",\r\n  "Protoco...</td>
          <td>[\r\n  {\r\n    "$id": "4",\r\n    "ResourceId...</td>
          <td>Detection</td>
          <td></td>
          <td></td>
          <td>[\r\n  {\r\n    "DetailBladeInputs": "protecte...</td>
          <td>Azure Security Center</td>
          <td></td>
          <td>SecurityAlert</td>
          <td></td>
        </tr>
        <tr>
          <th>4</th>
          <td>b1315f05-4a7a-45b4-811f-73e715f7c122</td>
          <td>2019-07-23 06:42:01</td>
          <td>Traffic from unrecommended IP addresses was de...</td>
          <td>Traffic from unrecommended IP addresses was de...</td>
          <td>Low</td>
          <td>Azure security center has detected incoming tr...</td>
          <td>AdaptiveNetworkHardenings</td>
          <td>Microsoft</td>
          <td>4e4173a6-1a27-451f-8a3c-25d10b306c30</td>
          <td>11813ab7-ab7c-4719-b0a1-ccb5d4a32223</td>
          <td>...</td>
          <td>{\r\n  "Destination Port": "3389",\r\n  "Proto...</td>
          <td>[\r\n  {\r\n    "$id": "4",\r\n    "ResourceId...</td>
          <td>Detection</td>
          <td></td>
          <td></td>
          <td>[\r\n  {\r\n    "DetailBladeInputs": "protecte...</td>
          <td>Azure Security Center</td>
          <td></td>
          <td>SecurityAlert</td>
          <td></td>
        </tr>
      </tbody>
    </table>
    <p>5 rows × 30 columns</p>
    </div>

|

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
          <th>TimeGenerated</th>
          <th>AlertDisplayName</th>
          <th>AlertName</th>
          <th>Severity</th>
          <th>Description</th>
          <th>ProviderName</th>
          <th>VendorName</th>
          <th>VendorOriginalId</th>
          <th>SystemAlertId</th>
          <th>...</th>
          <th>ExtendedProperties</th>
          <th>Entities</th>
          <th>SourceSystem</th>
          <th>WorkspaceSubscriptionId</th>
          <th>WorkspaceResourceGroup</th>
          <th>ExtendedLinks</th>
          <th>ProductName</th>
          <th>ProductComponentName</th>
          <th>Type</th>
          <th>CompromisedEntity</th>
        </tr>
      </thead>
      <tbody>
        <tr>
          <th>0</th>
          <td>b1315f05-4a7a-45b4-811f-73e715f7c122</td>
          <td>2019-07-26 06:03:16</td>
          <td>Traffic from unrecommended IP addresses was de...</td>
          <td>Traffic from unrecommended IP addresses was de...</td>
          <td>Low</td>
          <td>Azure security center has detected incoming tr...</td>
          <td>AdaptiveNetworkHardenings</td>
          <td>Microsoft</td>
          <td>c3144593-9bae-448e-87dd-b2d3c47de571</td>
          <td>d89ad3b2-f7a7-4cff-b8a4-3f6fa58b4760</td>
          <td>...</td>
          <td>{\r\n  "Destination Port": "22",\r\n  "Protoco...</td>
          <td>[\r\n  {\r\n    "$id": "4",\r\n    "ResourceId...</td>
          <td>Detection</td>
          <td></td>
          <td></td>
          <td>[\r\n  {\r\n    "DetailBladeInputs": "protecte...</td>
          <td>Azure Security Center</td>
          <td></td>
          <td>SecurityAlert</td>
          <td></td>
        </tr>
        <tr>
          <th>1</th>
          <td>b1315f05-4a7a-45b4-811f-73e715f7c122</td>
          <td>2019-07-23 06:42:01</td>
          <td>Traffic from unrecommended IP addresses was de...</td>
          <td>Traffic from unrecommended IP addresses was de...</td>
          <td>Low</td>
          <td>Azure security center has detected incoming tr...</td>
          <td>AdaptiveNetworkHardenings</td>
          <td>Microsoft</td>
          <td>4e4173a6-1a27-451f-8a3c-25d10b306c30</td>
          <td>11813ab7-ab7c-4719-b0a1-ccb5d4a32223</td>
          <td>...</td>
          <td>{\r\n  "Destination Port": "3389",\r\n  "Proto...</td>
          <td>[\r\n  {\r\n    "$id": "4",\r\n    "ResourceId...</td>
          <td>Detection</td>
          <td></td>
          <td></td>
          <td>[\r\n  {\r\n    "DetailBladeInputs": "protecte...</td>
          <td>Azure Security Center</td>
          <td></td>
          <td>SecurityAlert</td>
          <td></td>
        </tr>
        <tr>
          <th>2</th>
          <td>b1315f05-4a7a-45b4-811f-73e715f7c122</td>
          <td>2019-07-22 06:35:13</td>
          <td>Suspicious authentication activity</td>
          <td>Suspicious authentication activity</td>
          <td>Medium</td>
          <td>Although none of them succeeded, some of them ...</td>
          <td>Detection</td>
          <td>Microsoft</td>
          <td>8af9954d-f28d-40ff-a079-d9d4cc5a5268</td>
          <td>2518385291989119899_8af9954d-f28d-40ff-a079-d9...</td>
          <td>...</td>
          <td>{\r\n  "Activity start time (UTC)": "2019/07/2...</td>
          <td>[\r\n  {\r\n    "$id": "4",\r\n    "HostName":...</td>
          <td>Detection</td>
          <td>3b701f84-d04b-4479-89b1-fa8827eb537e</td>
          <td>sentineltest</td>
          <td>[\r\n  {\r\n    "Href": "https://interflowwebp...</td>
          <td>Azure Security Center</td>
          <td></td>
          <td>SecurityAlert</td>
          <td></td>
        </tr>
        <tr>
          <th>3</th>
          <td>b1315f05-4a7a-45b4-811f-73e715f7c122</td>
          <td>2019-07-22 06:35:13</td>
          <td>Suspicious authentication activity</td>
          <td>Suspicious authentication activity</td>
          <td>Medium</td>
          <td>Although none of them succeeded, some of them ...</td>
          <td>Detection</td>
          <td>Microsoft</td>
          <td>8af9954d-f28d-40ff-a079-d9d4cc5a5268</td>
          <td>5d60fff6-7dd2-4474-a4d0-4c8e3fa6fad6</td>
          <td>...</td>
          <td>{\r\n  "Activity start time (UTC)": "2019/07/2...</td>
          <td>[\r\n  {\r\n    "$id": "4",\r\n    "HostName":...</td>
          <td>Detection</td>
          <td>3b701f84-d04b-4479-89b1-fa8827eb537e</td>
          <td>sentineltest</td>
          <td>[\r\n  {\r\n    "Href": "https://interflowwebp...</td>
          <td>Azure Security Center</td>
          <td></td>
          <td>SecurityAlert</td>
          <td></td>
        </tr>
        <tr>
          <th>4</th>
          <td>b1315f05-4a7a-45b4-811f-73e715f7c122</td>
          <td>2019-07-22 07:02:42</td>
          <td>Traffic from unrecommended IP addresses was de...</td>
          <td>Traffic from unrecommended IP addresses was de...</td>
          <td>Low</td>
          <td>Azure security center has detected incoming tr...</td>
          <td>AdaptiveNetworkHardenings</td>
          <td>Microsoft</td>
          <td>ba07c315-0af5-4568-9ecd-6c788f9267ae</td>
          <td>b7adb73b-0778-4929-b46a-c0ed642bc61f</td>
          <td>...</td>
          <td>{\r\n  "Destination Port": "3389",\r\n  "Proto...</td>
          <td>[\r\n  {\r\n    "$id": "4",\r\n    "ResourceId...</td>
          <td>Detection</td>
          <td></td>
          <td></td>
          <td>[\r\n  {\r\n    "DetailBladeInputs": "protecte...</td>
          <td>Azure Security Center</td>
          <td></td>
          <td>SecurityAlert</td>
          <td></td>
        </tr>
      </tbody>
    </table>
    <p>5 rows × 30 columns</p>
    </div>

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
          <th>TimeGenerated</th>
          <th>DisplayName</th>
          <th>AlertName</th>
          <th>AlertSeverity</th>
          <th>Description</th>
          <th>ProviderName</th>
          <th>VendorName</th>
          <th>VendorOriginalId</th>
          <th>SystemAlertId</th>
          <th>...</th>
          <th>RemediationSteps</th>
          <th>ExtendedProperties</th>
          <th>Entities</th>
          <th>SourceSystem</th>
          <th>WorkspaceSubscriptionId</th>
          <th>WorkspaceResourceGroup</th>
          <th>ExtendedLinks</th>
          <th>ProductName</th>
          <th>ProductComponentName</th>
          <th>Type</th>
        </tr>
      </thead>
      <tbody>
        <tr>
          <th>0</th>
          <td>b1315f05-4a7a-45b4-811f-73e715f7c122</td>
          <td>2019-07-22 06:35:13</td>
          <td>Suspicious authentication activity</td>
          <td>Suspicious authentication activity</td>
          <td>Medium</td>
          <td>Although none of them succeeded, some of them ...</td>
          <td>Detection</td>
          <td>Microsoft</td>
          <td>8af9954d-f28d-40ff-a079-d9d4cc5a5268</td>
          <td>2518385291989119899_8af9954d-f28d-40ff-a079-d9...</td>
          <td>...</td>
          <td>[\r\n  "1. Enforce the use of strong passwords...</td>
          <td>{\r\n  "Activity start time (UTC)": "2019/07/2...</td>
          <td>[\r\n  {\r\n    "$id": "4",\r\n    "HostName":...</td>
          <td>Detection</td>
          <td>3b701f84-d04b-4479-89b1-fa8827eb537e</td>
          <td>sentineltest</td>
          <td>[\r\n  {\r\n    "Href": "https://interflowwebp...</td>
          <td>Azure Security Center</td>
          <td></td>
          <td>SecurityAlert</td>
        </tr>
        <tr>
          <th>1</th>
          <td>b1315f05-4a7a-45b4-811f-73e715f7c122</td>
          <td>2019-07-22 06:35:13</td>
          <td>Suspicious authentication activity</td>
          <td>Suspicious authentication activity</td>
          <td>Medium</td>
          <td>Although none of them succeeded, some of them ...</td>
          <td>Detection</td>
          <td>Microsoft</td>
          <td>8af9954d-f28d-40ff-a079-d9d4cc5a5268</td>
          <td>5d60fff6-7dd2-4474-a4d0-4c8e3fa6fad6</td>
          <td>...</td>
          <td>[\r\n  "1. Enforce the use of strong passwords...</td>
          <td>{\r\n  "Activity start time (UTC)": "2019/07/2...</td>
          <td>[\r\n  {\r\n    "$id": "4",\r\n    "HostName":...</td>
          <td>Detection</td>
          <td>3b701f84-d04b-4479-89b1-fa8827eb537e</td>
          <td>sentineltest</td>
          <td>[\r\n  {\r\n    "Href": "https://interflowwebp...</td>
          <td>Azure Security Center</td>
          <td></td>
          <td>SecurityAlert</td>
        </tr>
        <tr>
          <th>2</th>
          <td>b1315f05-4a7a-45b4-811f-73e715f7c122</td>
          <td>2019-07-22 07:02:42</td>
          <td>Traffic from unrecommended IP addresses was de...</td>
          <td>Traffic from unrecommended IP addresses was de...</td>
          <td>Low</td>
          <td>Azure security center has detected incoming tr...</td>
          <td>AdaptiveNetworkHardenings</td>
          <td>Microsoft</td>
          <td>ba07c315-0af5-4568-9ecd-6c788f9267ae</td>
          <td>b7adb73b-0778-4929-b46a-c0ed642bc61f</td>
          <td>...</td>
          <td>[\r\n  "1. Review the IP addresses and determi...</td>
          <td>{\r\n  "Destination Port": "3389",\r\n  "Proto...</td>
          <td>[\r\n  {\r\n    "$id": "4",\r\n    "ResourceId...</td>
          <td>Detection</td>
          <td></td>
          <td></td>
          <td>[\r\n  {\r\n    "DetailBladeInputs": "protecte...</td>
          <td>Azure Security Center</td>
          <td></td>
          <td>SecurityAlert</td>
        </tr>
        <tr>
          <th>3</th>
          <td>b1315f05-4a7a-45b4-811f-73e715f7c122</td>
          <td>2019-07-26 06:03:16</td>
          <td>Traffic from unrecommended IP addresses was de...</td>
          <td>Traffic from unrecommended IP addresses was de...</td>
          <td>Low</td>
          <td>Azure security center has detected incoming tr...</td>
          <td>AdaptiveNetworkHardenings</td>
          <td>Microsoft</td>
          <td>c3144593-9bae-448e-87dd-b2d3c47de571</td>
          <td>d89ad3b2-f7a7-4cff-b8a4-3f6fa58b4760</td>
          <td>...</td>
          <td>[\r\n  "1. Review the IP addresses and determi...</td>
          <td>{\r\n  "Destination Port": "22",\r\n  "Protoco...</td>
          <td>[\r\n  {\r\n    "$id": "4",\r\n    "ResourceId...</td>
          <td>Detection</td>
          <td></td>
          <td></td>
          <td>[\r\n  {\r\n    "DetailBladeInputs": "protecte...</td>
          <td>Azure Security Center</td>
          <td></td>
          <td>SecurityAlert</td>
        </tr>
        <tr>
          <th>4</th>
          <td>b1315f05-4a7a-45b4-811f-73e715f7c122</td>
          <td>2019-06-27 00:31:35</td>
          <td>Security incident with shared process detected</td>
          <td>Security incident with shared process detected</td>
          <td>High</td>
          <td>The incident which started on 2019-06-25 21:24...</td>
          <td>Detection</td>
          <td>Microsoft</td>
          <td>be88b671-2572-4373-af4a-323849b1da1d</td>
          <td>2518408029550429999_be88b671-2572-4373-af4a-32...</td>
          <td>...</td>
          <td>[\r\n  "1. Escalate the alert to the informati...</td>
          <td>{\r\n  "isincident": "true",\r\n  "Detected Ti...</td>
          <td>[\r\n  {\r\n    "$id": "4",\r\n    "DisplayNam...</td>
          <td>Detection</td>
          <td>3b701f84-d04b-4479-89b1-fa8827eb537e</td>
          <td>sentineltest</td>
          <td></td>
          <td>Azure Security Center</td>
          <td></td>
          <td>SecurityAlert</td>
        </tr>
      </tbody>
    </table>
    <p>5 rows × 29 columns</p>
    </div>


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


