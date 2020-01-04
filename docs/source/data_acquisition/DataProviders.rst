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
by calling QueryProvider(DATA_ENVIRONMENT). This will load the relavent
driver for connecting to the data environment we have selected as well
as provisioning a query store for us and adding queries from our default
query directory.

There are two other optional parameters we can pass when initializing
our Query Providers to further customize it: \* We can also chose to
initialize our Query Provider with a driver other than the defualt one
with QueryProvider(data_environment=DATA_ENVIRONMENT,
driver=QUERY_DRIVER) \* We can choose to import queries from a custom
query directory (see - `Creating a new set of queries <#new>`__ for more
details) with QueryProvider(data_environment=DATA_ENVIRONMENT,
driver=QUERY_DRIVER, query_path=QUERY_DIRECTORY_PATH).

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

    data_environments = QueryProvider.list_data_environments()
    print(data_environments)
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



.. raw:: html

    <html>
            <head>

            </head>
            <body>
            <div><p style='padding: 10px; color: #3a87ad; background-color: #d9edf7; border-color: #bce9f1'>Kqlmagic&nbsppackage&nbspis&nbspupdated&nbspfrequently.&nbspRun&nbsp&apos;!pip&nbspinstall&nbspKqlmagic&nbsp--no-cache-dir&nbsp--upgrade&apos;&nbspto&nbspuse&nbspthe&nbsplatest&nbspversion.<br>Kqlmagic&nbspversion:&nbsp0.1.100,&nbspsource:&nbsphttps://github.com/Microsoft/jupyter-Kqlmagic</p></div>
            </body>
            </html>



Connecting to a Data Environment
--------------------------------

Once we have instantiated the query
provider and loaded the relevent driver we can connect to the Data
Environment. This is done by calling the connect() function of the Query
Provider we just initialized and passing it a connection string to use.

For Log Analytics/Azure Sentinel the connection string is in the format
of loganalytics://code().tenant(“TENANT_ID”).workspace(“WORKSPACE_ID”).
Other Data Environments will have different connection string formats.

::

   connect(self, connection_str: str, **kwargs):

       Connect to data source.

       Parameters
       ----------
       connection_string : str
           Connection string for the data source

.. code:: ipython3

    ws_id = input('Workspace ID')
    ten_id = input('Tenant ID')
    la_connection_string = f'loganalytics://code().tenant("{ten_id}").workspace("{ws_id}")'
    qry_prov.connect(connection_str=f'{la_connection_string}')


.. parsed-literal::

    Workspace ID xxxxxxxxxxxxxxxxxxxxxxxxxxx
    Tenant ID xxxxxxxxxxxxxxxxxxxxxxxxxxx

Connecting to an OData Source
-----------------------------
:py:mod:`OData driver API documentation<msticpy.data.drivers.odata_driver>`

You can also connect to OData based data sources such as the MDATP API, 
or the Security Graph API. These connections often rely on having a 
dedicated Azure AD app for handling the authentication process. 

MDATP
~~~~~
:py:mod:`MDATP driver API documentation<msticpy.data.drivers.mdatp_driver>`

Details on registering an Azure AD application for MDATP can be found
`here <https://docs.microsoft.com/en-us/windows/security/threat-protection/microsoft-defender-atp/exposed-apis-create-app-webapp>`__.
Once you have registered the application you can use it to connect to
the MDATP API via the MDATP Data Environment. 

When connecting the required elements for connection can be passed in
a number of ways. The simpliest is to pass the required elements as 
kwargs. The required elements are:

* tenant_id -- The tenant ID of the MDATP workspace to connect to.
* client_id -- The ID of the application registered for MDATP.
* client_secret -- The secret used for by the application.

.. code:: ipython3

        ten_id = input('Tenant ID')
        client_id = input('Client ID')
        client_secret = input('Client Secret')
        mdatp_prov = QueryProvider('MDATP')
        mdatp_prov.connect(tenant_id=ten_id, client_id=client_id, client_secret=client_secret)

Alternatively you can store these details in the msticpyconfig.yaml 
file. Details should be included in the following format:

.. code:: yaml

      MDATPApp:
          Args:
            ClientId: "CLIENT ID"
            ClientSecret: "CLIENT SECRET"
            TenantId: "TENANT ID"

To use the stored variables when connecting pass app_name to the connect
function with the value passed being the heading used in msticpyconfig.yaml

.. code:: ipython3

        mdatp_prov = QueryProvider('MDATP')
        mdatp_prov.connect(app_name="MDATPApp")

For examples of using the MDATP connector see the sample MDATPQuery Notebook.

Security Graph API
~~~~~~~~~~~~~~~~~~
:py:mod:`Security Graph driver API documentation<msticpy.data.drivers.security_graph_driver>`

Connecting to the Security Graph API follows the same format as MDATP
connections with connection variables passed to the funciton in the
same way.

Details for registering an application for the Security Graph API can
be found `here <https://docs.microsoft.com/en-us/graph/auth-register-app-v2?context=graph%2Fapi%2F1.0&view=graph-rest-1.0>`__.

.. code:: ipython3

        mdatp_prov = QueryProvider('SecurityGraph')
        mdatp_prov.connect(app_name="SecurityGraphApp")


Listing available queries
-------------------------

Upon connecting to the relevant Data
Environment we need to look at what query options we have available to
us. In order to do this we can call QUERY_PROVIDER.list_queries(). This
will return a generator with the names of all the queries in our store.

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

    queries = qry_prov.list_queries()
    for query in queries:
        print(query)


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

To get further details on a specific query call:

qry_prov.{*query_group*}.{*query_name*}(‘?’) or

qry_prov.{*query_group*}.{*query_name*}(‘help’)

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
     {table} {query_project} | where {subscription_filter} | where TimeGenerated >= datetime({start}) | where TimeGenerated <= datetime({end}) | extend extendedProps = parse_json(ExtendedProperties) | extend CompromisedEntity = tostring(extendedProps["Compromised Host"]) | project-away extendedProps {add_query_items}




Running an pre-defined query
----------------------------

To run a query from our query store we
again call qry_prov.{*query_group*}.{*query_name*}(``**kwargs``) but this time
we simply pass required parameters for that query as key word arguments.

This will return a Pandas DataFrame of the results with the columns
determined by the query parameters. Should the query fail for some
reason an exception will be raised.

.. code:: ipython3

    alerts = qry_prov.SecurityAlert.list_alerts(start='2019-07-21 23:43:18.274492', end='2019-07-27 23:43:18.274492')
    alerts.head()



.. parsed-literal::

    <IPython.core.display.Javascript object>



.. parsed-literal::

    <IPython.core.display.Javascript object>




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



It is also possible to pass queries objects as arguments before defining
keywork arguments. For example if I wanted to define query times as an
object rather than defining a start and end via keywork arguments I
could simply pass a querytimes object to the pre-defined query.

.. code:: ipython3

    query_times = mas.nbwidgets.QueryTime(units='day',
                                max_before=40, max_after=1, before=5)
    query_times.display()



.. parsed-literal::

    HTML(value='<h4>Set query time boundaries</h4>')



.. parsed-literal::

    HBox(children=(DatePicker(value=datetime.date(2019, 7, 26), description='Origin Date'), Text(value='23:43:18.2…



.. parsed-literal::

    VBox(children=(IntRangeSlider(value=(-5, 1), description='Time Range (day):', layout=Layout(width='80%'), max=…


.. code:: ipython3

    qry_prov.SecurityAlert.list_alerts(query_times)



.. parsed-literal::

    <IPython.core.display.Javascript object>



.. parsed-literal::

    <IPython.core.display.Javascript object>




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



Running an ad-hoc query
-----------------------


It is also possible to run ad-hoc queries
via a similar method. Rather than calling a named query from the Query
Provider query store, we can pass a query directly to our Query Provider
with QUERY_PROVIDER.exec_query(query=QUERY_STRING). This will execute
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



.. parsed-literal::

    <IPython.core.display.Javascript object>



.. parsed-literal::

    <IPython.core.display.Javascript object>




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



Creating a new set of queries
-----------------------------

msticpy provides a number of
pre-defined queries to call with using the data package. You can also
add in additional queries to be imported and used by your Query
Provider, these are defined in YAML format files and examples of these
files can be found at the msticpy GitHub site
https://github.com/microsoft/msticpy/tree/master/msticpy/data/queries.

The required structure of these query definition files is as follows: -
metadata - version: The version number of the definition file -
description: A description of the purpose of this collection of query
definitions - data_environments[]: A list of the Data Environments that
the defined queries can be run against (1 or more) - data_families[]: A
list of Data Families the defined queries related to, these families are
defined as part of misticpy.nbtools.query_defns - tags[]: A list of tags
to help manage definition files - defaults: A set of defaults that apply
to all queries in the file - metadata: Metadata regarding a query -
data_source: The data source to be used for the query - parameters:
Parameters to be passed to the query - name: The parameter name -
description: A description of what the parameter is - type: The data
type of the parameter - default: The default value for that parameter -
sources: a set of queries - name: The name of the query -description: A
description of the query’s function -metadata: Any metadata associated
with the query -args: The arguments of the query -query: The query to be
executed -uri: A URI associated with the query -parameters: Any
parameters required by the query not covered by defaults - name: The
parameter name - description: A description of what the parameter is -
type: The data type of the parameter - default: The default value for
that parameter

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
QUERY_PROVIDER.import_query_file(query_file=PATH_TO_QUERY_FILE) This
will load the query file into the Query Provider’s Query Store from
where it can be called.

.. code:: ipython3

    qry_prov.import_query_file(query_file='C:\queries\example.yaml')

Once imported the queries in the files appear in the Query Provider’s
Query Store alongside the others and can be called in the same manner as
pre-defined queries.

If you have created a large number of query definition files and you
want to have the automatically imported into a Query Provider’s query
store at initialization you can specify a directory containing these
queries in the msticpyconfig.yaml file under QueryDefinitions: Custom:

For example if I have a folder at C:\queries I will set the
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
     {table} | where {subscription_filter} | where TimeGenerated >= datetime({start}) | where TimeGenerated <= datetime({end}) | where Computer == "{host_name}" | take 5


.. code:: ipython3

    qry_prov.LinuxSyslog.syslog_example(start='2019-07-21 23:43:18.274492', end='2019-07-27 23:43:18.274492', host_name='UbuntuDevEnv')



.. parsed-literal::

    <IPython.core.display.Javascript object>



.. parsed-literal::

    <IPython.core.display.Javascript object>




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



If you are having difficulties with a defined query and it is not
producing the expected results it can be useful to see the raw query
exactly as it is passed to the Data Environment. If you call a query
with ‘print’ and the parameters required by that query it will construct
and print out the query string to be run.

.. code:: ipython3

    qry_prov.LinuxSyslog.syslog_example('print', start='2019-07-21 23:43:18.274492', end='2019-07-27 23:43:18.274492', host_name='UbuntuDevEnv')




.. parsed-literal::

    ' Syslog | where true | where TimeGenerated >= datetime(2019-07-21 23:43:18.274492) | where TimeGenerated <= datetime(2019-07-27 23:43:18.274492) | where Computer == "UbuntuDevEnv" | take 5'


