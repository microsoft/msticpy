Microsoft Sentinel Provider - New Implementation
================================================

This is a new implementation of the MS Sentinel QueryProvider using
the
`azure-monitor-query SDK <https://learn.microsoft.com/python/api/overview/azure/monitor-query-readme?view=azure-python>`__
(the earlier implementation used
`Kqlmagic <https://github.com/microsoft/jupyter-Kqlmagic>`__)

.. note:: This provider currently in beta and is available for testing.
   It is available alongside the existing Sentinel provider for you
   to compare old and new.
   If you are using the existing implementation, see :doc:`./DataProv-MSSentinel`

Changes from the previous implementation
----------------------------------------

* Use the provider name ``MSSentinel_New`` when creating a QueryProvider
  instance.
* By default, it uses the *MSTICPy* built-in Azure authentication by
  default - you do not have to specify parameters to enable this.
* Supports simultaneous queries against multiple workspaces (see below).
* Supports user-specified timeout for queries.
* Supports proxies (via MSTICPy config or the ``proxies`` parameter to
  the ``connect`` method)
* Some of the previous parameters have been deprecated:

  * ``mp_az_auth`` is replaced by ``auth_types`` (the former still works
    but will be removed in a future release).
  * ``mp_az_auth_tenant_id`` is replaced by ``tenant_id`` (the former
    is no longer supported


Sentinel Configuration
----------------------

You store configuration for your workspace (or workspaces) in
your ``msticpyconfig.yaml``.

For more information on using and configuring *msticpyconfig.yaml* see
:doc:`msticpy Package Configuration <../getting_started/msticpyconfig>`
and :doc:`MSTICPy Settings Editor<../getting_started/SettingsEditor>`

The MS Sentinel connection settings are stored in the
``AzureSentinel\\Workspaces`` section of the file.
Here is an example.

.. code:: yaml

    AzureSentinel:
      Workspaces:
        # Workspace used if you don't explicitly name a workspace when creating WorkspaceConfig
        # Specifying values here overrides config.json settings unless you explicitly load
        # WorkspaceConfig with config_file parameter (WorkspaceConfig(config_file="../config.json")
        Default:
          WorkspaceId: 271f17d3-5457-4237-9131-ae98a6f55c37
          TenantId: 335b56ab-67a2-4118-ac14-6eb454f350af
          ResourceGroup: soc
          SubscriptionId: a5b24e23-a96a-4472-b729-9e5310c83e20
          WorkspaceName: Workspace1
        # To use these launch with an explicit name - WorkspaceConfig(workspace_name="Workspace2")
        Workspace1:
          WorkspaceId: "c88dd3c2-d657-4eb3-b913-58d58d811a41"
          TenantId: "335b56ab-67a2-4118-ac14-6eb454f350af"
          ResourceGroup: soc
          SubscriptionId: a5b24e23-a96a-4472-b729-9e5310c83e20
          WorkspaceName: Workspace1
        TestWorkspace:
          WorkspaceId: "17e64332-19c9-472e-afd7-3629f299300c"
          TenantId: "4ea41beb-4546-4fba-890b-55553ce6003a"
          ResourceGroup: soc
          SubscriptionId: a5b24e23-a96a-4472-b729-9e5310c83e20
          WorkspaceName: Workspace2

If you only use a single workspace, you only need to create a ``Default`` entry and
add the values for your *WorkspaceID* and *TenantID*. You can add other entries here,
for example, SubscriptionID, ResourceGroup. These are not required for the data
queries but are recommended since they are used by other *MSTICPy* components.

If you use multiple workspaces, you can add further entries here. The key for
each entry (e.g. ``Workspace1`` or ``TestWorkspace`` in the example above)
is normally the name of the Azure Sentinel workspace but
you can use any name you prefer. You use this entry name when connecting
to a workspace.


Loading a QueryProvider for Microsoft Sentinel
----------------------------------------------

.. code:: ipython3

    qry_prov = QueryProvider(
        data_environment="MSSentinel_New",
    )

    # or just
    qry_prov = QueryProvider("MSSentinel_New")

Optional parameters for the Sentinel QueryProvider
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``timeout`` : int (seconds)

Specify a timeout for queries. Default is 300 seconds.
This parameter can be set here or in the ``connect`` method
and overridden for individual queries.

``proxies`` : Dict[str, str]

Proxy settings for log analytics queries.
If proxies are configured in *msticpyconfig.yaml* this is used by default.
If specified as a parameter, specify proxies as a dictionary of the form
``{protocol: proxy_url}``

The only protocol used by the driver is "https" (other protocols
can be set in *msticpyconfig.yaml* but only https is used here).
The proxy_url can contain
optional authentication information in the format
"https://username:password@proxy_host:port"

If you have a proxy configuration set in *msticpyconfig.yaml* and
you do not want to use it, set ``proxies`` to None or an empty dictionary.
This parameter can be overridden in connect method.

Connecting to a MS Sentinel Workspace
-------------------------------------

Once you've created a QueryProvider you need to authenticate to Sentinel
Workspace. This is done by calling the connect() function of the Query
Provider. See :py:meth:`connect() <msticpy.data.drivers.azure_monitor_driver.AzureMonitorDriver.connect>`

This function takes an initial parameter (called ``connection_str`` for
historical reasons) that can be one of the following:

* A WorkspaceConfig instance
* A connection string (this is option is being deprecated)
* None - in this case it will connect with the ``Default`` entry from
  your *msticpyconfig.yaml* file.

If you omit this parameter you use the ``workspace`` parameter
to specify the workspace entry from ``msticpyconfig.yaml`` to use.


Connecting to a Sentinel workspace
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When connecting you can just pass the name of your workspace or
an instance of WorkspaceConfig to the query provider's ``connect`` method.

.. code:: IPython

    qry_prov.connect(workspace="Default")
    qry_prov.connect(workspace="MyOtherWorkspace")

    # or, passing WorkspaceConfig
    qry_prov.connect(WorkspaceConfig())
    # or
    qry_prov.connect(WorkspaceConfig(workspace="MyOtherWorkspace"))



MS Sentinel Authentication options
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

By default, the data provider will use Azure authentication
following the parameters defined in your ``msticpyconfig.yaml`` file
(or the default values if you have not configured them in this file).

To read more about Azure authentication see
:doc:`Azure Authentication <../getting_started/AzureAuthentication>`

You can override several authentication parameters including:

* auth_types - a list of authentication types to try in order
* tenant_id - the Azure tenant ID to use for authentication

If you are using a Sovereign cloud rather than the Azure global cloud,
you should follow the guidance in :doc:`Azure Authentication <../getting_started/AzureAuthentication>`
to configure the correct cloud.


Connecting to multiple Sentinel workspaces
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The Sentinel data provider supports connecting to multiple workspaces.
You can pass a list of workspace names or workspace IDs to the ``connect`` method.
using the ``workspaces`` or ``workspace_ids`` parameters respectively.

``workspace_ids`` should be a list or tuple of workspace IDs.

``workspaces`` should be a list or tuple of workspace names. In order
to use this parameter you must have these workspaces configured in
your *msticpyconfig.yaml*.

These parameters override the ``workspace`` parameter.

Connecting to multiple workspaces allows you to run queries across these
workspaces and return the combined results as a single Pandas DataFrame.
The workspaces must use common authentication credentials and are
expected to have the same data schema.

.. code:: ipython3

    qry_prov.connect(workspaces=["Default", "MyOtherWorkspace"])

    qry_prov.SecurityAlert.list_alerts()

This will return a DataFrame containing the results of the query,
the results from each workspace will be indicated by the
``TenantId`` column, which will contain the workspace ID of
each workspace.

.. note:: This is a mechanism implemented by the underlying
  **azure-monitor-query**
  client library. It is independent of the MSTICPy capability to
  add multiple connections to a query provider (and run parallel
  queries against each workspace). You can use either of these
  but we recommended using
  one or the other and not both simultaneously.

.. warning:: Connecting to multiple workspaces like this means
  that the ``schema`` property will not return anything. This
  only works if you connect to a single workspace. In this case,
  it will return the schema of this workspace.


Other parameters for Sentinel ``connect()`` method
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For ``timeout`` and ``proxies`` see the section above.

After connecting to
The WorkspaceConfig class
-------------------------

You do not need to know the details of this class but it is used
behind the scenes to provide workspace configuration information
to the Sentinel data provider.

``WorkspaceConfig`` handles loading your workspace configuration
and generating a connection string from your configuration.
See :py:mod:`WorkspaceConfig API documentation<msticpy.common.wsconfig>`

``WorkspaceConfig`` works with workspace configuration stored in *msticpyconfig.yaml*.

To use ``WorkspaceConfig``, simple create an instance of it. It will automatically build
your connection string for use with the query provider library.

.. code:: python3

    ws_config = WorkspaceConfig()

When called without parameters, *WorkspaceConfig* loads the "Default"
entry in your *msticpyconfig.yaml*. To specify a different workspace pass the ``workspace`` parameter
with the name of your workspace entry. This value is the name of
the section in the ``msticpyconfig.yaml`` ``Workspaces`` section.

.. note:: the ``workspace`` parameter value is the entry heading in
  your ``msticpyconfig.yaml``. As mentioned above, this may
  not necessarily be the same as your workspace name.

.. code:: python3

    ws_config = WorkspaceConfig(workspace="TestWorkspace")


To see which workspaces are configured in your *msticpyconfig.yaml* use
the ``list_workspaces()`` function.

.. tip:: ``list_workspaces`` is a class function, so you do not need to
   instantiate a WorkspaceConfig to call this function.

.. code:: python3

    WorkspaceConfig.list_workspaces()

.. parsed-literal::

    {'Default': {'WorkspaceId': '271f17d3-5457-4237-9131-ae98a6f55c37',
      'TenantId': '335b56ab-67a2-4118-ac14-6eb454f350af'},
     'Workspace1': {'WorkspaceId': 'c88dd3c2-d657-4eb3-b913-58d58d811a41',
       'TenantId': '335b56ab-67a2-4118-ac14-6eb454f350af'},
     'TestWorkspace': {'WorkspaceId': '17e64332-19c9-472e-afd7-3629f299300c',
       'TenantId': '4ea41beb-4546-4fba-890b-55553ce6003a'}}


Other MS Sentinel Documentation
-------------------------------

Built-in :ref:`data_acquisition/DataQueries:Queries for Microsoft Sentinel`.

See also: :py:mod:`Sentinel KQL driver API documentation <msticpy.data.drivers.azure_kusto_driver>`
