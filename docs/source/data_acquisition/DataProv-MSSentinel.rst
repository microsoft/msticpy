Microsoft Sentinel Provider
===========================

Sentinel Configuration
----------------------

You can store configuration for your workspace (or workspaces) in either
your ``msticpyconfig.yaml`` or a ``config.json`` file. The latter
file is auto-created in your Azure Machine Learning (AML) workspace when
you launch a notebook from the Sentinel portal. It can however, only
store details for a single workspace.

Sentinel Configuration in *msticpyconfig.yaml*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is the simplest place to store your workspace details.

You likely need to use a *msticpyconfig.yaml* anyway. If you are using other
*msticpy* features such as Threat Intelligence Providers, GeoIP Lookup, Azure Data,
etc., these all have their own configuration settings, so using a single
configuration file makes managing your settings easier. If you are running
notebooks in an AML workspace and you do not have a *msticpyconfig.yaml*
*MSTICPy* will create one and import settings from a *config.json*, if it can find
one.

For more information on using and configuring *msticpyconfig.yaml* see
:doc:`msticpy Package Configuration <../getting_started/msticpyconfig>`
and :doc:`MSTICPy Settings Editor<../getting_started/SettingsEditor>`

The MS Sentinel connection settings are stored in the
``AzureSentinel\\Workspaces`` section of the file. Here is an example.

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
for example, SubscriptionID, ResourceGroup. These are recommended but not required
for the QueryProvider (they may be used by other *MSTICPy* components however).

.. note:: The property names are spelled differently to the values in the
   *config.json* so be sure to enter these as shown in the example. These
   names are case-sensitive.

.. note:: The section names (Default, Workspace1 and TestWorkspace) do
   not have to be the same as the workspace name - you can choose friendlier
   aliases, if you wish.

If you use multiple workspaces, you can add further entries here. Each
workspace entry is normally the name of the Azure Sentinel workspace but
you can use any name you prefer.

Sentinel Configuration in *config.json*
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When you load a notebook from the MS Sentinel UI a configuration file *config.json*
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

If no *msticpyconfig.yaml* is found *MSTICPy* will automatically look for a
*config.json* file in the current
directory. If not found here, it will search the parent directory and in all
its subdirectories. It will use the first *config.json* file found.


Loading a QueryProvider for Microsoft Sentinel
----------------------------------------------

.. code:: ipython3

    qry_prov = QueryProvider(
        data_environment="MSSentinel",
    )

.. note::"LogAnalytics" and "AzureSentinel" are also aliases
   for "MSSentinel"


Connecting to a MS Sentinel Workspace
-------------------------------------

Once we have instantiated the QueryProvider we need to authenticate to Sentinel
Workspace. This is done by calling the connect() function of the Query
Provider.

connect() requires a connection string as its parameter. For MS Sentinel
we can use the ``WorkspaceConfig`` class.

WorkspaceConfig
~~~~~~~~~~~~~~~

This handles loading your workspace configuration and generating a
connection string from your configuration.
See :py:mod:`WorkspaceConfig API documentation<msticpy.common.wsconfig>`

``WorkspaceConfig``  works with workspace configuration stored in *msticpyconfig.yaml*
or *config.json* (although the former takes precedence).

To use ``WorkspaceConfig``, simple create an instance of it. It will automatically build
your connection string for use with the query provider library.

.. code:: IPython

    >>> ws_config = WorkspaceConfig()
    >>> ws_config.code_connect_str

    "loganalytics://code().tenant('335b56ab-67a2-4118-ac14-6eb454f350af').workspace('271f17d3-5457-4237-9131-ae98a6f55c37')"

You can use this connection string in the call to ``QueryProvider.connect()``

When called without parameters, *WorkspaceConfig* loads the "Default"
entry in your *msticpyconfig.yaml* (or falls back to loading the settings
in *config.json*). To specify a different workspace pass the ``workspace`` parameter
with the name of your workspace entry. This value is the name of
the section in the *msticpyconfig* ``Workspaces`` section, which may
not necessarily be the same as your workspace name.

.. code:: IPython

    >>> ws_config = WorkspaceConfig(workspace="TestWorkspace")


To see which workspaces are configured in your *msticpyconfig.yaml* use
the ``list_workspaces()`` function.

.. tip:: ``list_workspaces`` is a class function, so you do not need to
   instantiate a WorkspaceConfig to call this function.

.. code:: IPython

    >>> WorkspaceConfig.list_workspaces()

    {'Default': {'WorkspaceId': '271f17d3-5457-4237-9131-ae98a6f55c37',
      'TenantId': '335b56ab-67a2-4118-ac14-6eb454f350af'},
     'Workspace1': {'WorkspaceId': 'c88dd3c2-d657-4eb3-b913-58d58d811a41',
       'TenantId': '335b56ab-67a2-4118-ac14-6eb454f350af'},
     'TestWorkspace': {'WorkspaceId': '17e64332-19c9-472e-afd7-3629f299300c',
       'TenantId': '4ea41beb-4546-4fba-890b-55553ce6003a'}}

Entries in msticpyconfig always take precedence over settings in your
config.json. If you want to force use of the config.json, specify the path
to the config.json file in the ``config_file`` parameter to ``WorkspaceConfig``.

Connecting to the workspace
~~~~~~~~~~~~~~~~~~~~~~~~~~~

When connecting you can just pass an instance of WorkspaceConfig to
the query provider's ``connect`` method.

.. code:: IPython

    qry_prov.connect(WorkspaceConfig())
    # or
    qry_prov.connect(WorkspaceConfig(workspace="TestWorkspace"))

If you need use a specific instance of a config.json you can specify a full
path to the file you want to use when you create your ``WorkspaceConfig``
instance.


MS Sentinel Authentication options
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

By default, the data provider tries to use chained authentication,
attempting to use existing Azure credentials, if they are available.

- If you are running in an AML workspace, it will attempt to use
  integrated MSI authentication, using the identity that you used to
  authenticate to AML.
- If you have logged in to Azure CLI, the Sentinel provider will
  try to use your AzureCLI credentials
- If you have your credentials stored as environment variables, it
  will try to use those
- Finally, it will fall back on using interactive browser-based
  device authentication.

If you are using a Sovereign cloud rather than the Azure global cloud,
you should select the appropriate cloud in the Azure section of
the *msticpyconfig*.

.. warning:: Although msticpy allows you to configure multiple entries for
   workspaces in different tenants, you cannot currently authenticate to workspaces
   that span multiple tenants in the same notebook. If you need to do this, you
   should investigate
   `Azure Lighthouse <https://azure.microsoft.com/services/azure-lighthouse/>`__.
   This allows delegated access to workspaces in multiple tenants from a single
   tenant.

Other MS Sentinel Documentation
-------------------------------

For examples of using the MS Defender provider, see the sample
`M365 Defender Notebook<https://github.com/microsoft/msticpy/blob/master/docs/notebooks/Data_Queries.ipynb>`

Built-in :ref:`data_acquisition/DataQueries:Queries for Microsoft Sentinel`.

:py:mod:`Sentinel KQL driver API documentation<msticpy.data.drivers.kql_driver>`
