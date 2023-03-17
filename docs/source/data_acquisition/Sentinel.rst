Microsoft Sentinel APIs
=======================

.. toctree::
   :maxdepth: 2

   SentinelAnalytics
   SentinelBookmarks
   SentinelDynamicSummaries
   SentinelIncidents
   SentinelWatchlists
   SentinelSearch
   SentinelWorkspaces
   SentinelTI

Description
-----------

This package contains functionality making calls to Microsoft Sentinel directly.
These can be used to get data from Microsoft Sentinel, as well as perform
configuration and other actions on the Microsoft Sentinel Workspace

:py:mod:`Microsoft Sentinel API documentation<msticpy.context.azure.sentinel_core.MicrosoftSentinel>`


Instantiating and Connecting the Microsoft Sentinel API Connector
-----------------------------------------------------------------

See :py:class:`Microsoft Sentinel <msticpy.context.azure.sentinel_core.MicrosoftSentinel>`

When instantiating the class you need to pass it details of the Sentinel workspace
you want to interact with. This can be provided as a complete resource ID,
e.g.

.. code:: ipython3

    MicrosoftSentinel(res_id="subscriptions/fdee8146-8bcf-460f-86f3-3f788c285efd/resourceGroups/myRG/providers/Microsoft.OperationalInsights/workspaces/myWorkspace")

Alternatively you can provide the Subscription ID, Resource Group name and Workspace name
seperately and let the class build the resource ID for you.
Alternatively you can provide the Subscription ID, Resource Group name and Workspace name.
e.g.

.. code:: ipython3

    MicrosoftSentinel(sub_id="fdee8146-8bcf-460f-86f3-3f788c285efd"
        res_grp="myRG",
        ws_name="myWorkspace")

If no workspace details are provided it will attempt to use default workspace details from
your config file.

You can also specify a specific Azure Cloud if your workspace isn't in Azure's public cloud.

In order to connect to the Microsoft Sentinel API and retrieve the required data
we need to instantiate the MicrosoftSentinel class and authenticate to Azure.
Authentication to the Microsoft Sentinel API is handled via an the azure_auth package.

By default ``['cli', 'msi', 'devicecode']`` is used but you can provide an alternative
list to ``.connect`` via the auth_methods parameter.

.. code:: ipython3

        sentinel = MicrosoftSentinel()
        sentinel.connect(auth_methods=['cli','interactive'])


For more details see :doc:`../getting_started/AzureAuthentication`
and :ref:`getting_started/SettingsEditor:Azure Cloud and Authentication Settings`.

Get Microsoft Sentinel Workspaces
---------------------------------

See :py:meth:`list_sentinel_workspaces <msticpy.context.azure.sentinel_core.MicrosoftSentinel.get_sentinel_workspaces>`

If you want to see Sentinel workspace in a subscription you can call `list_sentinel_workspaces`.

``list_sentinel_workspaces`` returns a list of Microsoft Sentinel workspaces within
a specified subscription.

.. note:: this will only return workspaces that the authenticated account
   is permitted to view.

.. code:: ipython3

    sentinel.get_sentinel_workspaces(sub_id="3b701f84-d04b-4479-89b1-fa8827eb537e")

Incidents
---------

It is possible to get details of, update, and create Incidents via the MicrosoftSentinel connector.
Details on working with Incidents can be found in :doc:`Sentinel Incidents <SentinelIncidents>`

Hunting Queries
---------------

You can return a dataframe detailing all hunting queries configured in the workspace. This allows for
analysis and configuration of hunting queries, as well as the ability to take a
hunting query and run it with a QueryProvider.

See :py:meth:`list_hunting_queries <msticpy.context.azure.sentinel_core.MicrosoftSentinel.list_hunting_queries>`

.. code:: ipython3

    sentinel.list_hunting_queries()

Analytics
---------

You can interact with Analytics and Analytic templates, including creating new analytics via this feature.
More details can be found in :doc:`Sentinel Analytics <SentinelAnalytics>`


Bookmarks
---------

You can interact with Bookmarks, including creating new bookmarks and deleting existing bookmarks via this feature.
More details can be found in :doc:`Sentinel Bookmarks <SentinelBookmarks>`

Watchlists
----------

You can interact with Watchlists, including creating new watchlists and adding items to a watchlist via this feature.
More details can be found in :doc:`Sentinel Watchlists <SentinelWatchlists>`

Search
------

You can create and delete Microsoft Sentinel Search jobs via MSTICPy :doc:`Sentinel Search <SentinelSearch>`