Resource Graph Explorer Data Provider
=====================================

Description
-----------

This data provider allows for a connection to the `Azure Resource
Graph <https://docs.microsoft.com/en-us/azure/governance/resource-graph/overview>`__
and a way to query against the `Azure Resource Graph
Explorer <https://docs.microsoft.com/en-us/azure/governance/resource-graph/first-query-portal>`__.
The data connector functions in the same way as other data connectors
and uses the Kusto Query Language (KQL) and has with some subtle
differences to other connectors in they way that authentication is
handled.

You would use this data connector to flexibly and quickly get details on
deployed Azure resources within a subscription. It allows for bulk
queries on various aspects of resources and returns data in a very
structured format. This makes it much more effective and efficient than
getting resource specific details via the resource API.

More details about data providers in MSTICPy can be found in `the
documentation <https://msticpy.readthedocs.io/en/latest/data_acquisition/DataProviders.html>`__

Installation
~~~~~~~~~~~~

Installation of this data connector requires that MSTICPy be installed
with the Azure extras: ``pip install msticpy['azure']``

For more details on MSTICPy installation please refer to [the documentation](https://msticpy.readthedocs.io/en/latest/getting_started/Installing.html).

Initialization
~~~~~~~~~~~~~~

The provider for the Azure Resource Graph is named ``ResourceGraph``

.. code:: ipython3

    from msticpy.data.data_providers import QueryProvider
    qry_prov = QueryProvider("ResourceGraph")

Authentication
~~~~~~~~~~~~~~

Once initialized the first step in using the data provider is to
authenticate. The Resource Graph provider uses MSTICPy’s `Azure
authentication
features <https://msticpy.readthedocs.io/en/latest/data_acquisition/AzureData.html?highlight=azure#instantiating-and-connecting-with-an-azure-data-connector>`__
and you can provide a set of authentication methods when connecting. By
default the provider will attempt to authenticate using credentials
stored in msticpyconfig.yaml (or as environment variables) and an Azure
CLI connection but this can be customized with the ‘auth_methods’
keyword.

If storing details in msticpyconfig.yaml they must be under the
``AzureCLI`` DataProviders section - for more details see `this
documentation <https://msticpy.readthedocs.io/en/latest/getting_started/msticpyconfig.html>`__.

Once successfully connected you will be presented with a “Connected”
message.

.. code:: ipython3

    qry_prov.connect(auth_methods=["cli"])


.. parsed-literal::

    Connected


Listing available queries
-------------------------

As with other data providers there are a number of built-in queries with
this provider. Once connected you can view the available queries with
``QUERY_PROVIDER.list_queries()``.

Alternatively you can view query details in an interactive widget with
``QUERY_PROVIDER.browse_queries()``

For more information, refer documentation : `Listing available
queries <https://msticpy.readthedocs.io/en/latest/data_acquisition/DataProviders.html#listing-available-queries>`__.

.. code:: ipython3

    qry_prov.list_queries()




.. parsed-literal::

    ['ResourceGraph.list_detailed_virtual_machines',
     'ResourceGraph.list_public_ips',
     'ResourceGraph.list_resources',
     'ResourceGraph.list_resources_by_api_version',
     'ResourceGraph.list_resources_by_type',
     'ResourceGraph.list_virtual_machines']



Running pre-defined query
-------------------------

In order to run pre-defined query, execute with the query name,
e.g. ``QUERY_PROVIDER.ResoruceGraph.QUERY_NAME()``. You can pass
parameters to these queries to customize them, however they will also
run with default parameters if none as provider. The query browser will
provide details as to what parameters are avaliable with each query.

As with other data providers data is returned to you in a Pandas
DataFrame.

For more information , refer documentation - `Running an pre-defined
query <https://msticpy.readthedocs.io/en/latest/data_acquisition/DataProviders.html#running-an-pre-defined-query>`__

.. code:: ipython3

    qry_prov.ResourceGraph.list_resources_by_api_version()


==================================================  ==================
type                                                apiVersion
==================================================  ==================
microsoft.alertsmanagement/actionrules              2019-05-05-preview
microsoft.alertsmanagement/smartdetectoralertrules  2021-04-01
microsoft.apimanagement/service                     2019-12-01
microsoft.automanage/accounts                       2020-06-30-preview
microsoft.automation/automationaccounts             2018-06-30
==================================================  ==================


Running an ad-hoc query
~~~~~~~~~~~~~~~~~~~~~~~

You can also define a your own KQL query for the Resource Graph and run
with ``QUERY_PROVIDER.exec_query(QUERY)``

For more information, see the documentation on `Running an Ad-hoc
Query <https://msticpy.readthedocs.io/en/latest/data_acquisition/DataProviders.html#running-an-ad-hoc-query>`__

.. code:: ipython3

    query = "Resources | where type =~ 'Microsoft.Compute/virtualMachines' | take 3"
    qry_prov.exec_query(query)


===========================================================================================================================================  =============  =================================  ====================================  ======  ==========  ===============  ====================================  ===========  =====  ======  =======
id                                                                                                                                           name           type                               tenantId                              kind    location    resourceGroup    subscriptionId                        managedBy    sku    plan    zones
===========================================================================================================================================  =============  =================================  ====================================  ======  ==========  ===============  ====================================  ===========  =====  ======  =======
/subscriptions/1d228542-43d3-43fa-b9f6-e2a5e3d69d47/resourceGroups/LinuxTestLab/providers/Microsoft.Compute/virtualMachines/RHEL77Base       RHEL77Base     microsoft.compute/virtualmachines  1d228542-43d3-43fa-b9f6-e2a5e3d69d47          eastus2     linuxtestlab     1d228542-43d3-43fa-b9f6-e2a5e3d69d47
/subscriptions/1d228542-43d3-43fa-b9f6-e2a5e3d69d47/resourceGroups/LinuxTestLab/providers/Microsoft.Compute/virtualMachines/Ubuntu18ASC      Ubuntu18ASC    microsoft.compute/virtualmachines  1d228542-43d3-43fa-b9f6-e2a5e3d69d47          eastus2     linuxtestlab     1d228542-43d3-43fa-b9f6-e2a5e3d69d47
/subscriptions/1d228542-43d3-43fa-b9f6-e2a5e3d69d47/resourceGroups/monster-island/providers/Microsoft.Compute/virtualMachines/GodzillaTron1  GodzillaTron1  microsoft.compute/virtualmachines  1d228542-43d3-43fa-b9f6-e2a5e3d69d47          japanwest   monster-island   1d228542-43d3-43fa-b9f6-e2a5e3d69d47
===========================================================================================================================================  =============  =================================  ====================================  ======  ==========  ===============  ====================================  ===========  =====  ======  =======


.. container:: alert alert-block alert-info

   Note: Resource Graph queries are limited to 1000 rows of output each.
   If your query returns 1000 rows it is likely it has hit this limit,
   consider re-writing the query to return a smaller subset of data.
   This applies to both built in queries and ad-hoc queries.

End-to-end Example
------------------

In this example we want to take a look at all of the virtual machines we
have in our environment and they get specific details including public
IP on one of them:

.. code:: ipython3

    from msticpy.data.data_providers import QueryProvider
    # Initialize and connect to provider
    qry_prov = QueryProvider("ResourceGraph")
    qry_prov.connect()


.. parsed-literal::

    Connected


.. code:: ipython3

    # Get list of VMs and see how many we have
    vms = qry_prov.ResourceGraph.list_virtual_machines()
    print(f"Number of VMs found : {len(vms.index)}")
    # Filter the query to get a smaller dataset
    vms = qry_prov.ResourceGraph.list_virtual_machines(add_query_items="| where resourceGroup contains 'msticpy'")
    display(vms)
    # Set hostname for our next query
    hostname = vms.iloc[0]['name']

    Number of VMs found : 421

=================================================================================================================================  ==========  =================================  ====================================  ======  ==========  ===============  ====================================  ===========  =====  ======  =======
id                                                                                                                                 name        type                               tenantId                              kind    location    resourceGroup    subscriptionId                        managedBy    sku    plan    zones
=================================================================================================================================  ==========  =================================  ====================================  ======  ==========  ===============  ====================================  ===========  =====  ======  =======
/subscriptions/1d228542-43d3-43fa-b9f6-e2a5e3d69d47/resourceGroups/MSTICpy/providers/Microsoft.Compute/virtualMachines/MSTIC-DSVM  MSTIC-DSVM  microsoft.compute/virtualmachines  1d228542-43d3-43fa-b9f6-e2a5e3d69d47          eastus      msticpy          1d228542-43d3-43fa-b9f6-e2a5e3d69d47                              ['1']
=================================================================================================================================  ==========  =================================  ====================================  ======  ==========  ===============  ====================================  ===========  =====  ======  =======


Now we can get details on the specific VM using its hostname.

.. code:: ipython3

    qry_prov.ResourceGraph.list_detailed_virtual_machines(host_name=hostname)


=================================================================================================================================  ==========  ============  ======================================================================================================================================  ======================================================================================================================================  =================
vmId                                                                                                                               vmName      vmSize        nicId                                                                                                                                   publicIpId                                                                                                                              publicIpAddress
=================================================================================================================================  ==========  ============  ======================================================================================================================================  ======================================================================================================================================  =================
/subscriptions/1d228542-43d3-43fa-b9f6-e2a5e3d69d47/resourceGroups/MSTICpy/providers/Microsoft.Compute/virtualMachines/MSTIC-DSVM  MSTIC-DSVM  Standard_B2s  /subscriptions/1d228542-43d3-43fa-b9f6-e2a5e3d69d47/resourceGroups/MSTICpy/providers/Microsoft.Network/networkInterfaces/mstic-dsvm832  /subscriptions/40dcc8bf-0478-4f3b-b275-ed0a94f2c013/resourceGroups/MSTICpy/providers/Microsoft.Network/publicIPAddresses/MSTIC-DSVM-ip  11.11.11.111
=================================================================================================================================  ==========  ============  ======================================================================================================================================  ======================================================================================================================================  =================


References
----------

-  Azure Resource Graph:
   https://docs.microsoft.com/en-us/azure/governance/resource-graph/overview
-  Resource Graph Query Language:
   https://docs.microsoft.com/en-us/azure/governance/resource-graph/concepts/query-language
