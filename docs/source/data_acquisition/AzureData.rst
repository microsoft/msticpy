Azure Data Enrichment
=====================

Description
-----------

This package contains functionality for enriching data regarding Azure
host details with additional host details exposed via the Azure API.
By providing an Azure Resource or Azure Subscription the package returns
key contextual information regarding the Subscription or Resource.
This package is primarily aimed at Azure IaaS resources but will work
with any Azure Resource type.
This feature is currently a work in progress and additional data
enrichment capabilities will be added over time.

:py:mod:`Azure Data API documentation<msticpy.data.azure_data>`

The first step in using this package is to install the msticpy package.

.. code:: ipython3

    !pip install msticpy --upgrade --user


.. parsed-literal::

    Collecting msticpy
    Building wheels for collected packages: msticpy
      Building wheel for msticpy (setup.py): started
      Building wheel for msticpy (setup.py): finished with status 'done'
    Successfully built msticpy
    Installing collected packages: msticpy
    Successfully installed msticpy-0.3.0


.. code:: ipython3

    #Check we are running Python 3.6
    import sys
    MIN_REQ_PYTHON = (3,6)
    if sys.version_info < MIN_REQ_PYTHON:
        print('Check the Kernel->Change Kernel menu and ensure that Python 3.6')
        print('or later is selected as the active kernel.')
        sys.exit("Python %s.%s or later is required.\n" % MIN_REQ_PYTHON)

    #imports
    from msticpy.data.azure_data import AzureData
    print('Imports Complete')


.. parsed-literal::

    Imports Complete


Instantiating and Connecting with an Azure Data Connector
---------------------------------------------------------

See :py:class:`Azure Data <msticpy.data.azure_data.AzureData>`

In order to connect to the Azure API and retrieve the required data
we need to instantiate an Azure Data Connector and connect to the API.
Authentication to the Azure API is handled via an Azure Service
Principal and token credentials. Before using this package you will
need to register a Service Principal and collect the required details.
Details on registering the Service Principal with the correct
permissions can be found `here <https://docs.microsoft.com/en-us/cli/azure/create-an-azure-service-principal-azure-cli?toc=%2Fazure%2Fazure-resource-manager%2Ftoc.json&view=azure-cli-latest>`__.

Once the Service Principal has been registered the following details
are required (they can be found in the Azure Portal under
Azure Active Directory > App Registrations):

* tenant_id -- The tenant ID of the Azure tenant the Service Principal is in.
* client_id -- The ID of the application associated with the Service
  Principal.
* secret -- The password of the Service Principal.

When connecting the required elements for connection can be passed in
a number of ways. The simplest is to pass the required elements as
kwargs.

.. code:: ipython3

        ten_id = input('Tenant ID')
        client_id = input('Client ID')
        secret = input('Client Secret')
        az = AzureData()
        az.connect(tenant_id=ten_id, client_id=client_id, secret=secret)

Alternatively you can store these details in the ``msticpyconfig.yaml``
file. Details should be included in the following format:

.. code:: yaml

      AzureCLI:
        Args:
        clientId: "CLIENT ID"
        clientSecret: "CLIENT SECRET"
        tenantId: "TENANT ID"

To use the stored variables when connecting simply provide no arguments.

.. code:: ipython3

        az = AzureData()
        az.connect()

Get Azure Subscription Details
------------------------------

See :py:meth:`get_subscriptions <msticpy.data.azure_data.AzureData.get_subscriptions>`

Details about the subscription a resource is a member of can provide
vital context to a security analyst when conducting an investigation.
This package contains 2 functions to support this.

AZURE_DATA_CONNECTOR.list_subscriptions() returns a pandas DataFrame
with details of all the subscriptions within the tenant.

.. code:: ipython3

    az.get_subscriptions()


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
          <th>Subscription ID</th>
          <th>Display Name</th>
          <th>State</th>
        </tr>
      </thead>
      <tbody>
        <tr>
          <td>0</td>
          <td>3b701f84-d04b-4479-89b1-fa8827eb537e</td>
          <td>Visual Studio Enterprise</td>
          <td>SubscriptionState.enabled</td>
        </tr>
      </tbody>
    </table>
    </div>

|

See :py:meth:`get_subscription_info <msticpy.data.azure_data.AzureData.get_subscription_info>`

AZURE_DATA_CONNECTOR.get_subscription_info() gets information on a
specific subscription ID.


.. code:: ipython3

    az.get_subscription_info("3b701f84-d04b-4479-89b1-fa8827eb537e")


.. parsed-literal::

    {'Subscription ID': '3b701f84-d04b-4479-89b1-fa8827eb537e',
     'Display Name': 'Visual Studio Enterprise',
     'State': 'SubscriptionState.enabled',
     'Subscription Location Limits': 'Public_2014-09-01',
     'Subscription Quota': 'MSDN_2014-09-01',
     'Spending Limit': <SpendingLimit.on: 'On'>}

Get Azure Resource Details
--------------------------

See :py:meth:`get_resources <msticpy.data.azure_data.AzureData.get_resources>`

As well as subscriptions we can return details on a specific Azure
resource.
AZURE_DATA_CONNECTOR.get_resources() returns a pandas DataFrame with
details on all resources within a Subscription or Resource Group.
In addition, you can request full properties on each Resource with the
get_props = True parameter. However, this can take some time to return
results.

.. code:: ipython3

    resources = az.get_resources(sub_id="3b701f84-d04b-4479-89b1-fa8827eb537e")
    resources.head()


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
          <th>resource_id</th>
          <th>name</th>
          <th>resource_type</th>
          <th>location</th>
          <th>tags</th>
          <th>plan</th>
          <th>properties</th>
          <th>kind</th>
          <th>managed_by</th>
          <th>sku</th>
          <th>identity</th>
        </tr>
      </thead>
      <tbody>
        <tr>
          <td>0</td>
          <td>/subscriptions/3b701f84-d04b-4479-89b1-fa8827e...</td>
          <td>cloud-shell-storage-westeurope-vnet</td>
          <td>Microsoft.Network/virtualNetworks</td>
          <td>centralus</td>
          <td>{}</td>
          <td>None</td>
          <td>None</td>
          <td>None</td>
          <td>None</td>
          <td>None</td>
          <td>None</td>
        </tr>
        <tr>
          <td>1</td>
          <td>/subscriptions/3b701f84-d04b-4479-89b1-fa8827e...</td>
          <td>csb3b701f84d04bx4479x89b</td>
          <td>Microsoft.Storage/storageAccounts</td>
          <td>westeurope</td>
          <td>{'ms-resource-usage': 'azure-cloud-shell'}</td>
          <td>None</td>
          <td>None</td>
          <td>Storage</td>
          <td>None</td>
          <td>{'additional_properties': {}, 'name': 'Standar...</td>
          <td>None</td>
        </tr>
        <tr>
          <td>2</td>
          <td>/subscriptions/3b701f84-d04b-4479-89b1-fa8827e...</td>
          <td>bluepot-01_OsDisk_1_ad7a7c0383444f02830ba46418...</td>
          <td>Microsoft.Compute/disks</td>
          <td>westus</td>
          <td>None</td>
          <td>None</td>
          <td>None</td>
          <td>None</td>
          <td>/subscriptions/3b701f84-d04b-4479-89b1-fa8827e...</td>
          <td>None</td>
          <td>None</td>
        </tr>
        <tr>
          <td>3</td>
          <td>/subscriptions/3b701f84-d04b-4479-89b1-fa8827e...</td>
          <td>bluepot-02_OsDisk_1_dce988e082e54617ae3622eca0...</td>
          <td>Microsoft.Compute/disks</td>
          <td>westus</td>
          <td>None</td>
          <td>None</td>
          <td>None</td>
          <td>None</td>
          <td>/subscriptions/3b701f84-d04b-4479-89b1-fa8827e...</td>
          <td>None</td>
          <td>None</td>
        </tr>
        <tr>
          <td>4</td>
          <td>/subscriptions/3b701f84-d04b-4479-89b1-fa8827e...</td>
          <td>CentOS-Test_OsDisk_1_7ee38d36b893481e8a68405c0...</td>
          <td>Microsoft.Compute/disks</td>
          <td>westus</td>
          <td>None</td>
          <td>None</td>
          <td>None</td>
          <td>None</td>
          <td>/subscriptions/3b701f84-d04b-4479-89b1-fa8827e...</td>
          <td>{'additional_properties': {}, 'name': 'Premium...</td>
          <td>None</td>
        </tr>
      </tbody>
    </table>
    </div>

|

See :py:meth:`get_resource_details<msticpy.data.azure_data.AzureData.get_resource_details>`

You can return full details on a single resource with AZURE_DATA_CONNECTOR.get_resource_details() and passing a Resource ID.


.. code:: ipython3

    az.get_resource_details(resource_id="/subscriptions/3b701f84-d04b-4479-89b1-fa8827eb537e/resourceGroups/Contoso/providers/Microsoft.Compute/virtualMachines/UbuntuDevEnv")




.. parsed-literal::

    {'resource_id': '/subscriptions/3b701f84-d04b-4479-89b1-fa8827eb537e/resourceGroups/Contoso/providers/Microsoft.Compute/virtualMachines/UbuntuDevEnv',
     'name': 'UbuntuDevEnv',
     'resource_type': 'Microsoft.Compute/virtualMachines',
     'location': 'northeurope',
     'tags': {},
     'plan': None,
     'properties': {'vmId': 'f557c9da-309f-4ab9-93ec-b29d7c21be87',
      'hardwareProfile': {'vmSize': 'Standard_B1s'},
      'storageProfile': {'imageReference': {'publisher': 'Canonical',
        'offer': 'UbuntuServer',
        'sku': '18.04-LTS',
        'version': 'latest',
        'exactVersion': '18.04.201812040'},
       'osDisk': {'osType': 'Linux',
        'name': 'UbuntuDevEnv_OsDisk_1_fc3690fe9f2248a1b441c0a1616833c5',
        'createOption': 'FromImage',
        'caching': 'ReadWrite',
        'managedDisk': {'id': '/subscriptions/3b701f84-d04b-4479-89b1-fa8827eb537e/resourceGroups/CONTOSO/providers/Microsoft.Compute/disks/UbuntuDevEnv_OsDisk_1_fc3690fe9f2248a1b441c0a1616833c5'}},
       'dataDisks': [{'lun': 0,
         'name': 'UbuntuDevEnv_DataDisk_0',
         'createOption': 'Attach',
         'caching': 'None',
         'writeAcceleratorEnabled': False,
         'managedDisk': {'id': '/subscriptions/3b701f84-d04b-4479-89b1-fa8827eb537e/resourceGroups/Contoso/providers/Microsoft.Compute/disks/UbuntuDevEnv_DataDisk_0'},
         'toBeDetached': False}]},
      'osProfile': {'computerName': 'UbuntuDevEnv',
       'adminUsername': 'peteb',
       'linuxConfiguration': {'disablePasswordAuthentication': True,
        'ssh': {'publicKeys': [{'path': '/home/peteb/.ssh/authorized_keys',
           'keyData': ''}]},
        'provisionVMAgent': True},
       'secrets': [],
       'allowExtensionOperations': True},
      'networkProfile': {'networkInterfaces': [{'id': '/subscriptions/3b701f84-d04b-4479-89b1-fa8827eb537e/resourceGroups/Contoso/providers/Microsoft.Network/networkInterfaces/ubuntudevenv3'}]},
      'provisioningState': 'Succeeded'},
     'kind': None,
     'managed_by': None,
     'sku': None,
     'identity': None}


.. note:: You can also provide a dictionary of resource details if you
          don't have a complete Resource ID.
          The details dictionary must contain:
          * resource_group_name
          * resource_provider_namespace
          * parent_resource_path (if there isn't one leave as a empty string).
          * resource_type
          * resource_name

.. code:: ipython3

    resource_details = {"resource_group_name":"Contoso",
                       "resource_provider_namespace":"Microsoft.Compute",
                       "parent_resource_path":"",
                       "resource_type":"virtualMachines",
                       "resource_name":"UbuntuDevEnv"}
    az.get_resource_details(resource_details=resource_details)




.. parsed-literal::

    {'resource_id': '/subscriptions/3b701f84-d04b-4479-89b1-fa8827eb537e/resourceGroups/Contoso/providers/Microsoft.Compute/virtualMachines/UbuntuDevEnv',
     'name': 'UbuntuDevEnv',
     'resource_type': 'Microsoft.Compute/virtualMachines',
     'location': 'northeurope',
     'tags': {},
     'plan': None,
     'properties': {'vmId': 'f557c9da-309f-4ab9-93ec-b29d7c21be87',
      'hardwareProfile': {'vmSize': 'Standard_B1s'},
      'storageProfile': {'imageReference': {'publisher': 'Canonical',
        'offer': 'UbuntuServer',
        'sku': '18.04-LTS',
        'version': 'latest',
        'exactVersion': '18.04.201812040'},
       'osDisk': {'osType': 'Linux',
        'name': 'UbuntuDevEnv_OsDisk_1_fc3690fe9f2248a1b441c0a1616833c5',
        'createOption': 'FromImage',
        'caching': 'ReadWrite',
        'managedDisk': {'id': '/subscriptions/3b701f84-d04b-4479-89b1-fa8827eb537e/resourceGroups/CONTOSO/providers/Microsoft.Compute/disks/UbuntuDevEnv_OsDisk_1_fc3690fe9f2248a1b441c0a1616833c5'}},
       'dataDisks': [{'lun': 0,
         'name': 'UbuntuDevEnv_DataDisk_0',
         'createOption': 'Attach',
         'caching': 'None',
         'writeAcceleratorEnabled': False,
         'managedDisk': {'id': '/subscriptions/3b701f84-d04b-4479-89b1-fa8827eb537e/resourceGroups/Contoso/providers/Microsoft.Compute/disks/UbuntuDevEnv_DataDisk_0'},
         'toBeDetached': False}]},
      'osProfile': {'computerName': 'UbuntuDevEnv',
       'adminUsername': 'peteb',
       'linuxConfiguration': {'disablePasswordAuthentication': True,
        'ssh': {'publicKeys': [{'path': '/home/peteb/.ssh/authorized_keys',
           'keyData': ''}]},
        'provisionVMAgent': True},
       'secrets': [],
       'allowExtensionOperations': True},
      'networkProfile': {'networkInterfaces': [{'id': '/subscriptions/3b701f84-d04b-4479-89b1-fa8827eb537e/resourceGroups/Contoso/providers/Microsoft.Network/networkInterfaces/ubuntudevenv3'}]},
      'provisioningState': 'Succeeded'},
     'kind': None,
     'managed_by': None,
     'sku': None,
     'identity': None}


Get Azure Network Details
-------------------------

See :py:meth:`get_network_details <msticpy.data.azure_data.AzureData.get_network_details>`

If your Azure resources has a network interface associated with it (for example a VM) you can return details on the 
interface as associated Network Security Group (NSG). Calling this function is very similar to getting resource details
however instead of passing it a resource ID you provide the network interface ID for the network device you want details 
for.

.. code:: ipython3
    az.get_network_details(networkID=NETWORK_INTERFACE_ID, sub_id=SUBSCRIPTION_ID)

.. note:: If youa are looking for a VM network interface ID you can use get_resource_details to get details on the VM.
    The network interface will be under properties > networkProfile > networkInterfaces > id

This will return a DataFrame containing details of all IP addresses and subnets associated with the network interface.


Get Azure Metrics
-----------------

See :py:meth:`get_metrics <msticpy.data.azure_data.AzureData.get_metrics>`

Azure provides a range of metrics for resources. The types of metrics avaliable depends on the Azure resource in question, 
a full list of metrics can be found `here <https://docs.microsoft.com/en-us/azure/azure-monitor/platform/metrics-supported>`__.
 
You can return all of these metrics with get_metrics.

In order to call this function you need to provide the metrics you want to retrieve in a comma seperated string
e.g. ""Percentage CPU,Disk Read Bytes,Disk Write Bytes", along with the resource ID of the item you wish to retreive 
the metrics for, and the subscription ID that resource is part of. You can also choose to get the metrics sampled 
at either the minute or the hour interval, and for how many days preceeding you want metrics for. By default the 
function returns hourly metrics for the last 30 days.

.. code:: ipython3
    az.get_metrics(metrics="Percentage CPU", resource_id=resource_details['resource_id'], sub_id=sub_details['Subscription ID'], sample_time="hour", start_time=15)

This returns a dictionary of items with the metric name as they key and a DataFrame of the metrics as the value. 

.. note:: get_metrics is resource specific, so if you want to get metrics from more than one resource you will need 
    seperate function calls.