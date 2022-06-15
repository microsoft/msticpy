
Microsoft Graph API Provider
============================


Connecting to the Security Graph API follows the same format as MS Defender
connections with connection variables passed to the function in the
same way. The configuration format is also identical to that specified in the
previous section.

Microsoft Graph Configuration
-----------------------------

Creating a Client App for Microsoft Graph
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Details for registering an application for the Microsoft Graph API can
be found `here <https://docs.microsoft.com/graph/auth-register-app-v2?context=graph%2Fapi%2F1.0&view=graph-rest-1.0>`__.


Microsoft Graph Configuration in MSTICPy
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

You can store your connection details in *msticpyconfig.yaml*.

For more information on using and configuring *msticpyconfig.yaml* see
:doc:`msticpy Package Configuration <../getting_started/msticpyconfig>`
and :doc:`MSTICPy Settings Editor<../getting_started/SettingsEditor>`

The settings in the file should look like the following:

.. code:: yaml

    DataProviders:
      ...
      MicrosoftGraph:
          Args:
            ClientId: "CLIENT ID"
            ClientSecret: "CLIENT SECRET"
            TenantId: "TENANT ID"
            UserName: "USER NAME"


We strongly recommend storing the client secret value
in Azure Key Vault. You can replace the text value with a referenced
to a Key Vault secret using the MSTICPy configuration editor.

Your configuration when using Key Vault should look like the following:

.. code:: yaml

      MicrosoftGraph:
          Args:
            ClientId: "CLIENT ID"
            ClientSecret:
                KeyVault:
            TenantId: "TENANT ID"

You can create multiple instances of Microsoft Graph settings by adding
an instance string to the "MicrosoftGraph" section name.

.. code:: yaml

      MicrosoftGraph-Tenant1:
          Args:
            ClientId: "CLIENT ID"
            ClientSecret:
                KeyVault:
            TenantId: "TENANT ID"
      MicrosoftGraph-Tenant2:
          Args:
            ClientId: "CLIENT ID"
            UserName: "USER NAME"
            TenantId: "TENANT ID"

Loading a QueryProvider for Microsoft Graph
-------------------------------------------

.. code:: ipython3

        msg_prov = QueryProvider("SecurityGraph")


Connecting to Microsoft Graph
-----------------------------

The parameters required for connection to MS Graph can be passed in
a number of ways. The simplest is to configure your settings
in msticpyconfig. You can then just call connect with no parameters.

.. code:: ipython3

        msg_prov.connect()


If you have configured multiple instances you must specify
an instance name when you call connect.

.. code:: ipython3

        msg_prov.connect(instance="Tenant2")

If you want to use delegated authentication for your application
you can specify this when you call connect. By default, this will
attempt to use browser-based authentication, however you can also
use device code authentication (needed if using Azure ML) by setting
auth_type to "device".

.. code:: ipython3

        mdatp_prov.connect(delegated_auth=True, auth_type="device")

You can also pass connection parameters as
keyword arguments or a connection string.

To specify connection parameters as keyword arguments in the function call,
the required parameters are:

* tenant_id -- The tenant ID of the Defender workspace to connect to.
* client_id -- The ID of the application registered for MS Defender.
* client_secret -- The secret used for by the application.


.. code:: ipython3

        ten_id = input('Tenant ID')
        client_id = input('Client ID')
        client_secret = input('Client Secret')
        msg_prov = QueryProvider('SecurityGraph')
        msg_prov.connect(tenant_id=ten_id, client_id=client_id, client_secret=client_secret)

You can also specify these parameters as a connection string of the form:

"tenant_id='*my_tenant*'; client_id='*my_appid*'; client_secret='*my_secret*'"

.. code:: ipython3

    # The use of parentheses here is just to concatenate the strings
    # inside the parentheses, to create a single string.
    conn_str = (
        "tenant_id='243bb6be-4136-4b64-9055-fb661594199a'; "
        "client_id='a5b24e23-a96a-4472-b729-9e5310c83e20'; "
        "client_secret='[PLACEHOLDER]'"
    )
    msg_prov.connect(conn_str)


Other Microsoft Graph Documentation
-----------------------------------

Built-in :ref:`data_acquisition/DataQueries:Queries for Microsoft Graph`.

:py:mod:`Microsoft Graph driver API documentation<msticpy.data.drivers.security_graph_driver>`