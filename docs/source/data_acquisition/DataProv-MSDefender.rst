Microsoft 365 Defender Provider
===============================

This driver lets you query the Microsoft Defender APIs.

.. note:: This section applies to both Microsoft 365 Defender and Microsoft Defender
    for Endpoint (MDE). The former supersedes and is a subset of the the latter
    but both are still available to use.

    Many components in MSTICPy still use the old abbreviation **MDATP**
    (Microsoft Advanced Threat Protection).

M365 Defender Configuration
---------------------------

Creating a Client App for M365 Defender
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Details on registering an Azure AD application for MS 365 Defender can be found
`here <https://docs.microsoft.com/windows/security/threat-protection/microsoft-defender-atp/exposed-apis-create-app-webapp>`__.
Once you have registered the application, you can use it to connect to
the MS Defender API.

M365 Defender Configuration in MSTICPy
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

You can store your connection details in *msticpyconfig.yaml*.

For more information on using and configuring *msticpyconfig.yaml* see
:doc:`msticpy Package Configuration <../getting_started/msticpyconfig>`
and :doc:`MSTICPy Settings Editor<../getting_started/SettingsEditor>`

The settings in the file should look like the following:

.. code:: yaml

    DataProviders:
      ...
      MicrosoftDefender:
          Args:
            ClientId: "CLIENT ID"
            ClientSecret: "CLIENT SECRET"
            TenantId: "TENANT ID"
            UserName: "User Name"
            Cloud: "global"


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

You can create multiple instances of M365 Defender settings by adding
an instance string to the "MicrosoftDefender" section name.

.. code:: yaml

      MicrosoftDefender-Tenant1:
          Args:
            ClientId: "CLIENT ID"
            ClientSecret:
                KeyVault:
            TenantId: "TENANT ID"
      MicrosoftDefender-Tenant2:
          Args:
            ClientId: "CLIENT ID"
            UserName: "USER NAME"
            TenantId: "TENANT ID"


Loading a QueryProvider for M365 Defender
-----------------------------------------

.. code:: ipython3

        mdatp_prov = QueryProvider("M365D")

You can also use the aliases "MDE" and "MDATP".

Specifying the Defender Cloud Instance to Connect to
----------------------------------------------------

If connecting to the Defender API to run queries there are a number of
different endpoints you can connect to.
Which one is most applicable will depend on your location and which
cloud you are using.

By default 'https://api.securitycenter.microsoft.com/' or
'https://api.security.microsoft.com/' is used, but others can be
specified either in your MSTICPy config file, or by passing
in the name with the cloud keyword:

.. code:: ipython3

        mdatp_prov = QueryProvider("MDE", cloud="gcc")


If using an MDE-specific API endpoint, the "name" (the first parameter to QueryProvider in the example above) must be "MDE".

+----------+----------------------------------------------+----------------------------------------+
| Cloud    | MDE                                          | M365D                                  |
+==========+==============================================+========================================+
| global   | https://api.securitycenter.microsoft.com/    | https://api.security.microsoft.com/    |
+----------+----------------------------------------------+----------------------------------------+
| uk       | https://api-uk.securitycenter.microsoft.com/ | https://api-uk.security.microsoft.com/ |
+----------+----------------------------------------------+----------------------------------------+
| us       | https://api-us.securitycenter.microsoft.com/ | https://api-us.security.microsoft.com/ |
+----------+----------------------------------------------+----------------------------------------+
| eu       | https://api-eu.securitycenter.microsoft.com/ | https://api-eu.security.microsoft.com/ |
+----------+----------------------------------------------+----------------------------------------+
| gcc      | https://api-gcc.securitycenter.microsoft.us/ | NA                                     |
+----------+----------------------------------------------+----------------------------------------+
| gcc-high | https://api-gov.securitycenter.microsoft.us/ | NA                                     |
+----------+----------------------------------------------+----------------------------------------+
| dod      | https://api-gov.securitycenter.microsoft.us/ | NA                                     |
+----------+----------------------------------------------+----------------------------------------+

Connecting to M365 Defender
---------------------------

The parameters required for connection to Defender can be passed in
a number of ways. The simplest is to configure your settings
in msticpyconfig. You can then just call connect with no parameters.

.. code:: ipython3

        mdatp_prov.connect()


If you have configured multiple instances you must specify
an instance name when you call connect.

.. code:: ipython3

        mdatp_prov.connect(instance="Tenant2")

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
* username -- If using delegated auth for your application.


.. code:: ipython3

        ten_id = input('Tenant ID')
        client_id = input('Client ID')
        client_secret = input('Client Secret')
        md_prov = QueryProvider('M365D')
        md_prov.connect(tenant_id=ten_id, client_id=client_id, client_secret=client_secret)

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
    md_prov.connect(conn_str)

Other M365 Defender Documentation
---------------------------------

For examples of using the MS Defender provider, see the sample
`M365 Defender Notebook<https://github.com/microsoft/msticpy/blob/master/docs/notebooks/MDATPQuery.ipynb>`

Built-in :ref:`data_acquisition/DataQueries:Queries for Microsoft 365 Defender`.

:py:mod:`M365 Defender driver API documentation<msticpy.data.drivers.mdatp_driver>`
