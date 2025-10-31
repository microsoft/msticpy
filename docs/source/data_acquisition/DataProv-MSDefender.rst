Microsoft Defender Provider
===========================

This driver lets you query the Microsoft Defender APIs.

.. note:: This provider supports multiple API endpoints for accessing Microsoft Defender data:

    - **M365DGraph** (Recommended): Uses the Microsoft Graph API with ThreatHunting permissions.
      This is the preferred and most modern API.
    - **MDE/MDATP**: Uses the Microsoft Defender for Endpoint API (formerly MDATP).
      This is the fallback option when M365DGraph is not available.
    - **M365D** (Removed): The Microsoft 365 Defender API provider has been removed from
      MSTICPy. If you use the "M365D" provider name, it will automatically use the MDE
      API endpoints instead.

    Many components in MSTICPy still use the old abbreviation **MDATP**
    (Microsoft Advanced Threat Protection) for backwards compatibility.

Defender Configuration
----------------------

Creating a Client App for Microsoft Defender
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Microsoft Defender APIs can be accessed in both
`application <https://learn.microsoft.com/en-us/defender-endpoint/api/exposed-apis-create-app-webapp>`__
and `delegated user contexts <https://learn.microsoft.com/en-us/defender-endpoint/api/exposed-apis-create-app-nativeapp>`__.
Accessing Microsoft Defender APIs as an application requires
either a client secret or certificate, while delegated user auth requires
an interactive signin through a browser or via device code.

As such, the details on registering an Azure AD application for MS Defender
are different for application and delegated user auth scenarios. Please
see the above links for more information. Notably, delegated user auth
scenarios do not require a application credential and thus is preferable.

For delegated user auth scenarios, ensure that the application has a
"Mobile or Desktop Application" redirect URI configured as `http://localhost`.
A redirect URI is not required for applications with their own credentials.

API permissions for the client application will require tenant admin consent.
Ensure that the consented permissions are correct for the chosen data environment
and auth scenario (application or delegated user):

+-----------------------------+------------------------+------------------+
| API Name                    | Permission             | Data Environment |
+=============================+========================+==================+
| Microsoft Graph             | ThreatHunting.Read.All | M365DGraph       |
+-----------------------------+------------------------+------------------+
| WindowsDefenderATP          | AdvancedQuery.Read     | MDE, MDATP       |
+-----------------------------+------------------------+------------------+

.. note:: **M365DGraph is the recommended data environment** as it uses the modern
    Microsoft Graph API. The MDE/MDATP endpoints are maintained for backwards
    compatibility and as a fallback option.

    The Microsoft Threat Protection API provider (M365D) has been removed. If you
    specify "M365D" as the provider name, it will automatically use MDE endpoints
    for backwards compatibility.

Once you have registered the application, you can use it to connect to
the MS Defender API using your chosen data environment.

Defender Configuration in MSTICPy
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
            TenantId: "TENANT ID"
            UserName: "User Name"
            Cloud: "global"


If connecting to the MS Defender 365 API using application auth,
we strongly recommend storing the client secret value
in Azure Key Vault. You can replace the text value with a referenced
to a Key Vault secret using the MSTICPy configuration editor.
See :doc:`msticpy Settings Editor <../getting_started/SettingsEditor>`

Your configuration when using Key Vault should look like the following:

.. code:: yaml

      MicrosoftDefender:
          Args:
            ClientId: "CLIENT ID"
            ClientSecret:
                KeyVault:
            TenantId: "TENANT ID"

You can create multiple instances of Defender settings by adding
an instance suffix to the "MicrosoftDefender" section name.

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

When using a certificate with a private key, the configuration
should be:

.. code:: yaml

      MicrosoftDefender:
          Args:
            ClientId: "CLIENT ID"
            TenantId: "TENANT ID"
            Certificate: "Path to certificate"
            PrivateKey: "Path to private key"

If connecting to the MS Defender 365 API using application auth,
we strongly recommend using a secret on the private key and storing it
in Azure Key Vault. You can replace the text value with a referenced
to a Key Vault secret using the MSTICPy configuration editor.
See :doc:`msticpy Settings Editor <../getting_started/SettingsEditor>`.

.. code:: yaml

      MicrosoftDefender:
          Args:
            ClientId: "CLIENT ID"
            TenantId: "TENANT ID"
            PrivateKey: "Path to private key"
            Certificate: "Path to certificate"
            PrivateKeySecret:
                KeyVault:

Federated Authentication Configuration
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Federated authentication allows you to authenticate using credentials from
Microsoft Entra ID in place of an Application client secret or certificate.

When using federated authentication with MS Defender, you authenticate as a user
rather than as an application, which means:

* You don't need to store client secrets or certificates
* Authentication uses your organization's federated identity provider credentials
* You benefit from your organization's security policies (MFA, conditional access, etc.)
* Permissions are granted based on your user account rather than an app registration
* The authentication flow automatically redirects your Microsoft Entra ID

To use federated authentication with MS Defender, configure your settings
to include a username but **not** a client secret or certificate:

.. code:: yaml

      MicrosoftDefender:
          Args:
            ClientId: "CLIENT ID"
            TenantId: "TENANT ID"
            UserName: "user@domain.com"
            Cloud: "global"

The presence of a ``UserName`` field triggers delegated (user) authentication.
When you connect, the authentication flow will:

1. Contact your Microsoft Entra ID tenant
2. Detect that your user account is federated
3. Automatically redirect to your organization's configured identity provider
4. Return to Entra ID after successful authentication with your IdP
5. Issue the token needed to access MS Defender APIs

**Federated Authentication with Multiple Tenants**

If you work with multiple tenants that use federated authentication:

.. code:: yaml

      MicrosoftDefender-TenantA:
          Args:
            ClientId: "CLIENT ID A"
            TenantId: "TENANT ID A"
            UserName: "user@tenantA.com"
            Cloud: "global"
      MicrosoftDefender-TenantB:
          Args:
            ClientId: "CLIENT ID B"
            TenantId: "TENANT ID B"
            UserName: "user@tenantB.com"
            Cloud: "global"

**Token Caching Location**

By default, authentication tokens are cached in a file named ``token_cache.bin``
in the current directory. You can specify a custom location:

.. code:: yaml

      MicrosoftDefender:
          Args:
            ClientId: "CLIENT ID"
            TenantId: "TENANT ID"
            UserName: "user@domain.com"
            Location: "/path/to/custom/token_cache.bin"
            Cloud: "global"

.. note:: When using federated authentication, ensure your app registration
    has a "Mobile or Desktop Application" redirect URI configured as
    ``http://localhost``. This is required for the interactive authentication flow.


Loading a QueryProvider for Microsoft Defender
----------------------------------------------

**Recommended: Use M365DGraph**

.. code:: ipython3

        defender_prov = QueryProvider("M365DGraph")

**Alternative: Use MDE (legacy/fallback)**

.. code:: ipython3

        mde_prov = QueryProvider("MDE")

You can also use the alias "MDATP" for backwards compatibility.

.. note:: The "M365D" alias has been removed but is still accepted for backwards
    compatibility - it will automatically use MDE endpoints. We recommend using
    "M365DGraph" for new implementations.

Specifying the Defender Cloud Instance
--------------------------------------

If connecting to the Defender API to run queries there are a number of
different endpoints you can connect to.
Which one is most applicable will depend on your location and which
cloud you are using.

**For M365DGraph (Microsoft Graph API):**

By default 'https://graph.microsoft.com/' is used. For government clouds,
the appropriate Graph endpoint will be selected automatically based on the
cloud parameter.

.. code:: ipython3

        defender_prov = QueryProvider("M365DGraph", cloud="gcc")

**For MDE (Defender for Endpoint API):**

By default 'https://api.securitycenter.microsoft.com/' is used, but others can be
specified either in your MSTICPy config file, or by passing
in the name with the cloud keyword:

.. code:: ipython3

        mde_prov = QueryProvider("MDE", cloud="gcc")


+----------+----------------------------------------------+
| Cloud    | MDE Endpoint                                 |
+==========+==============================================+
| global   | https://api.securitycenter.microsoft.com/    |
+----------+----------------------------------------------+
| uk       | https://api-uk.securitycenter.microsoft.com/ |
+----------+----------------------------------------------+
| us       | https://api-us.securitycenter.microsoft.com/ |
+----------+----------------------------------------------+
| eu       | https://api-eu.securitycenter.microsoft.com/ |
+----------+----------------------------------------------+
| gcc      | https://api-gcc.securitycenter.microsoft.us/ |
+----------+----------------------------------------------+
| gcc-high | https://api-gov.securitycenter.microsoft.us/ |
+----------+----------------------------------------------+
| dod      | https://api-gov.securitycenter.microsoft.us/ |
+----------+----------------------------------------------+

.. note:: M365DGraph uses Microsoft Graph endpoints which are automatically
    configured for government clouds. The above table applies only to MDE/MDATP endpoints.

Connecting to Microsoft Defender
--------------------------------

The parameters required for connection to Defender can be passed in
a number of ways. The simplest is to configure your settings
in msticpyconfig. You can then just call connect with no parameters.

.. code:: ipython3

        defender_prov.connect()


If you have configured multiple instances you must specify
an instance name when you call connect.

.. code:: ipython3

        defender_prov.connect(instance="Tenant2")

If you want to use delegated authentication for your application
you can specify this when you call connect. By default, this will
attempt to use browser-based authentication, however you can also
use device code authentication (needed if using Azure ML) by setting
auth_type to "device".

.. code:: ipython3

        defender_prov.connect(delegated_auth=True, auth_type="device")

**Connecting with Federated Authentication**

When using federated authentication (username-based auth), the provider
will automatically use delegated authentication. You can choose between
interactive browser authentication (default) or device code authentication:

.. code:: ipython3

        # Browser-based authentication (default for federated users)
        mdatp_prov.connect()

        # Or explicitly specify interactive authentication
        mdatp_prov.connect(auth_type="interactive")

        # Device code authentication (useful in restricted environments)
        mdatp_prov.connect(auth_type="device")

When using browser-based authentication with a federated identity provider,
a browser window will open and redirect you to your organization's login page.
After authenticating with your IdP (which may include MFA), you'll be
redirected back and the connection will complete automatically.

For device code authentication, you'll be presented with a code and URL.
Open the URL in any browser (on any device), enter the code, and authenticate
through your federated identity provider.

**Connecting with Federated Auth Using Connection Parameters**

You can also pass federated authentication parameters directly:

.. code:: ipython3

        mdatp_prov.connect(
            tenant_id='your-tenant-id',
            client_id='your-client-id',
            username='user@domain.com',
            auth_type='interactive'  # or 'device'
        )

Or as a connection string:

.. code:: ipython3

        conn_str = (
            "tenant_id='243bb6be-4136-4b64-9055-fb661594199a'; "
            "client_id='a5b24e23-a96a-4472-b729-9e5310c83e20'; "
            "username='user@domain.com'"
        )
        mdatp_prov.connect(conn_str, auth_type='interactive')

.. note:: The ``delegated_auth`` parameter is automatically set to ``True``
    when a username is provided, so you don't need to specify it explicitly.

You can also pass connection parameters as
keyword arguments or a connection string.

To specify connection parameters as keyword arguments in the function call,
the required parameters are:

* tenant_id -- The tenant ID of the Defender workspace to connect to.
* client_id -- The ID of the application registered for MS Defender.
* client_secret -- The secret used for by the application.
* username -- If using delegated auth for your application.

The client_secret and username parameters are mutually exclusive.

.. code:: ipython3

        ten_id = input('Tenant ID')
        client_id = input('Client ID')
        client_secret = input('Client Secret')
        defender_prov = QueryProvider('M365DGraph')
        defender_prov.connect(tenant_id=ten_id, client_id=client_id, client_secret=client_secret)

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

Other Microsoft Defender Documentation
--------------------------------------

For examples of using the MS Defender provider, see the sample
`Microsoft Defender Notebook<https://github.com/microsoft/msticpy/blob/master/docs/notebooks/MDATPQuery.ipynb>`__

Built-in :ref:`data_acquisition/DataQueries:Queries for Microsoft Defender`.

:py:mod:`Microsoft Defender driver API documentation<msticpy.data.drivers.mdatp_driver>`
