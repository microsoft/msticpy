Azure Authentication in MSTICPy
===============================

There are several components in MSTICPy that use Azure Services and
require authentication to use them. Some examples are:

- Microsoft Sentinel, Microsoft Graph, Microsoft Defender and Azure
  Resource Graph data providers
- Microsoft Sentinel APIs
- Azure Data APIs
- Microsoft Sentinel TI provider

MSTICPy uses Azure SDKs and standard Azure credentials to authenticate
to these services.

Azure supports many different types of credential - the different
credential types are a function of the way that you authenticate to
Azure Active Directory (AAD).

Some credential types are distinguished based on what
you use to authenticate your identity to AAD. For example:

- **UsernamePasswordCredential** - simple username and password
- **CertificateCredential** - certificate private key
- **ClientSecretCredential** - AppID and App secret
- **ManagedIdentityCredential** - Uses the AAD assigned identity for something
  like a domain-joined Virtual machine.

Other types have helper functionality that allow you to authenticate
in specific circumstances:

- **InteractiveBrowserCredential** - invokes a browser to initiate the
  authentication flow.
- **EnvironmentCredential** - looks for specific environment variables
  that contain credential details such as a password or App secret.
- **DeviceCodeCredential** - creates a one-time device code and returns
  login URL to initiate the authentication flow.

Some types of credential can be obtained from applications or system
components in which you have previously authenticated
such as:

- **AzureCliCredential**
- **VisualStudioCodeCredential**
- **AzurePowerShellCredential**

A relatively complete list can be found here
`Azure Identity Python SDK Credential classes
<https://learn.microsoft.com/python/api/overview/azure/identity-readme?view=azure-python#credential-classes>`_


.. note:: In MSTICPy we refer to the different "credential types" as
    "authentication methods". The function parameter used to specify
    the method(s) that you want to use is ``auth_methods``.
    The configuration item in msticpyconfig.yaml is also named
    ``auth_methods`` and is in the Azure section of the settings.


How to choose a credential type
-------------------------------
Since there are so many credential types, working out which one to use
can be confusing.

MSTICPy, by default, will try to use the following credential types (and in this
order):

- Managed Identity
- Azure CLI
- Device Code

This means that MSTICPy will try to obtain credentials of each these
types in turn. If it finds a matching credential type it will try
to use that to authenticate you to the Azure service that you are
trying to access. In the default case it will do the following:

1. Check to see if you are running on a system (a host/virtual machine)
   that has a system-assigned identity OR (as in the case of Azure
   Machine Learning) has support for user-assigned managed identities.
   See here for detailed information about `Managed Identities
   <https://learn.microsoft.com/azure/active-directory/managed-identities-azure-resources/overview>`__
2. Look for an authenticated Azure CLI session and try to obtain
   the credential from Azure CLI
3. If both fail, it will prompt with a device code and login URL
   that you must browse to, paste in the device code and complete
   the authentication flow.

You can change the default authentication types used by MSTICPy
by configuring settings in your msticpyconfig.yaml - see
`Setting your MSTICPY authentication method defaults`_

Azure authentication methods supported by MSTICPy
-------------------------------------------------

- **env** - Use credentials set in environment variables
- **cli** - Use credentials available in an local AzureCLI logon
- **msi** - Use the Managed Service Identity (MSI) credentials of the
  machine you are running the notebook kernel on
- **devicecode** - use browser-based device code authentication flow
- **vscode** - Use credentials from your authenticated VS Code session
- **powershell** - Use credentials from an authenticated Azure Powershell session
- **clientsecret** - Use an Azure AppID and client secret
- **certificate** - Use client certificate authentication.
- **interactive** - Interactive browser logon

Common credential flows
-----------------------

Using Azure CLI
~~~~~~~~~~~~~~~

**auth_method = "cli"**

Due to its ability to cache credentials, we
recommend using Azure CLI logon. This allows all MSTICPy
Azure functions to try to obtain current credentials from Azure
CLI rather than initiate an interactive authentication.
This is especially helpful when using multiple Azure components
or when using multiple notebooks.

If the host running your notebook kernel does not have Azure CLI
installed you can install it from
the `Azure CLI install page <https://docs.microsoft.com/cli/azure/install-azure-cli>`__.

To log in using Azure CLI enter the following:

From a terminal:

.. code:: bash

   az login

From a notebook

.. code:: ipython3

   !az login

Using Environment Credentials
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

**auth_method = "env"**

You can use environment credentials with either username and
password or with an Azure AppID and secret.

For username and password you must set these environment
variables prior to attempting to authenticate:

==================== ========================================================
 variable name	      value
==================== ========================================================
AZURE_TENANT_ID	      id of the user's Azure Active Directory tenant
AZURE_USERNAME	      a username (usually an email address)
AZURE_PASSWORD	      that user's password
==================== ========================================================

To use AppID and secret, set these environment variables

==================== ========================================================
variable name	      value
==================== ========================================================
AZURE_CLIENT_ID	      id of an Azure Active Directory application
AZURE_TENANT_ID	      id of the application's Azure Active Directory tenant
AZURE_CLIENT_SECRET	  one of the application's client secrets
==================== ========================================================

Using Device-code Credentials
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

**auth_method = "devicecode"**

Using device code authentication causes a one-time code and login URL to
be displayed in the notebook or console.

1. Select and copy the code to the clipboard.
2. Open a browser and navigate to the login URL provided.
3. Paste the code into the dialog box.
4. Continue with the normal browser-based authentication flow (this may
   require multi-factor authentication depending on your organization's
   access policies)

User Managed Identity Credentials
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you have a usable managed identity on the system you are using
(the system where the Jupyter notebook kernel is running) the credentials
will be obtained automatically and used for authentication.

Using VSCode and AzurePowerShell Credentials
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

**auth_method = "vscode"**
**auth_method = "powershell"**

As long as you have an active authenticated session in VS Code or
PowerShell, the credentials should be automatically obtained and
used.

Setting your MSTICPY authentication method defaults
---------------------------------------------------

You can use the MSTICPy settings editor to specify which authentication
methods (credential types) you want to use.

See the section :ref:`getting_started/SettingsEditor:Default Azure authentication methods` for more
details.

.. note:: the settings editor does not currently allow you to
    specify a preference order for this. To do this, open
    `msticpyconfig.yaml` in an editor and manually set the order
    that you want.


Specifying authentication method preferences when authenticating
----------------------------------------------------------------

Several MSTICPy APIs support the ``auth_methods`` parameter. These
include:

- The :py:func:`az_connect <msticpy.auth.azure_auth.az_connect>` function.
- The :py:meth:`QueryProvider.connect <msticpy.data.core.data_providers.QueryProvider.connect>`
  for Azure data services (such as Microsoft Sentinel)

Specify the list of one or more ``auth_methods`` that you want to use
as a list of strings. The authentication methods will be tried in
the order specified in the list.

.. code:: python

    az_connect(auth_methods=["env", "cli"])

The values for the available authentication methods are list in
the table below.

================= ============================= ==================================================================
 value             Credential type
================= ============================= ==================================================================
 **env**           EnvironmentCredential         Use credentials set in environment variables
 **cli**           AzureCliCredential            Use credentials available in an local AzureCLI logon
 **msi**           ManagedIdentityCredential     Use the Managed Service Identity (MSI) credentials of the
                                                 machine you are running the notebook kernel on
 **devicecode**    DeviceCodeCredential          Use browser-based device code authentication flow
 **vscode**        VisualStudioCodeCredential    Use credentials from your authenticated VS Code session
 **powershell**    AzurePowerShellCredentialUse  Use credentials from an authenticated Azure Powershell session
 **clientsecret**  ClientSecretCredential        Use an Azure AppID and client secret
 **certificate**   CertificateCredential         Use client certificate authentication.
 **interactive**   InteractiveBrowserCredential  Use Interactive browser logon flow
================= ============================= ==================================================================