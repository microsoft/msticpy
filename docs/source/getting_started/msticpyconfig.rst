
*MSTICPy* Package Configuration
===============================

Some elements of *MSTICPy* require configuration parameters. An
example is the Threat Intelligence providers. Values for these
and other parameters can be set in the *msticpyconfig.yaml* file.
This file uses a YAML format.

.. warning:: MSTICPy has a *msticpyconfig.yaml* included as part of
  the package itself. This is used to supply some default settings
  and *should not be modified*. Any settings that you include in your
  own *msticpyconfig.yaml* will override the default settings.

You should also read the :doc:`MSTICPy Settings Editor <SettingsEditor>`
document to see how to configure settings using and interactive User
Interface from a Jupyter notebook.

.. important:: The settings in *msticpyconfig.yaml* are *case sensitive*. If you
   specify a setting in the config file with a different case to the
   setting name in the code, the setting will not be found.


How MSTICPy finds the config file
---------------------------------

MSTICPy uses the following logic to discover a configuration file.

1. If the environment variable ``MSTICPYCONFIG`` is set, MSTICPy will
   use the value of this variable as the path to the config file.
2. If the environment variable ``MSTICPYCONFIG`` is not set, MSTICPy
   will look for a file named ``msticpyconfig.yaml`` in the current
   directory.
3. If neither of these is successful, MSTICPy
   will look for a file named ``msticpyconfig.yaml`` in a folder named
   ".mstipcy" in the user's home
   directory. This is typically ``~/.msticpy`` on Linux and Mac and
   ``C:\Users\username\.msticpy`` on Windows.

If you are using the :py:func:`init_notebook <msticpy.init.nbinit.init_notebook>`
function to initialize MSTICPy, you can specify the path to a config file
as the ``config`` parameter to this function. This will override the logic above.

Configuration sections
----------------------

AzureSentinel
~~~~~~~~~~~~~
Here you can specify your default workspace IDs and tenant IDs and add additional
workspaces if needed. If you wish to use the Microsoft Sentinel API features you
can also specify Subscription Ids, Subscription names and Workspace names here.

.. note:: Although Microsoft Sentinel is no longer called "AzureSentinel"
  the configuration section is still called AzureSentinel to ensure
  compatibility with existing code.

Sample entry for MS Sentinel workspaces

.. code:: yaml

    AzureSentinel:
      Workspaces:
        Default:
          # An entry named "Default" allows authentication without
          # having to specify a workspace name
          WorkspaceId: "52b1ab41-869e-4138-9e40-2a4457f09bf3"
          TenantId: "72f988bf-86f1-41af-91ab-2d7cd011db49"
          SubscriptionId: "cd928da3-dcde-42a3-aad7-d2a1268c2f48"
          ResourceGroup: ABC
          WorkspaceName: Workspace1
        Workspace1:
          # You can also duplicate entries with different keys
          # that refer to the same workspace settings
          WorkspaceId: "52b1ab41-869e-4138-9e40-2a4457f09bf3"
          TenantId: "72f988bf-86f1-41af-91ab-2d7cd011db49"
          SubscriptionId: "cd928da3-dcde-42a3-aad7-d2a1268c2f48"
          ResourceGroup: ABC
          WorkspaceName: Workspace1
        MyTestWS:
          # minimal workspace configuration for queries
          WorkspaceId: "a927809c-8142-43e1-96b3-4ad87cfe95a3"
          TenantId: "69d28fd7-42a5-48bc-a619-af56397b9f28"


QueryDefinitions
~~~~~~~~~~~~~~~~
This allows you to specify paths to additional yaml query template files.

Sample

.. code:: yaml

    QueryDefinitions:
      Custom:
        - ./working
        - ~/.msticpy/queries

TIProviders
~~~~~~~~~~~
This allows you to configure which providers are run by default and to
supply any authorization keys needed to access the service.

Specifying ``Primary: True`` means that the TI Provider will
be included in lookups by default. If this is ``False`` you must specifically
include the provider name in the optional ``providers`` parameter
when you make a TI lookup call.

Currently supported provider names are:

- OTX - AlienVault OTX
- VirusTotal
- XForce - IBM XForce
- AzureSentinel - Microsoft Sentinel TI
- GreyNoise
- IntSights
- TorExitNodes
- OpenPageRank
- CrowdSec
- AbuseIPDB

.. code:: yaml

    TIProviders:
      OTX:
        Args:
          AuthKey: 1234567890
        Primary: True
        Provider: "OTX"
      VirusTotal:
        Args:
          AuthKey:
            EnvironmentVar: "VIRUSTOTAL_AUTH"
        Primary: True
        Provider: "VirusTotal"
      OpenPageRank:
        Args:
          AuthKey:
            KeyVault:
        Primary: False
        Provider: "OPR"
      CrowdSec:
        Args:
          AuthKey: [PLACEHOLDER]
        Primary: True
        Provider: "CrowdSec"
      AbuseIPDB:
        Args:
          AuthKey: 1234567890
        Primary: True
        Provider: "AbuseIPDB"

.. note:: You store values in the ``Args`` section as simple strings,
   as names of environment variables containing the value, or
   as references to Azure Key Vault (see the later sections
   `Specifying secrets as Environment Variables`_ and
   `Specifying secrets as Key Vault secrets`_).

OtherProviders
~~~~~~~~~~~~~~
This section is similar to the TIProviders section, allowing you
specify configuration options for specialist data providers such
as geo-location providers.

.. code:: yaml

    OtherProviders:
      GeoIPLite:
        Args:
          AuthKey:
            EnvironmentVar: "MAXMIND_AUTH"
          DBFolder: "~/.msticpy"
        Provider: "GeoLiteLookup"

DataProviders
~~~~~~~~~~~~~~
This section is similar to the previous two sections, allowing you
specify configuration options for other data providers.

.. code:: yaml

    DataProviders:
      MicrosoftDefender:
        Args:
          TenantId: 8360dd21-0294-4240-9128-89611f415c53
          ClientId: 66b9818a-26cd-4584-8eb0-7f7a499242aa
          ClientSecret:
            KeyVault:
      Cybereason:
        Args:
          TenantId: 8360dd21-0294-4240-9128-89611f415c53
          ClientId: 1234
          ClientSecret:
            EnvironmentVar: "CYBEREASON_AUTH"

Key Vault
~~~~~~~~~
This section contains Azure Key Vault settings. This is only used if you
choose to store secrets (e.g. API keys) in Key Vault. See the
section `Specifying secrets as Key Vault secrets`_ for more details.

Azure
~~~~~

This section provides configuration settings for Azure authentication,
including the Azure cloud.

Legal values for ``cloud`` are:

- global
- usgov - US government cloud
- cn - China national cloud
- de - German national cloud (no longer used)

The ``auth_methods`` key lists the types of authentication
methods that you want to enable.


.. important:: In some cases MSTICPy may find a valid
   credential type but is unable to authenticate with this type due
   to Conditional access policies or other restrictions. If you
   find issues authenticating, edit the ```auth_methods`` list to use specific
   credential types that can be authorized with the service that you
   are using.

Possible credential types (``auth_methods``) are:

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


.. warning:: Some environments (such as Azure Machine Learning notebooks)
   do not allow notebooks to open browser windows, so "interactive"
   credentials will fail. We have found Azure CLI to be reliable
   and maintains authentication tokens between notebook sessions.

.. code:: yaml

    Azure:
      cloud: "global"
      auth_methods: ["cli", "msi", "interactive"]

For more details on Azure authentication/credential types see
:doc:`./AzureAuthentication`

User Defaults
~~~~~~~~~~~~~

.. warning:: This feature is being deprecated. Please avoid using
  this. If you have a use case that requires this, please contact
  the MSTICPy team - msticpy@microsoft.com. We'd love to hear
  how you are using it and how we might be able to improve it.

This section controls loading of default providers when using the
package in a notebook. The settings here are loaded by the
:py:func:`init_notebook <msticpy.init.nbinit.init_notebook>`
function.

See the `User Defaults Section`_ below.

MSTICPy Global Settings
~~~~~~~~~~~~~~~~~~~~~~~

There are miscellaneous settings that control the behavior of
MSTICPy and some of the underlying libraries used by MSTICPy.

.. code:: yaml

    msticpy:
      FriendlyExceptions: True
      QueryDefinitions:
        - ./queries
        - ~/.msticpy/queries
      http_timeout: 30
      Proxies:
        https:
          Url: https://proxy:8080
          UserName: user
          Password:
            EnvironmentVar: "PROXY_PASSWORD"


**FriendlyExceptions** controls whether MSTICPy will catch and re-raise
exceptions with a more user-friendly message. This is mainly
applicable to use in Jupyter notebooks. Most of the MSTICPy exceptions
are displayable as an HTML message with details such as links
to relevant documentation and drop-down details of the exception.

If this is set to ``False`` then the exception will be raised
as a standard Python exception with details printed as simple text.

**QueryDefinitions** is a list of paths to folders containing query
definition files. You can add any number of these and MSTICPy will
search these for queries to add to QueryProviders.

**http_timeout** controls the default timeout for the httpx library
used by MSTICPy. This can be simple integer or float that sets the global
timeout values for connections. You can also specify this as
a dictionary or tuple of individual timeout components:

 - ConnectTimeout: Float
 - ReadTimeout
 - WriteTimeout
 - PoolTimeout

You can also specify as a Tuple or of (default_timeout, connect_timeout).
For more details on httpx timeouts see the
`HTTPX documentation <https://www.python-httpx.org/advanced/#setting-and-disabling-timeouts>`__.

**Proxies** is a dictionary of proxy settings. You can specify
different proxies for different protocols (although only the https
one is currently used in MSTICPy). We are gradually rolling out
support for this setting to components in MSTICPy - not all yet
support this setting.
If your proxy requires authentication you can specify a username
and password. These are combined into a single URL of the form
``https://username:password@proxy:port``.

.. note:: If you are using a proxy that requires authentication
   you should consider using the ``KeyVault`` setting to store
   the password (and optionally the username) rather than specifying
   it in the config file. You can also use the ``EnvironmentVar``
   to reference an environment variable that contains the password
   (and username).


Specifying secrets as Environment Variables
-------------------------------------------

Some configuration values can be references to environment Variables
rather than have a value explicitly stored within the configuration
file. You might want secrets such as API keys to be supplied this
way. The ``Args`` subsection of TIProvider and OtherProvider entries
supports storing values as simple strings or as references to named
environment variables. You can see examples of both in the sample
file below.

Specifying secrets as Key Vault secrets
---------------------------------------

*MSTICPy* can read secret values from Key Vault for use with TI and
other providers. To use this you need to specify settings for your
keyvault.

.. code:: yaml

    KeyVault:
      TenantId: 5d6a50cf-b1b6-4bfd-ad54-b9822b06ff92
      SubscriptionId: 40dcc8bf-0478-4f3b-b275-ed0a94f2c013
      ResourceGroup: YourResourceGroup
      AzureRegion: RegionToCreateKV
      VaultName: "myvault"
      UseKeyring: True
      Authority: global
      AuthnType: device

Under the top level ``KeyVault`` section the following entries
are accepted. Some of these are only required if you plan to
use *MSTICPy* to create a new Key Vault vault.

Required Settings
~~~~~~~~~~~~~~~~~
.. list-table::
   :widths: 15, 30

   * - TenantId
     - the Identifier of your Azure tenant
   * - VaultName
     - the name of the vault to use (note this can be
       overridden in the individual secret specifications
   * - Authority
     - You should use the ``cloud`` setting in the
       ``Azure`` configuration section unless you need to override
       this setting for Key Vault

Required to Create a Key Vault
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.. list-table::
   :widths: 15, 30

   * - SubscriptionId
     - the Azure subscription holding the Key Vault
   * - ResourceGroup
     - the Azure resource group in which to create the vault
   * - AzureRegion
     - the Azure region in which to create the vault

Optional Settings
~~~~~~~~~~~~~~~~~
.. list-table::
   :widths: 15, 30

   * - UseKeyring
     - if True (default) uses the Python keyring package
       to securely cache Key Vault secrets in your client session.
   * - VaultName
     - the name of the vault to use (note this can be
       overridden in the individual secret specifications)
   * - AuthnType
     - You should use the ``auth_methods``
       setting of the ``Azure`` section - see `Azure`_ above.
       unless you need to override the Azure defaults.

Specifying Key Vault Secrets in Provider Settings
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

There are three ways to specify the Key Vault *vault* and *secret* names
to use for a given setting.

.. code:: yaml

    TIProviders:

      OpenPageRank:
        Args:
          AuthKey:
            KeyVault:

Adding an empty subkey named ``KeyVault`` will cause *MSTICPy* to generate
a name for the secret,  built from the path of the setting. This is the default
usage. In the example below,
the secret name will be "TIProviders-OpenPageRank-Args-AuthKey".
The vault name is taken from the setting in the ``KeyVault`` settings
section.

.. code:: yaml

    OtherProviders:
      IPStack:
        Args:
          AuthKey:
            KeyVault: my_secret


This example specifies "my_secret" as the secret name.
The vault name is taken from the setting in the ``KeyVault`` settings
section.

.. code:: yaml

    OtherProviders:
      Contoso-GeopIp:
        Args:
          AuthKey:
            KeyVault: my_vault/my_secret

The final example specifies both a vault name and a secret name.
The ``VaultName`` setting in the ``KeyVault`` section is ignored
for this setting.

Populating Key Vault secrets from an existing msticpyconfig.yaml
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

You can use the *MSTICPy* settings editor to upload secrets to
a Key Vault one-by-one. This is described in in the in the
:ref:`Key Vault Secrets <getting_started/SettingsEditor:key vault secrets>`
section of *MSTICPy* Settings Editor document.


There is also a command line tool to move your secrets to Key Vault -
``config2ky.py``. This tool is available in the *MSTICPy* GitHub repo.
You can find it in the ``tools`` folder.

Running ``config2kv.py --help`` shows the usage of this utility.

The simplest way to use this tool is to populate your existing
secrets as strings in your *msticpyconfig.yaml*. (as shown in
some of the provider settings in the example at the end of this
page).

You can specify this as the input file using the ``--path`` parameter.
Alternatively, the tool will look for a msticpyconfig.yaml in the
location specified by the ``MSTICPYCONFIG`` environment variable.

Create a ``KeyVault``
configuration section in the file, supplying the values described
earlier. If you already have a vault that you want to use, put
the name of the vault in the ``VaultName`` setting and run
``config2kv.py`` with the ``--exists`` parameter. This will bypass
the Key Vault Management client section and the extra authentication
step that this requires. If you do not have a vault or wish to
create a new one, omit the ``--exists`` parameter and you will
be prompted to create one.

The tool will read secrets and create secret names based on the
path of the secret (as described above).

.. warning:: ``config2ky`` will only read and convert
   items in the provider ``Args`` sections. Currently, only
   ``ApiID`` and ``AuthKey`` values will be used.

The tool will then write the
secret values to the vault. Finally a replacement *msticpyconfig.yaml*
is written to the location specified in the ``--path`` argument.
You can then delete or securely store your old configuration file
and replace it with the one output by ``config2kv``.

.. tip:: you can run ``config2ky`` with the ``--show`` parameter to
   perform a rehearsal. This will show you the Key Vault secrets
   that will be created and show the text of the msticpyconfig.yaml
   file that would have been created.


Using **keyring** to cache secrets
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

**keyring** is available on most Python platforms: Windows, Linux
and MacOS. On Linux it requires the installation of optional
components - either KWallet or Secret Service. See the
`Keyring Readme <https://github.com/jaraco/keyring>`__ for more
information.

.. warning:: *keyring* caching is enabled by default. If you are working
   in an environment that does not have one of the supported *keyring*
   backends installed you should disable keyring caching by adding
   ``Keyring: false`` to you configuration settings.

The advantage of using *keyring* is that you do not need to re-authenticate
to Key Vault for each notebook that you use in each session. If you
have ``UseKeyring: true`` in your *msticpyconfig.yaml* file, the
first time that you access a Key Vault secret the secret value is
stored as a keyring password with the same name as the Key Vault secret.

Unfortunately, the *keyring* package provides no way to list or delete stored
secrets. If you need to remove the locally-stored secrets use the platform
utility for the appropriate backend. For example, on Windows, ``cmdkey``
lets you list and manipulate local stored credentials.

.. warning:: *keyring* secrets are not automatically synchronized
   with the Key Vault secret values. If you change the value of a
   secret in Key Vault you must delete the keyring secret so that
   the new value will be re-read from Key Vault.

   You can use the :py:func:`refresh_keyring <msticpy.common.provider_settings.refresh_keyring>`
   and :py:func:`clear_keyring <msticpy.common.provider_settings.clear_keyring>`
   to control these automatically-cached Key Vault values.


Manually managing your Key Vault secrets
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

You can use the Azure portal to create and manage your secrets. If you
prefer to do this, simply enter the name of the secret in the
corresponding section for the AuthKey or ApiID of your providers.

You can also use powershell or Python to manage these programmatically.
*MSTICPy* has some convenience wrappers around the Azure SDK functions.

The documentation for these is available here:
:py:mod:`keyvault_client<msticpy.auth.keyvault_client>`
:py:mod:`secrets_settings<msticpy.auth.secrets_settings>`


User Defaults Section
---------------------

.. warning:: This feature is being deprecated. Please avoid using
  this. If you have a use case that requires this, please contact
  the MSTICPy team - msticpy@microsoft.com. We'd love to hear
  how you are using it and how we might be able to improve it.

This section specifies the query and other providers that you want
to load by default. It is triggered from the
:py:func:`init_notebook<msticpy.init.nbinit.init_notebook>`
although you can call the
:py:func:`load_user_defaults<msticpy.init.user_config.load_user_defaults>`
function to do this manually.

If you do not have this section in your configuration ``init_notebook`` will
bypass auto-loading any components.


QueryProviders
~~~~~~~~~~~~~~

This is a list of query providers that you want to load. Most of the
providers have a single namespace/environment associated with them but in
the case of Azure Sentinel, you can load multiple copies of the query
provider for different workspaces. The example below shows three different
formats that you can used. Each workspace name under ``QueryProviders``
must exist as a workspace definition in the AzureSentinel section of this
file (see `Commented configuration file sample`_ below)

.. note:: Single-string entries in this and other sections (e.g.
   ``MyWorkspace:`` below) must be specified as empty dictionaries. This
   is done by adding a trailing ":" to the entry but no value on the other
   side of the colon. This is simply to make the settings parsing code
   a little easier. This is only when you are specifying a setting key -
   i.e. the first item on a line. The *key values* ("azsent", "sco" and
   "False" in this example) should be entered without a trailing colon.

   Note also that False is a boolean value, not a string. You should
   always enter True and False with proper capitalization.

.. code:: yaml

    UserDefaults
      QueryProviders:
        AzureSentinel:
          MyWorkspace:
          Default:
            alias: azsent
          CyberSoc:
            alias: soc
            connect: False

``MyWorkspace`` is loaded as-is - equivalent to calling:

.. code:: ipython3

    from msticpy.data import QueryProvider
    from msticpy.common.wsconfig import WorkspaceConfig

    qry_myworkspace = QueryProvider("AzureSentinel")
    ws_config = WorkspaceConfig(workspace="MyWorkspace")
    qry_myworkspace.connect(ws_config.code_connect_str)

The ``Default`` entry has a few differences. The name "Default" refers
to the default workspace definition in the AzureSentinel section of
the msticpyconfig file. The ``alias: azsent`` element is an alias that will be used
to rename the provider. It is equivalent to the following code:

.. code:: ipython3

    from msticpy.data import QueryProvider
    from msticpy.common.wsconfig import WorkspaceConfig

    qry_azsent = QueryProvider("AzureSentinel")
    ws_config = WorkspaceConfig()
    qry_azsent.connect(ws_config.code_connect_str)

The final ``CyberSoc`` entry has multiple key-value pairs under it.
The "alias" entry works exactly the same as the previous example.
The "connect" item tells the code not to automatically connect
(authenticate) to Azure Sentinel. It is equivalent to the following
code:

.. code:: ipython3

    from msticpy.data import QueryProvider

    qry_soc = QueryProvider("AzureSentinel")

In all three cases the query provider object (``qry_soc`` in the last
example) is stored in the global namespace of the notebook so you
can always refer to it using this variable name.

Query providers for non-Azure Sentinel data sources use the same
syntax for aliasing and suppressing connect/authenticate. For
example:

.. code:: yaml

    UserDefaults
      QueryProviders:
        AzureSentinel:
          ...
        Splunk:
          connect: false
        LocalData:
          alias: local

LoadComponents
~~~~~~~~~~~~~~

This section controls the loading and instantiation of a number
of other data providers and components.

.. code:: yaml

    UserDefaults
      ...
      LoadComponents:
        TILookup:
        GeoIpLookup:
          provider: GeoLiteLookup
        Notebooklets:
          query_provider:
            AzureSentinel:
              workspace: CyberSoc
        AzureData:
          auth_methods=['cli','interactive']
        AzureSentinelAPI:

Some of these accept additional parameters and some do not. Most
of the configuration parameters for GeoIP providers, for example,
are loaded from other sections of the configuration file.

``GeoIpLookup`` - requires one parameter - the name of the ``provider``
that you want to use for GeoIP location resolution.

``TILookup`` - no parameters, simply creates an instance of TILookup
using the settings in the ``TIProviders`` section.

``Notebooklets`` - to use this you must have MSTIC Notebooklets (msticnb
see `MSTICNB documentation <https://msticnb.readthedocs.io>`__). This
has a required configuration setting, which *MSTICPy* passes to the
notebooklets init function as the ``query_provider`` parameter. Other
key/pair values included under the "query_provider" key are passed to
the notebooklets initialization. Each parameter name is prefixed with
the provider name so that it knows which parameters to send to which
provider. In the example above notebooklets ``nbinit`` would be passed
the following parameters:

.. code:: ipython3

    nbinit(query_provider="AzureSentinel", AzureSentinel_workspace="CyberSoc")


The notebooklets
package is loaded after most of the other providers (but before Pivot if that
is included in the list) and is also sent the names of other
providers (query and others such as TILookip) as its ``providers`` parameter.
For more details see
`data_providers.init <https://msticnb.readthedocs.io/en/latest/msticnb.html#msticnb.data_providers.init>`__.


``AzureData`` and ``AzureSentinel`` load the Azure resource API and Azure
Sentinel API libraries respectively. Any key/pair values defined under either
of these entries are passed to the provider ``connect`` method. In the
AzureData example above this is equivalent to the following code.


.. code:: ipython3

    from msticpy.context.azure_data import AzureData
    az_data = AzureData()
    az_data.connect(auth_methods=['cli','interactive'])

The components in the LoadProviders section have built-in friendly
names for each component. These currently cannot be overridden from the
configuration settings:

- geoip
- ti_lookup
- nb
- az_data
- azs_api


*MSTICPy* current_providers Attribute
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you have loaded providers using the UserDefaults configuration the
provider instances created are also stored in an attribute of the
``msticpy`` top level module.


.. code:: ipython3

    >>> msticpy.current_providers

    {'qry_azsent': <msticpy.data.data_providers.QueryProvider at 0x21604110ac8>,
    'qry_myworkspace': <msticpy.data.data_providers.QueryProvider at 0x216041459c8>,
    'qry_cybersoc': <msticpy.data.data_providers.QueryProvider at 0x21660d41308>,
    'qry_splunk': <msticpy.data.data_providers.QueryProvider at 0x21661127208>,
    'qry_local': <msticpy.data.data_providers.QueryProvider at 0x216605a7c48>,
    'ti_lookup': <msticpy.context.tilookup.TILookup at 0x216611c7908>,
    'geoip': <msticpy.context.geoip.GeoLiteLookup at 0x21660659c88>,
    'az_data': <msticpy.context.azure_data.AzureData at 0x21668aaf708>,
    'azs_api': <msticpy.context.azure_sentinel.AzureSentinel at 0x21603f42388>,
    'nb': <module 'msticnb' from 'e:\\src\\msticnb\\msticnb\\__init__.py'>}


You can use this to reference any of these loaded components. Although
these values are normally also populated in the notebook global namespace
you can re-populate them if needed. To write them
back into the notebook namespace execute the following:

.. code:: ipython3

    >>> globals().update(msticpy.current_providers)


.. warning:: This will overwrite any global variable with the same name as
   any of the items in the ``current_providers`` dictionary.


Using msticpyconfig.yaml in code
--------------------------------

Settings are read using ``get_config`` method of the
:py:mod:`refresh_config<msticpy.common.settings>` module.
You can specify a setting path in dotted notation. For example:

.. code:: python3

    from msticpy.common.settings import get_config
    qry_prov = get_config("QueryProviders.MicrosoftGraph")

You can also set configuration settings using the ``set_config`` method.
This also supports dotted notation for the setting path. Any
changes made here are not persisted to the configuration file and
only available for the current Python session.

To force settings to be re-read after the package has been imported,
call :py:func:`refresh_config<msticpy.common.pkg_config.refresh_config>`.

The settings exposed in these attributes are python dictionaries that
reflect the underlying YAML data in the configuration file.

.. note:: the :py:mod:`~msticpy.common.wsconfig` module, TIProviders,
   OtherProviders and the data libraries use additional functionality
   to provide higher-level views of the configuration data. An example
   of this is the using environment variable references to replace
   the actual configuration value with the secret stored in the
   environment variables.

If you are building a component for MSTICPy or work alongside MSTICPy,
you can extend msticpyconfig to include additional sections to support
your component.

Commented configuration file sample
-----------------------------------


.. code:: yaml

    # msticpy global settings
    msticpy:
      FriendlyExceptions: True
      QueryDefinitions:
        - ./queries
        - ~/.msticpy/queries
      http_timeout: 30
      Proxies:
        https:
          Url: https://proxy:8080
          UserName: user
          Password:
            EnvironmentVar: "PROXY_PASSWORD"
    AzureSentinel:
      Workspaces:
        # Workspace used if you don't explicitly name a workspace when creating WorkspaceConfig
        # Specifying values here overrides config.json settings unless you explictly load
        # WorkspaceConfig with config_file parameter (WorkspaceConfig(config_file="../config.json")
        Default:
          WorkspaceId: "d973e3d2-28e6-458e-b2cf-d38876fb1ba4"
          TenantId: "4cdf87a8-f0fc-40bb-9d85-68bcf4ac8e61"
          SubscriptionId: "2e0acf9c-b2c4-4c9b-8f34-9899b9588492"
          WorkspaceName: "MyWorkspace"
          ResourceGroup: "MyResourceGroup"
        # To use these launch with an explicit name - WorkspaceConfig(workspace_name="Workspace2")
        Workspace2:
          WorkspaceId: "c88dd3c2-d657-4eb3-b913-58d58d811a41"
          TenantId: "f1f64e65-ff7c-4d71-ad5b-091b6ab39d51"
        Workspace3:
          WorkspaceId: "17e64332-19c9-472e-afd7-3629f299300c"
          TenantId: "4ea41beb-4546-4fba-890b-55553ce6003a"
    KustoClusters:

    QueryDefinitions:
      # Add paths to folders containing custom query definitions here
      Custom:
        - /var/global-queries
        - /home/myuser/queries
        - c:/users/myuser/documents
    TIProviders:
      # If a provider has Primary: True it will be run by default on IoC lookups
      # Secondary providers can be
      OTX:
        Args:
          AuthKey: "4ea41beb-4546-4fba-890b-55553ce6003a"
        Primary: True
        Provider: "OTX" # WARNING - Do not change Provider values!
      VirusTotal:
        Args:
          AuthKey: "4ea41beb-4546-4fba-890b-55553ce6003a"
        Primary: False
        Provider: "VirusTotal"
      XForce:
        # You can store items in an environment variable using this syntax
        Args:
          ApiID:
            EnvironmentVar: "XFORCE_ID"
          AuthKey:
            EnvironmentVar: "XFORCE_KEY"
        Primary: True
        Provider: "XForce"
      AzureSentinel:
        # Note this can be a different workspace/tenant from your main workspace
        # This only controls where the Azure Sentinel TI provider looks for the
        # ThreatIndicator table.
        Args:
          WorkspaceID: "c88dd3c2-d657-4eb3-b913-58d58d811a41"
          TenantID: "f1f64e65-ff7c-4d71-ad5b-091b6ab39d51"
        Primary: True
        Provider: "AzSTI"
      OpenPageRank:
        Args:
          AuthKey:
            KeyVault:
        Primary: False
        Provider: "OPR"
      TorExitNodes:
        Primary: True
        Provider: "Tor"
      RiskIQ:
        Args:
          ApiID: "user@host.com"
          AuthKey: "aaaa-bbbb-cccc-dddd-eeee"
        Provider: "RiskIQ"
        Primary: True
    OtherProviders:
      GeoIPLite:
        Args:
          AuthKey:
            EnvironmentVar: "MAXMIND_AUTH"
          DBFolder: "~/.msticpy"
        Provider: "GeoLiteLookup"
      IPStack:
        Args:
          AuthKey:
            KeyVault: my_secret
        Provider: "IPStackLookup"
      Contoso-GeopIp:
        Args:
          AuthKey:
            KeyVault: my_vault/my_secret
        Provider: "ContosoLookup"
    DataProviders:
      AzureCLI:
        Args:
          clientId: "69d28fd7-42a5-48bc-a619-af56397b1111"
          tenantId: "69d28fd7-42a5-48bc-a619-af56397b2222"
          clientSecret: "69d28fd7-42a5-48bc-a619-af56397b3333"
    # DEPRECATED
    UserDefaults:
      # DEPRECATED - List of query providers to load
      QueryProviders:
        - AzureSentinel:
          - Default: asi
          - CyberSoc:
            alias: soc
            connect: false
        - Splunk:
            connect: false
        - LocalData: local
      # DEPRECATED - List of other providers/components to load
      LoadComponents:
        - TILookup
        - GeoIpLookup: GeoLiteLookup
        - Notebooklets:
            query_provider:
              AzureSentinel: CyberSoc
        - Pivot
        - AzureData:
          auth_methods=['cli','interactive']
        - AzureSentinelAPI


See also
--------

:doc:`The Threat Intelligence Providers documentation <../data_acquisition/TIProviders>`
:doc:`Settings Editor <./SettingsEditor>`

:py:mod:`wsconfig<msticpy.common.wsconfig>`
:py:mod:`provider_settings<msticpy.common.provider_settings>`
:py:mod:`wsconfig<msticpy.common.pkg_config>`
