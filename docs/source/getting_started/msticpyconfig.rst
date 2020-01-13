
msticpy Package Configuration
=============================

Some elements of msticpy require configuration parameters. An
example is the Threat Intelligence providers. Values for these
and other parameters can be set in the `msticpyconfig.yaml` file.

The package has a default configuration file, which is stored in the
package directory. You should not need to edit this file directly.
Instead you can create a custom file with your own parameters - these
settings will combine with or override the settings in the default file.

By default, the custom `msticpyconfig.yaml` is read from the current
directory. You can specify an explicit location using an environment
variable ``MSTICPYCONFIG``.

Configuration sections
----------------------

AzureSentinel
~~~~~~~~~~~~~
Here you can specify your default workspace and tenant IDs and add additional
workspaces if needed.

QueryDefinitions
~~~~~~~~~~~~~~~~
This allows you to specify paths to additional yaml query template files.

TIProviders
~~~~~~~~~~~
This allows you to configure which providers are run by default and to
supply any authorization keys needed to access the service.

OtherProviders
~~~~~~~~~~~~~~
This section is similar to the TIProviders section, allowing you
specify configuration options for other data providers.


Specifying secrets as Environment Variables
-------------------------------------------

Some configuration values can be references to environment Variables
rather than have a value explicitly stored within the configuration
file. You might want secrets such as API keys to be supplied this
way. The ``Args`` subsection of TIProvider and OtherProvider entries
supports storing values as simple strings or as references to named
environment variables. You can see examples of both in the sample
file below.

Extending msticpyconfig.yaml
----------------------------

You can also extend msticpyconfig to include additional sections to
support other authentication and configuration options such as MDATP
API connections. Refer to documentation on these features for required
structures.

Settings are read by the
:py:mod:`refresh_config<msticpy.nbtools.pkg_config>` module.
Combined settings are available as the ``settings`` attribute of this
module. Default settings and custom settings (the settings that you
specify in your own msticpyconfig.yaml) also available separately in
the ``default_settings`` and ``custom_settngs`` attributes, respectively.

To force settings to be re-read after the package has been imported,
call :py:func:`refresh_config<msticpy.nbtools.pkg_config.refresh_config>`.

The settings exposed in these attributes are python dictionaries that
reflect the underlying YAML data in the configuration file.

.. note:: the :py:mod:`~msticpy.nbtools.wsconfig` module, TIProviders,
   OtherProviders and the data libraries use additional functionality
   to provide higher-level views of the configuration data. An example
   of this is the using environment variable references to replace
   the actual configuration value with the secret stored in the
   environment variables.


Comment configuration file sample
---------------------------------


.. code:: yaml

    AzureSentinel:
      Workspaces:
        # Workspace used if you don't explicitly name a workspace when creating WorkspaceConfig
        # Specifying values here overrides config.json settings unless you explictly load
        # WorkspaceConfig with config_file parameter (WorkspaceConfig(config_file="../config.json")
        Default:
          WorkspaceId: "d973e3d2-28e6-458e-b2cf-d38876fb1ba4"
          TenantId: "4cdf87a8-f0fc-40bb-9d85-68bcf4ac8e61"
        # To use these launch with an explicit name - WorkspaceConfig(workspace_name="Workspace2")
        Workspace2:
          WorkspaceId: "c88dd3c2-d657-4eb3-b913-58d58d811a41"
          TenantId: "f1f64e65-ff7c-4d71-ad5b-091b6ab39d51"
        Workspace3:
          WorkspaceId: "17e64332-19c9-472e-afd7-3629f299300c"
          TenantId: "4ea41beb-4546-4fba-890b-55553ce6003a"
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
          AuthKey: "c88dd3c2-d657-4eb3-b913-58d58d811a41"
        Primary: False
        Provider: "OPR"
      TorExitNodes:
        Primary: True
        Provider: "Tor"
    OtherProviders:
      GeoIPLite:
        Args:
          AuthKey:
            EnvironmentVar: "MAXMIND_AUTH"
          DBFolder: "~/.msticpy"
        Provider: "GeoLiteLookup"
      IPStack:
        Args:
          AuthKey: "987654321-222"
        Provider: "IPStackLookup"


See also
--------

:doc:`The Threat Intelligence Providers documention <../data_acquisition/TIProviders>`

:py:mod:`wsconfig<msticpy.nbtools.wsconfig>`
:py:mod:`provider_settings<msticpy.nbtools.provider_settings>`
:py:mod:`wsconfig<msticpy.nbtools.wsconfig>`
