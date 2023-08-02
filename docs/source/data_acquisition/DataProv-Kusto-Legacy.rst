Azure Data Explorer/Kusto Provider - Legacy Version
===================================================

Kusto Configuration
-------------------

Kusto Configuration in MSTICPy
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

You can store your connection details in *msticpyconfig.yaml*.

For more information on using and configuring *msticpyconfig.yaml* see
:doc:`msticpy Package Configuration <../getting_started/msticpyconfig>`
and :doc:`MSTICPy Settings Editor<../getting_started/SettingsEditor>`

The settings in the file should look like the following two examples:

.. code:: yaml

    DataProviders:
      ...
      Kusto:
        Args:
          Cluster: https://mstic.kusto.windows.net
          IntegratedAuth: True

.. code:: yaml

    DataProviders:
      ...
      Kusto:
        Args:
          Cluster: https://msticapp.kusto.windows.net
          ClientId: 69d28fd7-42a5-48bc-a619-af56397b1111
          TenantId: 69d28fd7-42a5-48bc-a619-af56397b9f28
          ClientSecret: "[PLACEHOLDER]"


We strongly recommend storing the client secret value
in Azure Key Vault. You can replace the text value with a referenced
to a Key Vault secret using the MSTICPy configuration editor.

Your configuration when using Key Vault should look like the following:

.. code:: yaml

        Kusto:
          Args:
            Cluster: https://msticapp.kusto.windows.net
            ClientId: 69d28fd7-42a5-48bc-a619-af56397b1111
            TenantId: 69d28fd7-42a5-48bc-a619-af56397b9f28
            ClientSecret:
              KeyVault:

You can create multiple instances of the Kusto settings for
multiple clusters by adding
an instance string to the "Kusto" section names

.. code:: yaml

    DataProviders:
      ...
      Kusto-mstic:
        Args:
          Cluster: https://mstic.kusto.windows.net
          IntegratedAuth: True
      Kusto-mstic2:
        Args:
          Cluster: https://mstic2.kusto.windows.net
          IntegratedAuth: True
      Kusto-msticapp:
        Args:
          Cluster: https://msticapp.kusto.windows.net
          ClientId: 69d28fd7-42a5-48bc-a619-af56397b1111
          TenantId: 69d28fd7-42a5-48bc-a619-af56397b9f28
          ClientSecret:
            KeyVault:


Data Query Format for Kusto clusters
------------------------------------

The query template format for Kusto queries should look like
the following.

.. code:: yaml

    metadata:
        version: 1
        description: Kusto Queries
        data_environments: [Kusto]
        data_families: [DeviceEvents.hostdata]
        cluster: https://msticapp.kusto.windows.net
        database: hostdata
        tags: ["user"]
    defaults:
      parameters:
        table:
            description: Table name
            type: str
            default: "DeviceProcessEvents"
        start:
            description: Query start time
            type: datetime
            default: -30
        end:
            description: Query end time
            type: datetime
            default: 0
        add_query_items:
            description: Additional query clauses
            type: str
            default: ""
    sources:
        list_host_processes:
            description: Lists all process creations for a host
            metadata:
            args:
            query: '
                {table}
                | where Timestamp >= datetime({start})
                | where Timestamp <= datetime({end})
                | where DeviceName has "{host_name}"
                {add_query_items}'
            uri: None
            parameters:
            host_name:
                description: Name of host
                type: str

Most of the query file is identical to queries for other drivers.
However, the metadata section has additional items: ``cluster`` and
``database``.

.. code-block:: yaml
   :emphasize-lines: 4, 5, 6

    metadata:
        version: 1
        description: Kusto Queries
        data_environments: [Kusto]
        data_families: [ALIAS[.DATABASE]]
        cluster: KUSTO_CLUSTER_URI
        database: DATABASE


The ``data_environments`` item must include "Kusto" in the list of
applicable environments.

You can specify the Kusto database to use in one of two ways:

1. Use the ``database`` key.
   Add the name of the database to connect to. The ``data_families`` key
   is used as a container name when adding attributes. Whatever string
   you specify here will be added as a prefix to the query name before attaching
   the query to the query provider.

2. Encode the database in the ``data_families`` item. If you do not
   specify a database key explicitly, you should use a dot-separated string
   for the data_families item:

   - the first part (before the dot) is an alias that will be used as a prefix
     when the queries are added to the query provider.
   - the second part is the Kusto database containing the data to be queried.

The ``cluster`` item in the query template file must match the ``Cluster``
setting in the *msticpyconfig* setting described in the previous section.

Here is are two examples.

.. code-block:: yaml

    metadata:
        version: 1
        description: Kusto Queries
        data_environments: [Kusto]
        data_families: [DeviceEvents]
        database: hostdata
        cluster: https://msticapp.kusto.windows.net

.. code-block:: yaml

    metadata:
        version: 1
        description: Kusto Queries
        data_environments: [Kusto]
        data_families: [DeviceEvents.hostdata]
        cluster: https://msticapp.kusto.windows.net

Queries using either of these metadata sections would be accessed and run as follows:

.. code:: ipython3

    kql_prov.DeviceEvents.list_host_processes(host_name="my_host", ...)

The file-level ``metadata`` section applies to all queries in the file by
default. You can specify a metadata section for individual queries. Any
settings here will override the file-level settings.

The example below shows overriding the ``data_families`` and ``cluster``
entries for an individual query.

.. code:: yaml

    metadata:
        version: 1
        description: Kusto Queries
        data_environments: [Kusto]
        data_families: [DeviceEvents.hostdata]
        cluster: https://msticapp.kusto.windows.net
        tags: ["user"]
    defaults:
      parameters:
        table:
            description: Table name
            type: str
            default: "DeviceProcessEvents"
        # ...
    sources:
        list_host_processes:
            description: Lists all process creations for a host
            metadata:
                data_families: [DeviceEvents.scrubbeddata]
                cluster: https://msticapp.kusto.windows.net
            args:
            query: '
                {table}
                | where Timestamp >= datetime({start})
                | where Timestamp <= datetime({end})
                | where DeviceName has "{host_name}"
                {add_query_items}'
            uri: None
            parameters:
            host_name:
                description: Name of host
                type: str

Loading a QueryProvider for Kusto
---------------------------------

.. code:: ipython3

        kql_prov = QueryProvider("Kusto")



Connecting to a Kusto cluster
-----------------------------

If you are using query files (as described above) you do not need to explicitly
connect - the connection will be made dynamically using the parameters in the
query definition.

To run add-hoc queries however, you need to explicitly connect to a cluster and
database. The parameters required for connection to a Kusto cluster can be passed in
a number of ways. You can provide a full connection string or parameters
for ``cluster`` and ``database``. In the latter case, you must have configured
settings for the cluster defined in your msticpyconfig.yaml.

The ``cluster`` name can be either the actual cluster name or the alias
that you used in your settings (i.e. the ``INSTANCE`` value in ``Kusto-INSTANCE``
configuration key). To connect, you must also specify a valid database
name in the cluster.


.. code:: ipython3

        kql_prov.connect(cluster="msticapp", database="hostdata")


If you have queries defined (in template files) for multiple clusters
and databases, you do not need to connect explicitly to each one.
You can call these queries by name - the driver will dynamically
read the connection parameters from the query file and attempt
to authenticate to the cluster.

Additional Kusto query parameters
---------------------------------

You can override the cluster and database for an individual
query by supply the ``cluster`` and/or ``database`` parameters
as query parameters.


.. code:: ipython3

        kql_prov.DeviceEvents.list_host_processes(
            host_name="my_host",
            cluster="https://somecluster.kusto.windows.net",
            database="archive"
            ...
        )


Other Kusto Documentation
-----------------------------------

For examples of using the Kusto provider, see the samples
`Kusto Analysis Notebook<https://github.com/microsoft/msticpy/blob/master/docs/notebooks/Kusto-Analysis.ipynb>`
and `Kusto Ingest Notebook<https://github.com/microsoft/msticpy/blob/master/docs/notebooks/Kusto-Ingest.ipynb>`

:py:mod:`Kusto driver API documentation<msticpy.data.drivers.kusto_driver>`