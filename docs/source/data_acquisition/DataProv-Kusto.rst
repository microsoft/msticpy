Azure Data Explorer/Kusto Provider
==================================

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
However, the metadata section has additional items.

.. code-block:: yaml
   :emphasize-lines: 4, 5, 6

    metadata:
        version: 1
        description: Kusto Queries
        data_environments: [Kusto]
        data_families: [ALIAS.DATABASE]
        cluster: KUSTO_CLUSTER_URI


The ``data_environments`` item must include "Kusto" in the list of
applicable environments.

The ``data_families`` item is composed of two parts, separated by a ".":

- the first half is friendly name or alias that will be used as a prefix
  when the queries are added to the query provider.
- the second part is the Kusto database containing the data to be queried.

The ``cluster`` item must match the ``Cluster`` setting in the *msticpyconfig*
setting described in the previous section.

Here is an example.

.. code-block:: yaml

    metadata:
        version: 1
        description: Kusto Queries
        data_environments: [Kusto]
        data_families: [DeviceEvents.hostdata]
        cluster: https://msticapp.kusto.windows.net

Queries using this metadata would be accessed and run as follows:

.. code:: ipython3

    kql_prov.DeviceEvents.list_host_processes(host_name=....)

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

The parameters required for connection to a Kusto cluster can be passed in
a number of ways. The simplest is to configure your settings
in msticpyconfig. You can then just call connect with no parameters.

.. code:: ipython3

        kql_prov.DeviceEvents.list_host_processes(
            host_name="my_host",
            cluster="https://somecluster."
        )


If you have configured multiple instances you must specify
an instance name when you call connect.

.. code:: ipython3

        kql_prov.connect(cluster="msticapp")


You can also pass connection parameters as
keyword arguments or a connection string.

Additional Kusto query parameters
---------------------------------

You can also override the cluster and database for an individual
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