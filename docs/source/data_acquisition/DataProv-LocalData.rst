The LocalData provider
======================

:py:mod:`LocalData driver documentation<msticpy.data.drivers.local_data_driver>`

The ``LocalData`` data provider is intended primarily for testing or demonstrations
where you may not be able to connect to an online data source reliably.

The data backing this driver can be in the form of a pickled pandas DataFrame
or a CSV file. In either case the data is converted to a DataFrame to be returned
from the query. Usage of this driver is a little different to most other drivers:

* You will need to provide a path to your data files when initializing
  the query provider (by default it will search in the current folder).
* You will also need to provide a query definition file (see following
  example) that maps the data file names that you are using to
  query names. The path to search for this is specified in the ``query_paths``
  parameter (see code examples below).
* Parameters to queries are ignored.

LocalData Configuration
-----------------------

LocalData Configuration in MSTICPy
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

You can store your connection details in *msticpyconfig.yaml*.

For more information on using and configuring *msticpyconfig.yaml* see
:doc:`msticpy Package Configuration <../getting_started/msticpyconfig>`
and :doc:`MSTICPy Settings Editor<../getting_started/SettingsEditor>`

The settings in the file should look like the following:

.. code:: yaml

    DataProviders:
      ...
      LocalData:
          data_paths:
            - /home/user1/sample_data
            - /home/shared/sample_data


Creating a LocalData Query File
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To define the queries you need to create a query definition file.
This is an example of a LocalData yaml query file. It is similar to the query
definition files for other providers but simpler. It only requires a
description, the data family that the query should be grouped under and
the name of the file containing the data.

.. code:: yaml

    metadata:
        version: 1
        description: Local Data Alert Queries
        data_environments: [LocalData]
        data_families: [SecurityAlert, WindowsSecurity, Network]
        tags: ['alert', 'securityalert', 'process', 'account', 'network']
    defaults:
    sources:
        list_alerts:
            description: Retrieves list of alerts
            metadata:
                data_families: [SecurityAlert]
            args:
                query: alerts_list.pkl
            parameters:
        list_host_logons:
            description: List logons on host
            metadata:
                data_families: [WindowsSecurity]
            args:
                query: host_logons.csv
            parameters:
        list_network_alerts:
            description: List network alerts.
            args:
                query: net_alerts.csv
            parameters:


In this example the value for the "query" is just the file name.
If the queries in your file are a mix of data from different data families,
you can group them by specifying one or more values for ``data_families``.
If this isn't specified for an individual query, it will inherit the setting
for ``data_families`` in the global ``metadata`` section at the top of the file.
Specifying more than one value for ``data_families``
will add links to the query under each data family grouping. This is to allow
for cases where a query may be relevant to multiple categories.
The ``data_families`` control only how the queries appear in query provider and
don't affect any other aspects of the query operation.

In the example shown, the ``list_alerts`` query has been added to the ``SecurityAlert``
attribute of the query provider, while ``list_host_logons`` is member of
``WindowsSecurity``. The entry for ``list_network_alerts`` had no ``data_families``
attribute so inherits the values from the file's ``metadata``. Since this has multiple
values, the query is added to all three families.

.. code:: ipython3

    # Structure of the query attributes added to the query provider
    qry_prov.list_queries()

.. parsed-literal::

    Network.list_host_logons
    Network.list_network_alerts
            ...<other queries>
    SecurityAlert.list_alerts
    SecurityAlert.list_network_alerts
            ...<other queries>
    WindowsSecurity.list_host_logons
    WindowsSecurity.list_network_alerts


Preparing to use the LocalData provider
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

1. Collect your data files into one or more directories or directory trees
   (the default location to search for data file is the current directory).
   Subdirectories are searched for ".pkl" and ".csv" files but only file
   names matching your query definitions will loaded.
2. Create one or more query definition yaml files (following the pattern above)
   and place these in a directory (this can be the same as the data files).
   The query provider will load and merge definitions from multiple YAML files.

QueryProvider defaults to searching for data files in the current directory
and subdirectories. The default paths for query definition files are a) the
built-in package queries path (msticpy/data/queries) and b) any custom
paths that you have added to msticpyconfig.yaml (see
:doc:`msticpy Package Configuration <../getting_started/msticpyconfig>`).

.. note:: The query definition files must have a ``.yaml`` extension.

Loading a QueryProvider for LocalData
-------------------------------------

This loads a LocalData query provider using configuration defaults.

.. code:: ipython3

    qry_prov = QueryProvider("LocalData")


Unless you have configured mstipyconfig to look in specific locations for
your localdata query and data files, you will need to specify these
as parameters to QueryProvider.

.. code:: ipython3

    data_path = "./my_data"
    query_path = "./myqueries"
    qry_prov = QueryProvider("LocalData", data_paths=[data_path], query_paths=[query_path])

    # list the queries loaded
    print(qry_prov.list_queries())


Connecting to LocalData
-----------------------

There is no connection step for the LocalData driver.


Example usage of LocalData driver
---------------------------------

.. code:: ipython3


    # list the queries loaded
    print(qry_prov.list_queries())

    # run a query
    my_alerts = qry_prov.SecurityAlert.list_alerts()

    # Specify path to look for data files
    data_path = "./my_data"
    qry_prov = QueryProvider("LocalData", data_paths=[data_path])

    # Show the schema of the data files read in
    print(qry_prov.schema)

    # Specify both data and query locations
    data_path = "./my_data"
    query_path = "./myqueries"
    qry_prov = QueryProvider("LocalData", data_paths=[data_path], query_paths=[query_path])

    host_logons_df = qry_prov.WindowsSecurity.list_host_logons()

    # parameters are accepted but ignored
    host_logons_df = qry_prov.WindowsSecurity.list_host_logons(
        start=st_date,
        end=end_date,
        host_name="myhost.com",
    )

Other LocalData Documentation
-----------------------------


Built-in :ref:`data_acquisition/DataQueries:Queries for Local Data`.

:py:mod:`LocalData driver API documentation<msticpy.data.drivers.local_data_driver>`
