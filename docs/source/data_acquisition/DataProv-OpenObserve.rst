OpenObserve Provider
==================

OpenObserve Configuration
-----------------------

You can store your connection details in *msticpyconfig.yaml*.

For more information on using and configuring *msticpyconfig.yaml* see
:doc:`msticpy Package Configuration <../getting_started/msticpyconfig>`
and :doc:`MSTICPy Settings Editor<../getting_started/SettingsEditor>`

The settings in the file should look like the following:

.. code:: yaml

    DataProviders:
      OpenObserve:
        Args:
          connection_str: openobserve_url
          user:
          password:

We strongly recommend storing the password value
in Azure Key Vault. You can replace the text value with a referenced
to a Key Vault secret using the MSTICPy configuration editor.

Your configuration when using Key Vault should look like the following:

.. code:: yaml

    DataProviders:
      OpenObserve:
        Args:
          connection_str: openobserve_url
          user:
          password:
            KeyVault:

Loading a QueryProvider for OpenObserve
-------------------------------------------

.. code:: ipython3

        qry_prov = QueryProvider("OpenObserve")


Connecting to OpenObserve
-----------------------------

The parameters required for connection to OpenObserve can be passed in
a number of ways. The simplest is to configure your settings
in msticpyconfig. You can then just call connect with no parameters.

Alternatively, you can pass the required connection parameters
to the driver as parameters to the driver.

.. code:: ipython3

        qry_prov.connect()


If you have configured multiple instances you must specify
an instance name when you call connect.

.. code:: ipython3

        qry_prov.connect(instance="Tenant2")

Running a OpenObserve query
-------------------------

OpenObserve supports a number of optional query time parameters.
Details of those parameters can be found here
:py:meth:`msticpy.data.drivers.openobserve_driver.query`

Be mindful that there is no standard schema by default in openobserve
and streams (aka table) naming is depending on setup choice.
Review corresponding streams before digging further.
Also know, that by default only a _timestamp field matching ingestion
or received time exists. the logs or message time must be extracted
through pipelines.

.. code:: ipython3

    df_streams = openobserve_prov.list_streams()
    df_streams[df_streams['stream_type'] == 'logs'][['name']].head()

    query = """SELECT host_name as "host_name",
       min(_timestamp) as "firstseen",
       max(_timestamp) as "lastseen",
       count() as "count"
       FROM "journald"  GROUP BY host_name
    """"
    df = openobserve_prov.exec_query(query, days=1, verbosity=3)
    df.head()

.. code:: ipython3

    query = """SELECT...
    df = openobserve_prov.exec_query(
        query,
        start=datetime.now() - timedelta(days=6.001),
        end=datetime.now() - timedelta(days=6)
    )
    df.head()

Other OpenObserve Documentation
-----------------------------

For examples of using the OpenObserve provider, see the sample
`OpenObserve Notebook <https://github.com/microsoft/msticpy/blob/master/docs/notebooks/OpenObserve-DataConnector.ipynb>`

Built-in :ref:`data_acquisition/DataQueries:Queries for OpenObserve`.

:py:mod:`OpenObserve driver API documentation<msticpy.data.drivers.openobserve_driver>`
