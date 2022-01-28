Sumologic Provider
==================

Sumologic Configuration
-----------------------

You can store your connection details in *msticpyconfig.yaml*.

For more information on using and configuring *msticpyconfig.yaml* see
:doc:`msticpy Package Configuration <../getting_started/msticpyconfig>`
and :doc:`MSTICPy Settings Editor<../getting_started/SettingsEditor>`

The settings in the file should look like the following:

.. code:: yaml

    DataProviders:
      Sumologic:
        Args:
          connection_str: Sumologic_url_endpoint
          accessid: accessid   # with search permissions to connect
          accesskey: accesskey

We strongly recommend storing the client secret value
in Azure Key Vault. You can replace the text value with a referenced
to a Key Vault secret using the MSTICPy configuration editor.

Your configuration when using Key Vault should look like the following:

.. code:: yaml

    DataProviders:
      Sumologic:
        Args:
          connection_str: Sumologic_url_endpoint
          accessid: accessid   # with search permissions to connect
          accesskey:
            KeyVault:

Loading a QueryProvider for Sumologic
-------------------------------------------

.. code:: ipython3

        qry_prov = QueryProvider("Sumologic")


Connecting to Sumologic
-----------------------------

The parameters required for connection to Sumologic can be passed in
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

Running a Sumologic query
-------------------------

Sumologic supports a number of optional query time parameters.
Details of those parameters can be found here
:py:meth:`msticpy.data.drivers.sumologic_driver.query`

.. code:: ipython3

    sumologic_query = """
        *
        | formatDate(_messageTime,"yyyy/dd/MM HH:mm:ss") as date
        | first(date), last(date) by _sourceCategory
        | count _sourceCategory,_first,_last
        | sort -_count
        """"
    df = sumologic_prov.exec_query(sumologic_query, days=0.0005, verbosity=3)
    df.head()

.. code:: ipython3

    sumologic_query = "_index=WINDOWS | count _sourceCategory,hostname"
    df = sumologic_prov.exec_query(
        sumologic_query,
        start=datetime.now() - timedelta(days=6.001),
        end=datetime.now() - timedelta(days=6)
    )
    df.head()

Other Sumologic Documentation
-----------------------------

For examples of using the Sumologic provider, see the sample
`Sumologic Notebook<https://github.com/microsoft/msticpy/blob/master/docs/notebooks/Sumologic-DataConnector.ipynb>`

Built-in :ref:`data_acquisition/DataQueries:Queries for Sumologic`.

:py:mod:`Sumologic driver API documentation<msticpy.data.drivers.sumologic_driver>`
