The OSQuery provider
====================

:py:mod:`OSQuery driver documentation<msticpy.data.drivers.local_os_query_driver>`

The ``OSQuery`` data provider can read OSQuery log files
and provide convenient query functions for each OSQuery "table"
(or event type) contained in the logs.

The provider can read in one or more log files, or multiple log files
in multiple folders. The files are read, converted to pandas
DataFrames and grouped by table/event. In addition, date fields
within the data are converted to pandas Timestamp format.

.. code::ipython3

    qry_prov = mp.QueryProvider("OSQueryLogs", data_paths=["~/my_logs"])
    qry_prov.connect()
    df_processes = qry_prov.os_query.processes()

The query provider query functions will ignore parameters and do
no further filtering. You can use pandas to do additional filtering
and sorting of the data, or use it directly with other MSTICPy
functionality.

OSQuery Configuration
---------------------

You can store your connection details in *msticpyconfig.yaml*,
instead of supplying the ``data_paths`` parameter to
the ``QueryProvider`` class.

For more information on using and configuring *msticpyconfig.yaml* see
:doc:`msticpy Package Configuration <../getting_started/msticpyconfig>`
and :doc:`MSTICPy Settings Editor<../getting_started/SettingsEditor>`

The OSQuery settings in the file should look like the following:

.. code:: yaml

    DataProviders:
        ...
        OSQuery:
            data_paths:
                - /home/user1/sample_data
                - /home/shared/sample_data
            cache_file: ~/.msticpy/os_query_cache.pkl

The cache_file entry is explained later.

Expected log file format
------------------------

The log file format must be a text file of JSON records. An example
is shown below

.. parsed-literal::

    {"name":"pack_osquery-snapshots-pack_python_packages","hostIdentifier":"jumpvm","calendarTime":"Thu Mar 16 09:22:33 2023 UTC","unixTime":1678958553,"epoch":0,"counter":0,"numerics":false,"decorations":{"host_uuid":"40443dd9-5b21-a345-8f89-aadde84c3719","username":"LOGIN"},"columns":{"author":"Python Packaging Authority","directory":"/usr/lib/python3.9/site-packages/","license":"UNKNOWN","name":"setuptools","path":"/usr/lib/python3.9/site-packages/setuptools-50.3.2.dist-info/","summary":"Easily download, build, install, upgrade, and uninstall Python packages","version":"50.3.2"},"action":"snapshot"}
    {"name":"pack_osquery-snapshots-pack_dns_resolvers","hostIdentifier":"jumpvm","calendarTime":"Thu Mar 16 13:14:10 2023 UTC","unixTime":1678972450,"epoch":0,"counter":0,"numerics":false,"decorations":{"host_uuid":"40443dd9-5b21-a345-8f89-aadde84c3719","username":"LOGIN"},"columns":{"address":"168.63.129.16","id":"0","netmask":"32","options":"705","type":"nameserver"},"action":"snapshot"}

Each JSON record is expected to have a ``name`` field, identifying
the event type, along with child dictionaries (``columns`` and ``decorations``.

.. code::JSON

    {
        "name": "pack_osquery-snapshots-pack_dns_resolvers",
        "hostIdentifier": "jumpvm",
        "calendarTime": "Thu Mar 16 13:14:10 2023 UTC",
        "unixTime": 1678972450,
        "epoch": 0,
        "counter": 0,
        "numerics": false,
        "decorations": {
            "host_uuid": "40443dd9-5b21-a345-8f89-aadde84c3719",
            "username": "LOGIN"
        },
        "columns": {
            "address": "u5r0qfkczeeejf3qb20cha0ihb.bx.internal.cloudapp.net",
            "id": "0",
            "netmask": "",
            "options": "705",
            "type": "search"
        },
        "action": "snapshot"
    }

Using the OSQuery provider
--------------------------

To use the OSQuery provider you need to create an QueryProvider
instance, passing the string "OSQueryLogs" as the ``data_environment``
parameter. If you have not configured ``data_paths`` in msticpyconfig.yaml,
you also need to add the ``data_paths`` parameter to specify
specific folders or files that you want to read.

.. code::ipython3

    qry_prov = mp.QueryProvider("OSQueryLogs", data_paths=["~/my_logs"])

Calling the ``connect`` method triggers the provider to read the
log files.

.. code::ipython3

    qry_prov.connect()

.. parsed-literal::

    100%|██████████| 2/2 [00:00<00:00, 25.01it/s]
    Data loaded.


Listing OSQuery tables
~~~~~~~~~~~~~~~~~~~~~~

.. code:: ipython3

    qry_prov.list_queries()

.. parsed-literal::

    ['osquery.acpi_tables',
    'osquery.device_nodes',
    'osquery.dns_resolvers',
    'osquery.events',
    'osquery.fim',
    'osquery.last',
    'osquery.listening_ports',
    'osquery.logged_in_users',
    'osquery.mounts',
    'osquery.open_sockets',
    'osquery.osquery_info',
    'osquery.osquery_packs',
    'osquery.osquerydb_size',
    'osquery.platform_info',
    'osquery.process_memory',
    'osquery.processes',
    'osquery.python_packages',
    'osquery.schedule',
    'osquery.shell_history']

Running an OSQuery query
~~~~~~~~~~~~~~~~~~~~~~~~

Each query returns the table of event types retrieved
from the logs.

.. code:: python3

    qry_prov.osquery.processes()

==================================  ================  =========================  =====  ==========  =========  ======  ========  ========  =====  ==========
name                                hostIdentifier    unixTime                    ...   username    cmdline      euid  tname\_     parent    uid  username
==================================  ================  =========================  =====  ==========  =========  ======  ========  ========  =====  ==========
pack_osquery-custom-pack_processes  jumpvm            2023-03-16 03:08:58+00:00   ...   LOGIN                       0  kthreadd         2      0  root
pack_osquery-custom-pack_processes  jumpvm            2023-03-16 03:08:58+00:00   ...   LOGIN                       0  kthreadd         2      0  root
pack_osquery-custom-pack_processes  jumpvm            2023-03-16 03:08:58+00:00   ...   LOGIN                       0  kthreadd         2      0  root
pack_osquery-custom-pack_processes  jumpvm            2023-03-16 03:08:58+00:00   ...   LOGIN                       0  kthreadd         2      0  root
pack_osquery-custom-pack_processes  jumpvm            2023-03-16 03:08:58+00:00   ...   LOGIN                       0  kthreadd         2      0  root
==================================  ================  =========================  =====  ==========  =========  ======  ========  ========  =====  ==========

.. note:: Columns in the the nested log data may be renamed
    if their name clashes with an existing name. See the
    example ``name_`` in the previous table.

Other OSQuery Provider Documentation
------------------------------------


Built-in :ref:`data_acquisition/DataQueries:Queries for Local Data`.

:py:mod:`LocalData driver API documentation<msticpy.data.drivers.local_os_query_driver>`
