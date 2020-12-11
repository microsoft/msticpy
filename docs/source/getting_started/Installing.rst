Installing
==========


Python 3.6 or Later
-------------------

*msticpy* requires Python 3.6 or later.
If you are running in hosted environment such as Azure Notebooks,
Python is already installed. Please ensure that the Python 3.6 (or later)
kernel is selected for your notebooks.

If you are running the notebooks locally, you will need to install Python 3.6
or later. The Ananconda distribution is a good starting point since it comes
with many of packages required by *msticpy* pre-installed.

Creating a virtual environment
------------------------------

.. note:: This is an optional step. You will most likely want to do this
   if you are installing msticpy in a local Python installation. If
   you are using a cloud notebook environment such as Azure ML you
   will usually not need to create a virtual environment.

*msticpy* has a significant number of dependencies. To avoid conflicts
with packages in your existing Python environment you may want to
create a Python virtual environment
or a conda environment and install the package there.

For standard python use the ``venv`` command to do this
(there are also several alternatives to venv available).

.. code:: bash

    ~$ python -m venv my_env
    ~$ ./my_env/scripts/activate
    (my_env) ~$

For Conda use the conda ``create`` command from a conda shell.

.. code:: bash

    (base) c:\users\ian> conda create -n my_env
    (base) c:\users\ian> conda activate my_env
    (my_env) c:\users\ian>

You should see the name of the environment that you've just
created and activated in the prompt.


Installation
------------

Run the following command to install the base configuation of *msticpy*.


``pip install msticpy``

or for the latest dev build

``pip install git+https://github.com/microsoft/msticpy``


Selective Installation - using "extras"
---------------------------------------

pip supports specification of an additional parameter sequence
known as extras. The syntax for this is:

``pip install package_name[extra1, extra2...]``

As of version 0.9.0 *msticpy* has its dependencies split into
extras. This allows you to install only the packages that you
need and avoid the overhead of time and diskspace of dependencies
that you do not need.

.. note:: extras do not affect the which code from *msticpy* is
   installed - only the external libraries on which certain
   functions inside *msticpy* need to work.

Extras in msticpy
~~~~~~~~~~~~~~~~~

The extras available in *msticpy* are described in the following table:

+-------------------------+------------------------------------+--------------+
| extra                   | Functionality                      | Install time |
+=========================+====================================+==============+
| default install         | - Most functionality (approx 75%)  |    45s       |
| (no extra)              |                                    |              |
+-------------------------+------------------------------------+--------------+
| keyvault                | - Key Vault and keyring storage of |     5s       |
|                         |   settings secrets                 |              |
+-------------------------+------------------------------------+--------------+
| azure                   | - Azure API data retrieval         |  1m:05s      |
|                         |   (subs, resources, Vms, etc.)     |              |
|                         | - Azure storage APIs               |              |
|                         | - Azure Sentinel APIs (not data    |              |
|                         |   query)                           |              |
|                         | - Also includes "keyvault"         |              |
+-------------------------+------------------------------------+--------------+
| kql                     | - Azure Sentinel data queries      |  2m:00s      |
|                         | - Kqlmagic                         |              |
+-------------------------+------------------------------------+--------------+
| azuresentinel           | - Combination of default install   |  3m:10s      |
|                         |   plus "azure", "keyvault" and     |              |
|                         |   "kql"                            |              |
+-------------------------+------------------------------------+--------------+
| ml                      | - Timeseries analysis              |    25s       |
|                         | - Event clustering                 |              |
|                         | - Outlier analysis                 |              |
+-------------------------+------------------------------------+--------------+
| splunk                  | - Splunk data queries              |     3s       |
+-------------------------+------------------------------------+--------------+
| vt3                     | - VirusTotal V3 graph API          |     4s       |
|                         |   (default VT lookup is included   |              |
|                         |   in base install)                 |              |
+-------------------------+------------------------------------+--------------+
| all                     | - Includes all of above packages   |  4m:00s      |
+-------------------------+------------------------------------+--------------+
| dev                     | - Development tools plus "base"    |  1m:00s      |
+-------------------------+------------------------------------+--------------+
| test                    | - "dev" plus "all"                 |  4m:20s      |
+-------------------------+------------------------------------+--------------+

If you do not specify an "extra" in your pip install command, the base
dependencies for msticpy will be installed. This has a lot of functionality
such as networking, pivoting, visualization but excludes dependencies
that are specific to a particular data environment like Azure Sentinel or
Splunk.

Some of the extras, like "all" and "azuresentinel" are combinations of
other options collected together as a convenience. You can also specify
multiple during install.

.. code:: bash

    pip install msticpy[azure, kql]

If you try to use functionality for a component that needs a dependency
that you have not installed you will usually get an informative
exception message telling you which "extra" option you need to use
to enable that feature.


.. figure:: _static/extra_exception.png
   :alt: Exception when trying to use a function that is not installed.
   :height: 3in

To fix this simply run pip install with the "extra" option shown in the
exception message:

.. code:: bash

    pip install msticpy[ml]

.. note:: In some cases you many not get an informative error. If
   experience a problem with some *msticpy* functionality, make sure
   that you have installed the *extra* that corresponds to the
   function you are trying to run.
