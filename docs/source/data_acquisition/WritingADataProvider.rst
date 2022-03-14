Writing and Contributing a Data Provider
========================================

A data provider lets you query data from a notebook in a standardized
way. A provider is implemented by adding a driver class for that provider.
The driver class encapsulates the following functionality:

- Authentication to the service (usually from configuration values but
  can also support specifying parameters such as passwords at run time).
- Querying data from the service - queries can be either:

  - ad hoc queries as strings
  - templated queries allowing substitutable parameters for common items
    such as time range, account and host names, etc.

- Returning the data as a pandas DataFrame. The driver is responsible for
  converting data types if needed. This is particularly important for
  ``datetime`` data that is usually returned as a string. Most MSTICPy
  functionality expects ``datetime`` in a timezone-aware pandas Timestamp.

Implementing a data provider
----------------------------

To implement a data provider you need to do the following:

1. Write the the driver class.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This must be derived from :py:class:`msticpy.data.drivers.driver_base.DriverBase`
You should implement the following methods:

- ``__init__``
- ``connect``
- ``query``
- ``query_with_results`` (optional)

Also see :ref:`Driver customization` below.

\_\_init\_\_
^^^^^^^^^^^^

See :py:meth:`<msticpy.data.drivers.driver_base.DriverBase.__init__>`

This initializes your driver with anything it needs to load.
It should call ``super().__init__(**kwargs)``
Keyword arguments are passed from the :py:class:`<msticpy.data.core.data_providers.QueryProvider>`
class when it is initialized with your provider name.
These ``kwargs`` will always include ``data_environment`` - the name of your provider
(see :py:class:`<msticpy.data.core.query_defns.DataEnvironment>`) and may include the
bool ``debug``, which you can use to output optional debug information.
Any other ``kwargs`` from QueryProvider are passed to your driver class.

At minimum you should set the instance attribute ``self._loaded`` to True when you
driver ``__init__`` completes successfully.

connect
^^^^^^^

See :py:meth:`<msticpy.data.drivers.driver_base.DriverBase.connect>`

This method is called from QueryProvider connect and is used to authenticate to
the service. It takes and optional `connect_str` parameter and a ``kwargs``
keyword argument dictionary.
Any per-connection configuration settings can be read in here using the
``DriverBase._get_config_settings(ProviderName)`` method. This returns the ``args``
section of your configuration settings from ``msticpyconfig.yaml``

Some existing drivers use an API key to authenticate, some use name/password and others
use Azure AAD (see :py:class:`<msticpy.data.drivers.kql_driver.KqlDriver>` for an example
of the latter.)

On successful authentication set ``self._connected`` to True.
On failure you can raise a :py:class:`<msticpy.common.exceptions.MsticpyConnectionError>`
and provide more details to the user for the reasons. See
:py:class:`<msticpy.data.drivers.kql_driver.SplunkDriver>` for an example.

query
^^^^^

See :py:meth:`<msticpy.data.drivers.driver_base.DriverBase.query>`

This takes the following parameters:

- ``query`` - string of query text
- ``query_source`` - this is populated if the query is a MSTICPy template query
  read from a query yaml file (see :ref:`Creating new queries`) and is an instance
  of :py:class:`<msticpy.data.core.query_source.QuerySource>`. This is a representation
  of the yaml query with extracted parameters and metadata available as explicit
  attributes
- ``kwargs`` - any other keyword arguments passed when running the query that are
  not consumed as query parameters, etc.

This method should submit the query to the service and handle the returned data.
The data should be returned as a pandas DataFrame.

.. note:: You should convert data types to their expected format. For example,
    dates and numeric values are often returned as strings. It is particularly
    important to convert date time values. MSTICPy expects ``datetimes`` to be
    pandas Timestamp format and timezone-aware (usually UTC but this is not
    mandatory)

In case of a query failure, it can return the failure response.

query\_with\_results
^^^^^^^^^^^^^^^^^^^^

See :py:meth:`<msticpy.data.drivers.driver_base.DriverBase.query_with_results>`

Implementing this is optional, it can be used if you need to be able to return
the raw response as well as the data in dataframe format. However, this
method isn't exposed in the data provider framework - so is more for
experimentation/debugging purposes. The ``query`` method can call this method
to avoid duplication of code.


Driver customization
~~~~~~~~~~~~~~~~~~~~

Exposing attributes via the QueryProvider
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

:py:class:`<msticpy.data.core.data_providers.QueryProvider>` is a facade class
for the driver classes. The user interacts with the former but not directly
with the latter.

If you want to expose and attribute from the driver class as an attribute
of query provider you can do the following:

- implement the attribute in the driver (this can be a method or other type)
- set ``self.public_attribs`` to a Python dictionary of ``{ name: value }``
  where ``name`` is the name of the attribute you want to appear and value
  is the value of the attribute supplied by the driver.


.. code:: Python3

    self.public_attribs = {
            "client": self.service,
            "saved_searches": self._saved_searches,
            "fired_alerts": self._fired_alerts,
        }

Custom parameter formatting
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The format for Dates and lists differ between different query languages. The
driver can implement a custom formatter to render datetime or list parameters
into the correct format before being substituted into the query string.

Datetime formatter functions should take a Python ``datetime`` and return a string
List formatter functions should take an Iterable and return a string.

.. code:: Python3

    # Parameter Formatting methods
    @staticmethod
    def _format_datetime(date_time: datetime) -> str:
        """Return datetime-formatted string."""
        return f'"{date_time.isoformat(sep=" ")}"'

    @staticmethod
    def _format_list(param_list: Iterable[Any]) -> str:
        """Return formatted list parameter."""
        fmt_list = [f'"{item}"' for item in param_list]
        return ",".join(fmt_list)

You must register these functions in the driver ``__init__`` method as
follows:

.. code:: Python3

    self.formatters = {
            Formatters.DATETIME: self._format_datetime,
            Formatters.LIST: self._format_list,
        }

See :py:class:`<msticpy.data.drivers.kql_driver.SplunkDriver>` for an example.

Customizing the query parameter substitution
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

MSTICPy uses the Python str.format method to substitute named parameters.
Here is an example query in a query yaml file:

.. code-block:: YAML
  :emphasize-lines: 6, 7, 8, 9, 10

    sources:
        list_files:
            description: Lists all file events by filename
            metadata:
            args:
            query: '
                {table}
                | where Timestamp >= datetime({start})
                | where Timestamp <= datetime({end})
                | where FileName has "{file_name}"
                {add_query_items}'

If you need to include explicit brace characters in the string
you can escape the substitution using double braces sequences: ``{{``
and ``}}``

While this works well for most query languages, in some cases (like
queries expressed as JSON strings), replacing all braces with escaped
double-braces is onerous. In this case you can opt to do the parameter
substitution in the driver itself. To do this implement a method that
expects two parameters:

- query - the raw query string from the yaml file
- param_dict - a dictionary of parameter name, parameter value

The param_dict values will already have been formatted into a suitable
string format using any methods you specified in :ref:`Custom parameter formatting`.
Substitute the parameter values into the raw query string and
return the query string. The query string will be passed to your driver's
query method.

You need to register the parameter substitution function in your driver's
``__init__`` method

.. code:: Python3

    self.formatters = {
            Formatters.PARAM_HANDLER: self._custom_param_handler,
            Formatters.DATETIME: self._format_datetime,
            Formatters.LIST: self._format_list,
        }

Registering the driver
----------------------

There are two updates to classes that you need to make to register your driver.

Add the provider as a DataEnvironment
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In the enum :py:class:`<msticpy.data.core.query_defns.DataEnvironment>` add an entry
for your provider using the next available enum value.

.. code-block:: Python3
  :emphasize-lines: 21

    @export
    class DataEnvironment(Enum):
        """
        Enumeration of data environments.

        Used to identify which queries are relevant for which
        data sources.
        """

        Unknown = 0
        AzureSentinel = 1  # alias of LogAnalytics
        LogAnalytics = 1
        MSSentinel = 1
        Kusto = 2
        ...
        ResourceGraph = 9
        Sumologic = 10
        M365D = 11
        Cybereason = 12
        Elastic = 14
        YourProvider = 15

You can also add aliases by re-using the same value(see the MSSentinel, AzureSentinel,
LogAnalytics, entries.)

Add an entry to the driver dynamic load table
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In the ``__init__.py`` module of data drivers
(`<https://github.com/microsoft/msticpy/tree/main/msticpy/data/drivers/__init__.py>`_)

.. code-block: Python3
  :emphasize-lines: 10

    _ENVIRONMENT_DRIVERS = {
        DataEnvironment.LogAnalytics: ("kql_driver", "KqlDriver"),
        DataEnvironment.AzureSecurityCenter: ("kql_driver", "KqlDriver"),
        DataEnvironment.SecurityGraph: ("security_graph_driver", "SecurityGraphDriver"),
        DataEnvironment.Kusto: ("kusto_driver", "KustoDriver"),
        DataEnvironment.MDATP: ("mdatp_driver", "MDATPDriver"),
        ...
        DataEnvironment.Cybereason: ("cybereason_driver", "CybereasonDriver"),
        DataEnvironment.Elastic: ("elastic_driver", "ElasticDriver"),
        DataEnvironment.YourProvider: ("your_module", "YourDriverClassName"),
    }


Queries
-------

Create a folder in msticpy/data/queries with the name of your provider and
add queries. For more details on creating queries, see :ref:`Creating new queries`

Query parameter names
---------------------

While you can choose whatever parameter names you like for your queries,
certain functionality in MSTICPy (e.g. Pivot functions) will uses
standardized names to add additional functionality. For example, all
queries with the ``host_name`` parameter are automatically added
as enrichment functions to the :py:class:`<msticpy.datamodel.entities.Host>` entity.

This is a list of commonly used parameter names:

==================  =================================
Parameter name      Use
==================  =================================
start               Query start time
end                 Query end time
account_name        User account name
commandline         Process command line
domain              DNS domain name
file_hash           File hash string
host_name           Host name (FQDN or simple)
ip_address          Dotted IP address string
logon_session_id    user logon session
process_id          process ID
process_name        Process or file name
resource_id         Azure resource ID
url                 URL
==================  =================================


Documentation
-------------

A data provider should have documentation describing its configuration and use.
This should be in restructured text for generating document pages
in Sphinx.

See the examples :doc:`./SplunkProvider` :doc:`DataProv-Sumologic`

Unit Tests
----------

Please add a unit test using mocks to simulate the service
responses. Code coverage should be at least 80%.

Do no add unit tests that call the live service. You can include
tests that do this but you must mark them as to be skipped during normal
unit test runs.


See the examples in
`MSTICPy data drivers unit tests<https://github.com/microsoft/msticpy/tree/main/tests/data/drivers>`_

