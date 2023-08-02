Writing and Contributing a Data Provider
========================================

See :doc:`../data_acquisition/DataProviders` for more details on use
of data providers.

A data provider lets you query data from a notebook in a standardized way.
Before reading further you should familiarize yourself with how the data
providers work from the :doc:`Querying and Importing Data <../DataAcquisition>`
section of the MSTICPy documentation.

The term provider is more a concept than defining a single piece of code.
There are several components to a provider, the most important of which is
a driver. The driver class encapsulates the following functionality:

- Authentication to the service (usually from configuration values but
  can also support specifying parameters such as passwords at run time).
- Querying data from the service - queries can be either:

  - ad hoc queries as strings
  - templated queries allowing substitutable parameters for common items
    such as time range, account and host names, etc.

- Returning the data as a pandas *DataFrame*. The driver is responsible for
  converting data types if needed. This is particularly important for
  *datetime* data that is usually returned as a string. Most MSTICPy
  functionality expects *datetime* in a timezone-aware pandas Timestamp.

Implementing a data provider
----------------------------

To implement a data provider you need to do the following:

1. Write the driver
2. Customize the driver (optional)
3. Register the driver
4. Add queries
5. Add settings definition
6. Create documentation
7. Create unit tests

1. Write the driver class
-------------------------

This must be derived from :py:class:`DriverBase <msticpy.data.drivers.driver_base.DriverBase>`
(`DriverBase source <https://github.com/microsoft/msticpy/tree/main/msticpy/data/drivers/driver_base.py>`__).
You should implement the following methods:

- ``__init__``
- ``connect``
- ``query``
- ``query_with_results`` (optional)

Also see `2. Customize the driver`_ below.

\_\_init\_\_
~~~~~~~~~~~~

See :py:meth:`DriverBase.__init__ <msticpy.data.drivers.driver_base.DriverBase.__init__>`

This initializes your driver with anything it needs to load.
It should call ``super().__init__(**kwargs)``.

Keyword arguments are passed from the :py:class:`QueryProvider <msticpy.data.data_providers.QueryProvider>`
class when it is initialized with your provider name.
These ``kwargs`` will always include ``data_environment`` - the name of your provider
(see :py:class:`DataEnvironment <msticpy.data.query_defns.DataEnvironment>`) and may include the
bool ``debug``, which you can use to output optional debug information.
Any other ``kwargs`` from QueryProvider are passed to your driver class.

At minimum you should set the instance attribute ``self._loaded`` to True when your
driver ``__init__`` completes successfully.

connect
~~~~~~~

See :py:meth:`DriverBase.connect <msticpy.data.drivers.driver_base.DriverBase.connect>`

This method is called from ``QueryProvider.connect`` and is used to authenticate to
the data service. It takes and optional ``connect_str`` parameter and a ``kwargs``
keyword argument dictionary.

Any per-connection configuration settings can be read in here using the
``DriverBase._get_config_settings(ProviderName)`` method. This returns the ``args``
section of your configuration settings from ``msticpyconfig.yaml``.

Some existing drivers use an API key to authenticate, some use name/password and others
use Azure Active Directory (AAD). See :py:class:`KqlDriver <msticpy.data.drivers.kql_driver.KqlDriver>`
(`KqlDriver source <https://github.com/microsoft/msticpy/tree/main/msticpy/data/drivers/kql_driver.py>`__)
for an example
of the latter.)

On successful authentication, set ``self._connected`` to True.
On failure, you can raise a :py:class:`MsticpyConnectionError <msticpy.common.exceptions.MsticpyConnectionError>`
and provide more details to the user for the reasons. See
:py:class:`SplunkDriver <msticpy.data.drivers.splunk_driver.SplunkDriver>` for an example.

query
~~~~~

See :py:meth:`DriverBase.query <msticpy.data.drivers.driver_base.DriverBase.query>`

This takes the following parameters:

- ``query`` - string of query text
- ``query_source`` - this is populated if the query is a MSTICPy template query
  read from a query yaml file (see
  :doc:`Creating new queries <../extending/Queries>`)
  and is an instance of
  :py:class:`QuerySource <msticpy.data.query_source.QuerySource>`. This is a representation
  of the yaml query with extracted parameters and metadata available as explicit
  attributes
- ``kwargs`` - any other keyword arguments passed when running the query that are
  not consumed as query parameters, etc.

This method should submit the query to the service and handle the returned data.
The data should be returned as a pandas *DataFrame*.

.. note:: You should convert data types to their expected format. For example,
    dates and numeric values are often returned as strings. It is particularly
    important to convert *datetime* values. MSTICPy expects *datetime* to be
    pandas *Timestamp* format and timezone-aware (usually UTC but this is not
    mandatory)

In case of a query failure, it can return the failure response instead of a *DataFrame*.

query\_with\_results
~~~~~~~~~~~~~~~~~~~~

See :py:meth:`DriverBase.query_with_results <msticpy.data.drivers.driver_base.DriverBase.query_with_results>`

Implementing this is optional, it can be used if you need to be able to return
the raw response as well as the data in *DataFrame* format. However, this
method isn't exposed in the data provider framework - so is more for
experimentation/debugging purposes. The ``query`` method can call this method
to avoid duplication of code.

If you do not implement any logic for this you must still create a dummy
method in your class ``query_with_results`` and return None, None.



2. Customize the driver
-----------------------

This section is optional but is needed for many providers.

Exposing attributes via the QueryProvider
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

:py:class:`QueryProvider <msticpy.data.data_providers.QueryProvider>` is a facade class
for the driver classes. The user interacts with the former but not directly
with the latter.

If you want to expose an attribute from the driver class as an attribute
of query provider you can do the following:

- implement the attribute that you want to expose in the driver
  (this can be a method or other type)
- set ``self.public_attribs`` to a Python dictionary of ``{ name: value }``
  where ``name`` is the name of the attribute you want to appear and value
  is the value of the attribute supplied by the driver, as shown the example
  below.


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

*Datetime* formatter functions should take a Python *datetime* and return a string.
*List* formatter functions should take an *Iterable* and return a string.

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

See :py:class:`SplunkDriver <msticpy.data.drivers.splunk_driver.SplunkDriver>`
(`SplunkDriver source <https://github.com/microsoft/msticpy/tree/main/msticpy/data/drivers/splunk_driver.py>`__)
for an example.

Code:

Customizing the query parameter substitution
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

Each value surrounded by braces is considered to be a substitutable parameter
name. If you need to include explicit brace characters in the string you can
escape the substitution using double braces sequences: ``{{``
and ``}}``. These get converted to single braces by str.format().

While this works well for most query languages, in some cases (like
queries expressed as JSON strings), replacing all braces with escaped
double-braces is onerous. In this case you can opt to do the parameter
substitution in the driver itself. To do this implement a method that
expects two parameters:

- query - the raw query string from the yaml file
- param_dict - a dictionary of parameter name, parameter value

The param_dict values will already have been formatted into a suitable
string format using any methods you specified in `Custom parameter formatting`_.
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

3. Register the driver
----------------------

There are two updates to classes that you need to make to register your driver.

Add the provider as a DataEnvironment
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In the enum :py:class:`DataEnvironment <msticpy.data.query_defns.DataEnvironment>`
(`DataEnvironments source <https://github.com/microsoft/msticpy/tree/main/msticpy/data/query_defns.py>`__)
add an entry for your provider using the next available enum value.

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
(`drivers sub-package __init__ source <https://github.com/microsoft/msticpy/tree/main/msticpy/data/drivers/__init__.py>`__)

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


4. Add queries
--------------

Create a folder in msticpy/data/queries with the name of your *DataEnvironment* and
add queries. The folder name must match the item that you added to the DataEnvironment
Enum class in step 3 above. The For more details on creating queries, see
:doc:`Creating new queries <../extending/Queries>`.

Query parameter names
~~~~~~~~~~~~~~~~~~~~~

While you can choose whatever parameter names you like for your queries,
certain functionality in MSTICPy (e.g. Pivot functions) will use
standardized names to add additional functionality. For example, all
queries with the ``host_name`` parameter are automatically added
as enrichment functions to the :py:class:`Host entity <msticpy.datamodel.entities.Host>`.

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
logon_session_id    User logon session
process_id          Process ID
process_name        Process or file name
resource_id         Azure resource ID
url                 URL
==================  =================================

5. Add settings definition
--------------------------

MSTICPy's settings editor uses configuration from a YAML file to
create UI settings. This allows user's to set settings interactively.

Define whatever settings you need as sub-keys of the *args* key

.. code:: YAML

    DataProviders:
      MicrosoftDefender:
        Args:
          ClientId: str(format=uuid)
          TenantId: str(format=uuid)
          # [SuppressMessage("Microsoft.Security", "CS002:SecretInNextLine", Justification="Test code")]
          ClientSecret: *cred_key

Use the examples and documentation in
`mpconfig_defaults.yaml <https://github.com/microsoft/msticpy/tree/main/msticpy/resources/mpconfig_defaults.yaml>`_
to specify your settings.

The special value ``*cred_key`` is a YAML macro and used where you need to store
a secret of some kind. Items of this type allow the user to store the value
in an environment variable or as an Azure Key Vault secret rather than
in the msticpyconfig file.


6. Add provider documentation
-----------------------------

A data provider should have documentation describing its configuration and use.
This should be in restructured text for generating document pages
in Sphinx.

See the examples :doc:`../data_acquisition/SplunkProvider` and :doc:`../data_acquisition/DataProv-Sumologic`

7. Create driver unit tests
---------------------------

Please add a unit test using mocks to simulate the service
responses. Code coverage should be at least 80%.

Do no add unit tests that call the live service. You can include
tests that do this but you must mark them as to be skipped during normal
unit test runs.

See the examples in
`MSTICPy data drivers unit tests <https://github.com/microsoft/msticpy/tree/main/tests/data/drivers>`_
