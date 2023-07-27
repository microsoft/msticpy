Writing Threat Intelligence and Context Providers
=================================================

See :doc:`../data_acquisition/TIProviders` for more details on use
of Threat Intelligence providers.

You can write your own provider by extending one
of the MSTICPy base classes:

- :py:class:`TIProvider <msticpy.context.tiproviders.ti_provider_base.TIProvider>`
- :py:class:`HttpTIProvider <msticpy.context.tiproviders.ti_http_provider.HttpTIProvider>`
- :py:class:`ContextProvider <msticpy.context.contextproviders.context_provider_base.ContextProvider>`

The first two classes are for creating Threat Intelligence providers,
the third is for a Context provider. Most of the content of this
document is applicable to both so you should read the entire
thing. A short section on the differences for Context providers
follows this first section.


Threat Intelligence Providers
-----------------------------

For most TI services you can use
:py:class:`HttpTIProvider <msticpy.context.tiproviders.ti_http_provider.HttpTIProvider>`.

You can derive a class from
:py:class:`TIProvider <msticpy.context.tiproviders.ti_provider_base.TIProvider>` but
here, you have to implement all of the logic and plumbing to query
your data source by implementing a lookup_ioc and lookup_ioc_asycnc
methods. If you need to do this look at the
`OpenPageRank provider <https://github.com/microsoft/msticpy/blob/main/msticpy/context/tiproviders/open_page_rank.py>`__
for an example.

The HttpTIProvider base class
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This has built-in support for making and processing the requests
to the service API. You typically need to define:

- The base API URL
- A mapping of IoC types to URL paths or query strings
- What authentication credentials the service needs and
  how these will be passed in the requests
- A function that processes the response and extracts
  severity and summary details.

Here is a toy example:

.. code:: python3

    from typing import Any, Dict, Tuple

    from msticpy.context.http_provider import APILookupParams
    from msticpy.context.tiproviders.ti_http_provider import HttpTIProvider, ResultSeverity

    class TIProviderHttpTest(HttpTIProvider):
        """Custom IT provider TI HTTP."""

        PROVIDER_NAME = "MyTIProvider"
        _BASE_URL = "https://api.service.com"
        _QUERIES = _QUERIES = {
            "ipv4": APILookupParams(path="/api/v1/indicators/IPv4/{observable}/general"),
            "ipv6": APILookupParams(path="/api/v1/indicators/IPv6/{observable}/general"),
            "file_hash": APILookupParams(path="/api/v1/indicators/file/{observable}/general"),
            "url": APILookupParams(path="/api/v1/indicators/url/{observable}/general"),
        }
        # aliases
        _QUERIES["md5_hash"] = _QUERIES["file_hash"]
        _QUERIES["sha1_hash"] = _QUERIES["file_hash"]
        _QUERIES["sha256_hash"] = _QUERIES["file_hash"]

        _REQUIRED_PARAMS = ["AuthKey"]

        def parse_results(self, response: Dict) -> Tuple[bool, ResultSeverity, Any]:
            """Return the details of the response."""
            if response["severity"] < 5:
                severity = ResultSeverity.high
            else:
                severity = ResultSeverity.warning
            details = {
                "Source": response.get("source_domain"),
                "RelatedIPs": response.get("source_ip_addrs")
            }
            return True, severity, details


We can see that the new provider class is derived from
:py:class:`HttpTIProvider <msticpy.context.tiproviders.ti_http_provider.HttpTIProvider>`.

This expects several items defined as class attributes:

- _BASE_URL - the root URL for API calls
- _QUERIES - definitions for each IoC type to create the appropriate
  http request.
- _REQUIRED_PARAMS - the mandatory items in the request parameters
  (usually the Api Key)

You also need to implement an instance method
:py:meth:`parse_results <msticpy.context.tiproviders.ti_provider_base.parse_results>`
(see below)

The QUERIES Dictionary and APILookupParams
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The ``_QUERIES`` dictionary is the most complex part and requires
further explanation.

Each entry has a key corresponding to an IoC type (e.g. "ipv4", "url", etc.).
The value of each item is an instance of
:py:class:`APILookupParams <msticpy.context.http_provider.APILookupParams>`
which specifies the HTTP request configuration for each IoC type.

You can re-use the same entry to create aliased items that map
multiple IoC types on the same request.
You can do this by adding existing values to the dictionary
with new keys, as shown below:

.. code:: python3

        # aliases
        _QUERIES["md5_hash"] = _QUERIES["file_hash"]
        _QUERIES["sha1_hash"] = _QUERIES["file_hash"]
        _QUERIES["sha256_hash"] = _QUERIES["file_hash"]

In this example, the service provider API accepts any type of hash using
the same request parameters. Creating multiple mappings like this
lets the user specify any of these types to perform a lookup. Also,
in cases where the user does not explicitly specify an ``ioc_type`` in
the call the ``lookup_ioc``, the TILookup class will try to infer the
type using regular expression matching and will pass the inferred type
to your provider class. By creating these aliases we can map all variants
of an IoC type (in this case a hash) to this one request definition.

You can also create compound keys. This is useful where a given IoC
type has sub-queries for different classes of data related to the IoC.

Here is an example from our Alienvault OTX provider, which has a
general "ipv4" path but also several types of more specialized
queries - passive DNS and geo-location.

.. code:: python3

    _QUERIES = {
        "ipv4": _OTXParams(path="/api/v1/indicators/IPv4/{observable}/general"),
        "ipv6": _OTXParams(path="/api/v1/indicators/IPv6/{observable}/general"),
        "ipv4-passivedns": _OTXParams(
            path="/api/v1/indicators/IPv4/{observable}/passive_dns"
        ),
        "ipv6-passivedns": _OTXParams(
            path="/api/v1/indicators/IPv6/{observable}/passive_dns"
        ),
        "ipv4-geo": _OTXParams(path="/api/v1/indicators/IPv4/{observable}/geo"),
        "ipv6-geo": _OTXParams(path="/api/v1/indicators/IPv6/{observable}/geo"),
        ...

This allows users to request the specific dataset for the IoC using
the ``query_type`` parameter:

.. code:: python3

    tilookup.lookup_ioc("123.4.56.78", query_type="passivedns")


Having decided on the keys needed, you can create the APILookupParams
instances to tell the TILookup module how to form the HTTP requests.

The :py:class:`APILookupParams <msticpy.context.http_provider.APILookupParams>` class
has the following attributes:

.. code:: python3

    class APILookupParams:
        """HTTP Lookup Params definition."""

        path: str = ""
        verb: str = "GET"
        full_url: bool = False
        headers: Dict[str, str] = Factory(dict)
        params: Dict[str, Union[str, int, float]] = Factory(dict)
        data: Dict[str, str] = Factory(dict)
        auth_type: str = ""
        auth_str: List[str] = Factory(list)
        sub_type: str = ""

The value of each item in the queries dictionary should be an
instance of an ``APILookupParams`` class or one derived from it.

.. note:: APILookupParams is an attrs class, if you create a subclass
    from it you should also make that an attrs class.

Several of the values of this class can have substitutable parameters
where runtime values (e.g. the observable value)
are inserted before making the request.

**path**

The sub-path for the query for this IoC type. This will be
appended to the ``_BASE_URL`` value.

.. code:: python3

    _QUERIES = _QUERIES = {
        "ipv4": APILookupParams(path="/api/v1/indicators/IPv4/{observable}/general"),

In this example you can see that "{observable}", the IoC value, is
a substitutable parameter.

**verb**

The provider framework currently only supports "GET"

**full_url**
If True, the ``_BASE_URL`` value is ignored and the ``path`` value
is treated as a full URL and used in the request as-is.

**headers**

A dictionary of request headers.
This also supports parameter substitution of any value surrounded with
braces.

Example:

.. code:: python3

     _QUERIES = _QUERIES = {
        "ipv4": APILookupParams(
            path="/api/v1/indicators/IPv4/{observable}/general"
            headers = {"X-OTX-API-KEY": "{AuthKey}"}
        ),
        ...

**params**
A dictionary of request parameters. This also supports
parameter substitution of values:

.. code:: python3

     _QUERIES = _QUERIES = {
        "ipv4": APILookupParams(
            path="/api/v1/indicators/IPv4"
            params={"iocValue": "{observable}"},
        ),
        ...

**data**
This is currently not supported but we will implement if and
when required. This is a dictionary that will be supplied as request data. supports
parameter substitution for values.

**auth_type**
Currently only "HTTPBasic" is supported.

**auth_str**
This is an list of values to supply as the request ``auth`` property.
Supports substitution.

.. code:: python3

     _QUERIES = _QUERIES = {
        "ipv4": APILookupParams(
            path="/api/v1/indicators/IPv4"
            auth_str = ["{ApiID}", "{AuthKey}"],
        ),
        ...

**sub_type**
Not currently used.

The parse_results method
~~~~~~~~~~~~~~~~~~~~~~~~

See :py:meth:`parse_results <msticpy.context.tiproviders.ti_provider_base.parse_results>`
for the method header.

This method is responsible for taking the JSON response (as
a Python dictionary) and extracting and returning severity
and relevant details.

The implementation in the example at the start of this document
(and below) shows a simple process, but it can be as complex as needed.

.. note:: we would recommend creating child methods to handle
    different response types if you need to do complex
    processing.

.. code:: python3

    def parse_results(self, response: Dict) -> Tuple[bool, ResultSeverity, Any]:
        """Return the details of the response."""
        if self._failed_response(response):
            return False, ResultSeverity.information, "Not found."

        if response["severity"] < 5:
            severity = ResultSeverity.high
        else:
            severity = ResultSeverity.warning
        details = {
            "Source": response.get("source_domain"),
            "RelatedIPs": response.get("source_ip_addrs")
        }
        return True, severity, details

The function returns a Tuple of:

- parsing success (bool) - return False if the request produced no
  useful data
- severity - using the :py:class:`ResultSeverity <msticpy.context.tiproviders.result_severity.ResultSeverity>`
  enumeration: high, warning, information, unknown
- details - a dictionary of information from the response that
  you want to highlight. The full raw response is always returned
  to the user.

In ``parse_results`` your responsibility is to check the response
data for an indication of severity - i.e. the level of threat posed
by the observable.

The ``details`` dictionary can contain arbitrary data extracted
from the response.

Provider configuration
~~~~~~~~~~~~~~~~~~~~~~

You can supply parameters (such as AuthKey and Api/User ID) to your
provider by creating an entry in ``msticpyconfig.yaml``.

.. code:: yaml

    TIProviders:
        MyProvider:
            Args:
                AuthKey:
                    EnvironmentVar: "MY_PROV_AUTH"
            Primary: True
            Provider: "MyProvider"

Assuming that your provider is implemented in ``MyProvider``,
TILookup will read and pass the value for "AuthKey" to
the provider to include in the requests to the service API.
(In the above example the value of "AuthKey" will be read
from the environment variable "MY_PROV_AUTH".)

For more details on MSTICPy configuration see :doc:`../getting_started/msticpyconfig`

Using the TI Provider
~~~~~~~~~~~~~~~~~~~~~

You can use the TI provider in one of two ways:

- You can use it as a MSTICPy plugin - see :doc:`PluginFramework`
- You can submit it as a pull request to the `MSTICPy repo <https://github.com/microsoft/msticpy>`__
  - see :doc:`../Development`

If you are going to do the second of these, please read the following
section.

Integrating the TI Provider into MSTICPy
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Make sure that you follow the coding guidelines given in
:doc:`../Development`.

To add your provider to the built-in providers, there are
some additional steps:

- add your module to ``msticpy/context/tiproviders``
- add an entry to msticpy.context.tiproviders.__init__
- configure the msticpy settings UI to allow you to manage
  the provider settings from MpConfigEdit.


In the file ``msticpy/context/tiproviders/__init__.py``,
add your provider to the ``TI_PROVIDERS`` dictionary.

.. code-block:: Python3
   :emphasize-lines: 5

    TI_PROVIDERS: Dict[str, Tuple[str, str]] = {
        "OTX": ("alienvault_otx", "OTX"),
        "AzSTI": ("azure_sent_byoti", "AzSTI"),
        ...
        "MyProvider": ("my_provider", "MyProviderClass"),
        ""
    }

The highlighted example has the following syntax:

*ProviderFriendlyName*: (*module_name*, *provider_class*)

- ProviderFriendlyName: is the name used to refer to the provider
  in ``msticpyconfig.yaml`` and when naming specific providers
  in the call to ``lookup_ioc(s)``
- module_name: is the name of the module holding your class.
- provider_class: is the name of the class implementing the provider.

To enable settings in the MSTICPy settings editor, edit the file
``msticpy/resources/mpconfig_defaults.yaml``.

Add an entry to the TIProviders section of this file.

.. code:: yaml

    TIProviders:
        # If a provider has Primary: True it will be run by default on IoC lookups
        # Secondary providers can be run optionally
        OTX:
            Args:
                AuthKey: *cred_key
            Primary: bool(default=True)
            Provider: "OTX"
        MyProvider:
            Args:
                AuthKey: *cred_key
                ApiID: *cred_key
            Primary: bool(default=True)
            Provider: "MyProvider"

Add an item in the "Args:" subsection for each value that you
want to be editable in the UI.
The special value "\*cred_key" tells the settings editor that this
value can be treated as a string, an environment variable or a
Key Vault secret.

If you have other configurable values you can add strings, booleans, etc.
- use the **Splunk** example in the ``DataProviders`` section to
help you.

Context Providers
-----------------

Context providers follow the same model as TI Providers.

The key differences are as follows.

parse_results method
~~~~~~~~~~~~~~~~~~~~

This method is used only to extract details - unlike the TI providers, there
is no severity scoring. It should be a tuple of (*Success*, *Details*) - where
*Success* is a boolean and *Details* is (usually) a Python dict.
*Details* can be any Python object but will ultimately be returned to the
user as a pandas DataFrame column, so should be something that
is easily extracted/viewed like a Python list or dict.

Registering your Context Provider
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The ``msticpy.context.contextproviders.__init__.py`` module uses
the same syntax described in `Integrating the TI Provider into MSTICPy`_
but adding an entry to the ``CONTEXT_PROVIDERS`` dictionary.

.. code-block:: python3
    :emphasize-lines: 3

    CONTEXT_PROVIDERS: Dict[str, Tuple[str, str]] = {
        "ServiceNow": ("servicenow", "ServiceNow"),
        "FriendlyName": ("module_name", "class_name"),
    }

