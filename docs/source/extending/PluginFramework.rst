MSTICPy Plugin Framework
========================

MSTICPy has several extensibility points, where you
can create your own modules to support data and
enrichment sources.

These currently include:

- Data Providers
- Threat Intelligence Providers
- Context Providers

If you create one of these and think that it would be useful
to other users, please consider contributing it to MSTICPy.

You can also create and keep local provider modules and
have them loaded into MSTICPy to work alongside the built-in
providers. This might be useful if you are creating something
that is very specific to your organization, for example.


For either of these cases, your classes must be derived
from the corresponding MSTICPy base classes. To read more
about building data, TI and context providers
see the following pages:

- :doc:`WritingDataProviders`
- :doc:`WritingTIAndContextProviders`

Specific Guidelines for plugin types
------------------------------------

TI and Context Providers
~~~~~~~~~~~~~~~~~~~~~~~~

Create a class attribute ``PROVIDER_NAME`` and assign
a friendly name to your provider. This is not mandatory -
if the class has no ``PROVIDER_NAME`` attribute, the
friendly name will default to the name of the class.

.. code:: python3

    class TIProviderTest(HttpTIProvider):
        """Custom IT provider TI Base."""

        PROVIDER_NAME = "MyProvider"


DataProviders
~~~~~~~~~~~~~

When you load a Data provider in MSTICPy you need to
pass the name of the ``DataEnvironment`` to the
:py:class:`QueryProvider <msticpy.data.core.data_providers.QueryProvider>`
class. E.g.

.. code:: python3

    qry_prov = mp.QueryProvider("MyDataSource")

By default, the name used to load your provider will be
name of your provider class. You can customize this by adding
a ``DATA_ENVIRONMENTS`` (list or tuple) attribute to your class. This should
be a list of strings. You can load your driver in the QueryProvider
by supplying any of the names in this list or tuple.
If you also want to use the name of the class, add it to the list.

.. code:: python3

    class CustomDataProvB(CustomDataProvA):
        """Custom provider."""

        DATA_ENVIRONMENTS = ["SQLTestProvider", "SQLProdProvider"]

The provider will be registered to load when any of the strings
assigned here is passed as the QueryProvider identifier.

Using multiple identifiers allows you to use aliases for
the provider.

Additionally, because the Data Environment identifier is
passed to your provider class (as the parameter ``data_environment``)
when it is loaded, you can also
have alternative behavior coded into the ``__init__`` and other
methods of your class. For example, you might have a single provider class
that can work with two different versions of an API.

Loading plugin classes
----------------------

Assuming that you have created one or more DataProvider
or Context/TI Provider classes, you should put these
modules in one or more folders accessible to your notebook
or python environment.

You can load modules interactively or add these paths
to your ``msticpyconfig.yaml`` to have them loaded automatically
each time you import MSTICPy.

Loading modules interactively
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To load modules from a folder run the
:py:func:`load_plugins <msticpy.load_plugins>` function.

.. code:: python3

    import msticpy as mp

    mp.load_plugins(plugin_paths="/my_modules")

    # or multiple paths
    mp.load_plugins(
        plugin_paths=["./my_modules", "./my_other_modules"]
    )

Loading modules from configuration
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Add plugin module paths to ``msticpyconfig.yaml`` you can
tell MSTICPy to always try to load plugins from these paths.

Add the following entry to ``msticpyconfig.yaml``:

.. code-block:: yaml
    :emphasize-lines: 4, 5

        ...
        Custom:
            - "testdata"
    PluginFolders:
        - tests/testdata/plugins
    Azure:
        cloud: "global"
        auth_methods: ["cli", "msi", "interactive"]

You can include multiple paths under the ``PluginFolders`` key.
