Automatic Loading of Query Providers and Components
===================================================

The ``mp_user_session.py`` module is designed to load and initialize query providers and other
components based on configuration provided in a YAML file.

This allows you to load multiple providers and components in a single step avoiding having to
write a lot of repetitive code in your notebooks.

The user is expected to supply the path to the YAML file to the ``load_user_config`` function.
Each key in the ``QueryProviders`` and ``Components`` sections of the YAML file will be the name
of the component variable in the local namespace.

Example YAML Configuration
--------------------------

Here is an example of a YAML configuration file that defines query providers and components:

.. code-block:: yaml

    QueryProviders:
        qry_prov_sent:
            DataEnvironment: MSSentinel
            InitArgs:
                debug: True
            Connect: True
            ConnectArgs:
                workspace: CyberSecuritySoc
                auth_methods: ['cli', 'device_code']
        qry_prov_md:
            DataEnvironment: M365D
        qry_kusto_mde:
            DataEnvironment: Kusto
            Connect: True
            ConnectArgs:
                cluster: MDE-Scrubbed
    Components:
        mssentinel:
            Module: msticpy.context.azure
            Class: MicrosoftSentinel
            InitArgs:
            Connect: True
            ConnectArgs:
                workspace: CyberSecuritySoc
                auth_methods: ['cli', 'device_code']

Each key in the ``QueryProviders`` and ``Components`` sections is the name of the instance of the
component created in your notebook environment. For example, the ``qry_prov_md`` entry is
equivalent to the code:

.. code-block:: python

    import msticpy as mp
    qry_prov_md = mp.QueryProvider("M365D")

You can also specify initialization arguments. The ``qry_prov_sent`` entry adds ``debug=True`` to
the parameters given to the query provider.

You can also ask the user session manager to call the ``connect`` method for the provider with the
``Connect`` property, and supply parameters to the ``connect`` call with the ``ConnectArgs``
property.

The ``Components`` section allows you to define non-query components and works in a similar way to
the ``QueryProviders`` section. The main difference here is that you need to specify the module and
class of the component that you want to load. In the example above, we are loading the
``MicrosoftSentinel`` class from the ``msticpy.context.azure`` module and requesting that the
``connect`` method is called with the parameters specified in the ``ConnectArgs`` property.
