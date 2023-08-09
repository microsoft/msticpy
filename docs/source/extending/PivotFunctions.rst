Adding Pivot functions
======================

See :doc:`../data_analysis/PivotFunctions` for more details on use
of pivot functions.

The pivot library supports adding functions as pivot functions from
any importable Python library. Not all functions will be wrappable.
Currently Pivot supports functions that take input parameters as
either scalar values (I'm including strings in this although that isn't
exactly correct), iterables or DataFrames with column specifications.

You can create a persistent pivot function definition (that
can be loaded from a yaml file) or an ad hoc definition
that you can create in code. The next two sections
describe creating a persistent function definition. Programmatically
creating pivots follows this.

Information needed to define a pivot function
---------------------------------------------

If you have a library function that you want to expose as a pivot function
you need to gather a bit of information about it.

This table describes the configuration parameters needed to create a
pivot function (most are optional).

+-------------------------+-------------------------------+------------+------------+
| Item                    | Description                   | Required   | Default    |
+=========================+===============================+============+============+
| src_module              | The src_module to containing  | Yes        | -          |
|                         | the class or function         |            |            |
+-------------------------+-------------------------------+------------+------------+
| class                   | The class containing function | No         | -          |
+-------------------------+-------------------------------+------------+------------+
| src_func_name           | The name of the function to   | Yes        | -          |
|                         | wrap                          |            |            |
+-------------------------+-------------------------------+------------+------------+
| func_new_name           | Rename the function           | No         | -          |
+-------------------------+-------------------------------+------------+------------+
| input type              | The input type that the       | Yes        | -          |
|                         | wrapped function expects      |            |            |
|                         | (DataFrame iterable value)    |            |            |
+-------------------------+-------------------------------+------------+------------+
| entity_map              | Mapping of entity and         | Yes        | -          |
|                         | attribute used for function   |            |            |
+-------------------------+-------------------------------+------------+------------+
| func_df_param_name      | The param name that the       | If DF      | -          |
|                         | function uses as input param  | input      |            |
|                         | for DataFrame                 |            |            |
+-------------------------+-------------------------------+------------+------------+
| func_df_col_param_name  | The param name that function  | If DF      | -          |
|                         | uses to identify the input    | input      |            |
|                         | column name                   |            |            |
+-------------------------+-------------------------------+------------+------------+
| func_out_column_name    | Name of the column in the     | If DF      | -          |
|                         | output DF to use as a key to  | output     |            |
|                         | join                          |            |            |
+-------------------------+-------------------------------+------------+------------+
| func_static_params      | dict of static name/value     | No         | -          |
|                         | params always sent to the     |            |            |
|                         | function                      |            |            |
+-------------------------+-------------------------------+------------+------------+
| func_input_value_arg    | Name of the param that the    | If not     | -          |
|                         | wrapped function uses for its | DF input   |            |
|                         | input value                   |            |            |
+-------------------------+-------------------------------+------------+------------+
| can_iterate             | True if the function supports | No         | Yes        |
|                         | being called multiple times   |            |            |
+-------------------------+-------------------------------+------------+------------+
| entity_container_name   | The name of the container in  | No         | custom     |
|                         | the entity where the func     |            |            |
|                         | will appear                   |            |            |
+-------------------------+-------------------------------+------------+------------+

The ``entity_map`` item specifies which entity or entities the pivot function
will be added to. Each
entry requires an Entity name (see
:py:mod:`entities<msticpy.datamodel.entities>`) and an
entity attribute name. The attribute name is only used if you want to
use an instance of the entity as a parameter to the function.
If you don't care about this you can pick any attribute.

For ``IpAddress`` in the (partial) example
below, the pivot function will try to extract the value of the
``Address`` attribute when an instance of IpAddress is used as a
function parameter.

.. code:: yaml

    ...
    entity_map:
      IpAddress: Address
      Host: HostName
      Account: Name
    ...

This means that you can specify different attributes of the same entity
for different functions (or even for two instances of the same function)

The ``func_df_param_name`` and ``func_df_col_param_name`` are needed
only if the source function (i.e. the function to be wrapped as a pivot
function) takes a DataFrame and column name as input
parameters.

``func_out_column_name`` is needed if the source function returns a
DataFrame. In order to *join* input data with output data this needs to be
the column in the output that has the same value as the function input
For example, if your function processes IP addresses and returns the IP
in a column named "ip_addr", put "ip_addr" as the value of ``func_out_column_name``.

Adding ad hoc pivot functions in code
-------------------------------------

You can also add ad hoc functions as pivot functions in code.

To do this use the Pivot method
:py:meth:`add_pivot_function<msticpy.init.pivot.pivot.Pivot.add_pivot_function>`

You can supply the
pivot registration parameters as keyword arguments to this function:

.. code:: python3

    def my_func(input: str):
        return input.upper()

    Pivot.add_pivot_function(
        func=my_func,
        container="change_case",
        input_type="value",
        entity_map={"Host": "HostName"},
        func_input_value_arg="input",
        func_new_name="upper_name",
    )

Alternatively, you can create a
:py:class:`PivotRegistration<msticpy.init.pivot_core.pivot_register.PivotRegistration>`
object and supply that (along with the ``func`` parameter), to
``add_pivot_function``.

.. code:: python3

    from msticpy.init.pivot_core.pivot_register import PivotRegistration

    def my_func(input: str):
        return input.upper()

    piv_reg = PivotRegistration(
        input_type="value",
        entity_map={"Host": "HostName"},
        func_input_value_arg="input",
        func_new_name="upper_name"
    )

    Pivot.add_pivot_function(my_func, piv_reg, container="change_case")

The function parameters and PivotRegistration attributes are
described in the previous section `Information needed to define a pivot function`_.

Creating a persistent pivot function definition
-----------------------------------------------

You can also add pivot definitions to yaml files and load your
pivots from this.

The top-level element of the file is ``pivot_providers``.

Example from the *MSTICPy* ip_utils ``who_is`` function

.. code:: yaml

   pivot_providers:
     ...
     who_is:
      src_module: msticpy.context.ip_utils
      src_func_name: get_whois_df
      func_new_name: whois
      input_type: dataframe
      entity_map:
        IpAddress: Address
      func_df_param_name: data
      func_df_col_param_name: ip_column
      func_out_column_name: query
      func_static_params:
        all_columns: True
        show_progress: False
      func_input_value_arg: ip_address

.. note:: the library also support creating pivots from ad hoc
   functions created in the current notebook (see below).

You can also put this function into a Python module.
If your module is in the current directory and is called
``my_new_module``, the value you specify for
``src_module`` will be "my_new_module".

Once you have your yaml definition file you can call
:py:meth:`register_pivot_providers<msticpy.init.pivot_core.pivot.Pivot.register_pivot_providers>`

.. code:: python3

       Pivot.register_pivot_providers(
           pivot_reg_path=path_to_your_yaml,
           namespace=globals(),
           def_container="my_container",
           force_container=True
       )

.. warning:: The pivot functions created will not persist
    across notebook sessions. You will need to call
    ``register_pivot_providers`` with your yaml files each
    time you start a new session. Currently there is no option
    to automate this via msticpyconfig.yaml but this would be
    easy to add - please open an issue in
    `the MSTICPy repo <https://github.com/microsoft/msticpy>`_
    if you would like to see this.

.. _creating-deleting-shortcut-pivot-functions:

Creating and deleting shortcut pivot functions
----------------------------------------------

If you are adding pivot functions of your own, you can add shortcuts
(i.e. direct methods of the entity, rather than methods in sub-containers)
to those functions.

Every entity class has the class method
:py:meth:`make_pivot_shortcut<msticpy.datamodel.entities.Entity.make_pivot_shortcut>`.
You can use this to add a shortcut to an existing pivot function on that
entity. Note that you must call this method on the entity *class* and not
on an instance of that Entity.

The parameters that you must supply are ``func_name`` and ``target``. The former
is the relative path to the pivot function that you want to make the shortcut
to, e.g. for ``IpAddress.util.whois`` you would use the string "util.whois".
``target`` is the string name that you want the shortcut function to be called.
This should be a valid Python identifier - a string starting with a letter or
underscore, followed by any combination of letters, digits and underscores. If
you supply a string that is not a valid identifier, the function will try to
transform it into one.

.. code:: python3

    >>> IpAddress.make_pivot_shortcut(func_name="util.whois", target="my_whois")
    >>> IpAddress.my_whois("157.53.1.1")

.. parsed-literal::

    ip_column    AsnDescription    whois_result
    157.53.1.1   NA                {'nir': None, 'asn_registry': 'arin', ...


If the shortcut function already exists, you will get an error (AttributeError).
You can force overwriting of an existing shortcut by adding ``overwrite=True``.

To delete a shortcut use
:py:meth:`del_pivot_shortcut<msticpy.datamodel.entities.Entity.del_pivot_shortcut>`,
giving the single parameter ``func_name`` with the name of the shortcut function
you want to remove.


Removing pivot functions from an entity or all entities
-------------------------------------------------------

Although not a common operation you can remove *all* pivot functions
from an entity or from all entities.

See
:py:meth:`remove_pivot_funcs<msticpy.init.pivot.pivot.Pivot.remove_pivot_funcs>`
for more details.