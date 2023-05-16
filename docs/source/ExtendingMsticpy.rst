Extending MSTICPy
=================

Introduction to MSTICPy extensibility
-------------------------------------

MSTICPy has several extensibility points. These range from adding
parameterized queries to writing your own data provider or
context provider.

Some of these require coding, while others can be done
by creating YAML configuration files. For Data Providers and
Context/TI providers there is also a plugin model that allows
you to create private providers and load them from a local
path.


Contributing
------------

If you decide to extend MSTICPy in one of these ways and
think that this would be useful to other users of the
package, please consider contributing them into the package.

Extension points documentation
------------------------------

.. toctree::
   :maxdepth: 2

   extending/Queries
   extending/PivotFunctions
   extending/WritingDataProviders
   extending/WritingTIAndContextProviders
   extending/PluginFramework
