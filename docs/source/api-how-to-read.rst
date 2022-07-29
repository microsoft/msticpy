Guide to Reading the API Reference
==================================

The API documentation is auto-generated using sphinx and the *napolean*
sphinx extension. Some of the layout and terminology may need some
more explanation.

The documentation is now built with *intersphinx* which makes references to
types from the Python standard library and most external packages clickable.
So, for example, if you see a function with a type `pd.DataFrame`
you can click on that and you will be taken to pandas documentation
for the pandas DataFrame.

API Documentation Structure
---------------------------
The API listings are grouped by sub-package, then module, then class.

The class layout can appear a little confusing because of
some limitations of the auto-documentation (or more likely some
limitations of my knowledge of how to use it).

In particular, the documented public **attributes** of classes (actually,
attributes of class instances) are included without a header section.

Class documentation is layed out in the following structure:

- **Class summary**: the ``__init__`` signature header
   * **Attributes**:
      Documented public attributes (other than
      explicit properties). The Attributes section has no title so everything
      you see until the instance creation documentation is an attribute.
   * **Class instantiation**:
      the ``__init__`` signature and
      documentation, i.e. the syntax to create
      a new instance of this class.
   * **Class methods** and **Class properties**.


Type Annotations
----------------
The package uses type annotations with types imported from the
``typing`` library. For a full explanation of how these are used
and how to interpret some of the odder-looking types please
see `PEP 484 <https://www.python.org/dev/peps/pep-0484/>`__.

``typing`` uses abstract classes to help deal with duck typing - e.g.
``MyFancyList`` may implement all required ``list`` interfaces but not
actually be derived from ``list``.

Most of the types used are easily interpretable (e.g. ``Tuple ~= tuple``)
with the advantage that you can supply type annotations to Tuple's
members - e.g. ``Tuple[str, int, float]``.

For all type annotations used from the Python ``typing`` module,
you can click on the type in the API documentation to navigate
to the Python docs official documentation.

Some members of ``typing`` commonly used in are a little more esoteric
than ``List`` or ``Tuple``:

``Mapping``
"""""""""""

An object supporting keyed access to its members (like a ``dict``).
E.g. ``Mapping[str, MyClass]``: when this appears in a parameter type
annotation, it means that any dict-like object (that takes a type str
as a key and returns an object of type MyClass) T2 is acceptable.
This is more rarely used in return type annotations but means that
the returned object will support keyed access to members but not necessarily
implement everything that ``dict`` supports.

``Iterable``
""""""""""""

An object that supports iterator interface.
e.g. ``Iterable[int]``

``Optional``
""""""""""""

The specified type maybe present or None.
e.g. ``Optional[str]``: means that either str or None is
accepted or returned.

``Union``
"""""""""

More general form of ``Optional`` where multiple types are
acceptable.
e.g. ``Union[str, float]``: means that either a str or a float is acceptable

``Any``
"""""""

Any type is acceptable.


References
----------

- `PEP 484 <https://www.python.org/dev/peps/pep-0484/>`__
- `Numpy docstring standards <https://numpydoc.readthedocs.io/en/latest/format.html#docstring-standard>`__
- `Sphinx autodoc extension <https://www.sphinx-doc.org/en/master/usage/extensions/autodoc.html#module-sphinx.ext.autodoc>`__
