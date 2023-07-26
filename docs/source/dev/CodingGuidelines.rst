Code Guidelines
===============

Unit Tests
----------

We use pytest although some of the older tests are in
Python `unittest` format.

Avoid making any calls to internet services in your unit tests.
If your feature connects to a live service, make sure that your unit tests mock
the sample responses expected. We use httpx for all our http requests -
you can use `respx <https://pypi.org/project/respx/>`__ to mock
http responses (search for "@respx" in our tests to see examples
of how this works).

Ensure your contribution has the highest possible of test coverage.
You should aim for a least 80% coverage and ideally reach 100%.
If you can't reach 80% for what ever reason let us know when you
raise a PR and we can work with you on this.

You can map your test coverage using the following command:

.. code:: bash

   pytest --cov=msticpy --cov-report=html

This will create a folder called htmlcov in your current directory. Open
the index.html file in this folder to see the coverage report.

You can also execute a subset of tests and check the coverage for
the areas of code that you are interesting in testing.

.. code:: bash

    pytest --cov=msticpy.data.drivers --cov-report=html tests/data/drivers/test_xyz_driver.py


Type hints
----------

Use type annotations for parameters and return values in public methods,
properties and functions.

.. code:: python

   from typing import Any, Dict, Optional, Union

   ...

   def build_process_tree(
       procs: pd.DataFrame,
       schema: Union[ProcSchema, Dict[str, Any]] = None,
       show_summary: bool = False,
       debug: bool = False,
       **kwargs,
   ) -> pd.DataFrame:
       """
       Build process trees from the process events.

`Python Type Hints
documentation <https://docs.python.org/3/library/typing.html>`__

Docstrings
----------

Our documentation is automatically built for Readthedocs using Sphinx.
All public modules, functions, classes and methods should be documented
using the numpy documenation standard.

.. code:: python

   def build_process_tree(
       procs: pd.DataFrame,
       schema: Union[ProcSchema, Dict[str, Any]] = None,
       show_summary: bool = False,
       debug: bool = False,
       **kwargs,
   ) -> pd.DataFrame:
       """
       Build process trees from the process events.

       Parameters
       ----------
       procs : pd.DataFrame
           Process events (Windows 4688 or Linux Auditd)
       schema : Union[ProcSchema, Dict[str, Any]], optional
           The column schema to use, by default None.
           If supplied as a dict it must include definitions for the
           required fields in the ProcSchema class
           If None, then the schema is inferred
       show_summary : bool
           Shows summary of the built tree, default is False.
       debug : bool
           If True produces extra debugging output,
           by default False

       Returns
       -------
       pd.DataFrame
           Process tree dataframe.

       See Also
       --------
       ProcSchema

       """

`numpy docstring
guide <https://numpydoc.readthedocs.io/en/latest/format.html>`__

Code Formatting
---------------

We use black everywhere and enforce this in the build.

`Black - The Uncompromising Code
Formatter <https://github.com/psf/black>`__

Linters/Code Checkers
---------------------

We use the following code checkers: - pylint: ``pylint msticpy`` - mypy:
``mypy msticpy`` - bandit:
``bandit -r -lll -s B303,B404,B603,B607 msticpy`` - flake8:
``flake8 --max-line-length=90 --ignore=E501,W503 --exclude tests`` -
pydocstyle: ``pydocstyle --convention=numpy msticpy`` - isort:
``isort --profile black msticpy``

Pre-Commit
----------

We have a pre-commit configuration in the msticpy repo. This runs the
checks above (apart from mypy) when you commit. See `Pre-Commit
Script <https://github.com/microsoft/msticpy/wiki/Pre-commit-scripts>`__
for more details.

VSCode support
--------------

See this page for task definitions to run `linters/checkers in
VSCode <https://github.com/microsoft/msticpy/wiki/VSCode-Build-tasks-script>`__

Create a branch
---------------

Before you submit a PR, create working branch in your fork and put your
changes in that. It's going to make it easier for you to re-sync the
main branch if this gets updated while you are working on your changes.

See also
--------

-  `Good coding
   guidelines <https://github.com/microsoft/msticpy/wiki/Good-coding-guidelines-tips>`__
-  `VS Code build
   tasks <https://github.com/microsoft/msticpy/wiki/VSCode-Build-tasks-script>`__
-  `Using
   Pre-commit <https://github.com/microsoft/msticpy/wiki/Pre-commit-scripts>`__

A musical guide
---------------

`The PEP8 Song <https://www.youtube.com/watch?v=hgI0p1zf31k>`__

Brilliantly written and performed by
[@lemonsaurus_rex](https://twitter.com/lemonsaurus_rex)
