Contributions of improvements, fixes and new features are welcomed.
We use a continuous integration pipeline that enforces unit tests and code style. We aim to keep
the code clear, testable and well-documented.

# Guidelines for code:

## Unit Tests
All new code should have unit tests with at least 80% code coverage. There are some exceptions to this: for example, code that accesses online data and requires authentication. We can work with you on getting this to work in our build.
We use pytest but some of the existing tests are also Python unittest compatible.

## Type hints
Use type annotations for parameters and return values in public methods, properties and functions.
[Python Type Hints documentation](https://docs.python.org/3/library/typing.html)

## Docstrings
Our documentation is automatically built for Readthedocs using Sphinx.
All public modules, functions, classes and methods should be documented using the numpy documenation standard.
[numpy docstring guide](https://numpydoc.readthedocs.io/en/latest/format.html)

## Code Formatting
We use black everywhere and enforce this in the build.
[Black - The Uncompromising Code Formatter](https://github.com/psf/black)

## Linters/Code Checkers
We use the following code checkers:
- pylint (with --disable=bad-continuation)
- mypy
- bandit (with -s B303,B404,B603,B607)
- flake8 (with --ignore=E501,W503)
- prospector (see prospector.yml in root of repo for config used). Prospector runs:
  - pycodestyle
  - pydocstyle
  - pep8
  - pyroma
  - pep257

## A musical guide
[The PEP8 Song](https://www.youtube.com/watch?v=hgI0p1zf31k)
Brilliantly written and performed by [@lemonsaurus_rex](https://twitter.com/lemonsaurus_rex)
