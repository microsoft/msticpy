# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
msticpy - Jupyter and Python Tools for InfoSec.

-----------------------------------------------

Requires Python 3.6 or later.

To quickly import common modules into a notebook run:

>>> from msticpy import init_notebook
>>> init_notebook(globals())

For more options:
>>> help(init_notebook)

-----------------------------------------------

Full documentation is available at:
    https://msticpy.readthedocs.io

GitHub repo:
    https://github.com/microsoft/msticpy

"""

# flake8: noqa: F403
from .nbtools.nbinit import init_notebook, current_providers
from .common import pkg_config as settings
from .common.check_version import check_version
from . import sectools
from . import nbtools
from . import data

from ._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen, Pete Bryan, Ashwin Patil"
