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

>>> import msticpy
>>> msticpy.init_notebook(globals())

For more options:
>>> help(msticpy.init_notebook)

-----------------------------------------------

Full documentation is available at:
https://msticpy.readthedocs.io

GitHub repo:
https://github.com/microsoft/msticpy


Package structure:

- analysis - analysis and data processing functions
- common - utility functions, authentication, secrets
- config - configuration tool
- data - queries, data access, context functions
- datamodel - entities and pivot functions
- nbtools - notebook initialization and tools
- vis - visualizations

Configuration:

- set MSTICPYCONFIG environment variable to point to the path
  of your `msticpyconfig.yaml` file.

"""

import os

# flake8: noqa: F403
from .nbtools.nbinit import init_notebook, current_providers
from .common import pkg_config as settings
from .common.check_version import check_version
from . import sectools
from . import nbtools
from . import data
from . import analysis
from .config.mp_config_edit import MpConfigEdit, MpConfigFile
from ._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen, Pete Bryan, Ashwin Patil"

if not os.environ.get("KQLMAGIC_EXTRAS_REQUIRES"):
    os.environ["KQLMAGIC_EXTRAS_REQUIRES"] = "jupyter-basic"
