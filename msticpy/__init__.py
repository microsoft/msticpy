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

- auth - authentication and secrets management
- analysis - analysis functions
- common - utility functions, common types, exceptions
- config - configuration tool
- data - queries, data access, context functions
- datamodel - entities and pivot functions
- init - package initialization
- nbtools - deprecated location
- nbwidgets - notebook widgets
- resources - data resource files
- transform - data transforms and decoding
- vis - visualizations

Configuration:

- set MSTICPYCONFIG environment variable to point to the path
  of your `msticpyconfig.yaml` file.

"""

import os

from . import nbwidgets

# flake8: noqa: F403
from ._version import VERSION
from .common.check_version import check_version
from .common.pkg_config import refresh_config, settings
from .common.utility import search_name as search
from .config.mp_config_edit import MpConfigEdit, MpConfigFile
from .data.core.data_providers import QueryProvider
from .init.nbinit import current_providers, init_notebook

__version__ = VERSION
__author__ = "Ian Hellen, Pete Bryan, Ashwin Patil"

if not os.environ.get("KQLMAGIC_EXTRAS_REQUIRES"):
    os.environ["KQLMAGIC_EXTRAS_REQUIRES"] = "jupyter-basic"
