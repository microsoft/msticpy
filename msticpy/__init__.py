# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
Jupyter and Python Tools for InfoSec.

-----------------------------------------------

Requires Python 3.8 or later.

To quickly import common modules into a notebook run:

>>> import msticpy as mp
>>> mp.init_notebook()

If not running in a notebook/IPython use
>>> mp.init_notebook(globals())

To see help on `init_notebook`:
>>> help(mp.init_notebook)

Search msticpy modules for a keyword:
>>> mp.search(keyword)

-----------------------------------------------

Full documentation is available at:
https://msticpy.readthedocs.io

GitHub repo:
https://github.com/microsoft/msticpy


Package structure:

- analysis - analysis functions
- auth - authentication and secrets management
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
import importlib
import os
from typing import Any

from . import nbwidgets

# flake8: noqa: F403
from ._version import VERSION
from .common import pkg_config as settings
from .common.check_version import check_version
from .common.utility import search_name as search

__version__ = VERSION
__author__ = "Ian Hellen, Pete Bryan, Ashwin Patil"

refresh_config = settings.refresh_config

if not os.environ.get("KQLMAGIC_EXTRAS_REQUIRES"):
    os.environ["KQLMAGIC_EXTRAS_REQUIRES"] = "jupyter-basic"

_STATIC_ATTRIBS = list(locals().keys())

_DEFAULT_IMPORTS = {
    "az_connect": "msticpy.auth.azure_auth",
    "current_providers": "msticpy.init.nbinit",
    "ContextLookup": "msticpy.context.contextlookup",
    "GeoLiteLookup": "msticpy.context.geoip",
    "init_notebook": "msticpy.init.nbinit",
    "reset_ipython_exception_handler": "msticpy.init.nbinit",
    "IPStackLookup": "msticpy.context.geoip",
    "MicrosoftSentinel": "msticpy.context.azure",
    "MpConfigEdit": "msticpy.config.mp_config_edit",
    "MpConfigFile": "msticpy.config.mp_config_file",
    "QueryProvider": "msticpy.data",
    "TILookup": "msticpy.context.tilookup",
    "TimeSpan": "msticpy.common.timespan",
    "WorkspaceConfig": "msticpy.common.wsconfig",
    "entities": "msticpy.datamodel",
    "Pivot": "msticpy.init.pivot",
}


def __getattr__(attrib: str) -> Any:
    """
    Import and return an attribute of MSTICPy.

    Parameters
    ----------
    attrib : str
        The attribute name

    Returns
    -------
    Any
        The attribute value.

    Raises
    ------
    AttributeError
        No attribute found.

    """
    if attrib in _DEFAULT_IMPORTS:
        module = importlib.import_module(_DEFAULT_IMPORTS[attrib])
        return getattr(module, attrib)
    raise AttributeError(f"msticpy has no attribute {attrib}")


def __dir__():
    """Return attribute list."""
    return sorted(set(_STATIC_ATTRIBS + list(_DEFAULT_IMPORTS)))
