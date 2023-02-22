# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
Jupyter and Python Tools for InfoSec.

-----------------------------------------------

Requires Python 3.8 or later.

Getting Started
---------------

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

Getting started with msticpy
https://msticpy.readthedocs.io/en/latest/getting_started/QuickStart.html

Configuration
https://msticpy.readthedocs.io/en/latest/getting_started/msticpyconfig.html

Package structure
-----------------

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

Configuration
-------------

Set MSTICPYCONFIG environment variable to point to the path
of your `msticpyconfig.yaml` file.

One frequent problem in bootstrapping MSTICPy is obtaining or
creating a valid `msticpyconfig.yaml`. This is needed for many
configuration settings such as Data providers,
MS Sentinel workspaces, Threat Intelligence (TI) providers,
Azure authentication, Key Vault settings and more.

If you are having trouble with this, you should check out the
following resources:

The basic structure of msticpyconfig.yaml

https://msticpy.readthedocs.io/en/latest/getting_started/msticpyconfig.html

Using the msticpy settings editor to create or modify
msticpyconfig.yaml

https://msticpy.readthedocs.io/en/latest/getting_started/SettingsEditor.html

MS Sentinel and Azure ML resources:

Getting started notebook
https://github.com/Azure/Azure-Sentinel-Notebooks/blob/master/A%20Getting%20Started%20Guide%20For%20Azure%20Sentinel%20ML%20Notebooks.ipynb

Configuring your environment notebook
https://github.com/Azure/Azure-Sentinel-Notebooks/blob/master/ConfiguringNotebookEnvironment.ipynb

Initialization
--------------

Running the nbinit function performs a number of functions
that allow you start working in a notebook more quickly:

>>> import msticpy as mp
>>> mp.init_notebook()

This module performs several steps to initialize MSTICPy:
- imports a number of standard packages (e.g. pandas) into the notebook
- imports a number of modules and functions from msticpy
- checks the version of MSTICPy
- checks for presence and validates msticpyconfig.yaml
- imports entities and initialized pivot functions
- initializes pandas with the .mp and .mp_plot accessors that
  give you quick access to many MSTICPy functions from a pandas
  DataFrame
- creates some MSTICPy notebook magics.

In the Azure ML and Azure Synapse environments, some additional
initialization and checks are performed.

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
