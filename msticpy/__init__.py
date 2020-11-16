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
from pkg_resources import parse_version

import requests

# flake8: noqa: F403
from .nbtools.nbinit import init_notebook
from .common import pkg_config as settings
from . import sectools
from . import nbtools
from . import data

from ._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen, Pete Bryan, Ashwin Patil"


def check_version():
    """Check the current version against latest on PyPI."""
    installed_version = parse_version(__version__)

    # fetch package metadata from PyPI
    pypi_url = "https://pypi.org/pypi/msticpy/json"
    pkg_data = requests.get(pypi_url).json()
    latest_version = pkg_data.get("info", {}).get("version", None)
    if latest_version:
        latest_version = parse_version(latest_version)
    else:
        latest_version = max(parse_version(s) for s in pkg_data["releases"].keys())

    print("msticpy version", "installed:", installed_version, "latest:", latest_version)
    if installed_version < latest_version:
        print(f"A newer version {latest_version} is available.")
        print("Upgrade with pip install --upgrade msticpy")
    else:
        print("Latest version is installed.")
