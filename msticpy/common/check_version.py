# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Check current version against PyPI."""
import httpx
from pkg_resources import parse_version

from .._version import VERSION
from .utility import mp_ua_header

__version__ = VERSION
__author__ = "Ian Hellen"


def check_version():
    """Check the current version against latest on PyPI."""
    installed_version = parse_version(__version__)

    # fetch package metadata from PyPI
    pypi_url = "https://pypi.org/pypi/msticpy/json"
    pkg_data = httpx.get(
        pypi_url, timeout=httpx.Timeout(10.0, connect=30.0), headers=mp_ua_header()
    ).json()
    latest_version = pkg_data.get("info", {}).get("version", None)
    if latest_version:
        latest_version = parse_version(latest_version)
    else:
        latest_version = max(parse_version(s) for s in pkg_data["releases"].keys())

    print(
        "msticpy version",
        "installed:",
        installed_version,
        "latest published:",
        latest_version,
    )
    if installed_version < latest_version:
        print(f"A newer version of msticpy - {latest_version} is available.")
        print("Upgrade with 'pip install --upgrade msticpy'")
    else:
        print("Latest version is installed.")
