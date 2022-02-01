# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
Check version of msticpy.

Based on code from stackoverflow
https://stackoverflow.com/questions/58648739/how-to-check-if-python
-package-is-latest-version-programmatically/58650826#58650826
"""

# import sys
from distutils.version import LooseVersion
from importlib_metadata import version

import httpx

__author__ = "Ian Hellen, hoefling"


# pylint: disable=invalid-name
if __name__ == "__main__":
    name = "msticpy"
    installed_version = LooseVersion(version(name))

    # fetch package metadata from PyPI
    pypi_url = f"https://pypi.org/pypi/{name}/json"
    pkg_data = httpx.get(pypi_url).json()
    latest_version = pkg_data.get("info", {}).get("version", None)
    if latest_version:
        latest_version = LooseVersion(latest_version)
    else:
        latest_version = max(LooseVersion(s) for s in pkg_data["releases"].keys())

    print("package:", name, "installed:", installed_version, "latest:", latest_version)
