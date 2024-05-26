# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Check current version against PyPI."""
from importlib.metadata import version

from packaging.version import Version
from packaging.version import parse as parse_version

from .._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"


def check_version() -> None:
    """Check the current version against latest on PyPI."""
    installed_version: Version = parse_version(__version__)

    # fetch package metadata from PyPI
    distrib_version: str = version("msticpy")

    latest_version: Version = parse_version(distrib_version)

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
        print("Latest known version is installed.")
