# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Deprecated placeholder for vtlookup.py."""
import warnings

from .._version import VERSION

__version__ = VERSION
__author__ = "Pete Bryan"


# flake8: noqa: F403, F401
# pylint: disable=unused-import, unused-wildcard-import, wildcard-import
from ..data.contextproviders.tiproviders.vtlookupv3.vtlookup import *

WARN_MSSG = (
    "This module has moved to msticpy.data.contextproviders.tiproviders.vtlookupv3.vtlookup\n"
    + "Please change your import to reflect this new location."
)
warnings.warn(WARN_MSSG, category=DeprecationWarning)
