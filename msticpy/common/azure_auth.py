# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
Deprecated - module common.azure_auth has moved.

See :py:mod:`msticpy.auth`
"""
import warnings

from .._version import VERSION

# flake8: noqa: F403, F401
# pylint: disable=wildcard-import, unused-wildcard-import, unused-import
from ..auth.azure_auth import *  # noqa: F401

__version__ = VERSION
__author__ = "Pete Bryan"

WARN_MSSG = (
    "This module has moved to msticpy.auth\n"
    "Please change your import to reflect this new location."
    "This will be removed in MSTICPy v2.2.0"
)
warnings.warn(WARN_MSSG, category=DeprecationWarning)
