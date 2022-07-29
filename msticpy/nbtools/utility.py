# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Deprecated path for common.utility.py."""
import warnings

# flake8: noqa: F403, F401
# pylint: disable=wildcard-import, unused-wildcard-import
from ..common.utility import *
from ..common.utility import md, md_warn

WARN_MSSG = (
    "This module has moved to msticpy.common.utility\n"
    "Please change your import to reflect this new location."
    "This will be removed in MSTICPy v2.2.0"
)
warnings.warn(WARN_MSSG, category=DeprecationWarning)
