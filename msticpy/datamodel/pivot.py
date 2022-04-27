# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
Deprecated - module pivot.py has moved.

See :py:mod:`msticpy.init.pivot`
"""
import warnings

from .._version import VERSION

__version__ = VERSION
__author__ = "Pete Bryan"

WARN_MSSG = (
    "This module has moved to msticpy.init.pivot\n"
    "Please change your import to reflect this new location."
    "This will be removed in MSTICPy v2.2.0"
)
warnings.warn(WARN_MSSG, category=DeprecationWarning)
