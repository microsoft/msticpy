# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
Deprecated - module proc_tree_build_winlx.py has moved.

See :py:mod:`msticpy.transform.proc_tree_build_winlx`
"""
import warnings

from .._version import VERSION

__version__ = VERSION
__author__ = "Pete Bryan"


# flake8: noqa: F403, F401
# pylint: disable=wildcard-import, unused-wildcard-import, unused-import
from ..transform.proc_tree_build_winlx import *

WARN_MSSG = (
    "This module has moved to msticpy.transform.proc_tree_build_winlx\n"
    "Please change your import to reflect this new location."
    "This will be removed in MSTICPy v2.2.0"
)
warnings.warn(WARN_MSSG, category=DeprecationWarning)
