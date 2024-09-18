# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
Deprecated - module data.azure has moved.

See :py:mod:`msticpy.context.azure`
"""
import warnings

from ..._version import VERSION

__version__ = VERSION
__author__ = "Pete Bryan"

# pylint: disable=unused-import
from ...context.azure.azure_data import AzureData  # noqa: F401
from ...context.azure.sentinel_core import MicrosoftSentinel  # noqa: F401

WARN_MSSG = (
    "This module has moved to msticpy.context.azure\n"
    "Please change your import to reflect this new location."
    "This will be removed in MSTICPy v2.2.0"
)
warnings.warn(WARN_MSSG, category=DeprecationWarning)
