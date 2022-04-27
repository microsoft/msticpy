# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Deprecated path for common.wsconfig.py."""
import warnings

# flake8: noqa: F401
# pylint: disable=unused-import
from ..common.wsconfig import WorkspaceConfig

WARN_MSSG = (
    "This module has moved to msticpy.common.wsconfig\n"
    "Please change your import to reflect this new location."
    "This will be removed in MSTICPy v2.2.0"
)
warnings.warn(WARN_MSSG, category=DeprecationWarning)
