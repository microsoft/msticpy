# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
Deprecated - module azure_blob_storage.py has moved.

See :py:mod:`msticpy.data.storage.azure_blob_storage`
"""
import warnings

# flake8: noqa: F403, F401
# pylint: disable=unused-import
from ..data.storage.azure_blob_storage import AzureBlobStorage

WARN_MSSG = (
    "This module has moved to msticpy.data.storage.azure_blob_storage\n"
    "Please change your import to reflect this new location."
    "This will be removed in MSTICPy v2.2.0"
)
warnings.warn(WARN_MSSG, category=DeprecationWarning)
