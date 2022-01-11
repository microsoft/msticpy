# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Deprecated path for data.azure."""
import warnings

# flake8: noqa: F403, F401
# pylint: disable=wildcard-import, unused-wildcard-import
from .azure.azure_blob_storage import AzureBlobStorage

WARN_MSSG = (
    "This module has moved to msticpy.data.azure.azure_blob_storage\n"
    + "Please change your import to reflect this new location."
)
warnings.warn(WARN_MSSG, category=DeprecationWarning)