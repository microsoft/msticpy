# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Data storage sub-package."""

from ..._version import VERSION

# pylint: disable=unused-import
# flake8: noqa: F403
from .azure_blob_storage import AzureBlobStorage

__version__ = VERSION
