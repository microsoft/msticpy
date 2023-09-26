# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Data provider sub-package."""

from ...lazy_importer import lazy_import

_LAZY_IMPORTS = {
    "msticpy.context.azure.azure_data.AzureData",
    "msticpy.context.azure.sentinel_core.MicrosoftSentinel",
}

module, __getattr__, __dir__ = lazy_import(__name__, _LAZY_IMPORTS)
