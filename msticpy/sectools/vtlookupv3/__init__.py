# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""VirusTotal V3 Subpackage."""

from ..._version import VERSION

# pylint: disable=unused-import
# flake8: noqa: F401
from .vtfile_behavior import VTFileBehavior
from .vtlookupv3 import (
    VT_API_NOT_FOUND,
    MsticpyVTNoDataError,
    VTEntityType,
    VTLookupV3,
    VTObjectProperties,
)
from .vtobject_browser import VTObjectBrowser

__version__ = VERSION
__author__ = "Ian Hellen"
