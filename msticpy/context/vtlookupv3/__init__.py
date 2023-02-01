# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""VirusTotal V3 Subpackage."""
import contextlib

from ..._version import VERSION

# pylint: disable=unused-import
# flake8: noqa: F401
VT3_AVAILABLE = False
with contextlib.suppress(ImportError):
    import nest_asyncio
    import vt
    import vt_graph_api

    VT3_AVAILABLE = True

if VT3_AVAILABLE:
    from ...vis.vtobject_browser import VTObjectBrowser
    from . import vtlookupv3
    from .vtfile_behavior import VTFileBehavior
    from .vtlookupv3 import (
        VT_API_NOT_FOUND,
        MsticpyVTNoDataError,
        VTEntityType,
        VTLookupV3,
        VTObjectProperties,
    )

__version__ = VERSION
__author__ = "Ian Hellen"
