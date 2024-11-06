# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Context Providers Subpackage."""
from __future__ import annotations

from typing import Any

from ..common.utility import ImportPlaceholder
from ..lazy_importer import lazy_import
from .vtlookupv3 import VT3_AVAILABLE

vtlookupv3: Any
if VT3_AVAILABLE:
    from .vtlookupv3 import vtlookupv3
else:
    # vtlookup3 will not load if vt package not installed
    vtlookupv3 = ImportPlaceholder(
        "vtlookupv3",
        ["vt-py", "vt-graph-api", "nest_asyncio"],
    )


_LAZY_IMPORTS: set[str] = {
    "msticpy.context.geoip.GeoLiteLookup",
    "msticpy.context.geoip.IPStackLookup",
    "msticpy.context.tilookup.TILookup",
}

module, __getattr__, __dir__ = lazy_import(__name__, _LAZY_IMPORTS)
