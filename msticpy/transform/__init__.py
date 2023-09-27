# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""MSTICPy Data Processing Tools."""

from .._version import VERSION
from ..lazy_importer import lazy_import

__version__ = VERSION

_LAZY_IMPORTS = {
    "msticpy.context.geoip.GeoLiteLookup",
    "msticpy.context.geoip.IPStackLookup",
    "msticpy.context.geoip.geo_distance",
    "msticpy.context.tilookup.TILookup",
    "msticpy.transform.iocextract.IoCExtract",
}

module, __getattr__, __dir__ = lazy_import(__name__, _LAZY_IMPORTS)
