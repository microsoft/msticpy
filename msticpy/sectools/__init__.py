# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
MSTICPy sectools.

.. warning: This sub-package is deprecated.
   All functionality has been removed from this sub-package and moved
   to other sub-packages:

- TI providers -> msticpy.context.tiproviders
  (including vtlookup and vtlookupv3)
- auditdextract -> msticpy.transform
- base64unpack  -> msticpy.transform
- cmd_line -> msticpy.context
- domain_utils -> msticpy.context
- eventcluster -> msticpy.analysis
- geoip -> msticpy.context
- iocextract -> msticpy.transform
- ip_utils -> msticpy.context
- proc_tree_builder -> msticpy.transform
- proc_tree_build_mde -> msticpy.transform
- proc_tree_build_winlx -> msticpy.transform
- proc_tree_schema -> msticpy.transform
- proc_tree_utils -> msticpy.transform
- sectools_magics -> msticpy.init.nbmagics
- syslog_utils -> msticpy.analysis

The sectools sub-package will be removed in version 2.0.0

"""
from .._version import VERSION
from ..lazy_importer import lazy_import

__version__ = VERSION

_LAZY_IMPORTS = {
    "msticpy.context.geoip.GeoLiteLookup",
    "msticpy.context.geoip.IPStackLookup",
    "msticpy.context.geoip.geo_distance",
    "msticpy.context.tilookup.TILookup",
    "msticpy.transform.base64unpack as base64",
    "msticpy.transform.iocextract.IoCExtract",
}

module, __getattr__, __dir__ = lazy_import(__name__, _LAZY_IMPORTS)
