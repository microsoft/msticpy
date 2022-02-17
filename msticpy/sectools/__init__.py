# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
MSTICy sectools

This sub-package is deprecated.
All functionality has been removed from this sub-package and moved
to other sub-packages:

- TI providers -> msticpy.data.context.tiproviders
  (including vtlookup and vtlookupv3)
- auditdextract -> msticpy.analysis.data
- base64unpack  -> msticpy.analysis.data
- cmd_line -> msticpy.data.context
- domain_utils -> msticpy.data.context
- eventcluster -> msticpy.analysis
- geoip -> msticpy.data.context
- iocextract -> msticpy.analysis.data
- ip_utils -> msticpy.data.context
- proc_tree_builder -> msticpy.analysis.data
- proc_tree_build_mde -> msticpy.analysis.data
- proc_tree_build_winlx -> msticpy.analysis.data
- proc_tree_schema -> msticpy.analysis.data
- proc_tree_utils -> msticpy.analysis.data
- sectools_magics -> msticpy.analysis
- syslog_utils -> msticpy.analysis

The sectools sub-package will be removed in version 2.0.0

"""

# from . import process_tree_utils as ptree
from .._version import VERSION
from ..analysis.data import base64unpack as base64

# flake8: noqa: F403
# pylint: disable=W0401
from ..analysis.data.iocextract import IoCExtract
from ..data.context.geoip import GeoLiteLookup, IPStackLookup, geo_distance
from ..data.context.tilookup import TILookup
from ..data.context.tiproviders.vtlookupv3.vtlookup import VTLookup

__version__ = VERSION
