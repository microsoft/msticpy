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
import contextlib

# from . import process_tree_utils as ptree
from .._version import VERSION
from ..context.geoip import GeoLiteLookup, IPStackLookup, geo_distance
from ..context.tilookup import TILookup
from ..transform import base64unpack as base64

# flake8: noqa: F403
# pylint: disable=W0401
from ..transform.iocextract import IoCExtract

with contextlib.suppress(ImportError):
    from IPython import get_ipython

    from ..init import nbmagics as sectool_magics
__version__ = VERSION
