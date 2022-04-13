# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
MSTICPy Analysis Tools.

This sub-package has a few classes of analysis tools:

- data - this sub-package contains data-processing
  tools for dealing with tranforming data (e.g. auditd, process tree)
  or extracting specific data formats (iocextract, b64, cmdline)
- other items are miscellaneous analysis modules such anomalous
  sequence, time series, eventcluster.

"""

from .._version import VERSION

__version__ = VERSION
