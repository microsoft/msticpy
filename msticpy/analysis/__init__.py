# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""MSTIC Analysis Tools."""

from .._version import VERSION

# flake8: noqa: F403
# pylint: disable=W0401
from .geoip import GeoLiteLookup, IPStackLookup, geo_distance

try:
    from IPython import get_ipython
    from . import sectools_magics
except ImportError as err:
    pass

__version__ = VERSION
