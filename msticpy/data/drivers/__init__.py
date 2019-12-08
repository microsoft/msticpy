# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Data provider sub-package."""
# flake8: noqa: F403
from .driver_base import DriverBase
from .kql_driver import KqlDriver
from .security_graph_driver import SecurityGraphDriver
from .mdatp_driver import MDATPDriver

from ..._version import VERSION

__version__ = VERSION
