# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Utility sub-package."""
from ..._version import VERSION
from .format import *  # noqa: F401, F403
from .package import *  # noqa: F401, F403
from .package import _MSTICPY_USER_AGENT

# All modules use the "export" decorator to control
# pylint: disable=unused-import
from .types import export, singleton  # noqa: F401

from .types import *  # isort: skip  # noqa: F401, F403
from .ipython import *  # isort: skip  # noqa: F401, F403

MSTICPY_USER_AGENT = _MSTICPY_USER_AGENT


__version__ = VERSION
__author__ = "Ian Hellen"
