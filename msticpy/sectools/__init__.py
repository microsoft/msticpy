# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""MSTIC Security Tools."""

# flake8: noqa: F403
# pylint: disable=W0401
from .._version import VERSION

try:
    from IPython import get_ipython
except ImportError as err:
    pass

__version__ = VERSION
