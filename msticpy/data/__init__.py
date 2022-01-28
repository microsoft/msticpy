# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Data sub-package."""
from ..common.exceptions import MsticpyImportExtraError

# flake8: noqa: F403
from .common.data_providers import QueryProvider
from .common.query_defns import DataEnvironment, DataFamily

from .._version import VERSION

__version__ = VERSION
