# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Data uploader sub-package."""

try:
    from .loganalytics_uploader import LAUploader  # noqa: F401
    from .splunk_uploader import SplunkUploader  # noqa: F401
except ImportError:
    pass

from ..._version import VERSION

__version__ = VERSION
