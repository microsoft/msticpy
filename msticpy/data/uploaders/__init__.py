# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Data uploader sub-package."""
# flake8: noqa: F403
from .uploader_base import UploaderBase

try:
    from .loganalytics_uploader import LAUploader
    from .splunk_uploader import SplunkUploader
except ImportError:
    pass

from ..._version import VERSION

__version__ = VERSION
