# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Data Providers Component Edit."""
from .._version import VERSION
from .ce_provider_base import CEProviders, HELP_URIS

__version__ = VERSION
__author__ = "Ian Hellen"


# pylint: disable=too-many-ancestors, duplicate-code
class CEDataProviders(CEProviders):
    """Data providers edit component."""

    _DESCRIPTION = "Settings for Data Providers"
    _COMP_PATH = "DataProviders"
    # _HELP_TEXT inherited from base
    _HELP_URI = {
        "Data Providers": (
            "https://msticpy.readthedocs.io/en/latest/" + "DataAcquisition.html"
        ),
        **HELP_URIS,
    }

    _COMPONENT_HELP = """
    <p><b>LocalData provider <i>data_paths</i></b>
    Enter one or more data paths, separated by new lines
    </p>
    """
