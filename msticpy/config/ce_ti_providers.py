# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""TI Providers Component Edit."""
from .._version import VERSION
from .ce_provider_base import CEProviders

__version__ = VERSION
__author__ = "Ian Hellen"


# pylint: disable=too-many-ancestors, duplicate-code
class CETIProviders(CEProviders):
    """Threat Intel providers edit component."""

    _DESCRIPTION = "Settings for Threat Intelligence Providers"
    _COMP_PATH = "TIProviders"
    # _HELP_TEXT inherited from base
    _HELP_URI = {
        "Threat Intel Providers": (
            "https://msticpy.readthedocs.io/en/latest/data_acquisition/"
            + "TIProviders.html#configuration-file"
        ),
        "Key Vault Configuration": (
            "https://msticpy.readthedocs.io/en/latest/getting_started/"
            + "msticpyconfig.html#specifying-secrets-as-key-vault-secrets"
        ),
        "MSTICPy Configuration": (
            "https://msticpy.readthedocs.io/en/latest/"
            + "getting_started/msticpyconfig.html"
        ),
        "Help on this tab": (
            "https://msticpy.readthedocs.io/en/latest/getting_started/"
            + "SettingsEditor.html#adding-threat-intelligence-ti-providers"
        ),
    }
