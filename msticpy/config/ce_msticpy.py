# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Key Vault component edit."""
from .._version import VERSION
from .ce_simple_settings import CESimpleSettings

__version__ = VERSION
__author__ = "Ian Hellen"


class CEMsticpy(CESimpleSettings):
    """Key Vault settings edit component."""

    _DESCRIPTION = "MSTICPy Settings"
    _COMP_PATH = "msticpy"
    _HELP_TEXT = """
    Set the global parameters for MSTICPy.<br>

    This section lets you set some global parameters such as:<br>

    <ul>
    <li>Paths to query files</li>
    <li>Enabling/disabling friendly exceptions</li>
    <li>HTTP timeouts</li>
    </ul>
    Note: Proxies cannot currently be set from this interface.
    Edit the msticpyconfig.yaml file directly to set proxies
    following the guidance in the link to the documentation.<br>
    """
    _HELP_URI = {
        "MSTICPy global settings": (
            "https://msticpy.readthedocs.io/en/latest/getting_started/"
            + "msticpyconfig.html#msticpy-global-settings"
        )
    }
