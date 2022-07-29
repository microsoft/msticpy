# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Azure component edit."""
from .._version import VERSION
from .ce_simple_settings import CESimpleSettings

__version__ = VERSION
__author__ = "Ian Hellen"


class CEAzure(CESimpleSettings):
    """Azure settings edit component."""

    _DESCRIPTION = "Azure Defaults"
    _COMP_PATH = "Azure"
    _HELP_TEXT = """
    Set the parameters for your Azure cloud settings here.<br>

    <b>cloud</b> should be set to the Azure Cloud that you use.
    Options are:
    <ul>
    <li>global (Commercial Azure cloud)</li>
    <li>usgov (US Government cloud)</li>
    <li>cn (China national cloud)</li>
    <li>de (German national cloud)</li>
    </ul>
    The default is "global".<br>

    <b>auth_methods</b> is the default set of methods to use for Azure authentication.
    These can be overridden by settings of individual component settings. The options are:
    <ul>
    <li>env - take credential information from environment variables</li>
    <li>msi - use Managed Service Identity credentials, if available</li>
    <li>cli - use Azure CLI credentials, if available</li>
    <li>interactive - use interactive browser authentication</li>
    <li>vscode" - to use VSCode credentials</li>
    <li>powershell" - to use PowerShell credentials</li>
    <li>cache" - to use shared token cache credentials</li>
    </ul>
    """
    _HELP_URI = {
        "MSTICPy Package Configuration": (
            "https://msticpy.readthedocs.io/en/latest/getting_started/msticpyconfig.html"
        )
    }
