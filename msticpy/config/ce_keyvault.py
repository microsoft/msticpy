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


class CEKeyVault(CESimpleSettings):
    """Key Vault settings edit component."""

    _DESCRIPTION = "Key Vault Setup"
    _COMP_PATH = "KeyVault"
    _HELP_TEXT = """
    Set the parameters for your Key Vault here to store secret
    values such as API Keys.<br>

    Check <b>UseKeyring</b> if you have Keyring installed and want to be
    able to cache the secrets locally. (Note: keyring is not supported
    by default on many Linux distributions)<br>

    The first five items are mandatory.<br>

    The value for <b>Authority</b> should be set to the Azure Cloud that you use.<br>
    Options are:
    <ul>
    <li>global (Commercial Azure cloud)</li>
    <li>usgov (US Government cloud)</li>
    <li>cn (China national cloud)</li>
    <li>de (German national cloud)</li>
    </ul>
    The default is "global".<br>
    """
    _HELP_URI = {
        "Key Vault Settings": (
            "https://msticpy.readthedocs.io/en/latest/getting_started/"
            + "msticpyconfig.html#specifying-secrets-as-key-vault-secrets"
        )
    }
