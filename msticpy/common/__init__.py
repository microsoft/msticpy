# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
Common MSTICPy modules.

This is sub-package containing core utility, authentication and configuration
modules and classes.

Some of the modules and classes here:

- azure_auth - az_connect function
- pkg_config - the main settings management module
- secret_settings - handles Azure Key Vault and keyring-stored secrets
- provider_settings - allows access to settings that can redirect appropriate
  settings to secret_settings to retrieve from Key Vault/keyring
- wsconfig - Workspace config manager for MS Sentinel authentication
- utility and timespan

"""
