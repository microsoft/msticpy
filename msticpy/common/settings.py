# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Facade module for settings functions."""

from .._version import VERSION

# pylint: disable=unused-import
from .pkg_config import (  # noqa: F401
    current_config_path,
    get_config,
    get_http_timeout,
    get_settings,
    refresh_config,
    set_config,
    validate_config,
)
from .provider_settings import (  # noqa: F401
    clear_keyring,
    get_protected_setting,
    get_provider_settings,
    refresh_keyring,
    reload_settings,
)
from .proxy_settings import get_http_proxies  # noqa: F401

__version__ = VERSION
__author__ = "Ian Hellen"
