# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Helper functions for configuration settings."""
from typing import Any, Dict, Optional

import attr
from attr import Factory

from .._version import VERSION
from ..nbtools import pkg_config as config

__version__ = VERSION
__author__ = "Ian Hellen"


# pylint: disable=too-few-public-methods
@attr.s(auto_attribs=True)
class ProviderSettings:
    """Provider settings."""

    name: str
    description: str
    provider: Optional[str] = None
    args: Dict[Optional[str], Any] = Factory(dict)
    primary: bool = False


def get_provider_settings(config_section="TIProviders") -> Dict[str, ProviderSettings]:
    """
    Read Provider settings from package config.

    Parameters
    ----------
    config_section : str, optional
        [description], by default "TIProviders"

    Returns
    -------
    Dict[str, ProviderSettings]
        Provider settings indexed by provider name.

    """
    prov_settings = config.settings.get(config_section)
    if not prov_settings:
        return {}

    settings = {}
    for provider, item_settings in prov_settings.items():
        prov_args = item_settings.get("Args")
        prov_settings = ProviderSettings(
            name=provider,
            description=item_settings.get("Description"),
            args=_get_setting_args(prov_args),
            primary=item_settings.get("Primary", False),
            provider=item_settings.get("Provider", provider),
        )
        settings[provider] = prov_settings

    return settings


def reload_settings():
    """Reload settings from config files."""
    config.refresh_config()


def _get_setting_args(prov_args: Optional[Dict[str, Any]]) -> Dict[Any, Any]:
    """Extract the provider args from the settings."""
    if not prov_args:
        return {}
    name_map = {
        "WorkspaceID": "workspace_id",
        "TenantID": "tenant_id",
        "SubscriptionID": "subscription_id",
    }
    return config.get_settings(conf_group=prov_args, name_map=name_map)
