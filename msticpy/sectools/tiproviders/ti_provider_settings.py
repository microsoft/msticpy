# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Helper functions for TI configuration settings."""
import warnings
from os import environ
from typing import Any, Dict, Optional

import attr
from attr import Factory

from ..._version import VERSION
from ...nbtools import pkg_config as config

__version__ = VERSION
__author__ = "Ian Hellen"


# pylint: disable=too-few-public-methods
@attr.s(auto_attribs=True)
class TIProviderSettings:
    """Provider settings."""

    name: str
    description: str
    provider: Optional[str] = None
    args: Dict[Optional[str], Any] = Factory(dict)
    primary: bool = False


def get_provider_settings() -> Dict[str, TIProviderSettings]:
    """
    Read TI Provider settings from package config.

    Returns
    -------
    Dict[str, TIProviderSettings]
        Provider settings indexed by provider name.

    """
    ti_settings = config.settings.get("TIProviders")
    if not ti_settings:
        return {}

    settings = {}
    for provider, item_settings in ti_settings.items():
        prov_args = item_settings.get("Args")
        prov_settings = TIProviderSettings(
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
    arg_dict: Dict[str, Any] = prov_args.copy()
    name_map = {
        "ApiID": "api_id",
        "AuthKey": "auth_key",
        "WorkspaceID": "workspace_id",
        "TenantID": "tenant_id",
        "SubscriptionID": "subscription_id",
    }
    for arg_name, arg_value in prov_args.items():
        target_name = name_map.get(arg_name, arg_name)
        if isinstance(arg_value, str):
            arg_dict[target_name] = arg_value
        elif isinstance(arg_value, dict):
            arg_dict[target_name] = _fetch_setting(arg_value)  # type: ignore
    return arg_dict


def _fetch_setting(config_setting: Dict[str, Any]) -> Optional[str]:
    """Return required value for indirect settings (e.g. getting env var)."""
    item_settings = next(iter(config_setting.values()))
    if "EnvironmentVar" in item_settings:
        env_value = environ.get(item_settings["EnvironmentVar"])
        if not env_value:
            warnings.warn(
                f"Environment variable {item_settings['EnvironmentVar']} "
                + " was not set"
            )
        return env_value
    if "KeyVaultURI" in item_settings:
        raise NotImplementedError("Keyvault support not yet implemented.")
    return None
