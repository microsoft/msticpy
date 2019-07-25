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
    args: Dict[str, str] = Factory(dict)
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


def _get_setting_args(prov_args: Dict[str, Any]) -> Dict[str, Any]:
    """Extract the provider args from the settings."""
    arg_dict = {}
    if "ApiID" in prov_args:
        if isinstance(prov_args["ApiID"], str):
            arg_dict["api_id"] = prov_args["ApiID"]
        else:
            arg_dict["api_id"] = _fetch_setting(prov_args["ApiID"])

    if "AuthKey" in prov_args:
        if isinstance(prov_args["AuthKey"], str):
            arg_dict["auth_key"] = prov_args["AuthKey"]
        else:
            arg_dict["auth_key"] = _fetch_setting(prov_args["AuthKey"])
    return prov_args


def _fetch_setting(config_setting: Dict[str, Any]) -> str:
    """Return required value for indirect settings (e.g. getting env var)."""
    item_settings = next(iter(config_setting.values()))
    if "EnvironmentVar" in item_settings:
        env_value = environ.get(item_settings["EnvironmentVar"])
        if not env_value:
            warnings.warn(
                f"Environment variable {item_settings['EnvironmentVar']} ",
                "was not set",
            )
        return env_value
    if "KeyVaultURI" in item_settings:
        raise NotImplementedError("Keyvault support not yet implemented.")
    return None
