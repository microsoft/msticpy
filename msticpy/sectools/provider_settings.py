# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Helper functions for configuration settings."""
from collections import UserDict
import os
from typing import Any, Dict, Optional, Union, Callable
import warnings

import attr
from attr import Factory

from .._version import VERSION
from ..common.secret_settings import SecretsClient
from ..nbtools import pkg_config as config
from ..nbtools.utility import MsticpyConfigException

__version__ = VERSION
__author__ = "Ian Hellen"


# pylint: disable=too-few-public-methods, too-many-ancestors
class ProviderArgs(UserDict):
    """ProviderArgs dictionary."""

    def __getitem__(self, key):
        """Return key value via SecretsClient.read_secret."""
        if key not in self.data:
            raise KeyError(key)
        return SecretsClient.read_secret(self.data[key])


@attr.s(auto_attribs=True)
class ProviderSettings:
    """Provider settings."""

    name: str
    description: str
    provider: Optional[str] = None
    args: Dict[Optional[str], Any] = Factory(ProviderArgs)  # type: ignore
    primary: bool = False


# pylint: enable=too-few-public-methods, too-many-ancestors


_SECRETS_SETTINGS: Optional[SecretsClient] = None
if "KeyVault" in config.settings:
    _SECRETS_SETTINGS = SecretsClient()


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
    # pylint: disable=global-statement
    global _SECRETS_SETTINGS
    # pylint: enable=global-statement
    if "KeyVault" in config.settings:
        _SECRETS_SETTINGS = SecretsClient()
    else:
        _SECRETS_SETTINGS = None
    prov_settings = config.settings.get(config_section)
    if not prov_settings:
        return {}

    settings = {}
    for provider, item_settings in prov_settings.items():
        prov_args = item_settings.get("Args")
        prov_settings = ProviderSettings(
            name=provider,
            description=item_settings.get("Description"),
            args=_get_setting_args(
                config_section=config_section,
                provider_name=provider,
                prov_args=prov_args,
            ),
            primary=item_settings.get("Primary", False),
            provider=item_settings.get("Provider", provider),
        )
        settings[provider] = prov_settings

    return settings


def reload_settings():
    """Reload settings from config files."""
    config.refresh_config()


def _get_setting_args(
    config_section: str, provider_name: str, prov_args: Optional[Dict[str, Any]]
) -> Dict[Any, Any]:
    """Extract the provider args from the settings."""
    if not prov_args:
        return {}
    name_map = {
        "workspaceid": "workspace_id",
        "tenantid": "tenant_id",
        "subscriptionid": "subscription_id",
    }
    return _get_settings(
        config_section=config_section,
        provider_name=provider_name,
        conf_group=prov_args,
        name_map=name_map,
    )


def _get_settings(
    config_section: str,
    provider_name: str,
    conf_group: Optional[Dict[str, Any]],
    name_map: Optional[Dict[str, str]] = None,
) -> Dict[Any, Any]:
    """
    Lookup configuration values config, environment or KeyVault.

    Parameters
    ----------
    config_section : str
        Configuration section
    provider_name: str
        The name of the provider section
    conf_group : Optional[Dict[str, Any]]
        The configuration dictionary
    name_map : Optional[Dict[str, str]], optional
        Optional mapping to re-write setting names,
        by default None

    Returns
    -------
    Dict[Any, Any]
        Dictionary of resolved settings

    Raises
    ------
    NotImplementedError
        Keyvault storage is not yet implemented

    """
    if not conf_group:
        return {}
    setting_dict: Dict[str, Any] = conf_group.copy()

    for arg_name, arg_value in conf_group.items():
        target_name = arg_name
        if name_map:
            target_name = name_map.get(target_name.casefold(), target_name)

        if isinstance(arg_value, str):
            setting_dict[target_name] = arg_value
        elif isinstance(arg_value, dict):
            try:
                setting_dict[target_name] = _fetch_setting(
                    config_section, provider_name, arg_name, arg_value
                )  # type: ignore
            except NotImplementedError:
                warnings.warn(
                    f"Setting type for setting {arg_value} not yet implemented. "
                )
    return setting_dict


def _fetch_setting(
    config_section: str,
    provider_name: str,
    arg_name: str,
    config_setting: Dict[str, Any],
) -> Union[Optional[str], Callable[[], Any]]:
    """Return required value for indirect settings (e.g. getting env var)."""
    if "EnvironmentVar" in config_setting:
        env_value = os.environ.get(config_setting["EnvironmentVar"])
        if not env_value:
            warnings.warn(
                f"Environment variable {config_setting['EnvironmentVar']}"
                + " was not set"
            )
        return env_value
    if "KeyVault" in config_setting:
        if not _SECRETS_SETTINGS:
            raise MsticpyConfigException(
                "Cannot use a KeyVault configuration setting without",
                "a KeyVault configuration section in msticpyconfig.yaml.",
            )
        config_path = [config_section, provider_name, "Args", arg_name]
        sec_func = _SECRETS_SETTINGS.get_secret_accessor(".".join(config_path))
        return sec_func
    return None
