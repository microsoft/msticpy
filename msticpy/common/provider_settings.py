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
from .exceptions import MsticpyImportExtraError
from . import pkg_config as config

try:
    from .secret_settings import SecretsClient

    _SECRETS_ENABLED = True
except ImportError:
    _SECRETS_ENABLED = False


__version__ = VERSION
__author__ = "Ian Hellen"


# pylint: disable=too-few-public-methods, too-many-ancestors
class ProviderArgs(UserDict):
    """ProviderArgs dictionary."""

    def __getitem__(self, key):
        """Return key value via SecretsClient.read_secret."""
        if key not in self.data:
            raise KeyError(key)
        if _SECRETS_ENABLED:
            return SecretsClient.read_secret(self.data[key])
        return self.data[key]


@attr.s(auto_attribs=True)
class ProviderSettings:
    """Provider settings."""

    name: str
    description: str
    provider: Optional[str] = None
    args: ProviderArgs = Factory(ProviderArgs)
    primary: bool = False


# pylint: enable=too-few-public-methods, too-many-ancestors


_SECRETS_CLIENT: Any = None
if "KeyVault" in config.settings and config.settings["KeyVault"] and _SECRETS_ENABLED:
    _SECRETS_CLIENT = SecretsClient()


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
    global _SECRETS_CLIENT
    # pylint: enable=global-statement
    if "KeyVault" in config.settings and config.settings["KeyVault"]:
        if _SECRETS_CLIENT is None and _SECRETS_ENABLED:
            print(
                "KeyVault enabled. Secrets access may require additional authentication."
            )
            _SECRETS_CLIENT = SecretsClient()
    else:
        _SECRETS_CLIENT = None
    section_settings = config.settings.get(config_section)
    if not section_settings:
        return {}

    settings = {}
    for provider, item_settings in section_settings.items():
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
    """
    Reload settings from config files.

    Parameters
    ----------
    clear_keyring : bool, optional
        Clears any secrets cached in keyring, by default False

    """
    config.refresh_config()


def _get_setting_args(
    config_section: str, provider_name: str, prov_args: Optional[Dict[str, Any]]
) -> ProviderArgs:
    """Extract the provider args from the settings."""
    if not prov_args:
        return ProviderArgs()
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
) -> ProviderArgs:
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
    ProviderArgs
        Dictionary of resolved settings

    Raises
    ------
    NotImplementedError
        Keyvault storage is not yet implemented

    """
    if not conf_group:
        return ProviderArgs()
    setting_dict: ProviderArgs = ProviderArgs(conf_group.copy())

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
                )
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
                + f" (provider {provider_name})"
                + " was not set"
            )
        return env_value
    if "KeyVault" in config_setting:
        if not _SECRETS_ENABLED:
            raise MsticpyImportExtraError(
                "Cannot use this feature without Key Vault support installed",
                title="Error importing Loading Key Vault and/or keyring libaries",
                extra="keyvault",
            )
        if not _SECRETS_CLIENT:
            warnings.warn(
                "Cannot use a KeyVault configuration setting without"
                + "a KeyVault configuration section in msticpyconfig.yaml"
                + f" (provider {provider_name})"
            )
            return None
        config_path = [config_section, provider_name, "Args", arg_name]
        return _SECRETS_CLIENT.get_secret_accessor(".".join(config_path))
    return None
