# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Helper functions for configuration settings."""
import os
import warnings
from collections import UserDict
from typing import Any, Callable, Dict, List, Optional, Union

import attr
from attr import Factory

from .._version import VERSION
from . import pkg_config as config
from .exceptions import MsticpyImportExtraError

try:
    from ..auth.secret_settings import SecretsClient

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
    args: ProviderArgs = Factory(ProviderArgs)  # type: ignore
    primary: bool = False


# pylint: enable=too-few-public-methods, too-many-ancestors


def _secrets_enabled() -> bool:
    return _SECRETS_ENABLED and _SECRETS_CLIENT


def get_secrets_client_func() -> Callable[..., Optional["SecretsClient"]]:
    """
    Return function to get or create secrets client.

    Returns
    -------
    Callable
        Function to get, replace or create a SecretsClient

    Notes
    -----
    This function creates closure that persists the secrets client
    instance.
    The inner function works as follows:

    - if called with no parameters and SecretsClient is not
      instantiated, it will try to instantiate a SecretsClient,
      assign it to the nonlocal `_secrets_client` and return this instance.
    - if called subsequently it will just return the secrets client.
    - if called with a SecretsClient instance as a parameter, it will
      replace the SecretsClient instance and return that.

    """
    _secrets_client: Optional["SecretsClient"] = None

    def _return_secrets_client(
        secrets_client: Optional["SecretsClient"] = None, **kwargs
    ) -> Optional["SecretsClient"]:
        """Return (optionally setting or creating) a SecretsClient."""
        nonlocal _secrets_client
        if not _SECRETS_ENABLED:
            return None
        if isinstance(secrets_client, SecretsClient):
            _secrets_client = secrets_client
        if _secrets_client is None:
            _secrets_client = SecretsClient(**kwargs)
        return _secrets_client

    return _return_secrets_client


# Create a SecretsClient instance if it can be imported when
# the module is imported.
_SECRETS_CLIENT: Any = None
# Create the secrets client closure
_SET_SECRETS_CLIENT: Callable[
    ..., Optional["SecretsClient"]
] = get_secrets_client_func()
# Create secrets client instance if SecretsClient can be imported
# and config has KeyVault settings.
if "KeyVault" in config.settings and config.settings["KeyVault"] and _SECRETS_ENABLED:
    _SECRETS_CLIENT = _SET_SECRETS_CLIENT()


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
            _SECRETS_CLIENT = _SET_SECRETS_CLIENT()
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
    """Reload settings from config files."""
    config.refresh_config()


def refresh_keyring():
    """Refresh local keyring secrets cache from Key Vault."""
    if _secrets_enabled():
        _SECRETS_CLIENT.refresh_keyring()


def clear_keyring():
    """Delete local keyring secrets cache."""
    if _secrets_enabled():
        _SECRETS_CLIENT.clear_keyring_secrets()


def auth_secrets_client(
    tenant_id: Optional[str] = None,
    auth_methods: List[str] = None,
    credential: Any = None,
    **kwargs,
):
    """
    Authenticate the Secrets/Key Vault client.

    Parameters
    ----------
    auth_methods : List[str], optional
        List of authentication methods to try
        Possible options are:
        - "env" - to get authentication details from environment variables
        - "cli" - to use Azure CLI authentication details
        - "msi" - to user Managed Service Identity details
        - "interactive" - to prompt for interactive login
        - "vscode" - to use VSCode credentials
        - "powershell" - to use PowerShell credentials
        - "interactive" - to prompt for interactive login
        - "cache" - to use shared token cache credentials
        - "devicecode" - to use device code with web login
        - "clientsecret" - to use client id/secret login.
        Default is ["env", "msi", "vscode", "cli", "powershell", "interactive"]
    tenant_id : str, optional
        The tenant to authenticate against. If not supplied, the default
        tenant for the identity will be used.
    silent : bool, optional
        Set True to hide all output during connection, by default False
    credential : Azure Credential, optional
        Use an existing Azure credential to authenticate.

    Other Parameters
    ----------------
    client_id : str
        Required when using "clientsecret" auth method
    client_secret : str
        Required when using "clientsecret" auth method

    """
    if _secrets_enabled():
        secrets_client = SecretsClient(
            tenant_id=tenant_id,
            auth_methods=auth_methods,
            credential=credential,
            **kwargs,
        )
        _SET_SECRETS_CLIENT(secrets_client=secrets_client)


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
        return _SECRETS_CLIENT.get_secret_accessor(  # type:ignore
            ".".join(config_path)
        )
    return None
