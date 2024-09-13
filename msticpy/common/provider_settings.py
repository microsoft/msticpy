# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Helper functions for configuration settings."""
from __future__ import annotations

from dataclasses import dataclass, field
import os
import warnings
from collections import UserDict
from typing import Any, Callable

from .._version import VERSION
from .exceptions import MsticpyImportExtraError
from .pkg_config import get_config, refresh_config

try:
    from ..auth.secret_settings import SecretsClient

    _SECRETS_ENABLED = True
except ImportError:
    _SECRETS_ENABLED = False


__version__ = VERSION
__author__ = "Ian Hellen"


class ProviderArgs(UserDict):
    """ProviderArgs dictionary."""

    def __getitem__(self, key) -> Any:
        """Return key value via SecretsClient.read_secret."""
        if key not in self.data:
            raise KeyError(key)
        if _SECRETS_ENABLED:
            return SecretsClient.read_secret(self.data[key])
        return self.data[key]


@dataclass
class ProviderSettings:
    """Provider settings."""

    name: str
    description: str
    provider: str | None = field(default=None)
    args: ProviderArgs = field(default_factory=ProviderArgs)
    primary: bool = field(default=False)


def _secrets_enabled() -> bool:
    return _SECRETS_ENABLED and _SECRETS_CLIENT


def get_secrets_client_func() -> Callable[..., "SecretsClient" | None]:
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
    _secrets_client: "SecretsClient" | None = None

    def _return_secrets_client(
        secrets_client: "SecretsClient" | None = None, **kwargs
    ) -> "SecretsClient" | None:
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
_SET_SECRETS_CLIENT: Callable[..., "SecretsClient" | None] = get_secrets_client_func()
# Create secrets client instance if SecretsClient can be imported
# and config has KeyVault settings.
if get_config("KeyVault", None) and _SECRETS_ENABLED:
    _SECRETS_CLIENT = _SET_SECRETS_CLIENT()


def get_provider_settings(config_section="TIProviders") -> dict[str, ProviderSettings]:
    """
    Read Provider settings from package config.

    Parameters
    ----------
    config_section : str, optional
        [description], by default "TIProviders"

    Returns
    -------
    dict[str, ProviderSettings]
        Provider settings indexed by provider name.

    """
    # pylint: disable=global-statement
    global _SECRETS_CLIENT
    # pylint: enable=global-statement
    if get_config("KeyVault", None):
        if _SECRETS_CLIENT is None and _SECRETS_ENABLED:
            print(
                "KeyVault enabled. Secrets access may require additional authentication."
            )
            _SECRETS_CLIENT = _SET_SECRETS_CLIENT()
    else:
        _SECRETS_CLIENT = None
    section_settings = get_config(config_section, None)
    if not section_settings:
        return {}

    settings = {}
    for provider, item_settings in section_settings.items():
        prov_args = item_settings.get("Args")
        prov_settings = ProviderSettings(  # type: ignore[call-arg]
            name=provider,
            description=item_settings.get("Description"),
            args=_get_setting_args(
                config_path=config_section,
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
    refresh_config()


def refresh_keyring():
    """Refresh local keyring secrets cache from Key Vault."""
    if _secrets_enabled():
        _SECRETS_CLIENT.refresh_keyring()


def clear_keyring():
    """Delete local keyring secrets cache."""
    if _secrets_enabled():
        _SECRETS_CLIENT.clear_keyring_secrets()


def auth_secrets_client(
    tenant_id: str | None = None,
    auth_methods: list[str] | None = None,
    credential: Any = None,
    **kwargs,
) -> None:
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
        secrets_client: SecretsClient = SecretsClient(
            tenant_id=tenant_id,
            auth_methods=auth_methods,
            credential=credential,
            **kwargs,
        )
        _SET_SECRETS_CLIENT(secrets_client=secrets_client)


def get_protected_setting(config_path, setting_name) -> Any:
    """Return a potentially protected setting value."""
    config_settings = get_config(config_path)
    prov_args = _get_protected_settings(config_path, config_settings)
    return prov_args.get(setting_name)


def _get_setting_args(
    config_path: str,
    provider_name: str,
    prov_args: dict[str, Any] | None,
) -> ProviderArgs:
    """Extract the provider args from the settings."""
    if not prov_args:
        return ProviderArgs()
    name_map: dict[str, str] = {
        "workspaceid": "workspace_id",
        "tenantid": "tenant_id",
        "subscriptionid": "subscription_id",
    }
    return _get_protected_settings(
        setting_path=f"{config_path}.{provider_name}.Args",
        section_settings=prov_args,
        name_map=name_map,
    )


def _get_protected_settings(
    setting_path: str,
    section_settings: dict[str, Any] | None,
    name_map: dict[str, str] | None = None,
) -> ProviderArgs:
    """
    Lookup configuration values config, environment or KeyVault.

    Parameters
    ----------
    setting_path : str
        Dotted path to the setting
    section_settings : Optional[dict[str, Any]]
        The configuration settings for this path.
    name_map : Optional[dict[str, str]], optional
        Optional mapping to re-write setting names,
        by default None

    Returns
    -------
    ProviderArgs
        Dictionary of resolved settings

    """
    if not section_settings:
        return ProviderArgs()
    setting_dict: ProviderArgs = ProviderArgs(section_settings.copy())

    for arg_name, arg_value in section_settings.items():
        target_name: str = arg_name
        if name_map:
            target_name = name_map.get(target_name.casefold(), target_name)

        try:
            setting_dict[target_name] = _fetch_secret_setting(
                f"{setting_path}.{arg_name}", arg_value
            )
        except NotImplementedError:
            warnings.warn(f"Setting type for setting {arg_value} not yet implemented. ")
    return setting_dict


def _fetch_secret_setting(
    setting_path: str,
    config_setting: str | dict[str, Any],
) -> str | Callable[[], Any] | None:
    """
    Return required value for potential secret setting.

    Parameters
    ----------
    setting_path : str
        Dotted path to the setting
    config_setting : Union[str, dict[str, Any]]
        Setting value (str or Dict)

    Returns
    -------
    Union[Optional[str], Callable[[], Any]]
        Either a string or accessor function.

    Raises
    ------
    MsticpyImportExtraError
        _description_
    NotImplementedError
        _description_

    """
    if isinstance(config_setting, str):
        return config_setting
    if not isinstance(config_setting, dict):
        err_msg: str = (
            "Configuration setting format not recognized. "
            f"'{setting_path}' should be a string or dictionary "
            "with either 'EnvironmentVar' or 'KeyVault' entry."
        )
        raise NotImplementedError(err_msg)
    if "EnvironmentVar" in config_setting:
        env_value: str | None = os.environ.get(config_setting["EnvironmentVar"])
        if not env_value:
            warnings.warn(
                f"Environment variable {config_setting['EnvironmentVar']}"
                f" ({setting_path})"
                " was not set"
            )
        return env_value
    if "KeyVault" in config_setting:
        if not _SECRETS_ENABLED:
            err_msg = "Cannot use this feature without Key Vault support installed"
            raise MsticpyImportExtraError(
                err_msg,
                title="Error importing Loading Key Vault and/or keyring libraries.",
                extra="keyvault",
            )
        if not _SECRETS_CLIENT:
            warnings.warn(
                "Cannot use a KeyVault configuration setting without"
                "a KeyVault configuration section in msticpyconfig.yaml"
                f" ({setting_path})",
                stacklevel=1,
            )
            return None
        return _SECRETS_CLIENT.get_secret_accessor(setting_path)
    err_msg = (
        "Configuration setting format not recognized. "
        f"'{setting_path}' should be a string or dictionary "
        "with either 'EnvironmentVar' or 'KeyVault' entry."
    )
    raise NotImplementedError(err_msg)
