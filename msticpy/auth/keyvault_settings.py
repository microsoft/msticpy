# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Keyvault client settings."""

import warnings
from typing import Any, List, Optional

from .._version import VERSION
from ..common import pkg_config as config
from ..common.exceptions import MsticpyKeyVaultConfigError
from ..common.utility import export
from .azure_auth_core import AzureCloudConfig
from .cloud_mappings import create_cloud_ep_dict, create_cloud_suf_dict

__version__ = VERSION
__author__ = "Ian Hellen"


@export
class KeyVaultSettings:
    """
    KeyVaultSettings class - reads settings from msticpyconfig.

    Notes
    -----
    The KeyVault section in msticpyconfig.yaml can contain
    the following::

        KeyVault:
            TenantId: {tenantid-to-use-for-authentication}
            SubscriptionId: {subscriptionid-containing-vault}
            ResourceGroup: {resource-group-containing-vault}
            AzureRegion: {region-for-vault}
            VaultName: {vault-name}
            UseKeyring: True
            Authority: global

    `SubscriptionId`, `ResourceGroup` and `AzureRegion` are only
    used when creating new vaults.
    `UseKeyring` instructs the `SecretsClient` to cache Keyvault
    secrets locally using Python keyring.
    `Authority` is one of 'global', 'usgov', 'de', 'cn'
    Alternatively, you can specify `AuthorityURI` with the value
    pointing to the URI for logon requests.

    """

    AAD_AUTHORITIES = create_cloud_ep_dict("active_directory")
    RES_MGMT_URIS = create_cloud_ep_dict("resource_manager")
    KV_SUFFIXES = create_cloud_suf_dict("keyvault_dns")
    KV_URIS = {
        cloud: f"https://{{vault}}{suffix}" for cloud, suffix in KV_SUFFIXES.items()
    }

    # Azure CLI Client ID
    CLIENT_ID = "04b07795-8ddb-461a-bbee-02f9e1bf7b46"  # xplat

    def __init__(self):
        """
        Initialize new instance of KeyVault Settings.

        Raises
        ------
        MsticpyKeyVaultConfigError
            If no Key Vault settings are found in
            msticpyconfig.yaml.

        """
        self.authority: Optional[str] = None
        self.auth_methods: List[str] = []
        try:
            kv_config = config.get_config("KeyVault")
        except KeyError as err:
            raise MsticpyKeyVaultConfigError(
                "No KeyVault section found in msticpyconfig.yaml",
                title="missing Key Vault configuration",
            ) from err
        norm_settings = {key.casefold(): val for key, val in kv_config.items()}
        self.__dict__.update(norm_settings)

        self._get_auth_methods_from_settings()
        self._get_authority_from_settings()

    def _get_auth_methods_from_settings(self):
        """Retrieve authentication methods from settings."""
        self.auth_methods = AzureCloudConfig().auth_methods

    def _get_authority_from_settings(self):
        """Get the authority (AAD) URI from settings."""
        if "authorityuri" in self:
            # For BlueHound compat - the "authority_uri" can be set directly
            # as a property of the object
            rev_lookup = {uri.casefold(): code for code, uri in self.AAD_AUTHORITIES}
            self.authority = rev_lookup.get(
                self["authorityuri"].casefold(), "global"
            ).casefold()
        elif not self.authority:
            self.authority = AzureCloudConfig().cloud

    def __getitem__(self, key: str):
        """Allow property get using dictionary key syntax."""
        if key.casefold() in self.__dict__:
            return self.__dict__[key.casefold()]
        raise KeyError

    def __setitem__(self, key: str, value: Any):
        """Allow property set using dictionary key syntax."""
        self.__dict__[key.casefold()] = value

    def __contains__(self, key: str):
        """Return true if key is a valid attribute."""
        return key.casefold() in self.__dict__

    def get(self, key: str, default: Any = None) -> Any:
        """Return dict value or default."""
        return self.__dict__.get(key.casefold(), default)

    @property
    def cloud(self) -> str:
        """Return the cloud for the settings."""
        return self.get("authority", "global").casefold()

    @property
    def authority_uri(self) -> str:
        """
        Return authority URI for cloud.

        Returns
        -------
        str
            Authority URI

        """
        if "authorityuri" in self:
            return self["authorityuri"]
        if self.cloud in self.AAD_AUTHORITIES:
            return self.AAD_AUTHORITIES[self.cloud]
        return self.AAD_AUTHORITIES["global"]

    @property
    def keyvault_uri(self) -> Optional[str]:
        """Return KeyVault URI template for current cloud."""
        kv_uri = self.KV_URIS.get(self.cloud)
        if not kv_uri:
            mssg = f"Could not find a valid KeyVault endpoint for {self.cloud}"
            warnings.warn(mssg)
        return kv_uri

    @property
    def mgmt_uri(self) -> Optional[str]:
        """Return Azure management URI template for current cloud."""
        mgmt_uri = self.RES_MGMT_URIS.get(self.cloud)
        if not mgmt_uri:
            mssg = f"Could not find a valid KeyVault endpoint for {self.cloud}"
            warnings.warn(mssg)
        return mgmt_uri

    def get_tenant_authority_uri(
        self, authority_uri: str = None, tenant: str = None
    ) -> str:
        """
        Return authority URI for tenant.

        Parameters
        ----------
        authority_uri : str, optional
            The authority URI - otherwise try to retrieve
            from settings
        tenant : str, optional
            TenantID or name, by default None.
            If not passed as a parameter try to get tenant from
            KeyVault configuration in msticpyconfig.yaml

        Returns
        -------
        str
            Tenant Authority

        Raises
        ------
        KeyVaultConfigException
            If tenant is not defined.

        """
        auth = authority_uri or self.authority_uri.strip()
        if not tenant:
            tenant = self.get("tenantid")
        if not tenant:
            raise MsticpyKeyVaultConfigError(
                "Could not get TenantId from function parameters or configuration.",
                "Please add this to the KeyVault section of msticpyconfig.yaml",
                title="missing tenant ID value.",
            )
        if auth.endswith("/"):
            return f"{auth}{tenant.strip()}"
        return f"{auth}/{tenant.strip()}"

    def get_tenant_authority_host(
        self, authority_uri: str = None, tenant: str = None
    ) -> str:
        """
        Return tenant authority URI with no leading scheme.

        Parameters
        ----------
        authority_uri : str, optional
            The authority URI - otherwise try to retrieve
            from settings
        tenant : str, optional
            TenantID or name, by default None.
            If not passed as a parameter try to get tenant from
            KeyVault configuration in msticpyconfig.yaml

        Returns
        -------
        str
            Tenant Authority

        Raises
        ------
        KeyVaultConfigException
            If tenant is not defined.

        """
        if not tenant:
            tenant = self.get("tenantid")
        if not tenant:
            raise MsticpyKeyVaultConfigError(
                "Could not get TenantId from function parameters or configuration.",
                "Please add this to the KeyVault section of msticpyconfig.yaml",
                title="missing tenant ID value.",
            )
        return (
            self.get_tenant_authority_uri(authority_uri, tenant)
            .lower()
            .replace("https://", "")
        )
