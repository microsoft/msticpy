# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Azure Cloud Mappings."""
import contextlib
from typing import Dict, List, Optional

from msrestazure import azure_cloud

from .._version import VERSION
from ..common import pkg_config as config
from ..common.exceptions import MsticpyAzureConfigError

__version__ = VERSION
__author__ = "Pete Bryan"

CLOUD_MAPPING = {
    "global": azure_cloud.AZURE_PUBLIC_CLOUD,
    "usgov": azure_cloud.AZURE_US_GOV_CLOUD,
    "de": azure_cloud.AZURE_GERMAN_CLOUD,
    "cn": azure_cloud.AZURE_CHINA_CLOUD,
}

CLOUD_ALIASES = {"public": "global", "gov": "usgov", "germany": "de", "china": "cn"}

_DEFENDER_MAPPINGS = {
    "global": "https://api.securitycenter.microsoft.com/",
    "us": "https://api-us.securitycenter.microsoft.com/",
    "eu": "https://api-eu.securitycenter.microsoft.com/",
    "uk": "https://api-uk.securitycenter.microsoft.com/",
    "gcc": "https://api-gcc.securitycenter.microsoft.us/",
    "gcc-high": "https://api-gov.securitycenter.microsoft.us/",
    "dod": "https://api-gov.securitycenter.microsoft.us/",
}

_M365D_MAPPINGS = {
    "global": "https://api.security.microsoft.com/",
    "us": "https://api-us.security.microsoft.com/",
    "eu": "https://api-eu.security.microsoft.com/",
    "uk": "https://api-uk.security.microsoft.com/",
}


def create_cloud_suf_dict(suffix: str) -> dict:
    """
    Get all the suffixes for a specific service in a cloud.

    Parameters
    ----------
    suffix : str
        The name of the suffix to get details for.

    Returns
    -------
    dict
        A dictionary of cloud names and suffixes.

    """
    return {
        cloud: getattr(msr_cloud.suffixes, suffix)
        for cloud, msr_cloud in CLOUD_MAPPING.items()
    }


def create_cloud_ep_dict(endpoint: str) -> dict:
    """
    Return lookup dict for cloud endpoints.

    Parameters
    ----------
    endpoint : str
        The name of the endpoint to retrieve for each cloud.

    Returns
    -------
    dict
        A dictionary of cloud names and endpoints.

    """
    return {
        cloud: getattr(msr_cloud.endpoints, endpoint)
        for cloud, msr_cloud in CLOUD_MAPPING.items()
    }


def get_all_endpoints(cloud: str) -> azure_cloud.CloudEndpoints:
    """
    Get a list of all the endpoints for an Azure cloud.

    Parameters
    ----------
    cloud : str
        The name of the Azure cloud to get endpoints for.

    Returns
    -------
    dict
        A dictionary of endpoints for the cloud.

    Raises
    ------
    MsticpyAzureConfigError
        If the cloud name is not valid.

    """
    cloud = CLOUD_ALIASES.get(cloud, cloud)
    try:
        endpoints = CLOUD_MAPPING[cloud].endpoints
    except KeyError as cloud_err:
        raise MsticpyAzureConfigError(
            f"""{cloud} is not a valid Azure cloud name.
        Valid names are 'global', 'usgov', 'de', 'cn'"""
        ) from cloud_err
    return endpoints


def get_all_suffixes(cloud: str) -> azure_cloud.CloudSuffixes:
    """
    Get a list of all the suffixes for an Azure cloud.

    Parameters
    ----------
    cloud : str
        The name of the Azure cloud to get suffixes for.

    Returns
    -------
    dict
        A dictionary of suffixes for the cloud.

    Raises
    ------
    MsticpyAzureConfigError
        If the cloud name is not valid.

    """
    cloud = CLOUD_ALIASES.get(cloud, cloud)
    try:
        endpoints = CLOUD_MAPPING[cloud].suffixes
    except KeyError as cloud_err:
        raise MsticpyAzureConfigError(
            f"""{cloud} is not a valid Azure cloud name.
        Valid names are 'global', 'usgov', 'de', 'cn'"""
        ) from cloud_err
    return endpoints


def get_azure_config_value(key, default):
    """Get a config value from Azure section."""
    with contextlib.suppress(KeyError):
        az_settings = config.get_config("Azure")
        if az_settings and key in az_settings:
            return az_settings[key]
    return default


def default_auth_methods() -> List[str]:
    """Get the default (all) authentication options."""
    return get_azure_config_value(
        "auth_methods", ["env", "msi", "vscode", "cli", "powershell", "devicecode"]
    )


class AzureCloudConfig:
    """Azure Cloud configuration."""

    def __init__(self, cloud: Optional[str] = None, tenant_id: Optional[str] = None):
        """
        Initialize AzureCloudConfig from `cloud` or configuration.

        Parameters
        ----------
        cloud : str, optional
            The cloud to retrieve configuration for. If not supplied,
            the cloud ID is read from configuration. If this is not available,
            it defaults to 'global'.
        tenant_id : str, optional
            The tenant to authenticate against. If not supplied,
            the tenant ID is read from configuration, or the default tenant
            for the identity.

        """
        self.cloud = cloud or get_azure_config_value("cloud", "global")
        self.tenant_id = tenant_id or get_azure_config_value("tenant_id", None)
        self.auth_methods = default_auth_methods()

    @property
    def cloud_names(self) -> List[str]:
        """Return a list of current cloud names."""
        return list(CLOUD_MAPPING.keys())

    @staticmethod
    def resolve_cloud_alias(alias) -> Optional[str]:
        """Return match of cloud alias or name."""
        alias_cf = alias.casefold()
        aliases = {alias.casefold(): cloud for alias, cloud in CLOUD_ALIASES.items()}
        if alias_cf in aliases:
            return aliases[alias_cf]
        return alias_cf if alias_cf in aliases.values() else None

    @property
    def endpoints(self) -> azure_cloud.CloudEndpoints:
        """
        Get the CloudEndpoints class for an Azure cloud.

        Returns
        -------
        azure_cloud.CloudEndpoints
            A CloudEndpoints class for the cloud.

        Raises
        ------
        MsticpyAzureConfigError
            If the cloud name is not valid.

        """
        return get_all_endpoints(self.cloud)

    @property
    def suffixes(self) -> azure_cloud.CloudSuffixes:
        """
        Get CloudSuffixes class an Azure cloud.

        Returns
        -------
        azure_cloud.CloudSuffixes
            A CloudSuffixes class for the cloud.

        Raises
        ------
        MsticpyAzureConfigError
            If the cloud name is not valid.

        """
        return get_all_suffixes(self.cloud)

    @property
    def endpoint(self) -> Dict[str, str]:
        """
        Get a dict of all the endpoints for an Azure cloud.

        Returns
        -------
        dict
            A dictionary of endpoints for the cloud.

        Raises
        ------
        MsticpyAzureConfigError
            If the cloud name is not valid.

        """
        return vars(get_all_endpoints(self.cloud))

    @property
    def suffix(self) -> azure_cloud.CloudSuffixes:
        """
        Get a dict of all the suffixes for an Azure cloud.

        Returns
        -------
        dict
            A dictionary of suffixes for the cloud.

        Raises
        ------
        MsticpyAzureConfigError
            If the cloud name is not valid.

        """
        return vars(get_all_suffixes(self.cloud))  # type: ignore

    @property
    def token_uri(self) -> str:
        """Return the resource manager token URI."""
        return f"{self.endpoints.resource_manager}.default"


def get_defender_endpoint(cloud: str) -> str:
    """Get the URI of the applicable Defender for Endpoint API."""
    return _DEFENDER_MAPPINGS[cloud.casefold()]


def get_m365d_endpoint(cloud: str) -> str:
    """Get the URI of the applicable Defender for Endpoint API."""
    return _M365D_MAPPINGS[cloud]
