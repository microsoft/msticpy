# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Azure Cloud Mappings."""
import contextlib
from functools import lru_cache
from typing import Any, Dict, List, Optional

import httpx

from .._version import VERSION
from ..common import pkg_config as config
from ..common.exceptions import MsticpyAzureConfigError
from ..common.pkg_config import get_http_timeout
from ..common.utility.package import unit_testing
from .cloud_mappings_offline import cloud_mappings_offline

__version__ = VERSION
__author__ = "Pete Bryan"

CLOUD_MAPPING = {
    "global": "https://management.azure.com/",
    "usgov": "https://management.usgovcloudapi.net/",
    "cn": "https://management.chinacloudapi.cn/",
}

CLOUD_ALIASES = {
    "public": "global",
    "global": "global",
    "gov": "usgov",
    "usgov": "usgov",
    "china": "cn",
    "cn": "cn",
}

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
    "gcc": "https://api-gcc.security.microsoft.us",
    "gcc-high": "https://api-gov.security.microsoft.us",
    "dod": "https://api-gov.security.microsoft.us",
}


def format_endpoint(endpoint: str) -> str:
    """Format an endpoint with "/" if needed ."""
    return endpoint if endpoint.endswith("/") else f"{endpoint}/"


@lru_cache(maxsize=None)
def get_cloud_endpoints(
    cloud: str, resource_manager_url: Optional[str] = None
) -> Dict[str, Any]:
    """
    Get the cloud endpoints for a specific cloud.

    If resource_manager_url is supplied, it will be used instead of the cloud name.

    Parameters
    ----------
    cloud : str
        The name of the cloud to get endpoints for.
    resource_manager_url : str, optional
        The resource manager url for a cloud.
        Can be used to get all endpoints for a specific cloud. Defaults to None.

    Returns
    -------
    dict
        A dictionary of endpoints and suffixes for the specified cloud/resource_manager_url.

    Raises
    ------
    MsticpyAzureConfigError
        If the cloud name is not valid.

    """
    response = None
    if resource_manager_url:
        response = get_cloud_endpoints_by_resource_manager_url(resource_manager_url)
    else:
        response = get_cloud_endpoints_by_cloud(cloud)
    if response:
        return response

    raise MsticpyAzureConfigError(
        f"Error retrieving endpoints for Cloud: {cloud}",
        f"Resource Manager Url: {resource_manager_url}.",
    )


def get_cloud_endpoints_by_cloud(cloud: str) -> Dict[str, Any]:
    """
    Get the cloud endpoints for a specific cloud.

    Parameters
    ----------
    cloud : str
        The name of the cloud to get endpoints for.

    Returns
    -------
    Dict
        Contains endpoints and suffixes for a specific cloud.

    """
    resource_manager_url = CLOUD_MAPPING.get(CLOUD_ALIASES.get(cloud, "global"))
    return get_cloud_endpoints_by_resource_manager_url(resource_manager_url)  # type: ignore


def get_cloud_endpoints_by_resource_manager_url(
    resource_manager_url: str,
) -> Dict[str, Any]:
    """
    Get the cloud endpoints for a specific resource manager url.

    Parameters
    ----------
    resource_manager_url : str
        The resource manager url to get endpoints for.

    Returns
    -------
    Dict
        Contains endpoints and suffixes for a specific cloud.

    """
    if unit_testing():
        return cloud_mappings_offline["global"]
    f_resource_manager_url = format_endpoint(resource_manager_url)
    endpoint_url = f"{f_resource_manager_url}metadata/endpoints?api-version=latest"
    try:
        resp = httpx.get(endpoint_url, timeout=get_http_timeout())
        if resp.status_code == 200:
            return resp.json()

    except httpx.RequestError:
        cloud = next(
            (
                key
                for key, val in CLOUD_MAPPING.items()
                if val == f_resource_manager_url
            ),
            "global",
        )
        return cloud_mappings_offline[cloud]

    return cloud_mappings_offline["global"]


def get_azure_config_value(key, default):
    """Get a config value from Azure section."""
    with contextlib.suppress(KeyError):
        az_settings = config.get_config("Azure", {})
        if az_settings and key in az_settings:
            return az_settings[key]
    return default


def default_auth_methods() -> List[str]:
    """Get the default (all) authentication options."""
    return get_azure_config_value(
        "auth_methods", ["env", "msi", "vscode", "cli", "powershell", "devicecode"]
    )


def get_defender_endpoint(cloud: str) -> str:
    """Get the URI of the applicable Defender for Endpoint API."""
    return _DEFENDER_MAPPINGS[cloud.casefold()]


def get_m365d_endpoint(cloud: str) -> str:
    """Get the URI of the applicable Defender for Endpoint API."""
    return _M365D_MAPPINGS[cloud]


def get_m365d_login_endpoint(cloud: str) -> str:
    """Get M365 login URL."""
    if cloud in {"gcc-high", "dod"}:
        return "https://login.microsoftonline.us/"
    return AzureCloudConfig().authority_uri


class AzureCloudConfig:
    """Azure Cloud configuration."""

    def __init__(
        self,
        cloud: Optional[str] = None,
        tenant_id: Optional[str] = None,
        resource_manager_url: Optional[str] = None,
    ):
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
        resource_manager_url : str, optional
            The resource manager URL to use. If not supplied,
            the URL is based on the cloud name within the configuration
            or defaults to 'https://management.azure.com/'.

        """
        self.cloud = cloud or get_azure_config_value("cloud", "global")
        self.tenant_id = tenant_id or get_azure_config_value("tenant_id", None)
        self.auth_methods = default_auth_methods()
        self.resource_manager_url = resource_manager_url or get_azure_config_value(
            "resource_manager_url", None
        )
        self.endpoints = get_cloud_endpoints(self.cloud, self.resource_manager_url)

    @property
    def cloud_names(self) -> List[str]:
        """Return a list of current cloud names."""
        return list(CLOUD_MAPPING.keys())

    @staticmethod
    def resolve_cloud_alias(
        alias,
    ) -> Optional[str]:
        """Return match of cloud alias or name."""
        alias_cf = alias.casefold()
        aliases = {alias.casefold(): cloud for alias, cloud in CLOUD_ALIASES.items()}
        if alias_cf in aliases:
            return aliases[alias_cf]
        return alias_cf if alias_cf in aliases.values() else None

    @property
    def suffixes(self) -> Dict[str, str]:
        """
        Get CloudSuffixes class an Azure cloud.

        Returns
        -------
        dict
            Dict of cloud endpoint suffixes.

        Raises
        ------
        MsticpyAzureConfigError
            If the cloud name is not valid.

        """
        return self.endpoints.get("suffixes", {})

    @property
    def token_uri(self) -> str:
        """Return the resource manager token URI."""
        rm_url = self.resource_manager_url or self.resource_manager
        rm_url = format_endpoint(rm_url)
        return f"{rm_url}.default"

    @property
    def authority_uri(self) -> str:
        """Return the AAD authority URI."""
        return format_endpoint(
            self.endpoints.get("authentication", {}).get("loginEndpoint")
        )

    @property
    def log_analytics_uri(self) -> str:
        """Return the AAD authority URI."""
        return format_endpoint(
            self.endpoints.get("logAnalyticsResourceId")  # type: ignore
        )

    @property
    def resource_manager(self) -> str:
        """Return the resource manager URI."""
        return format_endpoint(self.endpoints.get("resourceManager"))  # type: ignore
