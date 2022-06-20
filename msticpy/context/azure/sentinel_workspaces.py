# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Mixin Class for Sentinel Workspaces."""
import re
import urllib
from collections import namedtuple
from typing import Dict, Optional

import httpx
import pandas as pd
from msrestazure import tools as az_tools

from ..._version import VERSION
from ...auth.azure_auth_core import AzureCloudConfig
from ...common.data_utils import df_has_data
from ...common.pkg_config import get_http_timeout
from ...common.utility import mp_ua_header
from ...data import QueryProvider

__version__ = VERSION
__author__ = "Ian Hellen"


ParsedUrlComponents = namedtuple(
    "ParsedUrlComponents",
    "domain, resource_id, tenant_name, res_components, raw_res_id",
)


class SentinelWorkspacesMixin:
    """Mixin class for Sentinel workspaces."""

    _TENANT_URI = "{cloud_endpoint}/{tenant_name}/.well-known/openid-configuration"
    _RES_GRAPH_PROV = QueryProvider("ResourceGraph")

    @classmethod
    def get_resource_id_from_url(cls, portal_url: str) -> str:
        """Return resource ID components from Sentinel portal URL."""
        return cls._extract_resource_id(portal_url).resource_id

    @classmethod
    def get_workspace_details_from_url(
        cls, portal_url: str
    ) -> Dict[str, Dict[str, str]]:
        """
        Return workspace settings from portal URL.

        Parameters
        ----------
        portal_url : str
            URL from Sentinel Azure portal

        Returns
        -------
        Dict[str, Dict[str, str]]

        """
        resource_comps = cls._extract_resource_id(portal_url)
        tenant_id: Optional[str] = None
        if resource_comps.tenant_name:
            tenant_id = cls._get_tenantid_from_logon_domain(resource_comps.tenant_name)
        workspace_df = cls._lookup_workspace_by_res_id(
            resource_id=resource_comps.resource_id
        )
        if df_has_data(workspace_df):
            return cls._get_settings_for_workspace(
                workspace_name=workspace_df.iloc[0].workspaceName,
                workspace_id=workspace_df.iloc[0].workspaceId,
                tenant_id=tenant_id or workspace_df.iloc[0].tenantId,
                subscription_id=workspace_df.iloc[0].subscriptionId,
                resource_group=workspace_df.iloc[0].resourceGroup,
                workspace_tenant_id=workspace_df.iloc[0].tenantId,
            )
        print(
            "Failed to find Azure resource for workspace", "Returning partial results."
        )
        return cls._get_settings_for_workspace(
            workspace_name=resource_comps.res_components.get("name"),
            workspace_id="unknown",
            tenant_id=tenant_id or "unknown",
            subscription_id=resource_comps.res_components.get("subscription"),
            resource_group=resource_comps.res_components.get("resource_group"),
            workspace_tenant_id="unknown",
        )

    @classmethod
    def get_workspace_name(
        cls,
        workspace_id: Optional[str] = None,
        resource_id: Optional[str] = None,
    ) -> Optional[str]:
        """
        Return resolved name from workspace ID or resource ID.

        Parameters
        ----------
        workspace_id : Optional[str], optional
            The UUID of the Sentinel workspace, by default None
        resource_id : Optional[str], optional
            The Resource ID string of the workspace, by default None

        Returns
        -------
        Optional[str]
            The workspace name, if found, else None

        Raises
        ------
        ValueError
            If neither workspace_id or resource_id parameters are supplied.

        """
        settings = cls.get_workspace_settings(
            workspace_id=workspace_id, resource_id=resource_id
        )

        return next(iter(settings.values())).get("WorkspaceName") if settings else None

    @classmethod
    def get_workspace_id(
        cls,
        workspace_name: str,
        subscription_id: str = "",
        resource_group: str = "",
    ) -> Optional[str]:
        """
        Return the workspace ID given workspace name.

        Parameters
        ----------
        workspace_name : str
            Workspace name (case insensitive)
        subscription_id : str, optional
            Azure subscription UUID, by default ""
        resource_group : str, optional
            Azure resource group name, by default ""

        Returns
        -------
        Optional[str]
            The ID of the workspace if found, else None

        """
        settings = cls.get_workspace_settings_by_name(
            workspace_name, subscription_id, resource_group
        )
        return next(iter(settings.values())).get("WorkspaceId") if settings else None

    @classmethod
    def get_workspace_settings(
        cls,
        workspace_id: Optional[str] = None,
        resource_id: Optional[str] = None,
    ):
        """
        Return resolved workspace settings from workspace ID or resource ID.

        Parameters
        ----------
        workspace_id : Optional[str], optional
            The UUID of the Sentinel workspace, by default None
        resource_id : Optional[str], optional
            The Resource ID string of the workspace, by default None

        Returns
        -------
        Dict[str, str]
            The workspace name, if found, else None

        Raises
        ------
        ValueError
            If neither workspace_id or resource_id parameters are supplied.

        """
        if not (workspace_id or resource_id):
            raise ValueError("Either workspace_id or resource_id must be supplied.")
        if workspace_id:
            results_df = cls._lookup_workspace_by_ws_id(workspace_id)
        else:
            results_df = cls._lookup_workspace_by_res_id(resource_id)  # type: ignore
        if df_has_data(results_df):
            return cls._get_settings_for_workspace(
                workspace_name=results_df.iloc[0].workspaceName,
                workspace_id=results_df.iloc[0].workspaceId,
                tenant_id=results_df.iloc[0].tenantId,
                subscription_id=results_df.iloc[0].subscriptionId,
                resource_group=results_df.iloc[0].resourceGroup,
                workspace_tenant_id=results_df.iloc[0].tenantId,
            )
        return {}

    @classmethod
    def get_workspace_settings_by_name(
        cls,
        workspace_name: str,
        subscription_id: str = "",
        resource_group: str = "",
    ):
        """
        Return the workspace ID given workspace name.

        Parameters
        ----------
        workspace_name : str
            Workspace name (case insensitive)
        subscription_id : str, optional
            Azure subscription UUID, by default ""
        resource_group : str, optional
            Azure resource group name, by default ""

        Returns
        -------
        Optional[str]
            The ID of the workspace if found, else None

        """
        results_df = cls._lookup_workspace_by_name(
            workspace_name, subscription_id, resource_group
        )
        if df_has_data(results_df):
            if len(results_df) > 1:
                print(
                    "Warning: query returned multiple results.",
                    "Specify subscription_id and/or resource_group",
                    "for more accurate results.",
                )
            return cls._get_settings_for_workspace(
                workspace_name=results_df.iloc[0].workspaceName,
                workspace_id=results_df.iloc[0].workspaceId,
                tenant_id=results_df.iloc[0].tenantId,
                subscription_id=results_df.iloc[0].subscriptionId,
                resource_group=results_df.iloc[0].resourceGroup,
                workspace_tenant_id=results_df.iloc[0].tenantId,
            )
        return {}

    @classmethod
    def _lookup_workspace_by_name(
        cls,
        workspace_name: str,
        subscription_id: str = "",
        resource_group: str = "",
    ) -> pd.DataFrame:
        if not cls._RES_GRAPH_PROV.connected:
            cls._RES_GRAPH_PROV.connect()  # pragma: no cover
        return cls._RES_GRAPH_PROV.Sentinel.list_sentinel_workspaces_for_name(
            workspace_name=workspace_name,
            subscription_id=subscription_id,
            resource_group=resource_group,
        )

    @classmethod
    def _lookup_workspace_by_ws_id(cls, workspace_id: str) -> pd.DataFrame:
        if not cls._RES_GRAPH_PROV.connected:
            cls._RES_GRAPH_PROV.connect()  # pragma: no cover
        return cls._RES_GRAPH_PROV.Sentinel.get_sentinel_workspace_for_workspace_id(
            workspace_id=workspace_id
        )

    @classmethod
    def _lookup_workspace_by_res_id(cls, resource_id: str):
        if not cls._RES_GRAPH_PROV.connected:
            cls._RES_GRAPH_PROV.connect()  # pragma: no cover
        return cls._RES_GRAPH_PROV.Sentinel.get_sentinel_workspace_for_resource_id(
            resource_id=resource_id
        )

    @classmethod
    def _extract_resource_id(cls, url: str) -> ParsedUrlComponents:
        """Extract and return resource ID components from URL."""
        resid_pattern = (
            r"https://(?P<domain>[^/]+)/#?(@(?P<tenantname>[^/]+))?"
            ".*(?P<res_id>(%2F|/)subscriptions(%2F|/).*)"
        )

        uri_match = re.search(resid_pattern, url)
        if not uri_match:
            return ParsedUrlComponents(None, None, None, None, None)

        raw_res_id = uri_match.groupdict()["res_id"]
        raw_res_id = urllib.parse.unquote(raw_res_id)
        res_components = az_tools.parse_resource_id(raw_res_id)
        try:
            resource_id = cls._normalize_resource_id(res_components)
        except KeyError:
            print("Invalid Sentinel resource id")
            return ParsedUrlComponents(None, None, None, None, None)
        return ParsedUrlComponents(
            domain=uri_match.groupdict().get("domain"),
            resource_id=resource_id,
            tenant_name=uri_match.groupdict().get("tenantname"),
            res_components=res_components,
            raw_res_id=raw_res_id,
        )

    @staticmethod
    def _normalize_resource_id(res_components: Dict[str, str]) -> str:
        return (
            f"/subscriptions/{res_components['subscription']}"
            f"/resourcegroups/{res_components['resource_group']}"
            f"/providers/Microsoft.OperationalInsights"
            f"/workspaces/{res_components['name']}"
        )

    @classmethod
    def _get_tenantid_from_logon_domain(
        cls, domain, cloud: str = "global"
    ) -> Optional[str]:
        """Get the tenant ID from login domain."""
        cloud_config = AzureCloudConfig(cloud)
        login_endpoint = cloud_config.endpoints.active_directory
        t_resp = httpx.get(
            cls._TENANT_URI.format(cloud_endpoint=login_endpoint, tenant_name=domain),
            timeout=get_http_timeout(),
            headers=mp_ua_header(),
        )
        tenant_details = t_resp.json()

        if not tenant_details:
            return None
        tenant_ep_rgx = r"(?P<endpoint>https://[^/]+)/(?P<tenant_id>[^/]+).*"
        match = re.search(tenant_ep_rgx, tenant_details.get("token_endpoint", ""))
        return match.groupdict()["tenant_id"] if match else None

    @classmethod
    def _get_settings_for_workspace(
        cls,
        workspace_name: str,
        workspace_id: str,
        tenant_id: str,
        subscription_id: str,
        resource_group: str,
        workspace_tenant_id: str,
    ) -> Dict[str, Dict[str, str]]:
        """Return settings dictionary for workspace settings."""
        return {
            workspace_name: {
                "WorkspaceId": workspace_id,
                "TenantId": tenant_id,
                "SubscriptionId": subscription_id,
                "ResourceGroup": resource_group,
                "WorkspaceName": workspace_name,
                "WorkspaceTenantId": workspace_tenant_id,
            }
        }
