# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Mixin Class for Sentinel Workspaces."""
from __future__ import annotations

import logging
import re
from dataclasses import dataclass
from typing import TYPE_CHECKING, Any, ClassVar
from urllib import parse

import httpx
from msrestazure import tools as az_tools
from typing_extensions import Self

from msticpy.context.azure.sentinel_utils import SentinelUtilsMixin

from ..._version import VERSION
from ...auth.azure_auth_core import AzureCloudConfig
from ...common.data_utils import df_has_data
from ...common.pkg_config import get_http_timeout
from ...common.utility import mp_ua_header
from ...data.core.data_providers import QueryProvider

if TYPE_CHECKING:
    import pandas as pd

logger: logging.Logger = logging.getLogger(__name__)

__version__ = VERSION
__author__ = "Ian Hellen"


@dataclass
class ParsedUrlComponents:
    """Class to defined components for Parsed URLs."""

    domain: str | None
    resource_id: str
    tenant_name: str | None
    res_components: dict[str, str]
    raw_res_id: str


class SentinelWorkspacesMixin(SentinelUtilsMixin):
    """Mixin class for Sentinel workspaces."""

    _TENANT_URI: ClassVar[str] = (
        "{cloud_endpoint}/{tenant_name}/.well-known/openid-configuration"
    )
    _RES_GRAPH_PROV: ClassVar[QueryProvider | None] = None

    @classmethod
    def get_resource_id_from_url(
        cls: type[Self],
        portal_url: str,
    ) -> str | None:
        """Return resource ID components from Sentinel portal URL."""
        if (resource := cls._extract_resource_id(portal_url)) is not None:
            return resource.resource_id
        return None

    @classmethod
    def get_workspace_details_from_url(
        cls: type[Self],
        portal_url: str,
    ) -> dict[str, dict[str, str]]:
        """
        Return workspace settings from portal URL.

        Parameters
        ----------
        portal_url : str
            URL from Sentinel Azure portal

        Returns
        -------
        dict[str, dict[str, str]]

        """
        resource_comps: ParsedUrlComponents | None = cls._extract_resource_id(
            portal_url,
        )
        if not resource_comps:
            err_msg: str = f"Cannot retrieve workspace details from {portal_url}"
            raise ValueError(err_msg)
        tenant_id: str | None = None
        if resource_comps.tenant_name:
            tenant_id = cls._get_tenantid_from_logon_domain(resource_comps.tenant_name)
        workspace_df: pd.DataFrame = cls._lookup_workspace_by_res_id(
            resource_id=resource_comps.resource_id,
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
        logger.warning(
            "Failed to find Azure resource for workspace. Returning partial results.",
        )
        return cls._get_settings_for_workspace(
            workspace_name=resource_comps.res_components["name"],
            workspace_id="unknown",
            tenant_id=tenant_id or "unknown",
            subscription_id=resource_comps.res_components["subscription"],
            resource_group=resource_comps.res_components["resource_group"],
            workspace_tenant_id="unknown",
        )

    @classmethod
    def get_workspace_name(
        cls: type[Self],
        workspace_id: str | None = None,
        resource_id: str | None = None,
    ) -> str | None:
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
        settings: dict[str, Any] = cls.get_workspace_settings(
            workspace_id=workspace_id,
            resource_id=resource_id,
        )

        return next(iter(settings.values())).get("WorkspaceName") if settings else None

    @classmethod
    def get_workspace_id(
        cls: type[Self],
        workspace_name: str,
        subscription_id: str = "",
        resource_group: str = "",
    ) -> str | None:
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
        settings: dict[str, Any] = cls.get_workspace_settings_by_name(
            workspace_name,
            subscription_id,
            resource_group,
        )
        return next(iter(settings.values())).get("WorkspaceId") if settings else None

    @classmethod
    def get_workspace_settings(
        cls: type[Self],
        workspace_id: str | None = None,
        resource_id: str | None = None,
    ) -> dict[str, Any]:
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
        dict[str, str]
            The workspace name, if found, else None

        Raises
        ------
        ValueError
            If neither workspace_id or resource_id parameters are supplied.

        """
        if workspace_id:
            results_df: pd.DataFrame = cls._lookup_workspace_by_ws_id(workspace_id)
        else:
            if not resource_id:
                err_msg: str = "Either workspace_id or resource_id must be supplied."
                raise ValueError(err_msg)
            results_df = cls._lookup_workspace_by_res_id(resource_id)
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
        cls: type[Self],
        workspace_name: str,
        subscription_id: str = "",
        resource_group: str = "",
    ) -> dict[str, Any]:
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
        results_df: pd.DataFrame = cls._lookup_workspace_by_name(
            workspace_name,
            subscription_id,
            resource_group,
        )
        if df_has_data(results_df):
            if len(results_df) > 1:
                logger.warning(
                    "Warning: query returned multiple results. "
                    "Specify subscription_id and/or resource_group "
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
    def _get_resource_graph_provider(
        cls: type[Self],
    ) -> QueryProvider:
        if not cls._RES_GRAPH_PROV:
            cls._RES_GRAPH_PROV = QueryProvider("ResourceGraph")
        if not cls._RES_GRAPH_PROV.connected:
            cls._RES_GRAPH_PROV.connect()  # pragma: no cover
        return cls._RES_GRAPH_PROV

    @classmethod
    def _lookup_workspace_by_name(
        cls: type[Self],
        workspace_name: str,
        subscription_id: str = "",
        resource_group: str = "",
    ) -> pd.DataFrame:
        res_graph_prov: QueryProvider = cls._get_resource_graph_provider()
        return res_graph_prov.Sentinel.list_sentinel_workspaces_for_name(
            workspace_name=workspace_name,
            subscription_id=subscription_id,
            resource_group=resource_group,
        )

    @classmethod
    def _lookup_workspace_by_ws_id(
        cls: type[Self],
        workspace_id: str,
    ) -> pd.DataFrame:
        res_graph_prov: QueryProvider = cls._get_resource_graph_provider()
        return res_graph_prov.Sentinel.get_sentinel_workspace_for_workspace_id(
            workspace_id=workspace_id,
        )

    @classmethod
    def _lookup_workspace_by_res_id(
        cls: type[Self],
        resource_id: str | None,
    ) -> pd.DataFrame:
        res_graph_prov: QueryProvider = cls._get_resource_graph_provider()
        return res_graph_prov.Sentinel.get_sentinel_workspace_for_resource_id(
            resource_id=resource_id,
        )

    @classmethod
    def _extract_resource_id(
        cls: type[Self],
        url: str,
    ) -> ParsedUrlComponents | None:
        """Extract and return resource ID components from URL."""
        resid_pattern = (
            r"https://(?P<domain>[^/]+)/#?(@(?P<tenantname>[^/]+))?"
            ".*(?P<res_id>(%2F|/)subscriptions(%2F|/).*)"
        )

        uri_match: re.Match[str] | None = re.search(resid_pattern, url)
        if not uri_match:
            return None

        raw_res_id: str = uri_match.groupdict()["res_id"]
        raw_res_id = parse.unquote(raw_res_id)
        res_components: dict[str, Any] = az_tools.parse_resource_id(raw_res_id)
        try:
            resource_id: str = cls._normalize_resource_id(res_components)
        except KeyError:
            logger.exception("Invalid Sentinel resource id")
            return None
        return ParsedUrlComponents(
            domain=uri_match.groupdict().get("domain"),
            resource_id=resource_id,
            tenant_name=uri_match.groupdict().get("tenantname"),
            res_components=res_components,
            raw_res_id=raw_res_id,
        )

    @staticmethod
    def _normalize_resource_id(res_components: dict[str, str]) -> str:
        return (
            f"/subscriptions/{res_components['subscription']}"
            f"/resourcegroups/{res_components['resource_group']}"
            f"/providers/Microsoft.OperationalInsights"
            f"/workspaces/{res_components['name']}"
        )

    @classmethod
    def _get_tenantid_from_logon_domain(
        cls: type[Self],
        domain: str,
        cloud: str = "global",
    ) -> str | None:
        """Get the tenant ID from login domain."""
        az_cloud_config = AzureCloudConfig(cloud)
        login_endpoint: str = az_cloud_config.authority_uri
        t_resp: httpx.Response = httpx.get(
            cls._TENANT_URI.format(cloud_endpoint=login_endpoint, tenant_name=domain),
            timeout=get_http_timeout(),
            headers=mp_ua_header(),
        )
        tenant_details: dict[str, Any] = t_resp.json()

        if not tenant_details:
            return None
        tenant_ep_rgx = r"(?P<endpoint>https://[^/]+)/(?P<tenant_id>[^/]+).*"
        match: re.Match[str] | None = re.search(
            tenant_ep_rgx,
            tenant_details.get("token_endpoint", ""),
        )
        return match.groupdict()["tenant_id"] if match else None

    @classmethod
    def _get_settings_for_workspace(  # pylint:disable=too-many-arguments # noqa:PLR0913
        cls: type[Self],
        workspace_name: str,
        workspace_id: str,
        tenant_id: str,
        subscription_id: str,
        resource_group: str,
        workspace_tenant_id: str,
    ) -> dict[str, dict[str, str]]:
        """Return settings dictionary for workspace settings."""
        return {
            workspace_name: {
                "WorkspaceId": workspace_id,
                "TenantId": tenant_id,
                "SubscriptionId": subscription_id,
                "ResourceGroup": resource_group,
                "WorkspaceName": workspace_name,
                "WorkspaceTenantId": workspace_tenant_id,
            },
        }
