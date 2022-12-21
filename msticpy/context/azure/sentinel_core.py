# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Uses the Microsoft Sentinel APIs to interact with Microsoft Sentinel Workspaces."""

import contextlib
from typing import Dict, List, Optional, Tuple

import pandas as pd

from ..._version import VERSION
from ...common.exceptions import MsticpyUserConfigError
from ...common.wsconfig import WorkspaceConfig
from .azure_data import AzureData, get_token
from .sentinel_analytics import SentinelAnalyticsMixin, SentinelHuntingMixin
from .sentinel_bookmarks import SentinelBookmarksMixin
from .sentinel_dynamic_summary import SentinelDynamicSummaryMixin, SentinelQueryProvider
from .sentinel_incidents import SentinelIncidentsMixin
from .sentinel_search import SentinelSearchlistsMixin
from .sentinel_ti import SentinelTIMixin
from .sentinel_utils import (
    _PATH_MAPPING,
    SentinelUtilsMixin,
    parse_resource_id,
    validate_res_id,
)
from .sentinel_watchlists import SentinelWatchlistsMixin
from .sentinel_workspaces import SentinelWorkspacesMixin

__version__ = VERSION
__author__ = "Pete Bryan"


class MicrosoftSentinel(  # pylint: disable=too-many-ancestors
    SentinelAnalyticsMixin,
    SentinelHuntingMixin,
    SentinelBookmarksMixin,
    SentinelDynamicSummaryMixin,
    SentinelIncidentsMixin,
    SentinelUtilsMixin,
    SentinelWatchlistsMixin,
    SentinelSearchlistsMixin,
    SentinelWorkspacesMixin,
    SentinelTIMixin,
    AzureData,
):
    """Class for returning key Microsoft Sentinel elements."""

    def __init__(
        self,
        res_id: str = None,
        connect: bool = False,
        cloud: str = None,
        sub_id: str = None,
        res_grp: str = None,
        ws_name: str = None,
        **kwargs,
    ):
        """
        Initialize connector for Azure APIs.

        Parameters
        ----------
        res_id : str, optional
            Set the Sentinel workspace resource ID you want to use, if not specified
            defaults will be looked for or details can be passed separately with functions.
        connect : bool, optional
            Set true if you want to connect to API on initialization, by default False
        cloud : str, optional
            Specify cloud to use, overriding any configuration value.
            Default is to use configuration setting or public cloud if no
            configuration setting is available.
        sub_id : str, optional
            If not specifying a resource ID the Subscription ID of the Sentinel Workspace
            by default None
        res_grp : str, optional
            If not specifying a resource ID the Resource Group name of the
            Sentinel Workspace, by default None
        ws_name : str, optional
            If not specifying a resource ID, the Workspace name of the
            Sentinel Workspace, by default None
        workspace : str, optional
            Alias of ws_name

        """
        self.user_cloud = cloud
        super().__init__(connect=connect, cloud=self.user_cloud)
        self.base_url = self.endpoints.resource_manager
        self.default_subscription: Optional[str] = None
        self._default_workspace: Tuple[Optional[str], Optional[str]] = None, None
        self._default_resource_group: Optional[str] = None
        self.sent_urls: Dict[str, str] = {}
        self.sent_data_query: Optional[SentinelQueryProvider] = None  # type: ignore
        self.url: Optional[str] = None

        workspace = kwargs.get("workspace", ws_name)
        self.workspace_config = WorkspaceConfig(workspace)

        if res_id:
            # If a resource ID is supplied, use that
            self.url = self._build_sent_paths(res_id, self.base_url)
            res_id_parts = parse_resource_id(res_id)
            self.default_subscription = res_id_parts["subscription_id"]
            self._default_resource_group = res_id_parts["resource_group"]
            self._default_workspace = res_id_parts["workspace_name"], res_id
        else:
            # Otherwise - use details from specified workspace or default from settings
            self.default_subscription = self.workspace_config.get(
                "subscription_id", sub_id
            )
            self._default_resource_group = self.workspace_config.get(
                "resource_group", res_grp
            )
            workspace_name = self.workspace_config.get("workspace_name", workspace)
            res_id = self._build_sent_res_id(
                sub_id=self.default_subscription,
                res_grp=self._default_resource_group,
                ws_name=workspace_name,
            )
            self._default_workspace = workspace_name, res_id
            self.url: Optional[str] = self._build_sent_paths(res_id, self.base_url)

        if connect:
            self.connect()

    def connect(
        self,
        auth_methods: List = None,
        tenant_id: str = None,
        silent: bool = False,
        **kwargs,
    ):
        """
        Authenticate with the SDK & API.

        Parameters
        ----------
        auth_methods : List, optional
            list of preferred authentication methods to use, by default None
        tenant_id : str, optional
            Specify cloud tenant to use
        silent : bool, optional
            Set true to prevent output during auth process, by default False

        """
        if workspace := kwargs.get("workspace"):
            # override any previous default setting
            self.workspace_config = WorkspaceConfig(workspace)
        if not self.workspace_config:
            self.workspace_config = WorkspaceConfig()
        tenant_id = (
            tenant_id or self.workspace_config[WorkspaceConfig.CONF_TENANT_ID_KEY]
        )

        super().connect(auth_methods=auth_methods, tenant_id=tenant_id, silent=silent)
        if "token" in kwargs:
            self.token = kwargs["token"]
        else:
            self.token = get_token(
                self.credentials, tenant_id=tenant_id, cloud=self.user_cloud  # type: ignore
            )

        with contextlib.suppress(KeyError):
            self.default_subscription = self.workspace_config[
                WorkspaceConfig.CONF_SUB_ID_KEY
            ]
            self.set_default_workspace(
                self.default_subscription,
                self.workspace_config[WorkspaceConfig.CONF_WS_NAME_KEY],
            )
        self._create_api_paths_for_workspace(
            az_resource_id=None,
            subscription_id=self.workspace_config.get(WorkspaceConfig.CONF_SUB_ID_KEY),
            resource_group=self.workspace_config.get(
                WorkspaceConfig.CONF_RES_GROUP_KEY
            ),
            workspace_name=self.workspace_config.get(WorkspaceConfig.CONF_WS_NAME_KEY),
        )

    def _create_api_paths_for_workspace(
        self,
        az_resource_id: Optional[str] = None,
        subscription_id: Optional[str] = None,
        resource_group: Optional[str] = None,
        workspace_name: Optional[str] = None,
    ):
        """Save configuration and build API URLs for workspace."""
        if workspace_name:
            self.workspace_config = WorkspaceConfig(workspace=workspace_name)
        az_resource_id = az_resource_id or self._get_default_workspace()
        if not az_resource_id:
            az_resource_id = self._build_sent_res_id(
                subscription_id, resource_group, workspace_name
            )
        az_resource_id = validate_res_id(az_resource_id)
        self.url = self._build_sent_paths(az_resource_id, self.base_url)  # type: ignore

        self.sent_urls = {
            name: f"{self.url}{mapping}" for name, mapping in _PATH_MAPPING.items()
        }

    def set_default_subscription(self, subscription_id: str):
        """Set the default subscription to use to `subscription_id`."""
        subs_df = self.get_subscriptions()
        if subscription_id in subs_df["Subscription ID"].values:
            self.default_subscription = subscription_id
        else:
            print(f"Subscription ID {subscription_id} not found.")
            print(
                f"Subscriptions found: {', '.join(subs_df['Subscription ID'].values)}"
            )

    def set_default_workspace(
        self, sub_id: Optional[str], workspace: Optional[str] = None
    ):
        """
        Set the default workspace.

        Parameters
        ----------
        sub_id : Optional[str], optional
            Subscription ID containing the workspace. If not specified,
            the subscription will be taken from the `default_subscription`
            or from configuration.
        workspace : Optional[str], optional
            Name of the workspace, by default None.
            If not specified and there is only one workspace in the
            subscription, this will be set as the default.

        Raises
        ------
        ValueError
            If no current or default subscription has been set.

        """
        sub_id = sub_id or self.default_subscription
        if not sub_id:
            raise MsticpyUserConfigError(
                "No current or default subscription ID set.",
                "Please configure the subscription ID for your workspace in your"
                "msticpyconfig.yaml",
            )
        workspaces = self.get_sentinel_workspaces(sub_id=sub_id)
        if len(workspaces) == 1:
            self._default_workspace = next(iter(workspaces.items()))
        elif workspace in workspaces:
            self._default_workspace = workspace, workspaces[workspace]

    def _get_default_workspace(self):
        """Return the default workspace ResourceID."""
        return self.default_workspace[1] if self.default_workspace else None

    @property
    def default_workspace(self) -> Tuple[Optional[str], Optional[str]]:
        """Return tuple of default workspace name and resource ID."""
        if self._default_workspace is None and self.workspace_config is not None:
            try:
                self.set_default_workspace(
                    sub_id=self.workspace_config.get(WorkspaceConfig.CONF_SUB_ID_KEY),
                    workspace=self.workspace_config.get(
                        WorkspaceConfig.CONF_WS_NAME_KEY
                    ),
                )
            except ValueError:
                return None, None
        return self._default_workspace

    @property
    def default_workspace_name(self):
        """Return the default workspace ResourceID."""
        return self.default_workspace[0] if self.default_workspace else None

    def list_data_connectors(self) -> pd.DataFrame:
        """
        List deployed data connectors.

        Returns
        -------
        pd.DataFrame
            A DataFrame containing the deployed data connectors

        Raises
        ------
        CloudError
            If a valid result is not returned.

        """
        return self._list_items(item_type="data_connectors")


# Alias for old class name
AzureSentinel = MicrosoftSentinel
