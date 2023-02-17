# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Uses the Microsoft Sentinel APIs to interact with Microsoft Sentinel Workspaces."""
import warnings
from typing import Any, Dict, List, Optional

import pandas as pd

from ..._version import VERSION
from ...common.exceptions import MsticpyParameterError, MsticpyUserConfigError
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
    RTD_SENTINEL_URL,
    SentinelUtilsMixin,
    get_config_for_workspace_name,
    parse_resource_id,
    validate_res_id,
)
from .sentinel_watchlists import SentinelWatchlistsMixin
from .sentinel_workspaces import SentinelWorkspacesMixin

__version__ = VERSION
__author__ = "Pete Bryan"


# pylint: disable=too-many-ancestors, too-many-instance-attributes
class MicrosoftSentinel(
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
        workspace: Optional[str] = None,
        res_id: Optional[str] = None,
        connect: Optional[bool] = False,
        cloud: Optional[str] = None,
        **kwargs,
    ):
        """
        Initialize connector for Azure APIs.

        Parameters
        ----------
        workspace : str, optional
            the Workspace name of the Sentinel Workspace set in msticpyconfig.yaml,
            by default None
        res_id : str, optional
            Set the Sentinel workspace resource ID you want to use, if not specified
            defaults will be looked for or details can be passed separately
            with functions.
        connect : bool, optional
            Set true if you want to connect to API on initialization, by default False
        cloud : str, optional
            Specify cloud to use, overriding any configuration value.
            Default is to use configuration setting or public cloud if no
            configuration setting is available.

        Other Parameters
        ----------------
        sub_id : str, optional
            If not specifying a resource ID the Subscription ID of the Sentinel Workspace
            by default None [Deprecated]
        res_grp : str, optional
            If not specifying a resource ID the Resource Group name of the
            Sentinel Workspace, by default None [Deprecated]
        ws_name : str, optional
            If not specifying a resource ID, the Workspace name of the
            Sentinel Workspace, by default None [Deprecated]

        Notes
        -----
        You should supply either a) the Sentinel workspace resource id or
        b) the workspace name (configuration will be read from your
        msticpyconfig.yaml).

        If no parameters are supplied the settings will default to the
        "Default" MS Sentinel configuration in you msticpyconfig.yaml.

        You can have multiple instances of the MicrosoftSentinel class
        configured with different workspaces.

        The class still supports using `sub_id`, `res_group` and `ws_name`
        in place of a resource ID but this usage has been deprecated.
        You can build a resource ID from these components using the
        `MicrosoftSentinel.build_sentinel_resource_id` class method

        See Also
        --------
        SentinelAnalyticsMixin
        SentinelHuntingMixin
        SentinelBookmarksMixin
        SentinelDynamicSummaryMixin
        SentinelIncidentsMixin
        SentinelUtilsMixin
        SentinelWatchlistsMixin
        SentinelSearchlistsMixin
        SentinelWorkspacesMixin
        SentinelTIMixin
        AzureData
        MicrosoftSentinel.build_sentinel_resource_id

        """
        self.default_subscription: Optional[str] = None
        self._default_resource_group: Optional[str] = None
        self.sent_urls: Dict[str, str] = {}
        self.sent_data_query: Optional[SentinelQueryProvider] = None  # type: ignore
        self.url: Optional[str] = None
        # save parameter values
        self.user_cloud = cloud
        self._resource_id = res_id
        # set up workspace config from parameters or default
        workspace = workspace or kwargs.pop("ws_name", None)
        sub_id = kwargs.pop("sub_id", None)
        res_grp = kwargs.pop("res_grp", None)
        self._default_workspace: Optional[str] = workspace

        # set up the base class and set base URL
        super().__init__(cloud=self.user_cloud)
        self.base_url = self.endpoints.resource_manager

        # depending on the parameters set the default configuration
        if res_id:
            self._set_config_from_res_id(res_id)
        elif sub_id or res_grp:
            self._set_config_from_params(sub_id, res_grp, workspace)
        elif workspace:
            ws_key, _ = get_config_for_workspace_name(workspace)
            self._set_config_from_workspace(WorkspaceConfig(ws_key))
        else:
            self._set_config_from_workspace(WorkspaceConfig())

        # if we had parameters to set the resource id, build the url and paths
        if self._resource_id:
            self._build_url_and_paths()

        if connect:
            self.connect(**kwargs)

    def connect(
        self,
        auth_methods: Optional[List] = None,
        tenant_id: Optional[str] = None,
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

        Other Parameters
        ----------------
        res_id : str, optional
            Set the Sentinel workspace resource ID you want to use, if not specified
            defaults will be looked for or details can be passed separately with functions.
        ws_name : str, optional
            The Workspace name of a Sentinel Workspace, as configured in your
            msticpyconfig.yaml, by default None
        workspace : str, optional
            Alias of ws_name

        Notes
        -----
        By default the connect() function will use workspace parameters set when you
        initialized the class. To use these, just call connect() with no parameters.
        If you want to override these settings or, if you did not set a
        workspace in the initialization, you can specify a workspace name or
        resource ID as a parameter to `connect()`. This will become the
        default workspace for this instance of the MicrosoftSentinel class.

        """
        if res_id := kwargs.get("res_id"):
            self._set_config_from_res_id(res_id)
        elif workspace := kwargs.get("workspace", kwargs.get("ws_name")):
            # override any previous default setting
            self._set_config_from_workspace(WorkspaceConfig(workspace))
        elif not self.workspace_config:
            # if no params and workspace_config not yet set, revert
            # to default workspace from msticpyconfig
            self._set_config_from_workspace(WorkspaceConfig())

        if not self._resource_id:
            raise MsticpyParameterError(
                "Not enough workspace information to build API paths.",
                "You need to supply workspace configuration via parameters",
                "workspace configuration in your msticpyconfig.yaml",
                help_uri=(
                    "MSTICPy Microsoft Sentinel APIs",
                    RTD_SENTINEL_URL,
                ),
                config_uri=(
                    "https://msticpy.readthedocs.io/en/latest/"
                    "getting_started/msticpyconfig.html"
                ),
                parameters=["res_id", "sub_id", "res_group", "ws_name"],
            )
        tenant_id = (
            tenant_id or self.workspace_config[WorkspaceConfig.CONF_TENANT_ID_KEY]
        )
        # call azure_data base class to connect
        super().connect(
            auth_methods=auth_methods,
            tenant_id=tenant_id,
            silent=silent,
        )
        if "token" in kwargs:
            self.token = kwargs["token"]
        else:
            self.token = get_token(
                self.credentials, tenant_id=tenant_id, cloud=self.user_cloud  # type: ignore
            )

    def _set_config_from_workspace(self, workspace_config):
        """Set default configuration from workspace settings."""
        self.workspace_config = workspace_config
        self.default_subscription = workspace_config.get(
            WorkspaceConfig.CONF_SUB_ID_KEY
        )
        self._default_resource_group = workspace_config.get(
            WorkspaceConfig.CONF_RES_GROUP_KEY
        )
        self._default_workspace = workspace_config.get(WorkspaceConfig.CONF_WS_NAME_KEY)
        az_resource_id = self.build_sentinel_resource_id(
            self.default_subscription,
            self._default_resource_group,
            self._default_workspace,  # type: ignore
        )
        self._resource_id = validate_res_id(az_resource_id)
        self.url = self._build_workspace_url(self._resource_id, self.base_url)  # type: ignore

        self.sent_urls = {
            name: f"{self.url}{mapping}" for name, mapping in _PATH_MAPPING.items()
        }

    def _set_config_from_res_id(self, res_id: str):
        """Set workspace configuration from Azure resource ID."""
        # If a resource ID is supplied, use that
        self._resource_id = validate_res_id(res_id)
        res_id_parts = parse_resource_id(self._resource_id)
        self.default_subscription = res_id_parts["subscription_id"]
        self._default_resource_group = res_id_parts["resource_group"]
        self._default_workspace = res_id_parts["workspace_name"]
        ws_key, _ = get_config_for_workspace_name(self._default_workspace)
        if ws_key:
            self.workspace_config = WorkspaceConfig(ws_key)
        else:
            self.workspace_config = WorkspaceConfig.from_settings(
                {
                    "WorkspaceName": self._default_workspace,
                    "SubscriptionId": self.default_subscription,
                    "ResourceGroup": self._default_resource_group,
                }
            )

    def _set_config_from_params(
        self, sub_id: Optional[str], res_group: Optional[str], ws_name: Optional[str]
    ):
        """Set workspace configuration from parameters [Deprecated]."""
        warnings.warn(
            (
                "Using separate subscription ID, resource group"
                " and workspace is deprecated. Please use a configured"
                " workspace name or an Azure resource ID of the workspace."
            ),
            category=DeprecationWarning,
        )
        if not sub_id or not res_group or not ws_name:
            raise MsticpyParameterError(
                f"If using {self.__class__.__name__} with subscription id",
                "resource group and workspace name, all parameters must be supplied.",
                "Alternatively you can supply a workspace name for a workspace",
                "configured in your msticpyconfig.yaml, or a full Azure resource ID string.",
                help_uri=(
                    "MSTICPy Microsoft Sentinel APIs",
                    RTD_SENTINEL_URL,
                ),
                config_uri=(
                    "https://msticpy.readthedocs.io/en/latest/"
                    "getting_started/msticpyconfig.html"
                ),
                parameter=["sub_id", "res_group", "ws_name"],
            )
        # first try to get a configuration with the workspace name
        ws_key, _ = get_config_for_workspace_name(ws_name)
        if ws_key:
            workspace_config = WorkspaceConfig(ws_key)
            # check sub id and resource group match
            ws_sub_id = workspace_config.get(WorkspaceConfig.CONF_SUB_ID_KEY, "")
            ws_res_group = workspace_config.get(WorkspaceConfig.CONF_RES_GROUP_KEY, "")
            if (
                ws_sub_id.casefold() == sub_id.casefold()
                and ws_res_group.casefold() == res_group.casefold()
            ):
                self._set_config_from_workspace(workspace_config)
                return

        # otherwise create a dummy config from parameters
        workspace_config = WorkspaceConfig.from_settings(
            {
                "WorkspaceName": ws_name,
                "SubscriptionId": sub_id,
                "ResourceGroup": res_group,
            }
        )
        self._set_config_from_workspace(workspace_config)

    def _build_url_and_paths(self):
        """Build the base URL and service API paths."""
        self.url = self._build_workspace_url(
            self._resource_id, self.base_url  # type: ignore
        )
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

    _MISSING_CONFIG_EXCEPTION = (
        "No current or default subscription ID set.",
        "Please supply an a subscription ID for your workspace"
        "as a parameter or in your msticpyconfig.yaml",
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
        workspace_config: Optional[WorkspaceConfig]
        if workspace:
            ws_key, _ = get_config_for_workspace_name(workspace)
            if ws_key:
                workspace_config = WorkspaceConfig(workspace=ws_key)
                self._set_config_from_workspace(workspace_config)
                return
        sub_id = sub_id or self.default_subscription
        if not sub_id or not workspace:
            raise MsticpyUserConfigError(
                *self._MISSING_CONFIG_EXCEPTION,
                title="Not enough information to set workspace.",
                help_uri=RTD_SENTINEL_URL,
                config_uri=(
                    "https://msticpy.readthedocs.io/en/latest/"
                    "getting_started/msticpyconfig.html"
                ),
            )

        if workspace_config := self._find_workspace_from_subscription(
            sub_id, workspace
        ):
            self._set_config_from_workspace(workspace_config)
            return

        raise MsticpyUserConfigError(
            *self._MISSING_CONFIG_EXCEPTION,
            title="Not enough information to set workspace.",
            help_uri=RTD_SENTINEL_URL,
            config_uri=(
                "https://msticpy.readthedocs.io/en/latest/"
                "getting_started/msticpyconfig.html"
            ),
        )

    def _find_workspace_from_subscription(
        self, sub_id: str, workspace: str
    ) -> Optional[WorkspaceConfig]:
        """Use the subscription ID to try to find a matching workspace."""
        ws_res_id: Optional[str] = None
        ws_name: Optional[str] = workspace
        workspace_config: Optional[WorkspaceConfig] = None

        # if workspace not supplied trying looking up in subscription
        if not workspace:
            workspaces = self.get_sentinel_workspaces(sub_id=sub_id)
            if len(workspaces) == 1:
                # if only one, use that one
                ws_name, ws_res_id = next(iter(workspaces.items()))
        else:
            workspaces = self.get_sentinel_workspaces(sub_id=sub_id)
            ws_name, ws_res_id = next(
                iter(
                    (ws, res_id)
                    for ws, res_id in workspaces.items()
                    if workspace.casefold() in res_id.casefold()
                )
            )

        # if workspace is one that we have configuration for, get the details from there.
        if ws_name:
            ws_key, _ = get_config_for_workspace_name(ws_name)
        if ws_key:
            workspace_config = WorkspaceConfig(workspace=ws_key)

        elif ws_res_id:
            # otherwise construct partial settings
            res_id_parts = parse_resource_id(ws_res_id)
            if res_id_parts.get("workspace_name"):
                ws_key, _ = get_config_for_workspace_name(
                    res_id_parts["workspace_name"]
                )
            else:
                ws_key = ""
            if ws_key:
                workspace_config = WorkspaceConfig(ws_key)
            else:
                workspace_config = WorkspaceConfig.from_settings(
                    {
                        "WorkspaceName": self._default_workspace
                        or res_id_parts["workspace_name"],
                        "SubscriptionId": res_id_parts["subscription_id"],
                        "ResourceGroup": res_id_parts["resource_group"],
                    }
                )

        return workspace_config

    @property
    def default_workspace_settings(self) -> Optional[Dict[str, Any]]:
        """Return current default workspace settings."""
        return self.workspace_config.mp_settings

    @property
    def default_workspace_name(self):
        """Return the default workspace name."""
        return self._default_workspace

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
