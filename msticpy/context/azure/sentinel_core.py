# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Uses the Microsoft Sentinel APIs to interact with Microsoft Sentinel Workspaces."""
from __future__ import annotations

import logging
import warnings
from dataclasses import dataclass
from functools import partial
from typing import Any, Callable, Dict, List, Optional

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
    build_sentinel_resource_id,
    parse_resource_id,
    validate_resource_id,
)
from .sentinel_watchlists import SentinelWatchlistsMixin
from .sentinel_workspaces import SentinelWorkspacesMixin

__version__ = VERSION
__author__ = "Pete Bryan"

logger = logging.getLogger(__name__)

_SUB_ID = "subscription_id"
_RES_GRP = "resource_group"
_WS_NAME = "workspace_name"
_RES_ID = "resource_id"


@dataclass
class SentinelInstanceDetails:
    """Dataclass for Sentinel workspace details."""

    subscription_id: Optional[str]
    resource_group: Optional[str]
    workspace_name: Optional[str]

    @property
    def resource_id(self) -> Optional[str]:
        """Return the resource ID for the workspace."""
        if not all([self.subscription_id, self.resource_group, self.workspace_name]):
            return None
        return build_sentinel_resource_id(
            self.subscription_id, self.resource_group, self.workspace_name  # type: ignore
        )


def _get_ws_defaults(
    *,
    subscription_id: Optional[str] = None,
    resource_group: Optional[str] = None,
    workspace_name: Optional[str] = None,
) -> SentinelInstanceDetails:
    """Return default values for Sentinel workspace settings."""
    return SentinelInstanceDetails(
        subscription_id=subscription_id,
        resource_group=resource_group,
        workspace_name=workspace_name,
    )


_LEGACY_PARAM_NAMES = {
    "sub_id": _SUB_ID,
    "res_grp": _RES_GRP,
    "ws_name": _WS_NAME,
    "workspace": _WS_NAME,
    "res_id": _RES_ID,
}
_CORE_WS_PARAMETERS = [_SUB_ID, _RES_GRP, _WS_NAME]
_WS_PARAMETERS = _CORE_WS_PARAMETERS + [_RES_ID]


def _map_legacy_param_names(**kwargs) -> Dict[str, Any]:
    """Map legacy parameter names to current names."""
    return {_LEGACY_PARAM_NAMES.get(key, key): value for key, value in kwargs.items()}


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
        resource_id: Optional[str] = None,
        connect: Optional[bool] = False,
        cloud: Optional[str] = None,
        subscription_id: Optional[str] = None,
        resource_group: Optional[str] = None,
        workspace_name: Optional[str] = None,
        **kwargs,
    ):
        """
        Initialize connector for Azure APIs.

        Parameters
        ----------
        resource_id : str, optional
            Set the Sentinel workspace resource ID you want to use, if not specified
            defaults will be looked for or details can be passed separately with functions,
            by default None
            `res_id` is an alias for resource_id.
        connect : bool, optional
            Set true if you want to connect to API on initialization, by default False
        cloud : str, optional
            Specify cloud to use, overriding any configuration value.
            Default is to use configuration setting or public cloud if no
            configuration setting is available.
        subscription_id : str, optional
            If not specifying a resource ID the Subscription ID of the Sentinel Workspace
            by default None
            `sub_id` is an alias for subscription_id
        resource_group : str, optional
            If not specifying a resource ID the Resource Group name of the
            Sentinel Workspace, by default None
            `res_grp` is an alias for resource_group
        workspace_name : str, optional
            If not specifying a resource ID, the Workspace name of the
            Sentinel Workspace, by default None
            `ws_name` and `workspace` are aliases for workspace_name

        Notes
        -----
        There are multiple ways to set the default workspace for the Microsoft Sentinel
        class:
        1. Specify a full Azure resource ID for the workspace in the `resource_id` parameter.
        2. Specify the subscription ID and resource group and workspace name in the
        `subscription_id`, `resource_group` and `workspace` parameters.
        3. Specify only a workspace name in the `workspace` parameter. This will read
        the workspace details from the msticpyconfig configuration file.

        """
        super().__init__(connect=False, cloud=cloud)

        init_kwargs = _map_legacy_param_names(**kwargs)
        if resource_id:
            init_kwargs[_RES_ID] = resource_id
        if subscription_id:
            init_kwargs[_SUB_ID] = subscription_id
        if resource_group:
            init_kwargs[_RES_GRP] = resource_group
        if workspace_name:
            init_kwargs[_WS_NAME] = workspace_name

        self._default_settings = self._set_ws_defaults(_get_ws_defaults, **init_kwargs)

        self.base_url = self.az_cloud_config.resource_manager
        self.sent_urls: Dict[str, str] = {}
        self.sent_data_query: Optional[SentinelQueryProvider] = None  # type: ignore
        self.url: Optional[str] = None
        self._token: Optional[str] = None

        logger.info("Initializing Microsoft Sentinel connector")
        logger.info(
            "Params: Cloud=%s; ResourceId=%s; Workspace=%s, Subscription=%s, ResourceGroup=%s",
            self.cloud,
            self.default_resource_id,
            self.default_workspace_name,
            self.default_subscription_id,
            self.default_resource_group,
        )

        if not any(ws_param in init_kwargs for ws_param in _WS_PARAMETERS):
            # if we can't build a resource ID from the parameters, try to get the
            # default workspace settings from the configuration file.
            default_workspace = WorkspaceConfig(init_kwargs.get(_WS_NAME))
            self._default_settings = self._set_ws_defaults(
                self._default_settings,
                subscription_id=init_kwargs.get(
                    _SUB_ID,
                    default_workspace.get(WorkspaceConfig.CONF_SUB_ID),
                ),
                resource_group=init_kwargs.get(
                    _RES_GRP,
                    default_workspace.get(WorkspaceConfig.CONF_RES_GROUP),
                ),
                workspace_name=init_kwargs.get(
                    _WS_NAME,
                    default_workspace.get(WorkspaceConfig.CONF_WS_NAME),
                ),
            )

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
        cloud : str, optional
            What Azure cloud to connect to.
            By default it will attempt to use the cloud setting from config file.
            If this is not set it will default to Azure Public Cloud
        credential: AzureCredential, optional
            Credentials to use for authentication. This will use the credential
            directly and bypass the MSTICPy Azure credential selection process.
        workspace_name: str, optional
            If specified, this will override any default workspace settings
            set during initialization.
            `workspace` is an alias for workspace_name.

        Notes
        -----
        You can also supply override the default settings (set during initialization)
        for by supplying either a full `resource_id` or individiual `subscription_id`,
        `resource_group` and/or `workspace_name` parameters. For the individual
        parameters, supplying one or more of these will override the default settings
        from initialization.
        To revert to the default settings, run `connect()` again without any of
        these parameters.

        See Also
        --------
        msticpy.auth.azure_auth.az_connect : function to authenticate to Azure SDK

        """
        connect_kwargs = _map_legacy_param_names(**kwargs)
        if any(connect_kwargs.get(ws_param) for ws_param in _CORE_WS_PARAMETERS):
            self._create_api_paths_for_workspace(
                subscription_id=connect_kwargs.get(
                    _SUB_ID, self.default_subscription_id
                ),
                resource_group=connect_kwargs.get(
                    _RES_GRP, self.default_resource_group
                ),
                workspace_name=connect_kwargs.get(
                    _WS_NAME, self.default_workspace_name
                ),
            )
        else:
            self._create_api_paths_for_workspace(
                connect_kwargs.get(_RES_ID, self.default_resource_id)
            )

        if kwargs.get("cloud", self.cloud) != self.cloud:
            raise MsticpyUserConfigError(
                "Cannot switch to different cloud",
                f"Current cloud '{self.cloud}'",
                f"Create a new instance of `{self.__class__.__name__}`",
                "and specify the new cloud name using the `cloud` parameter.",
                title="Cannot switch cloud at connect time",
            )
        logger.info("Using tenant id %s", tenant_id)
        az_connect_kwargs = {
            key: value
            for key, value in connect_kwargs.items()
            if key not in _WS_PARAMETERS
        }
        if tenant_id:
            az_connect_kwargs["tenant_id"] = tenant_id
        self._token = az_connect_kwargs.pop("token", None)
        super().connect(auth_methods=auth_methods, silent=silent, **az_connect_kwargs)
        if not self._token:
            logger.info("Getting token for %s", tenant_id)
            self._token = get_token(
                self.credentials, tenant_id=tenant_id, cloud=self.cloud  # type: ignore
            )

    def _create_api_paths_for_workspace(
        self,
        az_resource_id: Optional[str] = None,
        subscription_id: str = "",
        resource_group: str = "",
        workspace_name: str = "",
    ):
        """Save configuration and build API URLs for workspace."""
        az_resource_id = (
            az_resource_id
            or self._build_sent_res_id(
                subscription_id or self.default_subscription_id or "",
                resource_group or self.default_resource_group or "",
                workspace_name or self.default_workspace_name or "",
            )
            or self.default_resource_id
        )
        try:
            validate_resource_id(az_resource_id)
        except MsticpyUserConfigError as err:
            logger.error("Error validating resource ID %s", err)
            raise MsticpyUserConfigError(
                "Unable to build a valid resource ID from the parameters provided.",
                "This class requires either a valid Azure resource ID or a combination of",
                "subscription ID, resource group and workspace name.",
                "Please ensure that one of the following is true:",
                (
                    "1. You provide a `workspace_name` parameter with corresponding "
                    "settings in the configuration file."
                ),
                (
                    "2. You provide a valid Azure `resource_id` as parameter to "
                    "initialize the class or when calling `connect()`."
                ),
                (
                    "3. You provide a valid Azure `subscription_id`, `resource_group` "
                    "and `workspace_name` as parameters."
                ),
                "4. Your default workspace (in msticpyconfig) has these settings.",
                title="Unable to build valid resource ID",
            ) from err
        self.url = self._build_sent_paths(az_resource_id, self.base_url)  # type: ignore

        self.sent_urls = {
            name: f"{self.url}{mapping}" for name, mapping in _PATH_MAPPING.items()
        }
        logger.info("API URLs set to %s", self.sent_urls)

    def set_default_subscription(self, subscription_id: str):
        """Set the default subscription to use to `subscription_id`."""
        raise NotImplementedError(
            "This method is deprecated. Use `set_default_workspace` instead "
            "or set the subscription ID during initialization."
        )

    def set_default_workspace(
        self,
        workspace: Optional[str] = None,
        resource_id: Optional[str] = None,
        **kwargs,
    ):
        """
        Set the default workspace.

        Sets the default workspace to use for subsequent queries. If the `workspace`
        parameter is specified, the workspace settings will be read from the
        msticpyconfig file. If the `subscription_id` parameter is specified, it will return
        the workspace from the subscription, if only one workspace is found.
        If neither are specified but a workspace resource ID has been set during
        initialization (the `resource_id` parameter), it will use the workspace details
        from that resource ID.
        Otherwise it will set the workspace to be the default workspace in the
        configuration file.

        Parameters
        ----------
        workspace : Optional[str], optional
            Name of the workspace, by default None.
            If not specified and there is only one workspace in the
            subscription, this will be set as the default.
        resource_id: Optional[str], optional
            Azure resource ID for the workspace. If specified, this will set
            the workspace to use the details from this resource ID.

        Raises
        ------
        ValueError
            If no current or default subscription has been set.

        """
        adjust_kwargs = _map_legacy_param_names(**kwargs)
        if workspace:
            adjust_kwargs[_WS_NAME] = workspace
        if resource_id:
            adjust_kwargs[_RES_ID] = resource_id
        if _SUB_ID in adjust_kwargs:
            warnings.warn(
                "Setting the workspace from the `subscription_id` parameter "
                "no longer supported. Please use the `workspace` parameter "
                "instead or set the workspace or "
                "Azure resource ID during initialization."
            )
        workspace = adjust_kwargs.get(_WS_NAME, workspace)

        if resource_id:
            self._default_settings = self._set_ws_defaults(
                _get_ws_defaults,
                resource_id=resource_id,
            )
            logger.info(
                "Workspace settings set from resource ID %s",
                resource_id,
            )
            return
        # if workspace is one that we have configuration for, get the details from there.
        if workspace in WorkspaceConfig.list_workspaces():
            logger.info("Workspace %s found in settings", workspace)
            workspace_config = WorkspaceConfig(workspace)
        else:
            workspace_config = WorkspaceConfig()
        # set these as defaults
        self._default_settings = self._set_ws_defaults(
            _get_ws_defaults,
            subscription_id=workspace_config.get(WorkspaceConfig.CONF_SUB_ID_KEY),
            resource_group=workspace_config.get(WorkspaceConfig.CONF_RES_GROUP_KEY),
            workspace_name=workspace_config.get(WorkspaceConfig.CONF_WS_NAME_KEY),
        )

    @property
    def default_workspace_settings(self) -> Dict[str, Any]:
        """Return current default workspace settings."""
        return WorkspaceConfig.from_settings(
            {
                WorkspaceConfig.CONF_SUB_ID: self.default_subscription_id,
                WorkspaceConfig.CONF_RES_GROUP: self.default_resource_group,
                WorkspaceConfig.CONF_WS_NAME: self.default_workspace_name,
            }
        ).mp_settings

    @property
    def default_subscription_id(self) -> Optional[str]:
        """Return the default subscription ID."""
        return self._default_settings().subscription_id

    @property
    def default_resource_group(self) -> Optional[str]:
        """Return the default resource group."""
        return self._default_settings().resource_group

    @property
    def default_workspace_name(self) -> Optional[str]:
        """Return the default workspace Name."""
        return self._default_settings().workspace_name

    @property
    def default_resource_id(self) -> Optional[str]:
        """Return the default resource ID."""
        return self._default_settings().resource_id

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

    def _set_ws_defaults(self, get_defaults_func: Callable, **kwargs) -> Callable:
        """Create a partial function with the defaults set based on the kwargs.

        Parameters
        ----------
        get_defaults_func : Callable
            The function that returns the default values

        Returns
        -------
        Callable
            A partial function with the defaults set based on the kwargs

        """
        # If workspace_name is provided, get the config values.
        if workspace_name := kwargs.get(_WS_NAME):
            if workspace_name in WorkspaceConfig.list_workspaces():
                workspace_config = WorkspaceConfig(workspace=workspace_name)
                config_values = {
                    _SUB_ID: workspace_config.get(WorkspaceConfig.CONF_SUB_ID_KEY),
                    _RES_GRP: workspace_config.get(WorkspaceConfig.CONF_RES_GROUP_KEY),
                    _WS_NAME: workspace_name,
                }
                logger.info("Workspace settings found for %s", workspace_name)
                get_defaults_func = partial(get_defaults_func, **config_values)

        # If any of the settings args are provided, set them as defaults
        # Even if they override the config values.
        for arg_name in _CORE_WS_PARAMETERS:
            if arg := kwargs.get(arg_name):
                get_defaults_func = partial(get_defaults_func, **{arg_name: arg})

        # If an explicit resource ID is provided, parse it and set the defaults
        # This overrides any other settings.
        if resource_id := kwargs.get(_RES_ID):
            get_defaults_func = partial(
                get_defaults_func, **parse_resource_id(resource_id)
            )

        return get_defaults_func


# Alias for old class name
AzureSentinel = MicrosoftSentinel
