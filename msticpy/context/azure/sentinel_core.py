# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Uses the Microsoft Sentinel APIs to interact with Microsoft Sentinel Workspaces."""
from __future__ import annotations

import logging
import warnings
from functools import partial
from typing import TYPE_CHECKING, Any, Callable

from typing_extensions import Self

from ..._version import VERSION
from ...common.exceptions import MsticpyUserConfigError
from ...common.wsconfig import WorkspaceConfig
from .azure_data import get_token
from .sentinel_analytics import SentinelAnalyticsMixin, SentinelHuntingMixin
from .sentinel_dynamic_summary import SentinelDynamicSummaryMixin, SentinelQueryProvider
from .sentinel_incidents import SentinelIncidentsMixin
from .sentinel_search import SentinelSearchlistsMixin
from .sentinel_ti import SentinelTIMixin
from .sentinel_utils import (
    _PATH_MAPPING,
    SentinelInstanceDetails,
    parse_resource_id,
    validate_resource_id,
)
from .sentinel_watchlists import SentinelWatchlistsMixin
from .sentinel_workspaces import SentinelWorkspacesMixin

if TYPE_CHECKING:
    import pandas as pd

__version__ = VERSION
__author__ = "Pete Bryan"

logger: logging.Logger = logging.getLogger(__name__)

_SUB_ID = "subscription_id"
_RES_GRP = "resource_group"
_WS_NAME = "workspace_name"
_RES_ID = "resource_id"


def _create_ws_defaults(
    *,
    subscription_id: str,
    resource_group: str,
    workspace_name: str,
) -> SentinelInstanceDetails:
    """Return default values for Sentinel workspace settings."""
    return SentinelInstanceDetails(
        subscription_id=subscription_id,
        resource_group=resource_group,
        workspace_name=workspace_name,
    )


_LEGACY_PARAM_NAMES: dict[str, str] = {
    "sub_id": _SUB_ID,
    "res_grp": _RES_GRP,
    "ws_name": _WS_NAME,
    "workspace": _WS_NAME,
    "res_id": _RES_ID,
}
_CORE_WS_PARAMETERS: list[str] = [_SUB_ID, _RES_GRP, _WS_NAME]
_WS_PARAMETERS: list[str] = [*_CORE_WS_PARAMETERS, _RES_ID]
_MISSING_PARAMS_ERR: list[str] = [
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
]


def _map_legacy_param_names(**kwargs) -> dict[str, Any]:
    """
    Map legacy parameter names to current names.

    Parameters
    ----------
    **kwargs : Dict[str, Any]
        The keyword arguments to map.

    Returns
    -------
    Dict[str, Any]
        The mapped keyword arguments with legacy names replaced by current names.

    Notes
    -----
    This is to allow for backward compatibility with older versions of the class.
    These used the following parameter names for workspace settings:
    res_id (resource_id), sub_id (subscription_id), res_grp (resource_group),
    ws_name or workspace (workspace_name).


    """
    return {_LEGACY_PARAM_NAMES.get(key, key): value for key, value in kwargs.items()}


# pylint: disable=too-many-ancestors, too-many-instance-attributes
class MicrosoftSentinel(
    SentinelAnalyticsMixin,
    SentinelHuntingMixin,
    SentinelDynamicSummaryMixin,
    SentinelWatchlistsMixin,
    SentinelSearchlistsMixin,
    SentinelWorkspacesMixin,
    SentinelTIMixin,
    SentinelIncidentsMixin,
):
    """Class for returning key Microsoft Sentinel elements."""

    def __init__(  # pylint:disable=too-many-arguments # noqa:PLR0913
        self: MicrosoftSentinel,
        resource_id: str | None = None,
        *,
        connect: bool = False,
        cloud: str | None = None,
        subscription_id: str | None = None,
        resource_group: str | None = None,
        workspace_name: str | None = None,
        **kwargs,
    ) -> None:
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
        workspace : str, optional
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
        super(SentinelIncidentsMixin, self).__init__(connect=False, cloud=cloud)

        init_kwargs: dict[str, Any] = _map_legacy_param_names(**kwargs)
        if resource_id:
            init_kwargs[_RES_ID] = resource_id
        if subscription_id:
            init_kwargs[_SUB_ID] = subscription_id
        if resource_group:
            init_kwargs[_RES_GRP] = resource_group
        if workspace_name:
            init_kwargs[_WS_NAME] = workspace_name

        self._default_settings: Callable[
            ...,
            SentinelInstanceDetails,
        ] = self._set_ws_defaults(_create_ws_defaults, **init_kwargs)
        self.base_url: str = self.az_cloud_config.resource_manager
        self.sent_data_query: SentinelQueryProvider | None = None
        self.url: str | None = None

        logger.info("Initializing Microsoft Sentinel connector")
        logger.info(
            "Params: Cloud=%s; ResourceId=%s; Workspace=%s, Subscription=%s, ResourceGroup=%s",
            self.cloud,
            self.default_resource_id,
            self.default_workspace_name,
            self.default_subscription_id,
            self.default_resource_group,
        )

        if connect:
            self.connect(**kwargs)

    def connect(  # noqa:PLR0913
        self: Self,
        auth_methods: list[str] | None = None,
        tenant_id: str | None = None,
        *,
        silent: bool = False,
        cloud: str | None = None,
        token: str | None = None,
        **kwargs,
    ) -> None:
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
        subscription_id: str, optional
            If specified, this will override the subscription ID
            set during initialization.
            `sub_id` is an alias for subscription_id.
        resource_group: str, optional
            If specified, this will override the resource group name
            set during initialization.
            `res_grp` is an alias for resource_group.
        token: str, optional
            If specified, utilize this token to authenticate against Azure.

        Notes
        -----
        You can also supply override the default settings (set during initialization)
        for by supplying either
        1) a full Azure `resource_id` for the workspace or
        2) individual `subscription_id`, `resource_group` and `workspace_name` parameters.
        For this last case, supplying one or more of these will override the default settings
        from initialization for the duration of the authenticated session. E.g. specifying
        a `workspace_name` will override the default workspace name but the subscription ID
        and resource group will remain as set during initialization.

        To revert to the initialization default settings, run `connect()` again without any of
        these parameters.

        See Also
        --------
        msticpy.auth.azure_auth.az_connect : function to authenticate to Azure SDK
        set_default_workspace : method to set the default workspace settings

        """
        connect_kwargs: dict[str, Any] = _map_legacy_param_names(**kwargs)
        if any(connect_kwargs.get(ws_param) for ws_param in _CORE_WS_PARAMETERS):
            try:
                sentinel_instance: SentinelInstanceDetails = SentinelInstanceDetails(
                    subscription_id=connect_kwargs.get(_SUB_ID)
                    or self.default_subscription_id,
                    resource_group=connect_kwargs.get(_RES_GRP)
                    or self.default_resource_group,
                    workspace_name=connect_kwargs.get(_WS_NAME)
                    or self.default_workspace_name,
                )
            except TypeError as err:
                raise MsticpyUserConfigError(
                    *_MISSING_PARAMS_ERR,
                    title="Unable to build valid Sentinel workspace instance",
                ) from err
        else:
            try:
                sentinel_instance = SentinelInstanceDetails.from_resource_id(
                    connect_kwargs.get(_RES_ID) or self.default_resource_id,
                )
            except TypeError as err:
                raise MsticpyUserConfigError(
                    *_MISSING_PARAMS_ERR,
                    title="Unable to build valid Sentinel workspace instance",
                ) from err
        self._create_api_paths_for_workspace(sentinel_instance)

        if cloud is not None and cloud != self.cloud:
            err_msg: str = (
                "Cannot switch to different cloud "
                "and specify the new cloud name using the `cloud` parameter."
            )
            raise MsticpyUserConfigError(
                err_msg,
                title="Cannot switch cloud at connect time",
            )
        logger.info("Using tenant id %s", tenant_id)
        az_connect_kwargs: dict[str, Any] = {
            key: value
            for key, value in connect_kwargs.items()
            if key not in _WS_PARAMETERS
        }
        if tenant_id:
            az_connect_kwargs["tenant_id"] = tenant_id
        self._token = token
        super().connect(auth_methods=auth_methods, silent=silent, **az_connect_kwargs)
        if not self.credentials:
            err_msg = "Could not connect."
            raise ValueError(err_msg)
        if not self._token:
            logger.info("Getting token for %s", tenant_id)
            self._token = get_token(
                self.credentials,
                tenant_id=tenant_id,
                cloud=self.cloud,
            )

    def _create_api_paths_for_workspace(
        self,
        sentinel_instance: SentinelInstanceDetails,
    ) -> None:
        """Save configuration and build API URLs for workspace."""
        try:
            validate_resource_id(sentinel_instance.resource_id)
        except MsticpyUserConfigError as err:
            logger.exception("Error validating resource ID")
            raise MsticpyUserConfigError(
                *_MISSING_PARAMS_ERR,
                title="Unable to build valid resource ID",
            ) from err
        self.url = self._build_sentinel_api_root(
            sentinel_instance=sentinel_instance,
            base_url=self.base_url,
        )

        self.sent_urls = {
            name: f"{self.url}{mapping}" for name, mapping in _PATH_MAPPING.items()
        }
        logger.info("API URLs set to %s", self.sent_urls)

    def set_default_subscription(self: Self, _: str) -> None:
        """Set the default subscription to use to `subscription_id`."""
        err_msg: str = (
            "This method is deprecated. Use `set_default_workspace` instead "
            "or set the subscription ID during initialization."
        )
        raise NotImplementedError(err_msg)

    def set_default_workspace(
        self,
        workspace: str | None = None,
        resource_id: str | None = None,
        **kwargs,
    ) -> None:
        """
        Set the default workspace from workspace name or resource id.

        Parameters
        ----------
        workspace : Optional[str], optional
            Name of the workspace, by default None.
        resource_id: Optional[str], optional
            Azure resource ID for the workspace, by default None.

        Notes
        -----
        If no workspace is specified, the workspace details will be the default
        workspace read from the msticpyconfig configuration file.
        After changing the default workspace, you will need to call `connect()` to
        authenticate with the new workspace.

        """
        adjust_kwargs: dict[str, Any] = _map_legacy_param_names(**kwargs)
        if workspace:
            adjust_kwargs[_WS_NAME] = workspace
        if resource_id:
            adjust_kwargs[_RES_ID] = resource_id
        if _SUB_ID in adjust_kwargs:
            warnings.warn(
                "Setting the workspace from the `subscription_id` parameter "
                "no longer supported. Please use the `workspace` parameter "
                "instead or set the workspace or "
                "Azure resource ID during initialization.",
                stacklevel=1,
            )
        workspace = adjust_kwargs.get(_WS_NAME, workspace)

        if resource_id:
            self._default_settings = self._set_ws_defaults(
                _create_ws_defaults,
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
            _create_ws_defaults,
            subscription_id=workspace_config.get(WorkspaceConfig.CONF_SUB_ID_KEY),
            resource_group=workspace_config.get(WorkspaceConfig.CONF_RES_GROUP_KEY),
            workspace_name=workspace_config.get(WorkspaceConfig.CONF_WS_NAME_KEY),
        )

    @property
    def default_workspace_settings(self: Self) -> dict[str, Any]:
        """Return current default workspace settings."""
        return WorkspaceConfig.from_settings(
            {
                WorkspaceConfig.CONF_SUB_ID: self.default_subscription_id,
                WorkspaceConfig.CONF_RES_GROUP: self.default_resource_group,
                WorkspaceConfig.CONF_WS_NAME: self.default_workspace_name,
            },
        ).mp_settings

    @property
    def default_subscription_id(self: Self) -> str:
        """Return the default subscription ID."""
        return self._default_settings().subscription_id

    @property
    def default_resource_group(self: Self) -> str:
        """Return the default resource group."""
        return self._default_settings().resource_group

    @property
    def default_workspace_name(self: Self) -> str:
        """Return the default workspace Name."""
        return self._default_settings().workspace_name

    @property
    def default_resource_id(self: Self) -> str:
        """Return the default resource ID."""
        return self._default_settings().resource_id

    def list_data_connectors(self: Self) -> pd.DataFrame:
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

    def _set_ws_defaults(
        self,
        create_defaults_func: Callable[..., SentinelInstanceDetails],
        **kwargs,
    ) -> Callable:
        """
        Create a partial function with the defaults set based on the kwargs.

        Parameters
        ----------
        create_defaults_func : Callable
            The function that returns the default values
        **kwargs : Dict[str, Any]
            The keyword arguments for individual workspace settings
            such as subscription_id, resource_group, workspace_name or resource_id

        Returns
        -------
        Callable
            A partial function with the defaults set based on the kwargs

        """
        non_null_kwargs: dict[str, Any] = {
            key: value for key, value in kwargs.items() if value
        }
        workspace_name: str | None = non_null_kwargs.get(_WS_NAME)
        workspace_config: WorkspaceConfig | None = None
        if not any(ws_param in non_null_kwargs for ws_param in _WS_PARAMETERS):
            # if we can't build a resource ID from the parameters, try to get the
            # default workspace settings from the configuration file.
            workspace_config = WorkspaceConfig()

        # If workspace_name is provided, get the config values.
        elif workspace_name and workspace_name in WorkspaceConfig.list_workspaces():
            workspace_config = WorkspaceConfig(workspace=workspace_name)
        if workspace_config:
            config_values: dict[str, Any] = {
                _SUB_ID: workspace_config.get(WorkspaceConfig.CONF_SUB_ID),
                _RES_GRP: workspace_config.get(WorkspaceConfig.CONF_RES_GROUP),
                _WS_NAME: workspace_config.get(WorkspaceConfig.CONF_WS_NAME),
            }
            logger.info("Workspace settings found for %s", workspace_name)
            create_defaults_func = partial(create_defaults_func, **config_values)

        # If any of the individual settings args are provided, set them as defaults
        # Even if they override the config values.
        for arg_name in _CORE_WS_PARAMETERS:
            if arg := non_null_kwargs.get(arg_name):
                create_defaults_func = partial(create_defaults_func, **{arg_name: arg})

        # If an explicit resource ID is provided, parse it and set the defaults
        # This overrides any other settings.
        if resource_id := non_null_kwargs.get(_RES_ID):
            create_defaults_func = partial(
                create_defaults_func,
                **parse_resource_id(resource_id),
            )

        return create_defaults_func


# Alias for old class name
AzureSentinel = MicrosoftSentinel
