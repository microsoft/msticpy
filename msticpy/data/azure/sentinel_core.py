# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Uses the Microsoft Sentinel APIs to interact with Microsoft Sentinel Workspaces."""
from typing import Dict, List, Optional, Tuple

import pandas as pd

from ..._version import VERSION
from .azure_data import AzureData, get_token
from .sentinel_analytics import SentinelAnalyticsMixin, SentinelHuntingMixin
from .sentinel_bookmarks import SentinelBookmarksMixin
from .sentinel_incidents import SentinelIncidentsMixin
from .sentinel_utils import _PATH_MAPPING, SentinelUtilsMixin, validate_res_id
from .sentinel_watchlists import SentinelWatchlistsMixin

__version__ = VERSION
__author__ = "Pete Bryan"


class MicrosoftSentinel(
    SentinelAnalyticsMixin,
    SentinelHuntingMixin,
    SentinelBookmarksMixin,
    SentinelIncidentsMixin,
    SentinelUtilsMixin,
    SentinelWatchlistsMixin,
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
    ):
        """
        Initialize connector for Azure APIs.

        Parameters
        ----------
        res_id : str, optional
            Set the Sentinel workspace resource ID you want to use, if not specified
            defaults will be looked for or details can be passed seperately with functions.
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
            If not specifying a resource ID the Workspace name of the
            Sentinel Workspace, by default None

        """
        super().__init__(connect=connect, cloud=cloud)
        self.config = None  # type: ignore
        self.base_url = self.endpoints.resource_manager
        self.default_subscription: Optional[str] = None
        self.default_workspace: Optional[Tuple[str, str]] = None
        res_id = res_id or self._get_default_workspace()
        if not res_id:
            res_id = self._build_sent_res_id(sub_id, res_grp, ws_name)
        res_id = validate_res_id(res_id)
        self.url = self._build_sent_paths(res_id, self.base_url)  # type: ignore
        self.sent_urls = {
            "bookmarks": self.url + _PATH_MAPPING["bookmarks"],
            "incidents": self.url + _PATH_MAPPING["incidents"],
            "alert_rules": self.url + _PATH_MAPPING["alert_rules"],
            "watchlists": self.url + _PATH_MAPPING["watchlists"],
        }

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
        if not tenant_id:
            config = self._check_config(["tenant_id"])
            tenant_id = config["tenant_id"]

        super().connect(auth_methods=auth_methods, tenant_id=tenant_id, silent=silent)
        if "token" in kwargs:
            self.token = kwargs["token"]
        else:
            self.token = get_token(self.credentials)  # type: ignore

        self.res_group_url = None
        self.prov_path = None

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

    def list_sentinel_workspaces(self, sub_id: str = None) -> Dict[str, str]:
        """
        Return a list of Microsoft Sentinel workspaces in a Subscription.

        Parameters
        ----------
        sub_id : str
            The subscription ID to get a list of workspaces from.
            If not provided it will attempt to get sub_id from config files.

        Returns
        -------
        Dict
            A dictionary of workspace names and ids

        """
        # If a subscription ID isn't provided try and get one from config files.
        sub_id = sub_id or self.default_subscription
        if not sub_id:
            config = self._check_config(["subscription_id"])
            sub_id = config["subscription_id"]

        print("Finding Microsoft Sentinel Workspaces...")
        res = self.get_resources(sub_id=sub_id)  # type: ignore
        # handle no results
        if isinstance(res, pd.DataFrame) and not res.empty:
            sentinel = res[
                (res["resource_type"] == "Microsoft.OperationsManagement/solutions")
                & (res["name"].str.startswith("SecurityInsights"))
            ]
            workspaces = []
            for wrkspace in sentinel["resource_id"]:
                res_details = self.get_resource_details(
                    sub_id=sub_id, resource_id=wrkspace  # type: ignore
                )
                workspaces.append(res_details["properties"]["workspaceResourceId"])

            workspaces_dict = {}
            for wrkspace in workspaces:
                name = wrkspace.split("/")[-1]
                workspaces_dict[name] = wrkspace
            return workspaces_dict

        print(f"No Microsoft Sentinel workspaces in {sub_id}")
        return {}

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

        """
        sub_id = sub_id or self.default_subscription
        workspaces = self.get_sentinel_workspaces(sub_id=sub_id)
        if len(workspaces) == 1:
            self.default_workspace = next(iter(workspaces.items()))
        elif workspace in workspaces:
            self.default_workspace = workspace, workspaces[workspace]

    def _get_default_workspace(self):
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

    # Get > List Aliases
    get_sentinel_workspaces = list_sentinel_workspaces


# Alias for old class name
AzureSentinel = MicrosoftSentinel
