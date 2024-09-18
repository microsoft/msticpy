# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Uses the Azure Python SDK to collect and return details related to Azure."""
from __future__ import annotations

import datetime
import logging
from dataclasses import asdict, dataclass, field
from importlib.metadata import version
from typing import TYPE_CHECKING, Any, Callable, Iterable

import numpy as np
import pandas as pd
from packaging.version import Version, parse
from typing_extensions import Self

from ..._version import VERSION
from ...auth.azure_auth import (
    AzCredentials,
    AzureCloudConfig,
    az_connect,
    fallback_devicecode_creds,
)
from ...auth.azure_auth_core import only_interactive_cred
from ...common.exceptions import (
    MsticpyAzureConfigError,
    MsticpyImportExtraError,
    MsticpyNotConnectedError,
    MsticpyResourceError,
)

try:
    from azure.common.exceptions import CloudError
    from azure.core.exceptions import ClientAuthenticationError
    from azure.mgmt.network import NetworkManagementClient
    from azure.mgmt.resource import ResourceManagementClient
    from azure.mgmt.resource.subscriptions import SubscriptionClient

    if parse(version("azure.mgmt.monitor")) > Version("1.0.1"):
        # Try new version but keep backward compat with 1.0.1
        from azure.mgmt.monitor import MonitorManagementClient
    else:
        from azure.mgmt.monitor import (  # type: ignore[attr-defined, no-redef]
            MonitorClient as MonitorManagementClient,
        )
    from azure.mgmt.compute import ComputeManagementClient

    if TYPE_CHECKING:
        from azure.mgmt.compute.models import VirtualMachineInstanceView
        from azure.mgmt.network.models import NetworkInterface
        from azure.mgmt.subscription.models import Subscription
except ImportError as imp_err:
    error_msg: str = (
        "Cannot use this feature without these azure packages installed:\n"
        "azure.mgmt.network\n"
        "azure.mgmt.resource\n"
        "azure.mgmt.monitor\n"
        "azure.mgmt.compute\n"
    )
    raise MsticpyImportExtraError(
        error_msg,
        title="Error importing azure module",
        extra="azure",
    ) from imp_err

__version__ = VERSION
__author__ = "Pete Bryan"

# pylint:disable=too-many-lines

logger: logging.Logger = logging.getLogger(__name__)

_CLIENT_MAPPING: dict[
    str,
    type[
        SubscriptionClient
        | ResourceManagementClient
        | NetworkManagementClient
        | MonitorManagementClient
        | ComputeManagementClient
    ],
] = {
    "sub_client": SubscriptionClient,
    "resource_client": ResourceManagementClient,
    "network_client": NetworkManagementClient,
    "monitoring_client": MonitorManagementClient,
    "compute_client": ComputeManagementClient,
}


@dataclass
class Items:  # pylint:disable=too-many-instance-attributes
    """attr class to build resource details dictionary."""

    resource_id: str | None = None
    name: str | None = None
    resource_type: str | None = None
    location: str | None = None
    tags: Any = None
    plan: Any = None
    properties: Any = None
    kind: str | None = None
    managed_by: str | None = None
    sku: str | None = None
    identity: str | None = None
    state: Any = None


@dataclass
class NsgItems:
    """attr class to build NSG rule dictionary."""

    rule_name: str | None = None
    description: str | None = None
    protocol: str | None = None
    direction: str | None = None
    src_ports: str | None = None
    dst_ports: str | None = None
    src_addrs: str | None = None
    dst_addrs: str | None = None
    action: str | None = None


@dataclass
class InterfaceItems:
    """attr class to build network interface details dictionary."""

    interface_id: str | None = None
    private_ip: str | None = None
    private_ip_allocation: str | None = None
    public_ip: str | None = None
    public_ip_allocation: str | None = None
    app_sec_group: list = field(default_factory=list)
    subnet: str | None = None
    subnet_nsg: Any = None
    subnet_route_table: Any = None


class AzureData:  # pylint:disable=too-many-instance-attributes
    """Class for returning data on an Azure tenant."""

    def __init__(
        self: AzureData,
        *,
        connect: bool = False,
        cloud: str | None = None,
    ) -> None:
        """Initialize connector for Azure Python SDK."""
        self.az_cloud_config = AzureCloudConfig(cloud)
        self.connected = False
        self.credentials: AzCredentials | None = None
        self.sub_client: SubscriptionClient | None = None
        self.resource_client: ResourceManagementClient | None = None
        self.network_client: NetworkManagementClient | None = None
        self.monitoring_client: MonitorManagementClient | None = None
        self.compute_client: ComputeManagementClient | None = None
        self.cloud: str | None = cloud or self.az_cloud_config.cloud
        self.endpoints: dict[str, Any] = self.az_cloud_config.endpoints
        self._token: str | None = None
        self.sent_urls: dict[str, Any] = {}
        self.base_url: str = ""
        self.url: str | None = None
        logger.info("Initialized AzureData")
        if connect:
            self.connect()

    def connect(
        self: Self,
        auth_methods: list[str] | None = None,
        tenant_id: str | None = None,
        *,
        silent: bool = False,
        cloud: str | None = None,
        **kwargs,
    ) -> None:
        """
        Authenticate to the Azure SDK.

        Parameters
        ----------
        auth_methods : List, optional
            list of preferred authentication methods to use, by default None
        tenant_id : str, optional
            The tenant to authenticate against. If not supplied, the default
            tenant for the identity will be used.
        silent : bool, optional
            Set true to prevent output during auth process, by default False
        cloud : str, optional
            What Azure cloud to connect to.
            By default it will attempt to use the cloud setting from config file.
            If this is not set it will default to Azure Public Cloud
        **kwargs
            Additional keyword arguments to pass to the az_connect function.

        Raises
        ------
        CloudError
            If no valid credentials are found or if subscription client can't be created

        See Also
        --------
        msticpy.auth.azure_auth.az_connect : function to authenticate to Azure SDK

        """
        if cloud:
            logger.info("Setting cloud to %s", cloud)
            self.cloud = cloud
            self.az_cloud_config = AzureCloudConfig(self.cloud)
        auth_methods = auth_methods or self.az_cloud_config.auth_methods
        tenant_id = tenant_id or self.az_cloud_config.tenant_id
        self.credentials = az_connect(
            auth_methods=auth_methods,
            tenant_id=tenant_id,
            silent=silent,
            **kwargs,
        )
        if not self.credentials:
            err_msg: str = "Could not obtain credentials."
            raise CloudError(err_msg)
        if only_interactive_cred(self.credentials.modern) and not silent:
            logger.warning("Check your default browser for interactive sign-in prompt.")

        self.sub_client = SubscriptionClient(
            credential=self.credentials.modern,
            base_url=self.az_cloud_config.resource_manager,
            credential_scopes=[self.az_cloud_config.token_uri],
        )
        if not self.sub_client:
            err_msg = "Could not create a Subscription client."
            raise CloudError(err_msg)
        logger.info("Connected to Azure Subscription Client")
        self.connected = True

    def get_subscriptions(self: Self) -> pd.DataFrame:
        """
        Get details of all subscriptions within the tenant.

        Returns
        -------
        pd.DataFrame
            Details of the subscriptions present in the users tenant.

        Raises
        ------
        MsticpyNotConnectedError
            If .connect() has not been called

        """
        if self.connected is False:
            err_msg: str = (
                "You need to connect to the service before using this function."
            )
            raise MsticpyNotConnectedError(
                err_msg,
                help_uri=MsticpyAzureConfigError.DEF_HELP_URI,
                title="Please call connect() before continuing.",
            )

        subscription_ids: list[str] = []
        display_names: list[str] = []
        states: list[str] = []
        if self.sub_client:
            try:
                sub_list: Iterable[Any] = list(
                    self.sub_client.subscriptions.list(),
                )
            except AttributeError:
                self._legacy_auth("sub_client")
                sub_list = list(self.sub_client.subscriptions.list())

            for item in sub_list:
                if item.subscription_id:
                    subscription_ids.append(item.subscription_id)
                if item.display_name:
                    display_names.append(item.display_name)
                states.append(str(item.state))

        return pd.DataFrame(
            {
                "Subscription ID": subscription_ids,
                "Display Name": display_names,
                "State": states,
            },
        )

    def get_subscription_info(self: Self, sub_id: str) -> dict:
        """
        Get information on a specific subscription.

        Parameters
        ----------
        sub_id : str
            The ID of the subscription to return details on.

        Returns
        -------
        dict
            Details on the selected subscription.

        Raises
        ------
        MsticpyNotConnectedError
            If .connect() has not been called.

        """
        if self.connected is False:
            err_msg: str = (
                "You need to connect to the service before using this function."
            )
            raise MsticpyNotConnectedError(
                err_msg,
                help_uri=MsticpyAzureConfigError.DEF_HELP_URI,
                title="Please call connect() before continuing.",
            )
        if not self.sub_client:
            err_msg = "sub_client must be defined to retrieve subscription info"
            raise ValueError(err_msg)

        try:
            sub: Subscription = self.sub_client.subscriptions.get(sub_id)
        except AttributeError:
            self._legacy_auth("sub_client")
            sub = self.sub_client.subscriptions.get(sub_id)
        sub_loc: dict[str, Any] | None = None
        quota_id: dict[str, Any] | None = None
        spending_limit: dict[str, Any] | None = None
        if sub.subscription_policies:
            sub_loc = sub.subscription_policies.location_placement_id
            quota_id = sub.subscription_policies.quota_id
            spending_limit = sub.subscription_policies.spending_limit

        return {
            "Subscription ID": sub.subscription_id,
            "Display Name": sub.display_name,
            "State": str(sub.state),
            "Subscription Location": sub_loc,
            "Subscription Quota": quota_id,
            "Spending Limit": spending_limit,
        }

    def list_sentinel_workspaces(self: Self, sub_id: str) -> dict[str, str]:
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
        logger.info("Finding Microsoft Sentinel Workspaces...")
        res: pd.DataFrame = self.get_resources(sub_id=sub_id)
        # handle no results
        if isinstance(res, pd.DataFrame) and not res.empty:
            sentinel: pd.DataFrame = res[
                (res["resource_type"] == "Microsoft.OperationsManagement/solutions")
                & (res["name"].str.startswith("SecurityInsights"))
            ]
            workspaces: list[str] = []
            for wrkspace in sentinel["resource_id"]:
                res_details: dict[str, Any] = self.get_resource_details(
                    sub_id=sub_id,
                    resource_id=wrkspace,
                )
                workspaces.append(res_details["properties"]["workspaceResourceId"])

            workspaces_dict: dict[str, Any] = {}
            for wrkspace in workspaces:
                name: str = wrkspace.split("/")[-1]
                workspaces_dict[name] = wrkspace
            return workspaces_dict

        logger.info("No Microsoft Sentinel workspaces in %s", sub_id)
        return {}

    # Get > List Aliases
    get_sentinel_workspaces: Callable[..., dict[str, Any]] = list_sentinel_workspaces

    def get_resources(
        self: Self,
        sub_id: str,
        rgroup: str | None = None,
        *,
        get_props: bool = False,
    ) -> pd.DataFrame:
        """
        Return details on all resources in a subscription or Resource Group.

        Parameters
        ----------
        sub_id: str
            The subscription ID to get resources for
        rgroup: str (Optional)
            The name of a Resource Group to get resources for
        get_props: bool (Optional)
            Set to True if you want to get the full properties of every resource
            Warning this may be a slow process depending on the number of resources

        Returns
        -------
        pd.DataFrame
            A dataframe of resource details

        """
        # Check if connection and client required are already present
        if self.connected is False:
            err_msg: str = (
                "You need to connect to the service before using this function."
            )
            raise MsticpyNotConnectedError(
                err_msg,
                help_uri=MsticpyAzureConfigError.DEF_HELP_URI,
                title="Please call connect() before continuing.",
            )

        self._check_client("resource_client", sub_id)
        if not self.resource_client:
            err_msg = "Resource client must be set to retrieve resources."
            raise ValueError(err_msg)

        resources: list[Any] = []
        if rgroup is None:
            resources.extend(iter(self.resource_client.resources.list()))
        else:
            resources.extend(
                iter(self.resource_client.resources.list_by_resource_group(rgroup)),
            )

        # Warn users about getting full properties for each resource
        if get_props:
            logger.info(
                "Collecting properties for every resource may take some time...",
            )

        resource_items: list[Any] = []

        # Get properties for each resource
        for resource in resources:
            if get_props:
                if resource.type == "Microsoft.Compute/virtualMachines":
                    state: VirtualMachineInstanceView | None = self._get_compute_state(
                        resource_id=resource.id,
                        sub_id=sub_id,
                    )
                else:
                    state = None
                try:
                    props = self.resource_client.resources.get_by_id(
                        resource.id,
                        "2019-08-01",
                    ).properties

                except CloudError:
                    props = self.resource_client.resources.get_by_id(
                        resource.id,
                        self._get_api(resource_id=resource.id, sub_id=sub_id),
                    ).properties
            else:
                props = resource.properties
                state = None

            # Parse relevant resource attributes into a dataframe and return it
            resource_details = asdict(
                Items(
                    resource.id,
                    resource.name,
                    resource.type,
                    resource.location,
                    resource.tags,
                    resource.plan,
                    props,
                    resource.kind,
                    resource.managed_by,
                    resource.sku,
                    resource.identity,
                    state,
                ),
            )
            resource_items.append(resource_details)

        return pd.DataFrame(resource_items)

    def get_resource_details(
        self: Self,
        sub_id: str,
        resource_id: str | None = None,
        resource_details: dict[str, Any] | None = None,
    ) -> dict:
        """
        Return the details of a specific Azure resource.

        Parameters
        ----------
        resource_id: str, optional
            The ID of the resource to get details on
        resource_details: dict, optional
            If ID is unknown provide the following details:
                -resource_group_name
                -resource_provider_namespace
                -resource_type
                -resource_name
                -parent_resource_path
        sub_id: str
            The ID of the subscription to get resources from

        Returns
        -------
        resource_details: dict
            The details of the requested resource

        """
        # Check if connection and client required are already present
        if self.connected is False:
            err_msg: str = (
                "You need to connect to the service before using this function."
            )
            raise MsticpyNotConnectedError(
                err_msg,
                help_uri=MsticpyAzureConfigError.DEF_HELP_URI,
                title="Please call connect() before continuing.",
            )
        self._check_client("resource_client", sub_id)
        if not self.resource_client:
            err_msg = "Cannot get resource details if resource client is not set."
            raise ValueError(err_msg)

        # If a resource id is provided use get_by_id to get details
        if resource_id is not None:
            try:
                resource = self.resource_client.resources.get_by_id(
                    resource_id,
                    api_version=self._get_api(resource_id=resource_id, sub_id=sub_id),
                )
            except AttributeError:
                self._legacy_auth("resource_client", sub_id)
                resource = self.resource_client.resources.get_by_id(
                    resource_id,
                    api_version=self._get_api(resource_id=resource_id, sub_id=sub_id),
                )
            if resource.type == "Microsoft.Compute/virtualMachines":
                state = self._get_compute_state(resource_id=resource_id, sub_id=sub_id)
            else:
                state = None
        # If resource details are provided use get to get details
        elif resource_details is not None:
            try:
                resource = self.resource_client.resources.get(
                    resource_details["resource_group_name"],
                    resource_details["resource_provider_namespace"],
                    resource_details["parent_resource_path"],
                    resource_details["resource_type"],
                    resource_details["resource_name"],
                    api_version=self._get_api(
                        resource_provider=(
                            resource_details["resource_provider_namespace"]
                            + "/"
                            + resource_details["resource_type"]
                        ),
                        sub_id=sub_id,
                    ),
                )
            except AttributeError:
                self._legacy_auth("resource_client", sub_id)
                resource = self.resource_client.resources.get(
                    resource_details["resource_group_name"],
                    resource_details["resource_provider_namespace"],
                    resource_details["parent_resource_path"],
                    resource_details["resource_type"],
                    resource_details["resource_name"],
                    api_version=self._get_api(
                        resource_provider=(
                            resource_details["resource_provider_namespace"]
                            + "/"
                            + resource_details["resource_type"]
                        ),
                        sub_id=sub_id,
                    ),
                )
            state = None
        else:
            err_msg = "Please provide either a resource ID or resource details"
            raise ValueError(err_msg)

        # Parse relevent details into a dictionary to return
        return asdict(
            Items(
                resource.id,
                resource.name,
                resource.type,
                resource.location,
                resource.tags,
                resource.plan,
                resource.properties,
                resource.kind,
                resource.managed_by,
                resource.sku,
                resource.identity,
                state,
            ),
        )

    @staticmethod
    def _normalize_resources(
        resource_id: str | None = None,
        resource_provider: str | None = None,
    ) -> tuple[str, str]:
        """Normalize elements depending on user input type."""
        if resource_id:
            try:
                return (
                    resource_id.split("/")[6],
                    resource_id.split("/")[7],
                )
            except IndexError as idx_err:
                err_msg = (
                    "Provided Resource ID isn't in the correct format. "
                    "It should look like:\n"
                    "/subscriptions/SUB_ID/resourceGroups/RESOURCE_GROUP/"
                    "providers/NAMESPACE/SERVICE_NAME/RESOURCE_NAME "
                )
                raise MsticpyResourceError(err_msg) from idx_err

        elif resource_provider:
            try:
                return (
                    resource_provider.split("/")[0],
                    resource_provider.split("/")[1],
                )
            except IndexError as idx_err:
                err_msg = (
                    "Provided Resource Provider isn't in the correct format.\n"
                    "It should look like: NAMESPACE/SERVICE_NAME"
                )
                raise MsticpyResourceError(err_msg) from idx_err
        else:
            err_msg = "Please provide an resource ID or resource provider namespace"
            raise ValueError(err_msg)

    def _get_api(
        self: Self,
        *,
        sub_id: str,
        resource_id: str | None = None,
        resource_provider: str | None = None,
    ) -> str:
        """
        Return the latest avaliable API version for the resource.

        Parameters
        ----------
        resource_id: str, optional
            The ID of the resources to get an API version for
        sub_id: str, optional
            The ID of the subscription to get details from
        resource_provider: str, optional
            The resource provider namespace and service to get an API version for

        Returns
        -------
        api_ver: str
            The latest available non-preview API version

        """
        # Check if connection and client required are already present
        if self.connected is False:
            err_msg: str = (
                "You need to connect to the service before using this function."
            )
            raise MsticpyNotConnectedError(
                err_msg,
                help_uri=MsticpyAzureConfigError.DEF_HELP_URI,
                title="Please call connect() before continuing.",
            )

        self._check_client("resource_client", sub_id)
        if not self.resource_client:
            err_msg = "Resource client must be set to get api."
            raise ValueError(err_msg)

        namespace, service = AzureData._normalize_resources(
            resource_id=resource_id,
            resource_provider=resource_provider,
        )

        # Get list of API versions for the service
        try:
            provider = self.resource_client.providers.get(namespace)
        except AttributeError:
            self._legacy_auth("resource_client", sub_id)
            provider = self.resource_client.providers.get(namespace)

        if not provider.resource_types:
            resource_types = None
        else:
            resource_types = next(
                (t for t in provider.resource_types if t.resource_type == service),
                None,
            )

        # Get first API version that isn't in preview
        if not resource_types:
            err_msg = "Resource provider not found"
            raise MsticpyResourceError(err_msg)

        api_version = [
            v for v in resource_types.api_versions if "preview" not in v.lower()
        ]
        if api_version is None or not api_version:
            api_ver = resource_types.api_versions[0]
        else:
            api_ver = api_version[0]
        return str(api_ver)

    def get_network_details(
        self: Self,
        network_id: str,
        sub_id: str,
    ) -> tuple[pd.DataFrame, pd.DataFrame]:
        """
        Return details related to an Azure network interface and associated NSG.

        Parameters
        ----------
        network_id: str
            The ID of the network interface to return details on
        sub_id: str
            The subscription ID that the network interface is part of

        Returns
        -------
        details: dict
            A dictionary of items related to the network interface

        """
        # Check if connection and client required are already present
        if self.connected is False:
            err_msg: str = (
                "You need to connect to the service before using this function."
            )
            raise MsticpyNotConnectedError(
                err_msg,
                help_uri=MsticpyAzureConfigError.DEF_HELP_URI,
                title="Please call connect() before continuing.",
            )

        self._check_client("network_client", sub_id)
        if not self.network_client:
            err_msg = "Cannot retrieve network details if the network client is not set"
            raise ValueError(err_msg)

        # Get interface details and parse relevant elements into a dataframe
        try:
            details: NetworkInterface = self.network_client.network_interfaces.get(
                network_id.split("/")[4],
                network_id.split("/")[8],
            )
        except AttributeError:
            self._legacy_auth("network_client", sub_id)
            details = self.network_client.network_interfaces.get(
                network_id.split("/")[4],
                network_id.split("/")[8],
            )

        ips: list[dict[str, Any]] = []
        for ip_addr in details.ip_configurations or []:
            ip_details: dict[str, Any] = asdict(
                InterfaceItems(
                    interface_id=network_id,
                    private_ip=ip_addr.private_ip_address,
                    private_ip_allocation=str(ip_addr.private_ip_allocation_method),
                    public_ip=(
                        ip_addr.public_ip_address.ip_address
                        if ip_addr.public_ip_address
                        else None
                    ),
                    public_ip_allocation=(
                        ip_addr.public_ip_address.public_ip_allocation_method
                        if ip_addr.public_ip_address
                        else None
                    ),
                    app_sec_group=(
                        ip_addr.application_security_groups
                        if ip_addr.application_security_groups
                        else []
                    ),
                    subnet=ip_addr.subnet.name if ip_addr.subnet else None,
                    subnet_nsg=(
                        ip_addr.subnet.network_security_group
                        if ip_addr.subnet
                        else None
                    ),
                    subnet_route_table=(
                        ip_addr.subnet.route_table if ip_addr.subnet else None
                    ),
                ),
            )
            ips.append(ip_details)

        ip_df = pd.DataFrame(ips)

        nsg_df = pd.DataFrame()
        if (
            details.network_security_group is not None
            and details.network_security_group.id is not None
        ):
            # Get NSG details and parse relevant elements into a dataframe
            nsg_details = self.network_client.network_security_groups.get(
                details.network_security_group.id.split("/")[4],
                details.network_security_group.id.split("/")[8],
            )
            nsg_rules = []
            for nsg in nsg_details.default_security_rules:
                rules = asdict(
                    NsgItems(
                        rule_name=nsg.name,
                        description=nsg.description,
                        protocol=str(nsg.protocol),
                        direction=str(nsg.direction),
                        src_ports=nsg.source_port_range,
                        dst_ports=nsg.destination_port_range,
                        src_addrs=nsg.source_address_prefix,
                        dst_addrs=nsg.destination_address_prefix,
                        action=str(nsg.access),
                    ),
                )
                nsg_rules.append(rules)

            nsg_df = pd.DataFrame(nsg_rules)

        return ip_df, nsg_df

    def get_metrics(  # pylint: disable=too-many-locals #noqa: PLR0913
        self: Self,
        metrics: str,
        resource_id: str,
        sub_id: str,
        sample_time: str = "hour",
        start_time: int = 30,
    ) -> dict[str, pd.DataFrame]:
        """
        Return specified metrics on Azure Resource.

        Parameters
        ----------
        metrics: str
            A string list of metrics you wish to collect
            (https://docs.microsoft.com/en-us/azure/azure-monitor/platform/metrics-supported)
        resource_id: str
            The resource ID of the resource to collet the metrics from
        sub_id: str
            The subscription ID that the resource is part of
        sample_time: str (Optional)
            You can select to collect the metrics every hour of minute - default is hour
            Accepted inputs = 'hour' or 'minute'
        start_time: int (Optional)
            The number of days prior to today to collect metrics for, default is 30

        Returns
        -------
        results: dict
            A Dictionary of DataFrames containing the metrics details

        """
        if sample_time.casefold().startswith("h"):
            interval = "PT1H"
        elif sample_time.casefold().startswith("m"):
            interval = "PT1M"
        else:
            err_msg: str = "invalid value for sample_time - specify 'hour', or 'minute'"
            raise ValueError(err_msg)

        # Check if connection and client required are already present
        if self.connected is False:
            err_msg = "You need to connect to the service before using this function."
            raise MsticpyNotConnectedError(
                err_msg,
                help_uri=MsticpyAzureConfigError.DEF_HELP_URI,
                title="Please call connect() before continuing.",
            )

        self._check_client("monitoring_client", sub_id)
        if not self.monitoring_client:
            err_msg = "Cannot get metrics if monitoring client is not set."
            raise ValueError(err_msg)

        # Get metrics in one hour chunks for the last 30 days
        start = datetime.datetime.now(tz=datetime.timezone.utc).date()
        end = start - datetime.timedelta(days=start_time)

        try:
            mon_details = self.monitoring_client.metrics.list(
                resource_id,
                timespan=f"{end}/{start}",
                interval=interval,
                metricnames=f"{metrics}",
                aggregation="Total",
            )
        except AttributeError:
            self._legacy_auth("monitoring_client", sub_id)
            mon_details = self.monitoring_client.metrics.list(
                resource_id,
                timespan=f"{end}/{start}",
                interval=interval,
                metricnames=f"{metrics}",
                aggregation="Total",
            )
        results: dict[str, Any] = {}
        # Create a dict of all the results returned
        for metric in mon_details.value:
            times: list = []
            output = []
            for time in metric.timeseries:
                for data in time.data:
                    times.append(data.time_stamp)
                    output.append(data.total)
            details = pd.DataFrame({"Time": times, "Data": output})
            details = details.replace(np.nan, 0)
            results[metric.name.value] = details
        return results

    def _get_compute_state(
        self: Self,
        resource_id: str,
        sub_id: str,
    ) -> VirtualMachineInstanceView:
        """
        Return the details on a Virtual Machine instance.

        Parameters
        ----------
        resource_id: str
            The Resource ID of the Virtual Machine
        sub_id: str
            The Subscription the Virtual Machine is part of

        Returns
        -------
        instance_details: VirtualMachineInstanceView
            The details of the Virtual Machine

        """
        if self.connected is False:
            err_msg: str = (
                "You need to connect to the service before using this function."
            )
            raise MsticpyNotConnectedError(
                err_msg,
                help_uri=MsticpyAzureConfigError.DEF_HELP_URI,
                title="Please call connect() before continuing.",
            )

        self._check_client("compute_client", sub_id)
        if not self.compute_client:
            err_msg = "Cannot provide compute state if compute_client is None."
            raise ValueError(err_msg)

        # Parse the Resource ID to extract Resource Group and Resource Name
        r_details = resource_id.split("/")
        r_group = r_details[r_details.index("resourceGroups") + 1]
        name = r_details[r_details.index("virtualMachines") + 1]

        # Get VM instance details and return them
        try:
            instance_details: VirtualMachineInstanceView = (
                self.compute_client.virtual_machines.instance_view(
                    r_group,
                    name,
                )
            )
        except AttributeError:
            self._legacy_auth("compute_client", sub_id)
            instance_details = self.compute_client.virtual_machines.instance_view(
                r_group,
                name,
            )

        return instance_details

    def _check_client(self: Self, client_name: str, sub_id: str | None = None) -> None:
        """
        Check required client is present, if not create it.

        Parameters
        ----------
        client_name : str
            The name of the client to be checked.
        sub_id : str, optional
            The subscription ID for the client to connect to, by default None

        """
        if not self.credentials:
            err_msg: str = "Credentials must be provided for _check_client to work."
            raise ValueError(err_msg)
        if getattr(self, client_name) is None:
            client: type[
                SubscriptionClient
                | ResourceManagementClient
                | NetworkManagementClient
                | MonitorManagementClient
                | ComputeManagementClient
            ] = _CLIENT_MAPPING[client_name]
            if sub_id is None:
                if issubclass(client, SubscriptionClient):
                    setattr(
                        self,
                        client_name,
                        client(
                            self.credentials.modern,
                            base_url=self.az_cloud_config.resource_manager,
                            credential_scopes=[self.az_cloud_config.token_uri],
                        ),
                    )
            else:
                setattr(
                    self,
                    client_name,
                    client(
                        self.credentials.modern,
                        subscription_id=sub_id,
                        base_url=self.az_cloud_config.resource_manager,
                        credential_scopes=[self.az_cloud_config.token_uri],
                    ),
                )

        if getattr(self, client_name) is None:
            err_msg = "Could not create client"
            raise CloudError(err_msg)

    def _legacy_auth(self: Self, client_name: str, sub_id: str | None = None) -> None:
        """
        Create client with v1 authentication token.

        Parameters
        ----------
        client_name : str
            The name of the client to be checked.
        sub_id : str, optional
            The subscription ID for the client to connect to, by default None

        """
        if not self.credentials:
            err_msg: str = (
                "Credentials must be provided for legacy authentication to work."
            )
            raise ValueError(err_msg)
        client: type[
            SubscriptionClient
            | ResourceManagementClient
            | NetworkManagementClient
            | MonitorManagementClient
            | ComputeManagementClient
        ] = _CLIENT_MAPPING[client_name]
        if sub_id is None:
            if issubclass(client, SubscriptionClient):
                setattr(
                    self,
                    client_name,
                    client(
                        self.credentials.legacy,
                        base_url=self.az_cloud_config.resource_manager,
                        credential_scopes=[self.az_cloud_config.token_uri],
                    ),
                )
        else:
            setattr(
                self,
                client_name,
                client(
                    self.credentials.legacy,
                    subscription_id=sub_id,
                    base_url=self.az_cloud_config.resource_manager,
                    credential_scopes=[self.az_cloud_config.token_uri],
                ),
            )


def get_api_headers(token: str) -> dict:
    """
    Return authorization header with current token.

    Parameters
    ----------
    token : str
        Azure auth token.

    Returns
    -------
    Dict
        A dictionary of headers to be used in API calls.

    """
    return {
        "Authorization": f"Bearer {token}",
        "Content-Type": "application/json",
    }


def get_token(
    credential: AzCredentials,
    tenant_id: str | None = None,
    cloud: str | None = None,
) -> str:
    """
    Extract token from a azure.identity object.

    Parameters
    ----------
    credential : AzCredentials
        Azure OAuth credentials.
    tenant_id : str, optional
        The tenant to connect to if not the users home tenant.
    cloud: str, optional
        The Azure cloud to connect to.

    Returns
    -------
    str
        A token to be used in API calls.

    """
    az_cloud_config = AzureCloudConfig(cloud)
    tenant_id = tenant_id or az_cloud_config.tenant_id
    if tenant_id:
        try:
            token = credential.modern.get_token(AzureCloudConfig().token_uri)
        except ClientAuthenticationError:
            credential = fallback_devicecode_creds(cloud=cloud)
            token = credential.modern.get_token(AzureCloudConfig().token_uri)
    else:
        try:
            token = credential.modern.get_token(
                AzureCloudConfig().token_uri,
                tenant_id=tenant_id,
            )
        except ClientAuthenticationError:
            credential = fallback_devicecode_creds(cloud=cloud, tenant_id=tenant_id)
            token = credential.modern.get_token(
                AzureCloudConfig().token_uri,
                tenant_id=tenant_id,
            )

    return token.token
