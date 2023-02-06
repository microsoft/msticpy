# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Uses the Azure Python SDK to collect and return details related to Azure."""
import datetime
from typing import Any, Dict, List, Optional, Tuple

import attr
import numpy as np
import pandas as pd
from azure.common.exceptions import CloudError
from azure.core.exceptions import ClientAuthenticationError
from azure.mgmt.resource.subscriptions import SubscriptionClient

from ..._version import VERSION
from ...auth.azure_auth import (
    AzCredentials,
    AzureCloudConfig,
    az_connect,
    fallback_devicecode_creds,
    only_interactive_cred,
)
from ...auth.cloud_mappings import get_all_endpoints
from ...common.exceptions import (
    MsticpyAzureConfigError,
    MsticpyImportExtraError,
    MsticpyNotConnectedError,
    MsticpyResourceError,
)

try:
    from azure.mgmt.network import NetworkManagementClient
    from azure.mgmt.resource import ResourceManagementClient

    try:
        # Try new version but keep backward compat with 1.0.1
        from azure.mgmt.monitor import MonitorManagementClient
    except ImportError:
        from azure.mgmt.monitor import MonitorClient as MonitorManagementClient  # type: ignore
    from azure.mgmt.compute import ComputeManagementClient
    from azure.mgmt.compute.models import VirtualMachineInstanceView
except ImportError as imp_err:
    raise MsticpyImportExtraError(
        "Cannot use this feature without azure packages installed",
        title="Error importing azure module",
        extra="azure",
    ) from imp_err

__version__ = VERSION
__author__ = "Pete Bryan"

_CLIENT_MAPPING = {
    "sub_client": SubscriptionClient,
    "resource_client": ResourceManagementClient,
    "network_client": NetworkManagementClient,
    "monitoring_client": MonitorManagementClient,
    "compute_client": ComputeManagementClient,
}


# pylint: disable=too-few-public-methods, too-many-instance-attributes
# attr class doesn't need a method
@attr.s(auto_attribs=True)
class Items:
    """attr class to build resource details dictionary."""

    resource_id: Optional[str] = None
    name: Optional[str] = None
    resource_type: Optional[str] = None
    location: Optional[str] = None
    tags: Optional[Any] = None
    plan: Optional[Any] = None
    properties: Optional[Any] = None
    kind: Optional[str] = None
    managed_by: Optional[str] = None
    sku: Optional[str] = None
    identity: Optional[str] = None
    state: Any = None


@attr.s(auto_attribs=True)
class NsgItems:
    """attr class to build NSG rule dictionary."""

    rule_name: Optional[str] = None
    description: Optional[str] = None
    protocol: Optional[str] = None
    direction: Optional[str] = None
    src_ports: Optional[str] = None
    dst_ports: Optional[str] = None
    src_addrs: Optional[str] = None
    dst_addrs: Optional[str] = None
    action: Optional[str] = None


@attr.s(auto_attribs=True)
class InterfaceItems:
    """attr class to build network interface details dictionary."""

    interface_id: Optional[str] = None
    private_ip: Optional[str] = None
    private_ip_allocation: Optional[str] = None
    public_ip: Optional[str] = None
    public_ip_allocation: Optional[str] = None
    app_sec_group: Optional[List[Any]] = None
    subnet: Optional[str] = None
    subnet_nsg: Any = None
    subnet_route_table: Any = None


class AzureData:
    """Class for returning data on an Azure tenant."""

    def __init__(self, connect: bool = False, cloud: Optional[str] = None):
        """Initialize connector for Azure Python SDK."""
        self.az_cloud_config = AzureCloudConfig(cloud)
        self.connected = False
        self.credentials: Optional[AzCredentials] = None
        self.sub_client: Optional[SubscriptionClient] = None
        self.resource_client: Optional[ResourceManagementClient] = None
        self.network_client: Optional[NetworkManagementClient] = None
        self.monitoring_client: Optional[MonitorManagementClient] = None
        self.compute_client: Optional[ComputeManagementClient] = None
        self.cloud = cloud or AzureCloudConfig().cloud
        self.endpoints = get_all_endpoints(self.cloud)  # type: ignore
        if connect:
            self.connect()

    def connect(
        self,
        auth_methods: Optional[List] = None,
        tenant_id: Optional[str] = None,
        silent: bool = False,
    ):
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

        Raises
        ------
        CloudError
            If no valid credentials are found or if subscription client can't be created

        """
        auth_methods = auth_methods or self.az_cloud_config.auth_methods
        tenant_id = tenant_id or self.az_cloud_config.tenant_id
        self.credentials = az_connect(
            auth_methods=auth_methods, tenant_id=tenant_id, silent=silent
        )
        if not self.credentials:
            raise CloudError("Could not obtain credentials.")
        self._check_client("sub_client")
        if only_interactive_cred(self.credentials.modern) and not silent:
            print("Check your default browser for interactive sign-in prompt.")

        self.sub_client = SubscriptionClient(
            credential=self.credentials.modern,
            base_url=self.endpoints.resource_manager,
            credential_scopes=[self.az_cloud_config.token_uri],
        )
        if not self.sub_client:
            raise CloudError("Could not create a Subscription client.")
        self.connected = True

    def get_subscriptions(self) -> pd.DataFrame:
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
            raise MsticpyNotConnectedError(
                "You need to connect to the service before using this function.",
                help_uri=MsticpyAzureConfigError.DEF_HELP_URI,
                title="Please call connect() before continuing.",
            )

        subscription_ids = []
        display_names = []
        states = []
        try:
            sub_list = list(self.sub_client.subscriptions.list())  # type: ignore
        except AttributeError:
            self._legacy_auth("sub_client")
            sub_list = list(self.sub_client.subscriptions.list())  # type: ignore

        for item in sub_list:  # type: ignore
            subscription_ids.append(item.subscription_id)  # type: ignore
            display_names.append(item.display_name)  # type: ignore
            states.append(str(item.state))  # type: ignore

        return pd.DataFrame(
            {
                "Subscription ID": subscription_ids,
                "Display Name": display_names,
                "State": states,
            }
        )

    def get_subscription_info(self, sub_id: str) -> dict:
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
            raise MsticpyNotConnectedError(
                "You need to connect to the service before using this function.",
                help_uri=MsticpyAzureConfigError.DEF_HELP_URI,
                title="Please call connect() before continuing.",
            )
        try:
            sub = self.sub_client.subscriptions.get(sub_id)  # type: ignore
        except AttributeError:
            self._legacy_auth("sub_client")
            sub = self.sub_client.subscriptions.get(sub_id)  # type: ignore

        sub_loc = sub.subscription_policies.location_placement_id  # type: ignore
        return {
            "Subscription ID": sub.subscription_id,
            "Display Name": sub.display_name,
            "State": str(sub.state),
            "Subscription Location": sub_loc,
            "Subscription Quota": sub.subscription_policies.quota_id,  # type: ignore
            "Spending Limit": sub.subscription_policies.spending_limit,  # type: ignore
        }

    def list_sentinel_workspaces(self, sub_id: str) -> Dict[str, str]:
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

    # Get > List Aliases
    get_sentinel_workspaces = list_sentinel_workspaces

    def get_resources(  # noqa: MC0001
        self, sub_id: str, rgroup: Optional[str] = None, get_props: bool = False
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
            raise MsticpyNotConnectedError(
                "You need to connect to the service before using this function.",
                help_uri=MsticpyAzureConfigError.DEF_HELP_URI,
                title="Please call connect() before continuing.",
            )

        self._check_client("resource_client", sub_id)

        resources = []  # type: List
        if rgroup is None:
            resources.extend(iter(self.resource_client.resources.list()))  # type: ignore
        else:
            resources.extend(
                iter(
                    self.resource_client.resources.list_by_resource_group(  # type: ignore
                        rgroup
                    )
                )
            )

        # Warn users about getting full properties for each resource
        if get_props:
            print("Collecting properties for every resource may take some time...")

        resource_items = []

        # Get properties for each resource
        for resource in resources:
            if get_props:
                if resource.type == "Microsoft.Compute/virtualMachines":
                    state = self._get_compute_state(
                        resource_id=resource.id, sub_id=sub_id
                    )
                else:
                    state = None
                try:
                    props = self.resource_client.resources.get_by_id(  # type: ignore
                        resource.id, "2019-08-01"
                    ).properties

                except CloudError:
                    props = self.resource_client.resources.get_by_id(  # type: ignore
                        resource.id, self._get_api(resource.id, sub_id=sub_id)
                    ).properties
            else:
                props = resource.properties
                state = None

            # Parse relevant resource attributes into a dataframe and return it
            resource_details = attr.asdict(
                Items(  # type: ignore
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
                )
            )
            resource_items.append(resource_details)

        return pd.DataFrame(resource_items)

    def get_resource_details(  # noqa: MC0001
        self,
        sub_id: str,
        resource_id: Optional[str] = None,
        resource_details: Optional[dict] = None,
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
            raise MsticpyNotConnectedError(
                "You need to connect to the service before using this function.",
                help_uri=MsticpyAzureConfigError.DEF_HELP_URI,
                title="Please call connect() before continuing.",
            )
        self._check_client("resource_client", sub_id)

        # If a resource id is provided use get_by_id to get details
        if resource_id is not None:
            try:
                resource = self.resource_client.resources.get_by_id(  # type: ignore
                    resource_id, api_version=self._get_api(resource_id, sub_id=sub_id)
                )
            except AttributeError:
                self._legacy_auth("resource_client", sub_id)
                resource = self.resource_client.resources.get_by_id(  # type: ignore
                    resource_id, api_version=self._get_api(resource_id, sub_id=sub_id)
                )
            if resource.type == "Microsoft.Compute/virtualMachines":
                state = self._get_compute_state(resource_id=resource_id, sub_id=sub_id)
            else:
                state = None
        # If resource details are provided use get to get details
        elif resource_details is not None:
            try:
                resource = self.resource_client.resources.get(  # type: ignore
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
                resource = self.resource_client.resources.get(  # type: ignore
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
            raise ValueError("Please provide either a resource ID or resource details")

        # Parse relevent details into a dictionary to return
        resource_details = attr.asdict(
            Items(  # type: ignore
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
            )
        )

        return resource_details

    def _get_api(  # noqa: MC0001
        self,
        resource_id: Optional[str] = None,
        sub_id: Optional[str] = None,
        resource_provider: Optional[str] = None,
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
            raise MsticpyNotConnectedError(
                "You need to connect to the service before using this function.",
                help_uri=MsticpyAzureConfigError.DEF_HELP_URI,
                title="Please call connect() before continuing.",
            )

        self._check_client("resource_client", sub_id)  # type: ignore

        # Normalize elements depending on user input type
        if resource_id is not None:
            try:
                namespace = resource_id.split("/")[6]
                service = resource_id.split("/")[7]
            except IndexError as idx_err:
                raise MsticpyResourceError(
                    "Provided Resource ID isn't in the correct format.",
                    "It should look like:",
                    "/subscriptions/SUB_ID/resourceGroups/RESOURCE_GROUP/"
                    + "providers/NAMESPACE/SERVICE_NAME/RESOURCE_NAME ",
                ) from idx_err

        elif resource_provider is not None:
            try:
                namespace = resource_provider.split("/")[0]
                service = resource_provider.split("/")[1]
            except IndexError as idx_err:
                raise MsticpyResourceError(
                    "Provided Resource Provider isn't in the correct format.",
                    "It should look like: NAMESPACE/SERVICE_NAME",
                ) from idx_err
        else:
            raise ValueError(
                "Please provide an resource ID or resource provider namespace"
            )

        # Get list of API versions for the service
        try:
            provider = self.resource_client.providers.get(namespace)  # type: ignore
        except AttributeError:
            self._legacy_auth("resource_client", sub_id)  # type: ignore
            provider = self.resource_client.providers.get(namespace)  # type: ignore

        resource_types = next(
            (
                t
                for t in provider.resource_types  # type: ignore
                if t.resource_type == service
            ),
            None,
        )

        # Get first API version that isn't in preview
        if not resource_types:
            raise MsticpyResourceError("Resource provider not found")

        api_version = [
            v
            for v in resource_types.api_versions  # type: ignore
            if "preview" not in v.lower()
        ]
        if api_version is None or not api_version:
            api_ver = resource_types.api_versions[0]  # type: ignore
        else:
            api_ver = api_version[0]
        return str(api_ver)

    def get_network_details(
        self, network_id: str, sub_id: str
    ) -> Tuple[pd.DataFrame, pd.DataFrame]:
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
            raise MsticpyNotConnectedError(
                "You need to connect to the service before using this function.",
                help_uri=MsticpyAzureConfigError.DEF_HELP_URI,
                title="Please call connect() before continuing.",
            )

        self._check_client("network_client", sub_id)

        # Get interface details and parse relevant elements into a dataframe
        try:
            details = self.network_client.network_interfaces.get(  # type: ignore
                network_id.split("/")[4], network_id.split("/")[8]
            )
        except AttributeError:
            self._legacy_auth("network_client", sub_id)
            details = self.network_client.network_interfaces.get(  # type: ignore
                network_id.split("/")[4], network_id.split("/")[8]
            )

        ips = []
        for ip_addr in details.ip_configurations:  # type: ignore
            ip_details = attr.asdict(
                InterfaceItems(  # type: ignore
                    id=network_id,
                    private_ip=ip_addr.private_ip_address,
                    private_ip_allocation=str(ip_addr.private_ip_allocation_method),
                    public_ip=ip_addr.public_ip_address.ip_address
                    if ip_addr.public_ip_address
                    else None,
                    public_ip_allocation=(
                        ip_addr.public_ip_address.public_ip_allocation_method
                        if ip_addr.public_ip_address
                        else None
                    ),
                    app_sec_group=ip_addr.application_security_groups,  # type: ignore
                    subnet=ip_addr.subnet.name,  # type: ignore
                    subnet_nsg=ip_addr.subnet.network_security_group,  # type: ignore
                    subnet_route_table=ip_addr.subnet.route_table,  # type: ignore
                )
            )
            ips.append(ip_details)

        ip_df = pd.DataFrame(ips)

        nsg_df = pd.DataFrame()
        if details.network_security_group is not None:
            # Get NSG details and parse relevant elements into a dataframe
            nsg_details = self.network_client.network_security_groups.get(  # type: ignore
                details.network_security_group.id.split("/")[4],  # type: ignore
                details.network_security_group.id.split("/")[8],  # type: ignore
            )
            nsg_rules = []
            for nsg in nsg_details.default_security_rules:  # type: ignore
                rules = attr.asdict(
                    NsgItems(  # type: ignore
                        rule_name=nsg.name,
                        description=nsg.description,
                        protocol=str(nsg.protocol),
                        direction=str(nsg.direction),
                        src_ports=nsg.source_port_range,
                        dst_ports=nsg.destination_port_range,
                        src_addrs=nsg.source_address_prefix,
                        dst_addrs=nsg.destination_address_prefix,
                        action=str(nsg.access),
                    )
                )
                nsg_rules.append(rules)

            nsg_df = pd.DataFrame(nsg_rules)

        return ip_df, nsg_df

    def get_metrics(  # pylint: disable=too-many-locals
        self,
        metrics: str,
        resource_id: str,
        sub_id: str,
        sample_time: str = "hour",
        start_time: int = 30,
    ) -> Dict[str, pd.DataFrame]:
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
            raise ValueError(
                "invalid value for sample_time - specify 'hour', or 'minute'"
            )

        # Check if connection and client required are already present
        if self.connected is False:
            raise MsticpyNotConnectedError(
                "You need to connect to the service before using this function.",
                help_uri=MsticpyAzureConfigError.DEF_HELP_URI,
                title="Please call connect() before continuing.",
            )

        self._check_client("monitoring_client", sub_id)

        # Get metrics in one hour chunks for the last 30 days
        start = datetime.datetime.now().date()
        end = start - datetime.timedelta(days=start_time)

        try:
            mon_details = self.monitoring_client.metrics.list(  # type: ignore
                resource_id,
                timespan=f"{end}/{start}",
                interval=interval,  # type: ignore
                metricnames=f"{metrics}",
                aggregation="Total",
            )
        except AttributeError:
            self._legacy_auth("monitoring_client", sub_id)
            mon_details = self.monitoring_client.metrics.list(  # type: ignore
                resource_id,
                timespan=f"{end}/{start}",
                interval=interval,  # type: ignore
                metricnames=f"{metrics}",
                aggregation="Total",
            )
        results = {}
        # Create a dict of all the results returned
        for metric in mon_details.value:  # type: ignore
            times = []
            output = []
            for time in metric.timeseries:
                for data in time.data:
                    times.append(data.time_stamp)
                    output.append(data.total)
            details = pd.DataFrame({"Time": times, "Data": output})
            details.replace(np.nan, 0, inplace=True)
            results[metric.name.value] = details
        return results

    # pylint: enable=too-many-locals, too-many-arguments

    def _get_compute_state(
        self, resource_id: str, sub_id: str
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
            raise MsticpyNotConnectedError(
                "You need to connect to the service before using this function.",
                help_uri=MsticpyAzureConfigError.DEF_HELP_URI,
                title="Please call connect() before continuing.",
            )

        self._check_client("compute_client", sub_id)

        # Parse the Resource ID to extract Resource Group and Resource Name
        r_details = resource_id.split("/")
        r_group = r_details[r_details.index("resourceGroups") + 1]
        name = r_details[r_details.index("virtualMachines") + 1]

        # Get VM instance details and return them
        try:
            instance_details = self.compute_client.virtual_machines.instance_view(  # type: ignore
                r_group, name
            )
        except AttributeError:
            self._legacy_auth("compute_client", sub_id)
            instance_details = self.compute_client.virtual_machines.instance_view(  # type: ignore
                r_group, name
            )

        return instance_details  # type: ignore

    def _check_client(self, client_name: str, sub_id: Optional[str] = None):
        """
        Check required client is present, if not create it.

        Parameters
        ----------
        client_name : str
            The name of the client to be checked.
        sub_id : str, optional
            The subscription ID for the client to connect to, by default None

        """
        if getattr(self, client_name) is None:
            client = _CLIENT_MAPPING[client_name]
            if sub_id is None:
                setattr(
                    self,
                    client_name,
                    client(
                        self.credentials.modern,  # type: ignore
                        base_url=self.endpoints.resource_manager,
                        credential_scopes=[self.az_cloud_config.token_uri],
                    ),
                )
            else:
                setattr(
                    self,
                    client_name,
                    client(
                        self.credentials.modern,  # type: ignore
                        sub_id,
                        base_url=self.endpoints.resource_manager,
                        credential_scopes=[self.az_cloud_config.token_uri],
                    ),
                )

        if getattr(self, client_name) is None:
            raise CloudError("Could not create client")

    def _legacy_auth(self, client_name: str, sub_id: Optional[str] = None):
        """
        Create client with v1 authentication token.

        Parameters
        ----------
        client_name : str
            The name of the client to be checked.
        sub_id : str, optional
            The subscription ID for the client to connect to, by default None

        """
        client = _CLIENT_MAPPING[client_name]
        if sub_id is None:
            setattr(
                self,
                client_name,
                client(
                    self.credentials.legacy,  # type: ignore
                    base_url=self.endpoints.resource_manager,
                    credential_scopes=[self.az_cloud_config.token_uri],
                ),
            )
        else:
            setattr(
                self,
                client_name,
                client(
                    self.credentials.legacy,  # type: ignore
                    sub_id,
                    base_url=self.endpoints.resource_manager,
                    credential_scopes=[self.az_cloud_config.token_uri],
                ),
            )


def get_api_headers(token: str) -> Dict:
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
    tenant_id: Optional[str] = None,
    cloud: Optional[str] = None,
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
    if tenant_id:
        try:
            token = credential.modern.get_token(AzureCloudConfig().token_uri)
        except ClientAuthenticationError:
            credential = fallback_devicecode_creds(cloud=cloud)
            token = credential.modern.get_token(AzureCloudConfig().token_uri)
    else:
        try:
            token = credential.modern.get_token(
                AzureCloudConfig().token_uri, tenant_id=tenant_id
            )
        except ClientAuthenticationError:
            credential = fallback_devicecode_creds(cloud=cloud, tenant_id=tenant_id)
            token = credential.modern.get_token(
                AzureCloudConfig().token_uri, tenant_id=tenant_id
            )

    return token.token
