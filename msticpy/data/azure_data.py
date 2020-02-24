# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Uses the Azure Python SDK to collect and return details related to Azure."""
from typing import Optional, Dict, Tuple
import datetime

import attr
import pandas as pd
import numpy as np

from azure.common.credentials import ServicePrincipalCredentials
from azure.mgmt.subscription import SubscriptionClient
from azure.mgmt.resource import ResourceManagementClient
from azure.mgmt.network import NetworkManagementClient
from azure.mgmt.monitor import MonitorManagementClient
from azure.mgmt.compute import ComputeManagementClient
from azure.mgmt.compute.models import VirtualMachineInstanceView
from azure.common.exceptions import CloudError

from ..nbtools import pkg_config as config
from ..nbtools.utility import MsticpyException
from .._version import VERSION

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
@attr.s
class Items:
    """attr class to build resource details dictionary."""

    resource_id = attr.ib()
    name = attr.ib()
    resource_type = attr.ib()
    location = attr.ib()
    tags = attr.ib()
    plan = attr.ib()
    properties = attr.ib()
    kind = attr.ib()
    managed_by = attr.ib()
    sku = attr.ib()
    identity = attr.ib()
    state = attr.ib()


@attr.s
class NsgItems:
    """attr class to build NSG rule dictionary."""

    rule_name = attr.ib()
    description = attr.ib()
    protocol = attr.ib()
    direction = attr.ib()
    src_ports = attr.ib()
    dst_ports = attr.ib()
    src_addrs = attr.ib()
    dst_addrs = attr.ib()
    action = attr.ib()


@attr.s
class InterfaceItems:
    """attr class to build network interface details dictionary."""

    interface_id = attr.ib()
    private_ip = attr.ib()
    private_ip_allocation = attr.ib()
    public_ip = attr.ib()
    public_ip_allocation = attr.ib()
    app_sec_group = attr.ib()
    subnet = attr.ib()
    subnet_nsg = attr.ib()
    subnet_route_table = attr.ib()


class MsticpyAzureException(MsticpyException):
    """Exception class for AzureData."""


# pylint: enable=too-few-public-methods, too-many-instance-attributes


class AzureData:
    """Class for returning data on an Azure tenant."""

    def __init__(self, connect: bool = False):
        """Initialize connector for Azure Python SDK."""
        self.connected = False
        self.credentials: Optional[ServicePrincipalCredentials] = None
        self.sub_client: Optional[SubscriptionClient] = None
        self.resource_client: Optional[ResourceManagementClient] = None
        self.network_client: Optional[NetworkManagementClient] = None
        self.monitoring_client: Optional[MonitorManagementClient] = None
        self.compute_client: Optional[ComputeManagementClient] = None
        if connect is True:
            self.connect()

    def connect(self, client_id: str = None, tenant_id: str = None, secret: str = None):
        """Authenticate with the SDK."""
        # Use details of msticpyyaml if not provided
        if client_id is None and tenant_id is None and secret is None:
            az_cli_config = config.settings.get("AzureCLI")
            if not az_cli_config:
                raise MsticpyAzureException(
                    "No AzureCLI configuration found in configuration settings."
                )
            config_items = az_cli_config["Args"]
            client_id = config_items["clientId"]
            tenant_id = config_items["tenantId"]
            secret = config_items["clientSecret"]

        # Create credentials and connect to the subscription client to validate
        self.credentials = ServicePrincipalCredentials(
            client_id=client_id, secret=secret, tenant=tenant_id
        )
        if not self.credentials:
            raise CloudError("Could not obtain credentials.")
        self.sub_client = SubscriptionClient(self.credentials)
        if not self.sub_client:
            raise CloudError("Could not create a Subscription client.")
        self.connected = True

    def get_subscriptions(self) -> pd.DataFrame:
        """Get details of all subscriptions within the tenant."""
        if self.connected is False:
            raise MsticpyAzureException("Please connect before continuing")

        subscription_ids = []
        display_names = []
        states = []
        for item in self.sub_client.subscriptions.list():  # type: ignore
            subscription_ids.append(item.subscription_id)
            display_names.append(item.display_name)
            states.append(str(item.state))

        sub_details = pd.DataFrame(
            {
                "Subscription ID": subscription_ids,
                "Display Name": display_names,
                "State": states,
            }
        )

        return sub_details

    def get_subscription_info(self, sub_id: str) -> dict:
        """
        Get information on a specific subscription.

        Parameters
        ----------
        sub_id : str
            The ID of the subscription to return details on.

        """
        if self.connected is False:
            raise MsticpyAzureException("Please connect before continuing")

        sub = self.sub_client.subscriptions.get(sub_id)  # type: ignore
        sub_details = {
            "Subscription ID": sub.subscription_id,
            "Display Name": sub.display_name,
            "State": str(sub.state),
            "Subscription Location": sub.subscription_policies.location_placement_id,
            "Subscription Quota": sub.subscription_policies.quota_id,
            "Spending Limit": sub.subscription_policies.spending_limit,
        }

        return sub_details

    def get_resources(
        self, sub_id: str, rgroup: str = None, get_props: bool = False
    ) -> pd.DataFrame:
        """
        Return details on all resources in a subscription or Resoruce Group.

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
        resrouce_df: pd.DataFrame
            A dataframe of resource details

        """
        # Check if connection and client required are already present
        if self.connected is False:
            raise MsticpyAzureException("Please connect before continuing")

        self._check_client("resource_client", sub_id)

        if rgroup is None:
            resources = self.resource_client.resources.list()  # type: ignore
        else:
            resources = self.resource_client.resources.list_by_resource_group(  # type: ignore
                rgroup
            )  # type: ignore

        # Warn users about getting full properties for each resource
        if get_props is True:
            print("Collecting properties for every resource may take some time...")

        resource_items = []

        # Get properites for each resource
        for resource in resources:
            if get_props is True:
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
                        resource.id, self._get_api(resource.id)
                    ).properties
            else:
                props = resource.properties
                state = None

            # Parse relevent resource attributes into a dataframe and return it
            resource_details = attr.asdict(
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
                )
            )
            resource_items.append(resource_details)

        resource_df = pd.DataFrame(resource_items)

        return resource_df

    def get_resource_details(
        self, sub_id: str, resource_id: str = None, resource_details: dict = None
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
        resource_deatils: dict
            The details of the requested resource

        """
        # Check if connection and client required are already present
        if self.connected is False:
            raise MsticpyAzureException("Please connect before continuing")

        self._check_client("resource_client", sub_id)

        # If a resource id is provided use get_by_id to get details
        if resource_id is not None:
            resource = self.resource_client.resources.get_by_id(  # type: ignore
                resource_id, api_version=self._get_api(resource_id)
            )
            if resource.type == "Microsoft.Compute/virtualMachines":
                state = self._get_compute_state(resource_id=resource_id, sub_id=sub_id)
            else:
                state = None
        # If resource details are provided use get to get details
        elif resource_details is not None:
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
                    )
                ),
            )
            state = None
        else:
            raise ValueError("Please provide either a resource ID or resource details")

        # Parse relevent details into a dictionary to return
        resource_details = attr.asdict(
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
            )
        )

        return resource_details

    def _get_api(
        self, resource_id: str = None, sub_id: str = None, resource_provider: str = None
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
            The latest avaliable non-preview API version

        """
        # Check if connection and client required are already present
        if self.connected is False:
            raise MsticpyAzureException("Please connect before continuing")

        self._check_client("resource_client", sub_id)  # type: ignore

        # Normalise elements depending on user input type
        if resource_id is not None:
            try:
                namespace = resource_id.split("/")[6]
                service = resource_id.split("/")[7]
            except IndexError:
                raise MsticpyAzureException(
                    """Provided Resource ID isn't in the correct format. It should look like:
                       /subscriptions/SUB_ID/resourceGroups/RESOURCE_GROUP/providers/NAMESPACE/SERVICE_NAME/RESOURCE_NAME """  # pylint: disable=line-too-long
                )

        elif resource_provider is not None:
            try:
                namespace = resource_provider.split("/")[0]
                service = resource_provider.split("/")[1]
            except IndexError:
                raise MsticpyAzureException(
                    """Provided Resource Provider isn't in the correct format. It should look like:
                       NAMESPACE/SERVICE_NAME"""
                )
        else:
            raise ValueError(
                "Please provide an resource ID or resource provider namespace"
            )

        # Get list of API versions for the service
        provider = self.resource_client.providers.get(namespace)  # type: ignore
        resource_types = next(
            (t for t in provider.resource_types if t.resource_type == service), None
        )

        # Get first API version that isn't in preview
        if resource_types:
            api_version = [
                v for v in resource_types.api_versions if "preview" not in v.lower()
            ]
            if api_version is None or not api_version:
                api_ver = resource_types.api_versions[0]
            else:
                api_ver = api_version[0]
        else:
            raise MsticpyAzureException("Resource provider not found")

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
            raise MsticpyAzureException("Please connect before continuing")

        self._check_client("network_client", sub_id)

        # Get interface details and parse relevent elements into a dataframe
        details = self.network_client.network_interfaces.get(  # type: ignore
            network_id.split("/")[4], network_id.split("/")[8]
        )
        ips = []
        for ip in details.ip_configurations:  # pylint: disable=invalid-name
            ip_details = attr.asdict(
                InterfaceItems(
                    network_id,
                    ip.private_ip_address,
                    ip.private_ip_allocation_method,
                    ip.public_ip_address.ip_address,
                    ip.public_ip_address.public_ip_allocation_method,
                    ip.application_security_groups,
                    ip.subnet.name,
                    ip.subnet.network_security_group,
                    ip.subnet.route_table,
                )
            )
            ips.append(ip_details)

        ip_df = pd.DataFrame(ips)

        # Get NSG details and parse relevent elements into a dataframe
        nsg_details = self.network_client.network_security_groups.get(  # type: ignore
            details.network_security_group.id.split("/")[4],
            details.network_security_group.id.split("/")[8],
        )
        nsg_rules = []
        for nsg in nsg_details.default_security_rules:
            rules = attr.asdict(
                NsgItems(
                    nsg.name,
                    nsg.description,
                    nsg.protocol,
                    nsg.direction,
                    nsg.source_port_range,
                    nsg.destination_port_range,
                    nsg.source_address_prefix,
                    nsg.destination_address_prefix,
                    nsg.access,
                )
            )
            nsg_rules.append(rules)

        nsg_df = pd.DataFrame(nsg_rules)

        return ip_df, nsg_df

    # pylint: disable=too-many-locals, too-many-arguments
    def get_metrics(
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
            raise MsticpyAzureException(
                "Select how often you want to sample data - 'hour', or 'minute'"
            )

        # Check if connection and client required are already present
        if self.connected is False:
            raise MsticpyAzureException("Please connect before continuing")

        self._check_client("monitoring_client", sub_id)

        # Get metrics in one hour chunks for the last 30 days
        start = datetime.datetime.now().date()
        end = start - datetime.timedelta(days=start_time)

        mon_details = self.monitoring_client.metrics.list(  # type: ignore
            resource_id,
            timespan=f"{end}/{start}",
            interval=interval,
            metricnames=f"{metrics}",
            aggregation="Total",
        )

        results = {}
        # Create a dict of all the results returned
        for metric in mon_details.value:
            times = []
            output = []
            for time in metric.timeseries:
                for data in time.data:
                    times.append(data.time_stamp)
                    output.append(data.total)
            details = pd.DataFrame({"Time": times, "Data": output})
            details.replace(np.nan, 0, inplace=True)
            results.update({metric.name.value: details})

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
            raise MsticpyAzureException("Please connect before continuing")

        self._check_client("compute_client", sub_id)

        # Parse the Resource ID to extract Resource Group and Resource Name
        r_details = resource_id.split("/")
        r_group = r_details[r_details.index("resourceGroups") + 1]
        name = r_details[r_details.index("virtualMachines") + 1]

        # Get VM instance details and return them
        instance_details = self.compute_client.virtual_machines.instance_view(  # type: ignore
            r_group, name
        )
        return instance_details

    def _check_client(self, client_name: str, sub_id: str):
        """
        Check required client is present, if not create it.

        Parameters
        ----------
        self:
        client_name:
            The name of the client to be checked.
        sub_id:
            The subscription ID for the client to connect to.

        """
        client = _CLIENT_MAPPING[client_name]
        if getattr(self, client_name) is None:
            setattr(self, client_name, client(self.credentials, sub_id))
            if getattr(self, client_name) is None:
                raise CloudError("Could not create client")
