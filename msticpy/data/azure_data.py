# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Uses the Azure Python SDK to collect and return details related to Azure."""
from abc import ABC
from typing import Optional
import datetime

import attr
import pandas as pd
import numpy as np

from azure.common.credentials import ServicePrincipalCredentials
from azure.mgmt.subscription import SubscriptionClient
from azure.mgmt.resource import ResourceManagementClient
from azure.mgmt.network import NetworkManagementClient
from azure.mgmt.monitor import MonitorManagementClient
from azure.common.exceptions import CloudError

from ..nbtools import pkg_config as config
from ..nbtools.utility import MsticpyException
from .._version import VERSION

__version__ = VERSION
__author__ = "Pete Bryan"


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


@attr.s
class NsgItems:
    """attr class to build NSG rule dictionary. """

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
    """attr class to build network interface details dictionary. """

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


class AzureData(ABC):
    """Class for returning data on an Azure tenant."""

    def __init__(self, connect: bool = False):
        """Initialize connector for Azure Python SDK."""
        self.connected = False
        self.credentials: Optional[ServicePrincipalCredentials] = None
        self.sub_client: Optional[SubscriptionClient] = None
        self.resource_client: Optional[ResourceManagementClient] = None
        self.network_client: Optional[NetworkManagementClient] = None
        self.monitoring_client: Optional[MonitorManagementClient] = None
        if connect is True:
            self.connect()

    def connect(self, client_id: str = None, tenant_id: str = None, secret: str = None):
        """Authenticate with the SDK."""
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
            "Subscription Location Limits": sub.subscription_policies.location_placement_id,
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
        if self.connected is False:
            raise MsticpyAzureException("Please connect before continuing")

        self.resource_client = ResourceManagementClient(self.credentials, sub_id)
        if not self.resource_client:
            raise CloudError("Could not create a ResourceManagementClient.")
        if rgroup is None:
            resources = self.resource_client.resources.list()
        else:
            resources = self.resource_client.resources.list_by_resource_group(rgroup)

        if get_props is True:
            print("Collecting properties for every resource may take some time...")

        resource_items = []

        for resource in resources:
            if get_props is True:
                try:
                    props = self.resource_client.resources.get_by_id(
                        resource.id, "2019-08-01"
                    ).properties
                except CloudError:
                    props = self.resource_client.resources.get_by_id(
                        resource.id, self.get_api(resource.id)
                    ).properties
            else:
                props = resource.properties

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
                )
            )
            resource_items.append(resource_details)

        resource_df = pd.DataFrame(resource_items)

        return resource_df

    def get_resource_details(
        self, resource_id: str = None, resource_details: dict = None, sub_id: str = None
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
        sub_id: str, optional
            The ID of the subscription to get resources from

        Returns
        -------
        resource_deatils: dict
            The details of the requested resource

        """
        if self.connected is False:
            raise MsticpyAzureException("Please connect before continuing")

        if self.resource_client is None:
            self.resource_client = ResourceManagementClient(self.credentials, sub_id)
            if not self.resource_client:
                raise CloudError("Could not create a ResourceManagementClient.")

        if resource_id is not None:
            resource = self.resource_client.resources.get_by_id(
                resource_id, self.get_api(resource_id)
            )
        elif resource_details is not None:
            resource = self.resource_client.resources.get(
                resource_details["resource_group_name"],
                resource_details["resource_provider_namespace"],
                resource_details["parent_resource_path"],
                resource_details["resource_type"],
                resource_details["resource_name"],
                self.get_api(
                    resource_provider=(
                        resource_details["resource_provider_namespace"]
                        + "/"
                        + resource_details["resource_type"]
                    )
                ),
            )
        else:
            raise ValueError("Please provide either a resource ID or resource details")

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
            )
        )

        return resource_details

    def get_api(
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
        if self.connected is False:
            raise MsticpyAzureException("Please connect before continuing")

        if self.resource_client is None:
            self.resource_client = ResourceManagementClient(self.credentials, sub_id)
            if not self.resource_client:
                raise CloudError("Could not create a ResourceManagementClient.")

        if resource_id is not None:
            namespace = resource_id.split("/")[6]
            service = resource_id.split("/")[7]
        elif resource_provider is not None:
            namespace = resource_provider.split("/")[0]
            service = resource_provider.split("/")[1]
        else:
            raise ValueError(
                "Please provide an resource ID or resource provider namespace"
            )

        provider = self.resource_client.providers.get(namespace)
        resource_types = next(
            (t for t in provider.resource_types if t.resource_type == service), None
        )
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

    def get_network_details(self, network_id: str, sub_id: str) -> dict:
        """
        Return details related to an Azure network interface.

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
        if self.connected is False:
            raise MsticpyAzureException("Please connect before continuing")

        if self.network_client is None:
            self.network_client = NetworkManagementClient(self.credentials, sub_id)
            if not self.network_client:
                raise CloudError("Could not create a NetworkManagementClient.")

        details = self.network_client.network_interfaces.get(
            network_id.split("/")[4], network_id.split("/")[8]
        )
        ips = []
        for ip in details.ip_configurations:
            ip_details = attr.asdict(InterfaceItems(network_id, ip.private_ip_address, ip.private_ip_allocation_method, ip.public_ip_address.ip_address, ip.public_ip_address.public_ip_allocation_method, ip.application_security_groups, ip.subnet.name, ip.subnet.network_security_group, ip.subnet.route_table))
            ips.append(ip_details)

        ip_df = pd.DataFrame(ips)
        
        nsg_details = self.network_client.network_security_groups.get(
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

    def get_cpu_metrics(self, resource_id: str, sub_id: str) -> pd.DataFrame:
        """
        Return CPU monitoring details for an Azure resource

        Parameters
        ----------
        resource_id: str
            The ID of the Azure resource to return details on
        sub_id: str
            The subscription ID that the network interface is part of

        Returns
        -------
        details: pd.DataFrame
            A DataFrame of CPU metrics

        """
        if self.connected is False:
            raise MsticpyAzureException("Please connect before continuing")

        if self.monitoring_client is None:
            self.monitoring_client = MonitorManagementClient(self.credentials, sub_id)
            if not self.monitoring_client:
                raise CloudError("Could not create a MonitorManagementClient.")

        start = datetime.datetime.now().date()
        end = start - datetime.timedelta(days=30)
        self.monitoring_client = MonitorManagementClient(self.credentials, sub_id)
        mon_details = self.monitoring_client.metrics.list(
            resource_id,
            timespan=f"{end}/{start}",
            interval="PT1H",
            metricnames="Percentage CPU",
            aggregation="Total",
        )
        times = []
        datas = []
        for mon_detail in mon_details.value:
            for time in mon_detail.timeseries:
                for data in time.data:
                    times.append(data.time_stamp)
                    datas.append(data.total)

        details = pd.DataFrame({"Time": times, "CPU Usage": datas})
        details.replace(np.nan, 0, inplace=True)
        return details

    def get_network_metrics(self, resource_id: str, sub_id: str) -> pd.DataFrame:
        """
        Return network monitoring details for an Azure resource

        Parameters
        ----------
        resource_id: str
            The ID of the Azure resource to return details on
        sub_id: str
            The subscription ID that the network interface is part of

        Returns
        -------
        details: dict
            A DataFrame of network metrics

        """
        if self.connected is False:
            raise MsticpyAzureException("Please connect before continuing")

        if self.monitoring_client is None:
            self.monitoring_client = MonitorManagementClient(self.credentials, sub_id)
            if not self.monitoring_client:
                raise CloudError("Could not create a MonitorManagementClient.")

        start = datetime.datetime.now().date()
        end = start - datetime.timedelta(days=30)

        net_mon = self.monitoring_client.metrics.list(
            resource_id,
            timespan=f"{end}/{start}",
            interval="PT1H",
            metricnames="Network In,Network Out",
            aggregation="Total",
        )
        # Extract the Network timeseries data
        times = []
        network_in = []
        network_out = []
        for net in net_mon.value:
            if net.name.value == "Network In":
                for time in net.timeseries:
                    for data in time.data:
                        times.append(data.time_stamp)
                        network_in.append(data.total)
            elif net.name.value == "Network Out":
                for time in net.timeseries:
                    for data in time.data:
                        network_out.append(data.total)

        # Build dataframe and transform it into a grouped set
        details = pd.DataFrame(
            {"Time": times, "Network In": network_in, "Network Out": network_out}
        )
        in_details = details[["Time", "Network In"]]
        in_details["Value"] = "Network In"
        in_details.rename(columns={"Network In": "Data"}, inplace=True)
        out_details = details[["Time", "Network Out"]]
        out_details["Value"] = "Network Out"
        out_details.rename(columns={"Network Out": "Data"}, inplace=True)
        details = pd.concat([in_details, out_details])
        details.replace(np.nan, 0, inplace=True)

        return details
