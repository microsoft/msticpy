# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Uses the Azure Python SDK to collect and return details related to Azure."""
from abc import ABC

import pandas as pd

from azure.common.credentials import ServicePrincipalCredentials
from azure.mgmt.subscription import SubscriptionClient
from azure.mgmt.resource import ResourceManagementClient
from azure.common.exceptions import CloudError

from ..nbtools import pkg_config as config
from .._version import VERSION

__version__ = VERSION
__author__ = "Pete Bryan"


class AzureData(ABC):
    """Class for returning data on an Azure tenant."""

    def __init__(self, connect: bool = False):
        """Initialize connector for Azure Python SDK."""
        self.connected = False
        self.credentials = None
        self.sub_client = None
        self.resource_client = None
        if connect is True:
            self.connect()

    def connect(self, client_id: str = None, tenant_id: str = None, secret: str = None):
        """Authenticate with the SDK."""
        if client_id is None and tenant_id is None and secret is None:
            config_items = config.settings.get("AzureCLI")["Args"]
            client_id = config_items["clientId"]
            tenant_id = config_items["tenantId"]
            secret = config_items["clientSecret"]

        self.credentials = ServicePrincipalCredentials(
            client_id=client_id, secret=secret, tenant=tenant_id
        )
        self.sub_client = SubscriptionClient(self.credentials)
        self.connected = True

    def get_subscriptions(self) -> pd.DataFrame:
        """Get details of all subscription within the tenant."""
        if self.connected is False:
            raise Exception("Please connect before continuing")

        subscription_ids = []
        display_names = []
        states = []
        for item in self.sub_client.subscriptions.list():
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
            raise Exception("Please connect before continuing")

        sub = self.sub_client.subscriptions.get(sub_id)
        sub_details = {
            "Subscription ID": sub.subscription_id,
            "Display Name": sub.display_name,
            "State": str(sub.state),
        }

        return sub_details

    # pylint: disable=too-many-locals

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
            raise Exception("Please connect before continuing")

        self.resource_client = ResourceManagementClient(self.credentials, sub_id)
        if rgroup is None:
            resources = self.resource_client.resources.list()
        else:
            resources = self.resource_client.resources.list_by_resource_group(rgroup)

        if get_props is True:
            print("Collecting properties for every resource may take some time...")

        ids = []
        names = []
        types = []
        locations = []
        tags = []
        plans = []
        properties = []
        kinds = []
        managed_by = []
        identities = []
        skus = []

        for resource in resources:
            ids.append(resource.id)
            names.append(resource.name)
            types.append(resource.type)
            locations.append(resource.location)
            tags.append(resource.tags)
            plans.append(resource.plan)
            if get_props is True:
                try:
                    properties.append(
                        self.resource_client.resources.get_by_id(
                            resource.id, "2019-08-01"
                        ).properties
                    )
                except CloudError:
                    properties.append(
                        self.resource_client.resources.get_by_id(
                            resource.id, self.get_api(resource.id)
                        ).properties
                    )
            else:
                properties.append(resource.properties)
            kinds.append(resource.kind)
            managed_by.append(resource.managed_by)
            identities.append(resource.identity)
            skus.append(resource.sku)

        resource_df = pd.DataFrame(
            {
                "ID": ids,
                "Name": names,
                "Type": types,
                "Location": locations,
                "Tags": tags,
                "Plan": plans,
                "Properties": properties,
                "Kind": kinds,
                "Managed By": managed_by,
                "Identity": identities,
                "SKU": skus,
            }
        )
        return resource_df

    # pylint: enable=too-many-locals

    def get_resource_details(
        self, resource_id: str = None, resource_details: dict = None, sub_id: str = None
    ) -> dict:
        """
        Return the details of a specific Azure resource.

        Parameters
        ----------
        resource_id: str (Optional)
            The ID of the resource to get details on
        resource_details: dict(Optional)
            If ID is unknown provide the following details:
                -resource_group_name
                -resource_provider_namespace
                -resource_type
                -resource_name
                -parent_resource_path

        Returns
        -------
        resource_deatils: dict
            The details of the requested resource

        """
        if self.connected is False:
            raise Exception("Please connect before continuing")

        if self.resource_client is None:
            self.resource_client = ResourceManagementClient(self.credentials, sub_id)

        if resource_id is None and resource_details is None:
            raise Exception("Please provide either a resource ID or resource details")

        if resource_id is not None:
            resource = self.resource_client.resources.get_by_id(
                resource_id, self.get_api(resource_id)
            )
        else:
            resource = self.resource_client.resources.get(
                resource_details["resource_group_name"],
                resource_details["resource_provider_namespace"],
                resource_details["parent_resource_path"],
                resource_details["resource_type"],
                resource_details["resource_name"],
                self.get_api(resource_id),
            )

        resource_details = {
            "ID": resource.id,
            "Name": resource.name,
            "Type": resource.type,
            "Location": resource.location,
            "Tags": resource.tags,
            "Plan": resource.plan,
            "Properties": resource.properties,
            "Kind": resource.kind,
            "Managed By": resource.managed_by,
            "SKU": resource.sku,
            "Identity": resource.identity,
        }

        return resource_details

    def get_api(self, resource_id: str, sub_id: str = None) -> str:
        """
        Return the latest avaliable API version for the resource.

        Parameters
        ----------
        resource_id: str
            The id of the resources to get an API version for

        Returns
        -------
        api_ver: str
            The latest avaliable non-preview API version

        """
        if self.connected is False:
            raise Exception("Please connect before continuing")

        if self.resource_client is None:
            self.resource_client = ResourceManagementClient(self.credentials, sub_id)

        provider = self.resource_client.providers.get(resource_id.split("/")[6])
        resource_types = next(
            (
                t
                for t in provider.resource_types
                if t.resource_type == resource_id.split("/")[7]
            ),
            None,
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
            raise Exception("Resource provider not found")

        return str(api_ver)
