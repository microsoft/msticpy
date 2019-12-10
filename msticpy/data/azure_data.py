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

from ..nbtools import pkg_config as config
from .._version import VERSION

__version__ = VERSION
__author__ = "Pete Bryan"


class AzureData(ABC):
    """Class for returning data on an Azure tenant."""

    def __init__(self):
        """Initialize connector for Azure Python SDK."""
        self.connected = False
        self.credentials = None
        self.sub_client = None

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
