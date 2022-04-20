# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
from collections import namedtuple
from unittest.mock import patch

import pandas as pd
import pytest
from azure.identity import InteractiveBrowserCredential

from msticpy.auth.azure_auth_core import (
    AzCredentials,
    ChainedTokenCredential,
    CredentialWrapper,
)
from msticpy.data.drivers.resource_graph_driver import ResourceGraphDriver

# pylint: disable=redefined-outer-name


class _MockSub:
    """Mock of Subscription Class."""

    def list(self):
        """Provide list of subscriptions."""
        SubDets = namedtuple("SubDetails", ["subscription_id", "name"])
        return [SubDets("123", "sub1")]


class _MockSubClient:
    """Mock of SubscriptionClient."""

    def __init__(self):
        self.subscriptions = _MockSub()


class _MockResourceGraphClient:
    """Mock of ResourceGraphClient."""

    def resources(self, query):
        """Mock response to query."""
        del query
        return _MockResponse()


class _MockResponse:
    """Mocked response object."""

    def __init__(self):
        self.result_truncated = False
        self.data = [
            {
                "id": "/subscriptions/123/resourceGroups/RG/providers/Microsoft.Compute/virtualMachines/vm1",
                "name": "vm1",
                "type": "microsoft.compute/virtualmachines",
                "tenantId": "123",
                "kind": "",
                "location": "westeurope",
                "resourceGroup": "rg",
                "subscriptionId": "123",
            },
            {
                "id": "/subscriptions/123/resourceGroups/RG/providers/Microsoft.Compute/virtualMachines/vm2",
                "name": "vm2",
                "type": "microsoft.compute/virtualmachines",
                "tenantId": "123",
                "kind": "",
                "location": "westeurope",
                "resourceGroup": "rg",
                "subscriptionId": "123",
            },
            {
                "id": "/subscriptions/123/resourceGroups/RG/providers/Microsoft.Compute/virtualMachines/vm3",
                "name": "vm3",
                "type": "microsoft.compute/virtualmachines",
                "tenantId": "123",
                "kind": "",
                "location": "westeurope",
                "resourceGroup": "rg",
                "subscriptionId": "123",
            },
        ]


_AZ_CREDENTIALS = AzCredentials(
    legacy=CredentialWrapper("credential"),
    modern=ChainedTokenCredential(InteractiveBrowserCredential()),
)


@pytest.fixture(scope="module")
@patch(ResourceGraphDriver.__module__ + ".SubscriptionClient")
@patch(ResourceGraphDriver.__module__ + ".az_connect")
@patch(ResourceGraphDriver.__module__ + ".ResourceGraphClient")
def rgd(mock_res_client, mock_creds, mock_sub_client):
    """Pytest fixture to create ResourceGraphDriver for other tests."""
    mock_sub_client.return_value = _MockSubClient()
    mock_creds.return_value = _AZ_CREDENTIALS
    mock_res_client.return_value = _MockResourceGraphClient()
    rgd = ResourceGraphDriver()
    rgd.connect()
    return rgd


def test_connect(rgd):
    """Test connection protocol has worked."""
    assert rgd.connected is True


def test_query(rgd):
    """Test query calling returns data in expected format."""
    data = rgd.query("test")
    assert isinstance(data, pd.DataFrame)
    assert data.iloc[0]["name"] == "vm1"
