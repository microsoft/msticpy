# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Azure Sentinel unit tests."""
import re
from unittest.mock import patch

import pandas as pd
import pytest
import respx
from azure.core.exceptions import ClientAuthenticationError
from msticpy.data.azure import AzureData
from msticpy.data.azure import MicrosoftSentinel

# pylint: disable=redefined-outer-name

_RESOURCES = pd.DataFrame(
    {
        "resource_type": [
            "Microsoft.OperationsManagement/solutions",
            "Microsoft.OperationsManagement/solutions",
            "Microsoft.Insights/components",
        ],
        "name": ["SecurityInsightsTest1", "Test2", "Test3"],
        "resource_id": ["123", "456", "789"],
    }
)
_RESOURCE_DETAILS = {"properties": {"workspaceResourceId": "ABC"}}


def test_azuresent_init():
    """Test class initalization."""
    azs = MicrosoftSentinel(sub_id="123", res_grp="RG", ws_name="WSName")
    assert isinstance(azs, MicrosoftSentinel)
    azs = MicrosoftSentinel(
        res_id="subscriptions/123/resourceGroups/RG/providers/Microsoft.OperationalInsights/workspaces/WSNAME"
    )
    assert isinstance(azs, MicrosoftSentinel)


def test_azuresent_connect_exp():
    """Test connect failure."""
    with pytest.raises(ClientAuthenticationError):
        azs = MicrosoftSentinel(
            res_id="subscriptions/123/resourceGroups/RG/providers/Microsoft.OperationalInsights/workspaces/WSNAME"
        )
        azs.connect(auth_methods=["env"])


@pytest.fixture(scope="module")
@patch(MicrosoftSentinel.__module__ + ".MicrosoftSentinel.connect")
def azs_loader(mock_creds):
    """Generate MicrosoftSentinel for testing."""
    mock_creds.return_value = None
    azs = MicrosoftSentinel(sub_id="123", res_grp="RG", ws_name="WSName")
    azs.connect()
    azs.token = "123"
    return azs


@patch(AzureData.__module__ + ".AzureData.get_resources")
@patch(AzureData.__module__ + ".AzureData.get_resource_details")
def test_azuresent_workspaces(mock_res_dets, mock_res, azs_loader):
    """Test Sentinel workspaces feature."""
    mock_res.return_value = _RESOURCES
    mock_res_dets.return_value = _RESOURCE_DETAILS
    workspaces = azs_loader.get_sentinel_workspaces(sub_id="123")
    assert isinstance(workspaces, dict)
    assert workspaces["ABC"] == "ABC"
