# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
import os
from unittest.mock import patch

from pytest import raises

from ..msticpy.data.azure_data import AzureData
from ..msticpy.common.utility import MsticpyException


def test_azure_init():
    az = AzureData()
    assert type(az) == AzureData


def test_azure_connect_exp():
    with raises(MsticpyException):
        az = AzureData()
        az.connect()


@patch(AzureData.__module__ + ".SubscriptionClient")
@patch(AzureData.__module__ + ".ServicePrincipalCredentials")
def test_azure_connect(mock_sub_client, mock_creds):
    mock_sub_client.return_value = "Client"
    mock_creds.return_value = "Creds"
    az = AzureData()
    az.connect(client_id="XXX", tenant_id="XXX", secret="XXX")
    assert az.connected == True
