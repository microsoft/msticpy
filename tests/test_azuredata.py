# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
from collections import namedtuple
from pathlib import Path
from unittest.mock import patch

import pytest
from msticpy.common import pkg_config
from msticpy.common.provider_settings import get_provider_settings
from msticpy.data.azure_data import AzureData

from .unit_test_lib import custom_mp_config, get_test_data_path

_TEST_DATA = get_test_data_path()


def test_azure_init():
    az = AzureData()
    assert type(az) == AzureData


@pytest.mark.skip
def test_azure_connect_exp():
    with pytest.raises(AttributeError):
        az = AzureData()
        az.connect()


@patch(AzureData.__module__ + ".SubscriptionClient")
@patch(AzureData.__module__ + ".az_connect")
def test_azure_connect(mock_creds, mock_sub_client):
    AzCredentials = namedtuple("AzCredentials", ["legacy", "modern"])
    mock_sub_client.return_value = "Client"
    mock_creds.return_value = AzCredentials("cred", "cred")
    az = AzureData()
    az.connect()
    assert az.connected is True


def test_get_config():
    test_config1 = Path(_TEST_DATA).joinpath(pkg_config._CONFIG_FILE)
    with custom_mp_config(test_config1):
        data_provs = get_provider_settings(config_section="DataProviders")
        az_cli_config = data_provs.get("AzureCLI")

        assert bool(az_cli_config)
        config_items = az_cli_config.args
        assert bool(config_items)

        assert bool(config_items["clientId"])
        assert bool(config_items["tenantId"])
        assert bool(config_items["clientSecret"])
