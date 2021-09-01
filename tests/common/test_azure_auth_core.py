# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Module docstring."""
import pytest
import pytest_check as check
from msrestazure import azure_cloud
from msticpy.common.azure_auth_core import AzureCloudConfig, default_auth_methods

from ..unit_test_lib import custom_mp_config, get_test_data_path

__author__ = "Ian Hellen"

# pylint: disable=redefined-outer-name


@pytest.fixture(scope="module")
def mp_config_file():
    """Fixture_docstring."""
    return get_test_data_path().joinpath("msticpyconfig.yaml")


def test_default_auth_methods(mp_config_file):
    """Test default auth methods function."""
    with custom_mp_config(mp_config_file):
        check.is_in("env", default_auth_methods())
        check.is_in("msi", default_auth_methods())
        check.is_in("cli", default_auth_methods())
        check.is_in("interactive", default_auth_methods())


def test_azure_cloud_config(mp_config_file):
    """Test the Azure cloud config."""
    with custom_mp_config(mp_config_file):
        az_config = AzureCloudConfig()
    check.equal(az_config.cloud, "global")
    check.is_in("env", az_config.auth_methods)
    check.is_in("msi", az_config.auth_methods)
    check.is_in("cli", az_config.auth_methods)
    check.is_in("interactive", az_config.auth_methods)
    glob_rm_uri = azure_cloud.AZURE_PUBLIC_CLOUD.endpoints.resource_manager
    check.equal(f"{glob_rm_uri}.default", az_config.token_uri)
