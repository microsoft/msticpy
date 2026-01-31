# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Unit tests for AzureSearchDriver."""

import json
import re
from unittest.mock import patch

import pandas as pd
import pytest
import pytest_check as check
import respx

from msticpy.auth.azure_auth_core import AzCredentials
from msticpy.common.exceptions import MsticpyDataQueryError, MsticpyKqlConnectionError
from msticpy.data.drivers.azure_search_driver import AzureSearchDriver
from msticpy.data.drivers.driver_base import DriverProps

from ...unit_test_lib import custom_mp_config, get_test_data_path


@pytest.fixture(scope="module")
def read_schema():
    """Read a mock schema file for testing."""
    # You can reuse the same schema file used by test_azure_monitor_driver
    # or create your own endpoint mock responses.
    schema_path = get_test_data_path().joinpath("azmondata/az_mon_schema.json")
    with open(schema_path, "r", encoding="utf-8") as schema_file:
        return json.loads(schema_file.read())


@pytest.fixture(scope="module")
def mock_credentials():
    """Return mock credentials object."""

    class MockToken:
        token = "test_token"

    class MockCredentials:
        def get_token(self, *args, **kwargs):
            return MockToken()

    return MockCredentials()


def test_azure_search_driver_init():
    """Test AzureSearchDriver initialization."""
    driver = AzureSearchDriver()
    check.is_false(driver.connected, "Driver should not be connected on init")
    check.is_none(driver._auth_header, "Auth header not set initially")
    # Verify the EFFECTIVE_ENV is set to MSSentinelSearch
    effective_env = driver.get_driver_property(DriverProps.EFFECTIVE_ENV)
    check.equal(effective_env, "MSSentinelSearch", "EFFECTIVE_ENV should be MSSentinelSearch")


@respx.mock
@patch("msticpy.data.drivers.azure_search_driver.az_connect")
def test_azure_search_driver_connect(mock_az_connect, mock_credentials, read_schema):
    """Test connection via AzureSearchDriver."""
    # Prepare mock credentials
    mock_az_connect.return_value = AzCredentials(legacy=None, modern=mock_credentials)

    # Mock calls to retrieve workspace schema or other info
    respx.get(re.compile(r"https://management\.azure\.com/.*")).respond(
        status_code=200, json=read_schema
    )
    respx.get(re.compile(r"https://api\.loganalytics\.io.*")).respond(
        status_code=200, json=read_schema
    )

    # Connect driver
    driver = AzureSearchDriver(debug=True)
    with custom_mp_config(
        get_test_data_path().parent.joinpath("msticpyconfig-test.yaml")
    ):
        driver.connect(workspace="MyTestWS")

    check.is_true(driver.connected, "Driver should be marked connected after connect()")
    check.is_not_none(driver._auth_header, "Auth header should be set after connect()")


def test_azure_search_driver_query_not_connected():
    """Test that calling query without connect raises an error."""
    driver = AzureSearchDriver()
    with pytest.raises(MsticpyKqlConnectionError):
        driver.query_with_results("MyTable | take 10")


@respx.mock
@patch("msticpy.data.drivers.azure_search_driver.az_connect")
def test_azure_search_driver_query_fail_time_params(
    mock_az_connect, mock_credentials, read_schema
):
    """Test a successful query."""
    mock_az_connect.return_value = AzCredentials(legacy=None, modern=mock_credentials)
    # Prepare mock credentials
    mock_az_connect.return_value = AzCredentials(legacy=None, modern=mock_credentials)

    # Mock calls to retrieve workspace schema or other info
    respx.get(re.compile(r"https://management\.azure\.com/.*")).respond(
        status_code=200, json=read_schema
    )
    respx.get(re.compile(r"https://api\.loganalytics\.io/\/.default")).respond(
        status_code=200, json=read_schema
    )

    # Mock the /search endpoint
    respx.post(
        re.compile(r"https://api\.loganalytics\.io/v1/workspaces/.*/search")
    ).respond(
        status_code=200,
        json={
            "tables": [
                {
                    "name": "Table_0",
                    "columns": [
                        {"name": "TimeGenerated", "type": "datetime"},
                        {"name": "Message", "type": "string"},
                    ],
                    "rows": [
                        ["2025-02-05T00:00:00Z", "Test Log 1"],
                        ["2025-02-05T01:00:00Z", "Test Log 2"],
                    ],
                }
            ]
        },
    )

    driver = AzureSearchDriver(debug=True)
    driver.connect(workspace="MyTestWS")

    with pytest.raises(MsticpyDataQueryError):
        driver.query_with_results("SomeTable | take 10")
    with pytest.raises(MsticpyDataQueryError):
        driver.query_with_results("SomeTable | take 10", start="2025-02-05T00:00:00Z")
    with pytest.raises(MsticpyDataQueryError):
        driver.query_with_results("SomeTable | take 10", end="2025-02-05T01:00:00Z")


@respx.mock
@patch("msticpy.data.drivers.azure_search_driver.az_connect")
def test_azure_search_driver_query(mock_az_connect, mock_credentials, read_schema):
    """Test a successful query."""
    mock_az_connect.return_value = AzCredentials(legacy=None, modern=mock_credentials)
    # Prepare mock credentials
    mock_az_connect.return_value = AzCredentials(legacy=None, modern=mock_credentials)

    # Mock calls to retrieve workspace schema or other info
    respx.get(re.compile(r"https://management\.azure\.com/.*")).respond(
        status_code=200, json=read_schema
    )
    respx.get(re.compile(r"https://api\.loganalytics\.io/\/.default")).respond(
        status_code=200, json=read_schema
    )

    # Mock the /search endpoint
    respx.post(
        re.compile(r"https://api\.loganalytics\.io/v1/workspaces/.*/search")
    ).respond(
        status_code=200,
        json={
            "tables": [
                {
                    "name": "Table_0",
                    "columns": [
                        {"name": "TimeGenerated", "type": "datetime"},
                        {"name": "Message", "type": "string"},
                    ],
                    "rows": [
                        ["2025-02-05T00:00:00Z", "Test Log 1"],
                        ["2025-02-05T01:00:00Z", "Test Log 2"],
                    ],
                }
            ]
        },
    )

    driver = AzureSearchDriver(debug=True)
    driver.connect(workspace="MyTestWS")

    time_span = dict(start="2025-02-05T00:00:00Z", end="2025-02-05T01:00:00Z")
    df, status = driver.query_with_results("SomeTable | take 10", time_span=time_span)
    check.is_instance(df, pd.DataFrame, "Should get a DataFrame back")
    check.equal(len(df), 2, "Expected two rows returned")
    check.equal(status.get("status"), "success", "Status should indicate success")


@respx.mock
@patch("msticpy.data.drivers.azure_search_driver.az_connect")
def test_azure_search_driver_query_http_error(mock_az_connect, mock_credentials):
    """Test query with a 400 or 500 error response."""
    mock_az_connect.return_value = AzCredentials(legacy=None, modern=mock_credentials)

    # Mock a failed request to the /search endpoint
    respx.post(
        re.compile(r"https://api\.loganalytics\.io/v1/workspaces/.*/search")
    ).respond(
        status_code=400,
        text="Bad Request",
    )

    driver = AzureSearchDriver(debug=True)
    driver.connect(workspace="MyTestWS")

    time_span = dict(start="2025-02-05T00:00:00Z", end="2025-02-05T01:00:00Z")
    with pytest.raises(MsticpyKqlConnectionError):
        driver.query_with_results("Invalid query", time_span=time_span)


def test_azure_search_driver_no_workspace_ids():
    """Test that we raise an error if no workspace is configured."""
    driver = AzureSearchDriver()
    driver._connected = True  # Fake connecting, but without workspace
    time_span = dict(start="2025-02-05T00:00:00Z", end="2025-02-05T01:00:00Z")
    with pytest.raises(MsticpyKqlConnectionError):
        driver.query_with_results(query="SomeTable | take 10", time_span=time_span)
