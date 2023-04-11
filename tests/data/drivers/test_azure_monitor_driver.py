# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""KQL driver query test class."""
import json
import re
from pathlib import Path
from unittest.mock import patch

import pandas as pd
import pytest
import pytest_check as check
import respx

from msticpy.auth.azure_auth_core import AzCredentials
from msticpy.common.exceptions import (
    MsticpyDataQueryError,
    MsticpyKqlConnectionError,
    MsticpyNoDataSourceError,
    MsticpyNotConnectedError,
)
from msticpy.data.core.data_providers import QueryProvider
from msticpy.data.core.query_defns import DataEnvironment
from msticpy.data.core.query_source import QuerySource
from msticpy.data.drivers.azure_monitor_driver import AzureMonitorDriver

from ...unit_test_lib import custom_mp_config

# pylint: disable=protected-access, unused-argument, redefined-outer-name


@pytest.fixture(scope="session")
def read_schema():
    """Read schema file."""
    with open(
        "tests/testdata/azmon/az_mon_schema.json", "r", encoding="utf-8"
    ) as schema_file:
        return json.loads(schema_file.read())


@pytest.fixture(scope="session")
def read_query():
    """Read query file."""
    with open(
        "tests/testdata/azmon/az_mon_query.json", "r", encoding="utf-8"
    ) as query_file:
        return json.loads(query_file.read())


def get_test_ids(test_params):
    """Return test ids for parameterized tests."""
    return [f"{next(iter(param), 'No-params')}" for param, _ in test_params]


_TEST_INIT_PARAMS = (
    ({"connection_str": "test"}, ("_def_connection_str", "test")),
    (
        {"data_environment": DataEnvironment.MSSentinel},
        ("environment", DataEnvironment.MSSentinel),
    ),
)


@pytest.mark.parametrize(
    "params, expected", _TEST_INIT_PARAMS, ids=get_test_ids(_TEST_INIT_PARAMS)
)
def test_azmon_driver_init(params, expected):
    """Test KqlDriverAZMon init."""
    azmon_driver = AzureMonitorDriver(**params)
    check.is_false(azmon_driver.connected)
    check.is_none(azmon_driver._query_client)
    check.is_none(azmon_driver._ws_config)
    check.is_none(azmon_driver._connect_auth_types)
    check.is_true(azmon_driver._ua_policy._user_agent.startswith("MSTICPy"))
    check.equal(getattr(azmon_driver, expected[0]), expected[1])


class Token:
    """Mock token class."""

    token = "test_token"


class Credentials:
    """Mock credentials class."""

    def get_token(self, *args, **kwargs):
        """Mock get_token."""
        return Token()


_WS_IDS = [
    "a927809c-8142-43e1-96b3-4ad87cfe95a3",
    "a927809c-8142-43e1-96b3-4ad87cfe95a4",
]

_TEST_CONNECT_PARAMS = (
    (
        {"auth_types": ["cli", "environment", "msi"]},
        [("_connect_auth_types", ["cli", "environment", "msi"])],
    ),
    ({"auth_types": "cli"}, [("_connect_auth_types", ["cli"])]),
    ({"tenant_id": "test"}, [("_az_tenant_id", "test")]),
    ({"connection_str": "test"}, [(None, MsticpyKqlConnectionError)]),
    (
        {"mp_az_auth": ["cli", "environment", "msi"]},
        [("_connect_auth_types", ["cli", "environment", "msi"])],
    ),
    ({"mp_az_auth": True}, [("_connect_auth_types", None)]),
    (
        {"workspace": "MyTestWS"},
        [("_workspace_id", "a927809c-8142-43e1-96b3-4ad87cfe95a3")],
    ),
    ({}, [("_workspace_id", "52b1ab41-869e-4138-9e40-2a4457f09bf3")]),
    (
        {"workspaces": ["MyTestWS", "MyTestWS2"]},
        [
            ("_workspace_ids", _WS_IDS),
            ("_az_tenant_id", "69d28fd7-42a5-48bc-a619-af56397b9f28"),
        ],
    ),
    (
        {"workspace_ids": _WS_IDS, "tenant_id": "69d28fd7-42a5-48bc-a619-af56397b9f28"},
        [
            ("_workspace_ids", _WS_IDS),
            ("_az_tenant_id", "69d28fd7-42a5-48bc-a619-af56397b9f28"),
        ],
    ),
    ({"workspace_ids": _WS_IDS}, [(None, MsticpyKqlConnectionError)]),
)


@respx.mock
@pytest.mark.parametrize(
    "params, expected", _TEST_CONNECT_PARAMS, ids=get_test_ids(_TEST_CONNECT_PARAMS)
)
@patch("msticpy.data.drivers.azure_monitor_driver.az_connect")
def test_azmon_driver_connect(az_connect, params, expected, read_schema):
    """Test KqlDriverAZMon connect."""
    az_connect.return_value = AzCredentials(legacy=None, modern=Credentials())

    respx.get(re.compile(r"https://management\.azure\.com/")).respond(
        status_code=200, json=read_schema
    )

    with custom_mp_config(Path("tests/msticpyconfig-test.yaml")):
        azmon_driver = AzureMonitorDriver()
        exception_expected = next(iter(expected))[1]
        if isinstance(exception_expected, type) and issubclass(
            exception_expected, Exception
        ):
            with pytest.raises(exception_expected):
                azmon_driver.connect(**params)
        else:
            azmon_driver.connect(**params)

            check.is_true(azmon_driver.connected)
            check.is_not_none(azmon_driver._query_client)
            # check.is_not_none(azmon_driver._ws_config)

            # check.is_none(azmon_driver._connect_auth_types)
            for exp in expected:
                if isinstance(exp[1], list):
                    for item in exp[1]:
                        check.is_in(item, getattr(azmon_driver, exp[0]))
                else:
                    check.equal(getattr(azmon_driver, exp[0]), exp[1])


@respx.mock
@patch("msticpy.data.drivers.azure_monitor_driver.az_connect")
def test_get_schema(az_connect, read_schema):
    """Test KqlDriverAZMon get_schema."""
    az_connect.return_value = AzCredentials(legacy=None, modern=Credentials())

    respx.get(re.compile(r"https://management\.azure\.com/")).respond(
        status_code=200, json=read_schema
    )
    azmon_driver = AzureMonitorDriver()
    azmon_driver.connect()
    check.is_not_none(azmon_driver.schema)
    check.equal(len(azmon_driver.schema), 17)
    check.is_in("AppServiceAntivirusScanAuditLogs", azmon_driver.schema)
    check.is_in("AzureActivity", azmon_driver.schema)
    for col in ("TenantId", "SourceSystem", "CallerIpAddress", "OperationId"):
        check.is_in(col, azmon_driver.schema["AzureActivity"])
        check.is_in(azmon_driver.schema["AzureActivity"][col], ("string", "guid"))

    respx.get(re.compile(r"https://management\.azure\.com/")).respond(
        status_code=404, content=b"not found"
    )
    azmon_driver = AzureMonitorDriver()
    azmon_driver.connect()
    check.is_false(azmon_driver.schema)


def test_query_not_connected():
    """Test KqlDriverAZMon query when not connected."""
    with pytest.raises(MsticpyNotConnectedError):
        azmon_driver = AzureMonitorDriver()
        azmon_driver.query("AzureActivity")


QUERY_SOURCE = {
    "args": {
        "query": " let start = datetime({start}); let end = datetime({end}); "
        "{table} {event_filter} {query_project} | where "
        "{subscription_filter} | where Computer {host_op} "
        '"{host_name}" | where TimeGenerated >= start | where '
        "TimeGenerated <= end {add_query_items}"
    },
    "description": "Retrieves list of processes on a host",
    "metadata": {
        "pivot": {"direct_func_entities": ["Host"], "short_name": "processes"}
    },
    "parameters": {
        "host_name": {"description": "Name of host", "type": "str"},
        "host_op": {
            "default": "has",
            "description": "The hostname match operator",
            "type": "str",
        },
        "table": {
            "default": "SecurityEvent",
            "description": "The table to query",
            "type": "str",
        },
    },
}


@respx.mock
@patch("msticpy.data.drivers.azure_monitor_driver.az_connect")
def test_query_unknown_table(az_connect, read_schema):
    """Test KqlDriverAZMon query when not connected."""
    az_connect.return_value = AzCredentials(legacy=None, modern=Credentials())

    respx.get(re.compile(r"https://management\.azure\.com/")).respond(
        status_code=200, json=read_schema
    )
    with pytest.raises(MsticpyNoDataSourceError):
        azmon_driver = AzureMonitorDriver()
        azmon_driver.connect()
        query_source = QuerySource(
            name="my_test_query", source=QUERY_SOURCE, defaults={}, metadata={}
        )
        azmon_driver.query(query="UnknownTable", query_source=query_source)


@respx.mock
@patch("msticpy.data.drivers.azure_monitor_driver.az_connect")
def test_load_provider(az_connect, read_schema):
    """Test KqlDriverAZMon query when not connected."""
    az_connect.return_value = AzCredentials(legacy=None, modern=Credentials())

    respx.get(re.compile(r"https://management\.azure\.com/")).respond(
        status_code=200, json=read_schema
    )

    query_prov = QueryProvider("MSSentinel_New")
    query_prov.connect()
