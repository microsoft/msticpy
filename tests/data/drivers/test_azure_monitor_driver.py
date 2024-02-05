# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""KQL driver query test class."""
import json
import pickle
import re
from datetime import datetime, timedelta, timezone
from unittest.mock import patch

import pandas as pd
import pytest
import pytest_check as check
import respx
from azure.core.exceptions import HttpResponseError

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
from msticpy.data.drivers import azure_monitor_driver
from msticpy.data.drivers.azure_monitor_driver import AzureMonitorDriver

from ...unit_test_lib import custom_mp_config, get_test_data_path

# pylint: disable=protected-access, unused-argument, redefined-outer-name


@pytest.fixture(scope="module")
def read_schema():
    """Read schema file."""
    with open(
        get_test_data_path().joinpath("azmondata/az_mon_schema.json"),
        "r",
        encoding="utf-8",
    ) as schema_file:
        file_text = schema_file.read()
        print("read test schema length", len(file_text))
        return json.loads(file_text)


@pytest.fixture(scope="module")
def read_query_response():
    """Read query file."""
    with open(
        get_test_data_path().joinpath("/azmondata/query_response.pkl"), "rb"
    ) as query_file:
        return pickle.loads(query_file.read())


def get_test_ids(test_params):
    """Return test ids for parameterized tests."""
    return [f"{next(iter(param), 'No-params')}" for param, _ in test_params]


_TEST_INIT_PARAMS = (
    ({"connection_str": "test"}, ("_def_connection_str", "test")),
    (
        {"data_environment": DataEnvironment.MSSentinel_New},
        ("effective_environment", DataEnvironment.MSSentinel.name),
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

_VALID_CONN_STR = (
    f"loganalytics://tenant='{_WS_IDS[0]}';workspace='{_WS_IDS[1]}'"
    f";alias='wksp';clientid='{_WS_IDS[0]}';client_secret='{_WS_IDS[1]}'"
)

_TEST_CONNECT_PARAMS = (
    (
        {"auth_types": ["cli", "environment", "msi"]},
        [("_connect_auth_types", ["cli", "environment", "msi"])],
    ),
    ({"auth_types": "cli"}, [("_connect_auth_types", ["cli"])]),
    ({"tenant_id": "test"}, [("_az_tenant_id", "test")]),
    ({"connection_str": _VALID_CONN_STR}, [("_workspace_id", _WS_IDS[1])]),
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

    respx.get(re.compile(r"https://management\.azure\.com/.*")).respond(
        status_code=200, json=read_schema
    )
    respx.get(re.compile(r"https://api\.loganalytics\.io.*")).respond(
        status_code=200, json=read_schema
    )

    with custom_mp_config(
        get_test_data_path().parent.joinpath("msticpyconfig-test.yaml")
    ):
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
def test_get_schema(az_connect, read_schema, monkeypatch):
    """Test KqlDriverAZMon get_schema."""
    az_connect.return_value = AzCredentials(legacy=None, modern=Credentials())

    respx.get(re.compile(r"https://management\.azure\.com/.*")).respond(
        status_code=200, json=read_schema
    )
    respx.get(re.compile(r"https://api\.loganalytics\.io.*")).respond(
        status_code=200, json=read_schema
    )
    monkeypatch.setattr(azure_monitor_driver, "LogsQueryClient", LogsQueryClient)
    with custom_mp_config(
        get_test_data_path().parent.joinpath("msticpyconfig-test.yaml")
    ):
        azmon_driver = AzureMonitorDriver(debug=True)
        azmon_driver.connect(workspace="MyTestWS")
    assert isinstance(azmon_driver._query_client, LogsQueryClient)
    check.is_not_none(azmon_driver.schema)
    check.equal(len(azmon_driver.schema), 17)
    check.is_in("AppServiceAntivirusScanAuditLogs", azmon_driver.schema)
    check.is_in("AzureActivity", azmon_driver.schema)
    for col in ("TenantId", "SourceSystem", "CallerIpAddress", "OperationId"):
        check.is_in(col, azmon_driver.schema["AzureActivity"])
        check.is_in(azmon_driver.schema["AzureActivity"][col], ("string", "guid"))

    respx.get(re.compile(r"https://management\.azure\.com/.*")).respond(
        status_code=404, content=b"not found"
    )
    with custom_mp_config(
        get_test_data_path().parent.joinpath("msticpyconfig-test.yaml")
    ):
        azmon_driver = AzureMonitorDriver(debug=True)
        azmon_driver.connect(workspace="MyTestWS")
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
def test_query_unknown_table(az_connect, read_schema, monkeypatch):
    """Test KqlDriverAZMon query when not connected."""
    az_connect.return_value = AzCredentials(legacy=None, modern=Credentials())

    respx.get(re.compile(r"https://management\.azure\.com/.*")).respond(
        status_code=200, json=read_schema
    )
    respx.get(re.compile(r"https://api\.loganalytics\.io.*")).respond(
        status_code=200, json=read_schema
    )
    monkeypatch.setattr(azure_monitor_driver, "LogsQueryClient", LogsQueryClient)
    with pytest.raises(MsticpyNoDataSourceError):
        with custom_mp_config(
            get_test_data_path().parent.joinpath("msticpyconfig-test.yaml")
        ):
            azmon_driver = AzureMonitorDriver(debug=True)
            azmon_driver.connect(workspace="MyTestWS")
        assert azmon_driver.schema is not None
        assert isinstance(azmon_driver._query_client, LogsQueryClient)
        query_source = QuerySource(
            name="my_test_query", source=QUERY_SOURCE, defaults={}, metadata={}
        )
        azmon_driver.query(query="UnknownTable", query_source=query_source)


@respx.mock
@patch("msticpy.data.drivers.azure_monitor_driver.az_connect")
def test_load_provider(az_connect, read_schema, monkeypatch):
    """Test KqlDriverAZMon query when not connected."""
    az_connect.return_value = AzCredentials(legacy=None, modern=Credentials())

    respx.get(re.compile(r"https://management\.azure\.com/.*")).respond(
        status_code=200, json=read_schema
    )
    respx.get(re.compile(r"https://api\.loganalytics\.io.*")).respond(
        status_code=200, json=read_schema
    )

    monkeypatch.setattr(azure_monitor_driver, "LogsQueryClient", LogsQueryClient)

    with custom_mp_config(
        get_test_data_path().parent.joinpath("msticpyconfig-test.yaml")
    ):
        query_prov = QueryProvider("MSSentinel_New", debug=True)
        query_prov.connect(workspace="MyTestWS")
    assert isinstance(query_prov._query_provider._query_client, LogsQueryClient)

    check.greater(len(query_prov.list_queries()), 100)
    check.equal(len(query_prov.schema), 17)


class LogsQueryClient:
    """Mock LogsQueryClient class."""

    with open(
        get_test_data_path().joinpath("azmondata/query_response.pkl"), "rb"
    ) as query_file:
        _data = pickle.loads(query_file.read())

    def __init__(self, *args, **kwargs):
        """Mock LogsQueryClient class."""
        self.args = args
        self.kwargs = kwargs
        self.query_workspace_args = {}

    def query_workspace(self, query, timespan=None, **kwargs):
        """Mock query_workspace method."""
        self.query_workspace_args = {"query": query, "timespan": timespan, **kwargs}

        if query == "HttpResponseError":
            raise HttpResponseError(
                message="LA response error",
            )
        if query == "Exception":
            raise ValueError("Unknown exception")
        return self._data


@respx.mock
@patch("msticpy.data.drivers.azure_monitor_driver.az_connect")
def test_queries(az_connect, read_schema, monkeypatch):
    """Test KqlDriverAZMon queries."""
    az_connect.return_value = AzCredentials(legacy=None, modern=Credentials())
    respx.get(re.compile(r"https://management\.azure\.com/.*")).respond(
        status_code=200, json=read_schema
    )
    respx.get(re.compile(r"https://api\.loganalytics\.io.*")).respond(
        status_code=200, json=read_schema
    )
    monkeypatch.setattr(azure_monitor_driver, "LogsQueryClient", LogsQueryClient)

    with custom_mp_config(
        get_test_data_path().parent.joinpath("msticpyconfig-test.yaml")
    ):
        query_prov = QueryProvider("MSSentinel_New", debug=True)
        query_prov.connect(workspace="MyTestWS")
    assert isinstance(query_prov._query_provider._query_client, LogsQueryClient)
    query = "Testtable | take 10"
    results = query_prov.exec_query(query)

    check.is_in("credential", query_prov._query_provider._query_client.kwargs)
    check.is_in("endpoint", query_prov._query_provider._query_client.kwargs)
    check.is_in("proxies", query_prov._query_provider._query_client.kwargs)

    query_ws_args = query_prov._query_provider._query_client.query_workspace_args
    check.equal(query_ws_args["query"], query)
    check.is_not_none(query_ws_args["workspace_id"])
    check.equal(query_ws_args["server_timeout"], 300)
    check.is_none(query_ws_args["additional_workspaces"])
    check.is_instance(results, pd.DataFrame)
    check.equal(len(results), 3)

    # fail because not in schema
    with pytest.raises(MsticpyNoDataSourceError):
        query_prov.Azure.get_vmcomputer_for_ip(
            ip_address="192.1.2.3",
            start=datetime.now(tz=timezone.utc) - timedelta(1),
            end=datetime.now(tz=timezone.utc),
        )
    # should succeed
    results = query_prov.Azure.list_azure_activity_for_ip(
        ip_address_list=["192.1.2.3"],
        start=datetime.now(tz=timezone.utc) - timedelta(1),
        end=datetime.now(tz=timezone.utc),
    )
    check.is_instance(results, pd.DataFrame)
    check.equal(len(results), 3)

    # Fail due to service errors
    with pytest.raises(MsticpyDataQueryError):
        query_prov.exec_query("HttpResponseError")

    with pytest.raises(MsticpyDataQueryError):
        query_prov.exec_query("Exception")


@patch("msticpy.data.drivers.azure_monitor_driver.az_connect")
def test_query_multiple_workspaces(az_connect, monkeypatch):
    """Test KqlDriverAZMon query when not connected."""
    az_connect.return_value = AzCredentials(legacy=None, modern=Credentials())

    monkeypatch.setattr(azure_monitor_driver, "LogsQueryClient", LogsQueryClient)

    query_prov = QueryProvider("MSSentinel_New", debug=True)
    with pytest.raises(MsticpyKqlConnectionError):
        query_prov.connect(
            workspace_ids=[
                "a927809c-8142-43e1-96b3-4ad87cfe95a3",
                "a927809c-8142-43e1-96b3-4ad87cfe95a4",
            ]
        )

    query_prov.connect(
        tenant_id="72f988bf-86f1-41af-91ab-2d7cd011db49",
        workspace_ids=[
            "a927809c-8142-43e1-96b3-4ad87cfe95a3",
            "a927809c-8142-43e1-96b3-4ad87cfe95a4",
        ],
    )
    query = "Testtable | take 10"
    results = query_prov.exec_query(query)
    check.is_instance(results, pd.DataFrame)
    check.equal(len(results), 3)

    with custom_mp_config(
        get_test_data_path().parent.joinpath("msticpyconfig-test.yaml")
    ):
        query_prov.connect(
            tenant_id="72f988bf-86f1-41af-91ab-2d7cd011db49",
            workspaces=["MyTestWS", "MyTestWS2"],
        )
    query = "Testtable | take 10"
    results = query_prov.exec_query(query)
    check.is_instance(results, pd.DataFrame)
    check.equal(len(results), 3)
