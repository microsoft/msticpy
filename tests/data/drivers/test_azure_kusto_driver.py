# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Azure Kusto driver query test class."""
import json
import re
from datetime import timedelta
from pathlib import Path
from typing import Callable, List, NamedTuple, Optional
from unittest.mock import patch

import pandas as pd
import pytest
import pytest_check as check
import respx
import yaml

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
from msticpy.data.drivers import azure_kusto_driver
from msticpy.data.drivers.azure_kusto_driver import AzureKustoDriver

from ...unit_test_lib import get_test_data_path
from .test_azure_monitor_driver import QUERY_SOURCE

# from azure.kusto.data.request import KustoConnectionStringBuilder

# pylint: disable=protected-access, unused-argument, redefined-outer-name


_KUSTO_CONFIG_SETTINGS_TEXT = """
DataProviders:
    Kusto-MDE:
        Args:
            Cluster: https://test.kusto.windows.net
            IntegratedAuth: true
    Kusto-MSTICTI:
        Args:
            Cluster: https://msticti.kusto.windows.net
            IntegratedAuth: true
    Kusto-Help:
        Args:
            Cluster: https://help.kusto.windows.net
            IntegratedAuth: true

KustoClusters:
    ClusterDefaults:
        Args:
            TenantId: 72f988bf-86f1-41af-91ab-2d7cd011db47
    MDE:
        Args:
            Cluster: https://test.kusto.windows.net
            IntegratedAuth: true
            ClientSecret:
                EnvironmentVar: COMPUTERNAME
    MSTICTI:
        Args:
            Cluster: https://msticti.kusto.windows.net
            IntegratedAuth: true
    Help:
        Args:
            Cluster: https://help.kusto.windows.net
            IntegratedAuth: true

"""
_KUSTO_CONFIG_SETTINGS = yaml.safe_load(_KUSTO_CONFIG_SETTINGS_TEXT)
_KUSTO_LEGACY_CONFIG_SETTINGS = {
    "DataProviders": _KUSTO_CONFIG_SETTINGS["DataProviders"]
}
_KUSTO_NEW_CONFIG_SETTINGS = {"KustoClusters": _KUSTO_CONFIG_SETTINGS["KustoClusters"]}


def get_test_df():
    return pd.read_csv(get_test_data_path().joinpath("host_logons.csv"), index_col=0)


def test_init():
    # Test that __init__ sets the current_connection property correctly
    driver = AzureKustoDriver(connection_str="https://test.kusto.windows.net")
    assert driver.current_connection == "https://test.kusto.windows.net"

    # Test that __init__ sets the _connection_props property correctly
    driver = AzureKustoDriver(timeout=300)
    assert driver._connection_props._options["servertimeout"] == timedelta(seconds=300)

    # Test that __init__ raises a TypeError when timeout is not an integer
    with pytest.raises(TypeError):
        driver = AzureKustoDriver(timeout="invalid_timeout_value")


class MPConfig:
    """Mock MPConfig."""

    def __init__(self, config=None):
        """Initialize mock MPConfig."""
        self._config = config or {}

    def get(self, key, default=None):
        """Return a Kusto config dictionary."""
        return self._config.get(key, default)


_TEST_CONFIG = (
    (_KUSTO_CONFIG_SETTINGS, ["MDE", "MSTICTI", "Help"], 3, "combined"),
    (_KUSTO_LEGACY_CONFIG_SETTINGS, ["MDE", "MSTICTI", "Help"], 3, "legacy"),
    (_KUSTO_NEW_CONFIG_SETTINGS, ["MDE", "MSTICTI", "Help"], 3, "new"),
    ({}, [], 0, "empty"),
)


@pytest.mark.parametrize("conf, keys, items, name", _TEST_CONFIG)
def test_get_kusto_config(conf, keys, items, name, monkeypatch):
    """Test creation of Kusto config entries."""
    config = MPConfig(conf)
    monkeypatch.setattr(azure_kusto_driver, "get_config", config.get)

    driver = AzureKustoDriver(connection_str="https://test.kusto.windows.net")
    for index in ("url", "name", "id"):
        check.equal(len(driver._kusto_settings[index]), items)
    for key in keys:
        check.is_in(key, driver._kusto_settings["id"])
        check.is_instance(
            driver._kusto_settings["id"][key], azure_kusto_driver.KustoConfig
        )

    for name, k_config in driver._kusto_settings["id"].items():
        check.is_in("Cluster", k_config.args)
        if "IntegratedAuth" in k_config.args:
            check.equal(k_config.args["IntegratedAuth"], k_config.integrated_auth)
        if "TenantId" in k_config.args:
            check.equal(k_config.args["TenantId"], k_config.tenant_id)
        check.is_not_none(k_config.path)
        check.is_not_none(k_config.cluster)
        check.is_not_none(k_config.name)


class Token:
    """Mock token class."""

    token = "test_token"


class Credentials:
    """Mock credentials class."""

    def get_token(self, *args, **kwargs):
        """Mock get_token."""
        return Token()


def az_connect(*args, **kwargs):
    """Mock az_connect."""
    return AzCredentials(legacy=[args, kwargs], modern=Credentials())


class ConnectTest(NamedTuple):
    """Test configuration for Kusto connection tests."""

    init_args: dict
    connect_args: dict
    name: str
    tests: List[Callable]


_TEST_CONNECT_ARGS = (
    ConnectTest(
        name="connection_str",
        init_args={"connection_str": "https://test1.kusto.windows.net"},
        connect_args={"connection_str": "https://test.kusto.windows.net"},
        tests=[
            lambda driver: driver.client._kusto_cluster
            == "https://test.kusto.windows.net"
        ],
    ),
    ConnectTest(
        name="cluster-no-init",
        init_args={},
        connect_args={"cluster": "https://test.kusto.windows.net"},
        tests=[
            lambda driver: driver.client._kusto_cluster
            == "https://test.kusto.windows.net",
            lambda driver: driver._current_config.cluster
            == "https://test.kusto.windows.net",
        ],
    ),
    ConnectTest(
        name="cluster-id-no-init",
        init_args={
            "cluster": "https://test.kusto.windows.net",
        },
        connect_args={
            "cluster": "Help",
            "auth_types": ["device_code"],
            "tenant_id": "test_tenant_id",
        },
        tests=[
            lambda driver: driver.client._kusto_cluster
            == "https://help.kusto.windows.net",
            lambda driver: driver._current_config.cluster
            == "https://help.kusto.windows.net",
            lambda driver: driver._az_tenant_id == "test_tenant_id",
            lambda driver: driver._az_auth_types == ["device_code"],
        ],
    ),
    ConnectTest(
        name="cluster-name-no-init",
        init_args={
            "cluster": "https://test.kusto.windows.net",
        },
        connect_args={
            "cluster": "help",
            "auth_types": ["device_code"],
            "tenant_id": "test_tenant_id",
            "database": "test_db",
        },
        tests=[
            lambda driver: driver.client._kusto_cluster
            == "https://help.kusto.windows.net",
            lambda driver: driver._current_config.cluster
            == "https://help.kusto.windows.net",
            lambda driver: driver._az_tenant_id == "test_tenant_id",
            lambda driver: driver._az_auth_types == ["device_code"],
            lambda driver: driver._default_database == "test_db",
        ],
    ),
    ConnectTest(
        name="mp_az_auth-bool",
        init_args={
            "cluster": "https://test.kusto.windows.net",
        },
        connect_args={
            "cluster": "Help",
            "mp_az_auth": True,
        },
        tests=[
            lambda driver: driver._az_auth_types is None,
        ],
    ),
    ConnectTest(
        name="mp_az_auth-str",
        init_args={
            "cluster": "https://test.kusto.windows.net",
        },
        connect_args={
            "cluster": "Help",
            "mp_az_auth": "msi",
        },
        tests=[
            lambda driver: driver._az_auth_types == ["msi"],
        ],
    ),
    ConnectTest(
        name="mp_az_auth-list",
        init_args={
            "cluster": "https://test.kusto.windows.net",
        },
        connect_args={
            "cluster": "Help",
            "mp_az_auth": ["msi", "cli"],
        },
        tests=[
            lambda driver: driver._az_auth_types == ["msi", "cli"],
        ],
    ),
)


def unit_test_ids(tests):
    """Test Kusto connection."""
    return [test.name for test in tests]


@pytest.mark.parametrize(
    "test_config", _TEST_CONNECT_ARGS, ids=unit_test_ids(_TEST_CONNECT_ARGS)
)
def test_kusto_connect(test_config, monkeypatch):
    config = MPConfig(_KUSTO_NEW_CONFIG_SETTINGS)
    monkeypatch.setattr(azure_kusto_driver, "get_config", config.get)
    monkeypatch.setattr(azure_kusto_driver, "az_connect", az_connect)
    driver = AzureKustoDriver(**(test_config.init_args))
    driver.connect(**(test_config.connect_args))
    for test in test_config.tests:
        check.is_true(test(driver))


class KustoResponseDataSet:
    """Mock Kusto response dataset."""

    def __init__(self, data, query_status=None):
        """Initialize dataset."""
        self._data = data
        self._query_status = query_status or self._df_status()

    @property
    def primary_results(self):
        """Mock primary_results."""
        return [self._data]

    @property
    def tables_names(self):
        """Mock table_names."""
        return ["primary", "QueryCompletionInformation"]

    @property
    def tables(self):
        """Mock tables."""
        return [self._data, self._query_status]

    def to_dataframe(self):
        """Mock to_dataframe."""
        return self._data

    def _df_status(self):
        """Mock _df_status."""
        return pd.DataFrame(
            data=[["TestStatus", '{"payload": "TestPayload"}']],
            columns=["EventTypeName", "Payload"],
        )


_TEST_DF = get_test_df()


class KustoClient:
    """Mock Kusto client."""

    def __init__(self, *args, **kwargs):
        """Initialize client."""
        self.args = args
        self.kwargs = kwargs
        self._data = _TEST_DF

    def execute(self, query, database=None, *args, **kwargs):
        """Mock execute."""
        if query is None:
            raise ValueError("Query cannot be None")
        if database is None:
            raise ValueError("Database cannot be None")
        if query == "test":
            return KustoResponseDataSet(self._data)

        raise ValueError(f"Invalid query {query}")


def create_query_source(data_families=None, database=None, cluster=None):
    """Create custom query source for testing."""
    metadata = {
        "data_environments": ["Kusto"],
        "cluster": cluster or "https://default.kusto.windows.net",
    }
    if data_families:
        metadata["data_families"] = data_families
    if database:
        metadata["database"] = database
    return QuerySource(
        name="test",
        source=QUERY_SOURCE,
        defaults={},
        metadata=metadata,
    )


class QueryTest(NamedTuple):
    """Test configuration for Kusto query tests."""

    connect_args: dict
    query_args: dict
    name: str
    tests: List[Callable]
    exception: Optional[type] = None


_TEST_QUERY_ARGS = (
    QueryTest(
        name="query-no-db",
        connect_args={"connection_str": "https://test.kusto.windows.net"},
        query_args={"query": "test"},
        tests=[],
        exception=ValueError,
    ),
    QueryTest(
        name="simple-query",
        connect_args={"cluster": "https://test.kusto.windows.net"},
        query_args={"query": "test", "database": "test_db"},
        tests=[
            lambda result: isinstance(result, pd.DataFrame),
        ],
    ),
    QueryTest(
        name="query-get-db-from-source",
        connect_args={"cluster": "help"},
        query_args={
            "query": "test",
            "cluster": "https://param_cluster.kusto.windows.net",
            "query_source": {"database": "test_db"},
        },
        tests=[
            lambda result: isinstance(result, pd.DataFrame),
        ],
    ),
    QueryTest(
        name="query-get-db-family-from-source",
        connect_args={"cluster": "help"},
        query_args={
            "query": "test",
            "cluster": "https://param_cluster.kusto.windows.net",
            "query_source": {"data_families": ["MyKusto.param_test_db"]},
        },
        tests=[
            lambda result: isinstance(result, pd.DataFrame),
        ],
    ),
    QueryTest(
        name="query-get-db-and-cluster-from-source",
        connect_args={"cluster": "help"},
        query_args={
            "query": "test",
            "query_source": {
                "database": "test_db",
                "cluster": "https://param_cluster.kusto.windows.net",
            },
        },
        tests=[
            lambda result: isinstance(result, pd.DataFrame),
        ],
    ),
)


@pytest.mark.parametrize(
    "query_config", _TEST_QUERY_ARGS, ids=unit_test_ids(_TEST_QUERY_ARGS)
)
def test_kusto_query(query_config, monkeypatch):
    config = MPConfig(_KUSTO_NEW_CONFIG_SETTINGS)

    monkeypatch.setattr(azure_kusto_driver, "get_config", config.get)
    monkeypatch.setattr(azure_kusto_driver, "az_connect", az_connect)
    monkeypatch.setattr(
        azure_kusto_driver, "dataframe_from_result_table", lambda resp: resp
    )
    monkeypatch.setattr(azure_kusto_driver, "KustoClient", KustoClient)

    test_config = _TEST_CONNECT_ARGS[2]
    driver = AzureKustoDriver(**(test_config.init_args))
    driver.connect(**(test_config.connect_args))

    if query_config.exception:
        with pytest.raises(query_config.exception):
            driver.query(**(query_config.query_args))
    else:
        query_args = query_config.query_args
        if "query_source" in query_args:
            query_args["query_source"] = create_query_source(
                **query_args["query_source"]
            )
        result = driver.query(**query_args)
        for test in query_config.tests:
            check.is_true(test(result))
        check.is_instance(result, pd.DataFrame)


# def test_kusto_get_table_names(monkeypatch):
