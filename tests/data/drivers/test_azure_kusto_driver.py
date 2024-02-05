# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Azure Kusto driver query test class."""
from datetime import timedelta
from itertools import product
from typing import Callable, List, NamedTuple, Optional

import pandas as pd
import pytest
import pytest_check as check
import yaml
from azure.kusto.data.exceptions import KustoApiError, KustoServiceError

from msticpy.auth.azure_auth_core import AzCredentials
from msticpy.common import proxy_settings
from msticpy.common.exceptions import (
    MsticpyDataQueryError,
    MsticpyNotConnectedError,
    MsticpyParameterError,
)
from msticpy.data.core.data_providers import QueryProvider
from msticpy.data.core.query_defns import DataEnvironment
from msticpy.data.core.query_source import QuerySource
from msticpy.data.drivers import azure_kusto_driver
from msticpy.data.drivers.azure_kusto_driver import AzureKustoDriver

from ...unit_test_lib import custom_get_config, get_test_data_path
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
        ClusterGroups: [Group1]
        Args:
            Cluster: https://test.kusto.windows.net
            ClientSecret:
                EnvironmentVar: COMPUTERNAME
    MSTICTI:
        ClusterGroups: [Group1]
        Args:
            Cluster: https://msticti.kusto.windows.net
    Help:
        Args:
            Cluster: https://help.kusto.windows.net
            IntegratedAuth: true
    G3Cluster:
        ClusterGroups: [Group3]
        Args:
            Cluster: https://g3.kusto.windows.net
            ClientId: random_client_id
            ClientSecret: [PLACEHOLDER]
            Database: G3DB
    G2Cluster:
        ClusterGroups: [Group2]
        Args:
            Cluster: https://g2.kusto.windows.net

"""
_KUSTO_CONFIG_SETTINGS = yaml.safe_load(_KUSTO_CONFIG_SETTINGS_TEXT)
_KUSTO_LEGACY_CONFIG_SETTINGS = {
    "DataProviders": _KUSTO_CONFIG_SETTINGS["DataProviders"]
}
_KUSTO_NEW_CONFIG_SETTINGS = {"KustoClusters": _KUSTO_CONFIG_SETTINGS["KustoClusters"]}


def get_test_df():
    return pd.read_csv(get_test_data_path().joinpath("host_logons.csv"), index_col=0)


def test_init():
    """Test initialization of AzureKustoDriver."""
    driver = AzureKustoDriver(
        connection_str="cluster='https://test.kusto.windows.net', db='Security'"
    )
    assert (
        driver.current_connection
        == "cluster='https://test.kusto.windows.net', db='Security'"
    )

    # Test that __init__ sets the _connection_props property correctly
    driver = AzureKustoDriver(timeout=300)
    assert driver._def_timeout == 300

    driver = AzureKustoDriver(proxies={"https": "https://test.proxy.com"})
    assert driver._def_proxies == {"https": "https://test.proxy.com"}


class MPConfig:
    """Mock MPConfig."""

    def __init__(self, config=None):
        """Initialize mock MPConfig."""
        self._config = config or {}

    def get(self, key, default=None):
        """Return a Kusto config dictionary."""
        return self._config.get(key, default)


_TEST_CONFIG = (
    (_KUSTO_CONFIG_SETTINGS, ["MDE", "MSTICTI", "Help"], 5, "combined"),
    (_KUSTO_LEGACY_CONFIG_SETTINGS, ["MDE", "MSTICTI", "Help"], 3, "legacy"),
    (_KUSTO_NEW_CONFIG_SETTINGS, ["MDE", "MSTICTI", "Help"], 5, "new"),
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
    for key in [key.casefold() for key in keys]:
        check.is_in(key, driver._kusto_settings["id"])
        check.is_instance(
            driver._kusto_settings["id"][key], azure_kusto_driver.KustoConfig
        )

    for _, k_config in driver._kusto_settings["id"].items():
        check.is_in("Cluster", k_config.args)
        if "IntegratedAuth" in k_config.args:
            check.equal(k_config.args["IntegratedAuth"], k_config.integrated_auth)
        if "TenantId" in k_config.args:
            check.equal(k_config.args["TenantId"], k_config.tenant_id)
        check.is_not_none(k_config.path)
        check.is_not_none(k_config.cluster)
        check.is_not_none(k_config.name)
        if "ClusterGroups" in k_config:
            check.is_in("Group1", k_config.cluster_groups)


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


class AzConnectState:
    """Mock az_connect to capture args."""

    def __init__(self):
        """Initialize mock az_connect state."""
        self.connect_args = []
        self.connect_kwargs = {}

    def az_connect(self, *args, **kwargs):
        """Mock az_connect."""
        self.connect_args = args
        self.connect_kwargs = kwargs
        return az_connect(*args, **kwargs)


class ConnectTest(NamedTuple):
    """Test configuration for Kusto connection tests."""

    init_args: dict
    connect_args: dict
    name: str
    tests: List[Callable]
    exception: Optional[type] = None
    additional_config: Optional[dict] = None


_TEST_CONNECT_ARGS = (
    ConnectTest(
        name="connection_str",
        init_args={"connection_str": "https://test1.kusto.windows.net"},
        connect_args={"connection_str": "https://test.kusto.windows.net"},
        tests=[
            lambda driver: driver.client._kusto_cluster.startswith(
                "https://test.kusto.windows.net"
            )
        ],
    ),
    ConnectTest(
        name="cluster-no-init",
        init_args={},
        connect_args={"cluster": "https://test.kusto.windows.net"},
        tests=[
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
    ConnectTest(
        name="mp_az_auth-client_id-override",
        init_args={
            "cluster": "https://test.kusto.windows.net",
        },
        connect_args={
            "cluster": "G3",
            "mp_az_auth": ["msi", "cli"],
        },
        tests=[
            lambda az_connect: "clientsecret"
            in az_connect.connect_kwargs["auth_types"],
        ],
    ),
    ConnectTest(
        name="no-cluster-or-connection_str",
        init_args={},
        connect_args={},
        tests=[],
        exception=MsticpyParameterError,
    ),
    ConnectTest(
        name="connection_str-with-proxies",
        init_args={},
        connect_args={"connection_str": "https://test.kusto.windows.net"},
        tests=[
            lambda driver: driver.client._kusto_cluster.startswith(
                "https://test.kusto.windows.net"
            ),
            lambda driver: driver.client._proxy_url == "https://test.com",
        ],
        additional_config={
            "msticpy": {"Proxies": {"https": {"Url": "https://test.com"}}}
        },
    ),
    ConnectTest(
        name="cluster-not-in-config",
        init_args={},
        connect_args={"cluster": "https://random.kusto.windows.net"},
        tests=[
            lambda driver: driver.client._kusto_cluster.startswith(
                "https://random.kusto.windows.net"
            )
        ],
    ),
    ConnectTest(
        name="cluster-not-in-config-and-not-url",
        init_args={},
        connect_args={"cluster": "random_name"},
        tests=[],
        exception=MsticpyDataQueryError,
    ),
)


def unit_test_ids(tests):
    """Test Kusto connection."""
    return [test.name for test in tests]


@pytest.mark.parametrize(
    "test_config", _TEST_CONNECT_ARGS, ids=unit_test_ids(_TEST_CONNECT_ARGS)
)
def test_kusto_connect(test_config, monkeypatch):
    """Test Kusto connect function."""
    config_settings = {
        **_KUSTO_NEW_CONFIG_SETTINGS,
        **(test_config.additional_config or {}),
    }

    with custom_get_config(
        monkeypatch=monkeypatch,
        settings=config_settings,
        add_modules=[
            "msticpy.common.proxy_settings",
            "msticpy.data.drivers.azure_kusto_driver",
        ],
    ):
        az_connect_obj = AzConnectState()
        monkeypatch.setattr(azure_kusto_driver, "az_connect", az_connect_obj.az_connect)
        driver = AzureKustoDriver(**(test_config.init_args))
        if test_config.exception:
            with pytest.raises(test_config.exception):
                driver.connect(**(test_config.connect_args))
            return
        driver.connect(**(test_config.connect_args))
        for idx, test in enumerate(test_config.tests):
            print(
                f"Testcase [{idx}]:",
                test_config.name,
                test.__code__.co_filename,
                "line:",
                test.__code__.co_firstlineno,
            )
            if "driver" in test.__code__.co_varnames:
                check.is_true(test(driver))
            elif "az_connect" in test.__code__.co_varnames:
                check.is_true(test(az_connect_obj))

        cluster_name = test_config.connect_args.get("cluster")
        if cluster_name:
            driver.set_cluster(cluster_name)
            if cluster_name.startswith("https://"):
                exp_cluster = cluster_name
            else:
                exp_cluster = f"https://{cluster_name.casefold()}.kusto.windows.net"
            check.is_true(driver.client._kusto_cluster.startswith(exp_cluster))


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

    injected_exception = None

    def __init__(self, *args, **kwargs):
        """Initialize client."""
        self.args = args
        self.kwargs = kwargs
        self._data = _TEST_DF

    def execute(self, query, database=None, *args, **kwargs):
        """Mock execute."""
        if self.injected_exception:
            if self.injected_exception == KustoApiError:
                raise self.injected_exception({"error": "Query failed"})
            raise self.injected_exception("Query failed")
        if query is None:
            raise ValueError("Query cannot be None")
        if database is None:
            raise ValueError("Database cannot be None")
        if query == "test":
            return KustoResponseDataSet(self._data)

        raise ValueError(f"Invalid query {query}")

    def execute_mgmt(self, database, query, *args, **kwargs):
        """Mock execute_mgmt."""
        if self.injected_exception:
            if self.injected_exception == KustoApiError:
                raise self.injected_exception({"error": "Query failed"})
            raise self.injected_exception("Query failed")
        if query == "service_error":
            raise KustoServiceError("Query failed")
        if query == ".show databases":
            return KustoResponseDataSet(
                pd.DataFrame(["dv1", "dv2"], columns=["DatabaseName"])
            )
        if query.endswith("schema"):
            return KustoResponseDataSet(
                pd.DataFrame(
                    data=[
                        ["Table1", "TimeGenerated", "System.datetime"],
                        ["Table1", "TestColumn", "System.string"],
                        ["Table2", "TimeGenerated", "System.datetime"],
                        ["Table2", "TestColumn2", "System.string"],
                    ],
                    columns=["TableName", "ColumnName", "ColumnType"],
                )
            )

        pytest.fail("Unexpected test case")


def create_query_source(
    data_families=None, database=None, cluster=None, clusters=None, cluster_groups=None
):
    """Create custom query source for testing."""
    metadata = {
        "data_environments": ["Kusto"],
    }
    if cluster:
        metadata["cluster"] = cluster
    if clusters:
        metadata["clusters"] = clusters
    if cluster_groups:
        metadata["cluster_groups"] = cluster_groups
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
    injected_exception: Optional[type] = None


_TEST_QUERY_ARGS = (
    QueryTest(
        name="query-no-db",
        connect_args={"connection_str": "https://test.kusto.windows.net"},
        query_args={"query": "test"},
        tests=[],
        exception=MsticpyDataQueryError,
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
            "cluster": "https://ignore.kusto.windows.net",
            "query_source": {
                "database": "test_db",
                "cluster": "https://help.kusto.windows.net",
            },
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
            "query_source": {
                "data_families": ["MyKusto.param_test_db"],
                "cluster": "help",
            },
        },
        tests=[
            lambda result: isinstance(result, pd.DataFrame),
        ],
    ),
    QueryTest(
        name="query-get-db-family-no-dot-from-source",
        connect_args={"cluster": "help"},
        query_args={
            "query": "test",
            "query_source": {
                "data_families": ["param_test_db"],
                "cluster": "help",
            },
        },
        tests=[
            lambda result: isinstance(result, pd.DataFrame),
        ],
    ),
    QueryTest(
        name="query-no-db-or-families-in-source",
        connect_args={"cluster": "help"},
        query_args={
            "query": "test",
            "query_source": {
                "cluster": "help",
            },
        },
        tests=[],
        exception=MsticpyDataQueryError,
    ),
    QueryTest(
        name="query-get-db-and-cluster-from-source",
        connect_args={"cluster": "help"},
        query_args={
            "query": "test",
            "query_source": {
                "database": "test_db",
                "cluster": "https://help.kusto.windows.net",
            },
        },
        tests=[
            lambda result: isinstance(result, pd.DataFrame),
        ],
    ),
    QueryTest(
        name="query-not-connected",
        connect_args={},
        query_args={"query": "test"},
        tests=[],
        exception=MsticpyNotConnectedError,
    ),
    QueryTest(
        name="kusto-api-error",
        connect_args={"cluster": "https://test.kusto.windows.net"},
        query_args={"query": "test", "database": "test_db"},
        tests=[],
        injected_exception=KustoApiError,
        exception=MsticpyDataQueryError,
    ),
    QueryTest(
        name="kusto-service-error",
        connect_args={"cluster": "https://test.kusto.windows.net"},
        query_args={"query": "test", "database": "test_db"},
        tests=[],
        injected_exception=KustoServiceError,
        exception=MsticpyDataQueryError,
    ),
    QueryTest(
        name="no-db-error",
        connect_args={"cluster": "https://test.kusto.windows.net"},
        query_args={"query": "test"},
        tests=[],
        injected_exception=KustoServiceError,
        exception=MsticpyDataQueryError,
    ),
    QueryTest(
        name="db-specified-in-config",
        connect_args={"cluster": "https://g3.kusto.windows.net"},
        query_args={"query": "test"},
        tests=[
            lambda result: isinstance(result, pd.DataFrame),
        ],
    ),
    QueryTest(
        name="no-db-error-query-source",
        connect_args={"cluster": "https://test.kusto.windows.net"},
        query_args={
            "query": "test",
            "query_source": {
                "database": "test_db",
                "cluster": "https://help.kusto.windows.net",
            },
        },
        tests=[],
        injected_exception=KustoServiceError,
        exception=MsticpyDataQueryError,
    ),
    QueryTest(
        name="query-cluster-in-clusters",
        connect_args={"cluster": "help"},
        query_args={
            "query": "test",
            "query_source": {
                "database": "test_db",
                "clusters": [
                    "https://test.kusto.windows.net",
                    "https://help.kusto.windows.net",
                ],
            },
        },
        tests=[
            lambda result: isinstance(result, pd.DataFrame),
        ],
    ),
    QueryTest(
        name="query-cluster-in-cluster-group",
        connect_args={"cluster": "MDE"},
        query_args={
            "query": "test",
            "query_source": {
                "database": "test_db",
                "cluster_groups": ["Group1", "Group2"],
            },
        },
        tests=[
            lambda result: isinstance(result, pd.DataFrame),
        ],
    ),
    QueryTest(
        name="query-cluster-mismatch",
        connect_args={"cluster": "help"},
        query_args={
            "query": "test",
            "query_source": {
                "database": "test_db",
                "cluster": "https://test.kusto.windows.net",
            },
        },
        tests=[],
        exception=MsticpyDataQueryError,
    ),
    QueryTest(
        name="query-cluster-not-in-clusters",
        connect_args={"cluster": "help"},
        query_args={
            "query": "test",
            "query_source": {
                "database": "test_db",
                "clusters": [
                    "https://test.kusto.windows.net",
                    "https://msticti.kusto.windows.net",
                ],
            },
        },
        tests=[],
        exception=MsticpyDataQueryError,
    ),
    QueryTest(
        name="query-cluster-not-in-cluster-group",
        connect_args={"cluster": "help"},
        query_args={
            "query": "test",
            "query_source": {
                "database": "test_db",
                "cluster_groups": ["Group1", "Group2"],
            },
        },
        tests=[],
        exception=MsticpyDataQueryError,
    ),
    QueryTest(
        name="query-cluster-not-in-cluster-group2",
        connect_args={"cluster": "g3cluster"},
        query_args={
            "query": "test",
            "query_source": {
                "database": "test_db",
                "cluster_groups": ["Group1", "Group2"],
            },
        },
        tests=[],
        exception=MsticpyDataQueryError,
    ),
    # This succeeds because we optimistically try the query
    # if the query has no cluster metadata
    QueryTest(
        name="query-no-query_source-cluster-metadata",
        connect_args={"cluster": "g3cluster"},
        query_args={
            "query": "test",
            "query_source": {
                "database": "test_db",
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
    """Test Kusto query with different parameters."""
    monkeypatch.setattr(azure_kusto_driver, "az_connect", az_connect)
    monkeypatch.setattr(
        azure_kusto_driver, "dataframe_from_result_table", lambda resp: resp
    )
    if query_config.injected_exception:
        monkeypatch.setattr(
            KustoClient, "injected_exception", query_config.injected_exception
        )
    monkeypatch.setattr(azure_kusto_driver, "KustoClient", KustoClient)

    with custom_get_config(
        monkeypatch=monkeypatch,
        settings=_KUSTO_NEW_CONFIG_SETTINGS,
        add_modules=[
            "msticpy.common.proxy_settings",
            "msticpy.data.drivers.azure_kusto_driver",
        ],
    ):
        test_config = _TEST_CONNECT_ARGS[2]
        driver = AzureKustoDriver(**(test_config.init_args))
        if query_config.connect_args:
            driver.connect(**(query_config.connect_args))

        if query_config.exception:
            with pytest.raises(query_config.exception):
                query_args = query_config.query_args
                if "query_source" in query_args:
                    query_args["query_source"] = create_query_source(
                        **query_args["query_source"]
                    )
                driver.query(**query_args)
        else:
            query_args = query_config.query_args
            if "query_source" in query_args:
                query_args["query_source"] = create_query_source(
                    **query_args["query_source"]
                )
            result = driver.query(**query_args)
            for test in query_config.tests:
                print(
                    "Testcase:",
                    query_config.name,
                    test.__code__.co_filename,
                    "line:",
                    test.__code__.co_firstlineno,
                )
                check.is_true(test(result))
            check.is_instance(result, pd.DataFrame)


def test_set_database_query(monkeypatch):
    """Test that we can set the database via the set_database method."""
    monkeypatch.setattr(azure_kusto_driver, "az_connect", az_connect)
    monkeypatch.setattr(
        azure_kusto_driver, "dataframe_from_result_table", lambda resp: resp
    )
    monkeypatch.setattr(azure_kusto_driver, "KustoClient", KustoClient)

    with custom_get_config(
        monkeypatch=monkeypatch,
        settings=_KUSTO_NEW_CONFIG_SETTINGS,
        add_modules=[
            "msticpy.common.proxy_settings",
            "msticpy.data.drivers.azure_kusto_driver",
        ],
    ):
        test_config = _TEST_CONNECT_ARGS[2]
        driver = AzureKustoDriver(**(test_config.init_args))
        query_config = QueryTest(
            name="query-no-db",
            connect_args={"connection_str": "https://test.kusto.windows.net"},
            query_args={"query": "test"},
            tests=[],
        )
        if query_config.connect_args:
            driver.connect(**(query_config.connect_args))

        driver.set_database("test_db")
        query_args = query_config.query_args
        result = driver.query(**query_args)
        check.is_instance(result, pd.DataFrame)


_TEST_CLUSTERS_SUCC = ("MDE", "https://test.kusto.windows.net", "test")


def create_cluster_combo_tests(connect_names, query_source_names):
    """Create tests for cluster name combos."""
    for cluster in connect_names:
        for connect_cl, query_cl in product([cluster], query_source_names):
            yield QueryTest(
                name=f"success-combo-{connect_cl}-{query_cl}",
                connect_args={"cluster": connect_cl},
                query_args={
                    "query": "test",
                    "query_source": {
                        "data_families": ["MyKusto.param_test_db"],
                        "cluster": query_cl,
                    },
                },
                tests=[lambda result: isinstance(result, pd.DataFrame)],
                exception=ValueError,
            )


_TEST_CLUSTER_ID_COMBOS = tuple(
    create_cluster_combo_tests(_TEST_CLUSTERS_SUCC, _TEST_CLUSTERS_SUCC)
)


@pytest.mark.parametrize(
    "query_config", _TEST_CLUSTER_ID_COMBOS, ids=unit_test_ids(_TEST_CLUSTER_ID_COMBOS)
)
def test_cluster_name_combos(query_config, monkeypatch):
    """Test combinations of different cluster name formats."""
    monkeypatch.setattr(azure_kusto_driver, "az_connect", az_connect)
    monkeypatch.setattr(
        azure_kusto_driver, "dataframe_from_result_table", lambda resp: resp
    )
    monkeypatch.setattr(azure_kusto_driver, "KustoClient", KustoClient)
    with custom_get_config(
        monkeypatch=monkeypatch,
        settings=_KUSTO_NEW_CONFIG_SETTINGS,
        add_modules=[
            "msticpy.common.proxy_settings",
            "msticpy.data.drivers.azure_kusto_driver",
        ],
    ):
        driver = AzureKustoDriver()
        driver.connect(**(query_config.connect_args))

        query_args = query_config.query_args
        if "query_source" in query_args:
            query_args["query_source"] = create_query_source(
                **query_args["query_source"]
            )
        result = driver.query(**query_args)
        for test in query_config.tests:
            print(
                "Testcase:",
                query_config.name,
                test.__code__.co_filename,
                "line:",
                test.__code__.co_firstlineno,
            )
            check.is_true(test(result))


_TEST_CLUSTERS_FAIL = ("Help", "https://help.kusto.windows.net", "help")

_TEST_CLUSTER_ID_COMBOS_FAIL = tuple(
    create_cluster_combo_tests(_TEST_CLUSTERS_SUCC, _TEST_CLUSTERS_FAIL)
)


@pytest.mark.parametrize(
    "query_config",
    _TEST_CLUSTER_ID_COMBOS_FAIL,
    ids=unit_test_ids(_TEST_CLUSTER_ID_COMBOS_FAIL),
)
def test_cluster_name_combos_fail(query_config, monkeypatch):
    """Test failing combinations of different cluster name formats."""
    monkeypatch.setattr(azure_kusto_driver, "az_connect", az_connect)
    monkeypatch.setattr(
        azure_kusto_driver, "dataframe_from_result_table", lambda resp: resp
    )
    monkeypatch.setattr(azure_kusto_driver, "KustoClient", KustoClient)
    with custom_get_config(
        monkeypatch=monkeypatch,
        settings=_KUSTO_NEW_CONFIG_SETTINGS,
        add_modules=[
            "msticpy.common.proxy_settings",
            "msticpy.data.drivers.azure_kusto_driver",
        ],
    ):
        driver = AzureKustoDriver()
        driver.connect(**(query_config.connect_args))

        query_args = query_config.query_args
        if "query_source" in query_args:
            query_args["query_source"] = create_query_source(
                **query_args["query_source"]
            )
        with pytest.raises(MsticpyDataQueryError):
            driver.query(**query_args)


_TEST_GET_DB_NAMES = (
    ({"cluster": "MDE"}, None, None),
    (None, None, MsticpyNotConnectedError),
    ({"cluster": "MDE"}, KustoServiceError, MsticpyDataQueryError),
)


@pytest.mark.parametrize("connect_args, inj_exc, exp_exc", _TEST_GET_DB_NAMES)
def test_kusto_get_database_names(connect_args, inj_exc, exp_exc, monkeypatch):
    """Test get_database_names method."""
    monkeypatch.setattr(azure_kusto_driver, "az_connect", az_connect)
    monkeypatch.setattr(
        azure_kusto_driver, "dataframe_from_result_table", lambda resp: resp
    )
    KustoClient.injected_exception = inj_exc
    monkeypatch.setattr(azure_kusto_driver, "KustoClient", KustoClient)

    with custom_get_config(
        monkeypatch=monkeypatch,
        settings=_KUSTO_NEW_CONFIG_SETTINGS,
        add_modules=[
            "msticpy.common.proxy_settings",
            "msticpy.data.drivers.azure_kusto_driver",
        ],
    ):
        driver = AzureKustoDriver()
        if connect_args:
            driver.connect(**connect_args)

        if exp_exc:
            with pytest.raises(exp_exc):
                driver.get_database_names()
        else:
            result = driver.get_database_names()
            check.is_instance(result, list)
            check.equal(len(result), 2)

        KustoClient.injected_exception = None


_TEST_GET_DB_SCHEMA = (
    ({"cluster": "MDE", "database": "test"}, None, None),
    (None, None, MsticpyNotConnectedError),
    ({"cluster": "MDE"}, None, ValueError),
    ({"cluster": "MDE", "database": "test"}, KustoServiceError, MsticpyDataQueryError),
)


@pytest.mark.parametrize("connect_args, inj_exc, exp_exc", _TEST_GET_DB_SCHEMA)
def test_kusto_get_database_schema(connect_args, inj_exc, exp_exc, monkeypatch):
    """Test get_database_schema method."""
    monkeypatch.setattr(azure_kusto_driver, "az_connect", az_connect)
    monkeypatch.setattr(
        azure_kusto_driver, "dataframe_from_result_table", lambda resp: resp
    )
    KustoClient.injected_exception = inj_exc
    monkeypatch.setattr(azure_kusto_driver, "KustoClient", KustoClient)

    with custom_get_config(
        monkeypatch=monkeypatch,
        settings=_KUSTO_NEW_CONFIG_SETTINGS,
        add_modules=[
            "msticpy.common.proxy_settings",
            "msticpy.data.drivers.azure_kusto_driver",
        ],
    ):
        driver = AzureKustoDriver()
        if connect_args:
            driver.connect(**connect_args)

        if exp_exc:
            with pytest.raises(exp_exc):
                driver.get_database_schema()

            check.equal(driver.schema, {})
        else:
            result = driver.get_database_schema()
            check.is_instance(result, dict)
            check.equal(len(result), 2)
            check.is_in("Table1", result)
            check.is_in("Table2", result)
            check.equal(result["Table1"]["TimeGenerated"], "datetime")
            check.equal(result, driver.schema)

        KustoClient.injected_exception = None


_TOTAL_QUERIES = 55

_TEST_QUERY_LOAD = (
    ConnectTest(
        name="connect-cluster-mde",
        init_args={},
        connect_args={"cluster": "MDE"},
        tests=[
            lambda load_queries: len(load_queries) == _TOTAL_QUERIES,
            lambda connect_queries: len(connect_queries) == 22,
            lambda connect_queries: all(
                q[:2] in ("T1", "T2", "T3", "T4") for q in connect_queries
            ),
        ],
    ),
    ConnectTest(
        name="connect-cluster-msticti",
        init_args={},
        connect_args={"cluster": "msticti"},
        tests=[
            lambda load_queries: len(load_queries) == _TOTAL_QUERIES,
            lambda connect_queries: len(connect_queries) == 18,
            lambda connect_queries: all(
                q[:2] in ("T2", "T3", "T4") for q in connect_queries
            ),
        ],
    ),
    ConnectTest(
        name="connect-cluster-msticti-strict",
        init_args={"strict_query_match": True},
        connect_args={"cluster": "msticti"},
        tests=[
            lambda load_queries: len(load_queries) == _TOTAL_QUERIES,
            lambda connect_queries: len(connect_queries) == 14,
            lambda connect_queries: all(q[:2] in ("T2", "T3") for q in connect_queries),
        ],
    ),
    ConnectTest(
        name="connect-cluster-help",
        init_args={},
        connect_args={"cluster": "help"},
        tests=[
            lambda load_queries: len(load_queries) == _TOTAL_QUERIES,
            lambda connect_queries: len(connect_queries) == 8,
            lambda connect_queries: all(q[:2] in ("T4", "T5") for q in connect_queries),
        ],
    ),
    ConnectTest(
        name="connect-cluster-g2",
        init_args={},
        connect_args={"cluster": "G2Cluster"},
        tests=[
            lambda load_queries: len(load_queries) == _TOTAL_QUERIES,
            lambda connect_queries: len(connect_queries) == 11,
            lambda connect_queries: all(q[:2] in ("T4", "T6") for q in connect_queries),
        ],
    ),
    ConnectTest(
        name="connect-cluster-g2-strict",
        init_args={"strict_query_match": True},
        connect_args={"cluster": "G2Cluster"},
        tests=[
            lambda load_queries: len(load_queries) == _TOTAL_QUERIES,
            lambda connect_queries: len(connect_queries) == 7,
            lambda connect_queries: all(q[:2] in ("T6") for q in connect_queries),
        ],
    ),
)


@pytest.mark.parametrize("connect_test", _TEST_QUERY_LOAD, ids=lambda x: x.name)
def test_provider_load_queries(connect_test, monkeypatch):
    """Test provider load queries for different cluster configs."""
    monkeypatch.setattr(azure_kusto_driver, "az_connect", az_connect)
    monkeypatch.setattr(
        azure_kusto_driver, "dataframe_from_result_table", lambda resp: resp
    )
    monkeypatch.setattr(azure_kusto_driver, "KustoClient", KustoClient)

    test_queries = get_test_data_path().joinpath("kusto")
    with custom_get_config(
        monkeypatch=monkeypatch,
        settings=_KUSTO_NEW_CONFIG_SETTINGS,
        add_modules=[
            "msticpy.common.proxy_settings",
            "msticpy.data.drivers.azure_kusto_driver",
        ],
    ):
        qry_prov = QueryProvider(
            "Kusto_New", query_paths=[test_queries], **connect_test.init_args
        )
        load_queries = qry_prov.list_queries()
        qry_prov.connect(**connect_test.connect_args)
        connect_queries = qry_prov.list_queries()
        print(len(load_queries), len(connect_queries))
        print(connect_queries)

        for test in connect_test.tests:
            print(
                "Testcase:",
                connect_test.name,
                test.__code__.co_filename,
                "line:",
                test.__code__.co_firstlineno,
            )
            if "load_queries" in test.__code__.co_varnames:
                check.is_true(test(load_queries))
            if "connect_queries" in test.__code__.co_varnames:
                check.is_true(test(connect_queries))
