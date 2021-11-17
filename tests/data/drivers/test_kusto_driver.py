# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Kusto driver unit tests."""
from unittest.mock import Mock
import pytest
import pytest_check as check

from msticpy.common.exceptions import MsticpyUserConfigError, MsticpyParameterError
from msticpy.data import QueryProvider
from msticpy.data.drivers.kusto_driver import KustoDriver
from msticpy.data.drivers.kql_driver import KqlDriver

from ...unit_test_lib import get_test_data_path, custom_mp_config

__author__ = "Ian Hellen"

# pylint: disable=redefined-outer-name, protected-access

_KUSTO_SETTINGS = """
DataProviders:
    Kusto-MSTIC:
      args:
        Cluster: https://msticti.kusto.windows.net
        ClientId: UUID
        TenantId: UUID
        ClientSecret: [PLACEHOLDER]

    Kusto-AppAuthCluster:
      args:
        Cluster: https://msticapp.kusto.windows.net
        ClientId: UUID
        TenantId: UUID
        ClientSecret: [PLACEHOLDER]

"""


@pytest.fixture(scope="module")
def kusto_qry_prov():
    """Return query provider with query paths."""
    qry_path = str(get_test_data_path().joinpath("kusto"))
    msticpy_config = get_test_data_path().joinpath("msticpyconfig.yaml")
    with custom_mp_config(msticpy_config):
        return QueryProvider("Kusto", query_paths=[qry_path])


_TEST_CON_STR = [
    "azure_data-Explorer://",
    "tenant='69d28fd7-42a5-48bc-a619-af56397b9f28';",
    "clientid='69d28fd7-42a5-48bc-a619-af56397b1111';",
    "clientsecret='[PLACEHOLDER]';",
    "cluster='https://msticapp.kusto.windows.net';",
    "database='scrubbeddata'",
]
_KUSTO_TESTS = [
    ("no_params", {}),
    ("cluster_uri", {"cluster": "https://msticapp.kusto.windows.net"}),
    ("cluster", {"cluster": "msticapp"}),
    ("database", {"database": "scrubbeddata"}),
    (
        "both",
        {
            "cluster": "https://msticapp.kusto.windows.net",
            "database": "scrubbeddata",
        },
    ),
    ("con_str", {"connection_str": "".join(_TEST_CON_STR)}),
]


def _mock_connect(self, *args, **kwargs):
    """Mock connect for KqlDriver"""
    print(args, kwargs)


@pytest.mark.parametrize("inst, qry_args", _KUSTO_TESTS)
def test_kusto_driver_connect(inst, qry_args, monkeypatch, kusto_qry_prov):
    """Test class Kusto load and execute query driver."""
    qry_prov = kusto_qry_prov
    driver = qry_prov._query_provider
    check.is_instance(driver, KustoDriver)
    check.greater_equal(len(qry_prov.list_queries()), 4)

    print(inst)
    # set up mock
    mock_driver = Mock(KqlDriver)
    mock_driver.connect.return_value = None
    monkeypatch.setattr(driver.__class__.__mro__[1], "connect", _mock_connect)

    # Call connect
    driver.connect(**qry_args)
    if inst in ("both", "con_str"):
        # We expect successful connection with either both cluster
        # and database params or full connection string
        check.is_not_none(driver.current_connection)
        for expected in _TEST_CON_STR:
            check.is_in(expected, driver.current_connection)
    else:
        check.is_none(driver.current_connection)


@pytest.mark.parametrize("inst, qry_args", _KUSTO_TESTS)
def test_kusto_driver_queries(inst, qry_args, monkeypatch, kusto_qry_prov):
    """Test class Kusto load and execute query driver."""
    qry_prov = kusto_qry_prov
    driver = qry_prov._query_provider
    check.is_instance(driver, KustoDriver)
    check.greater_equal(len(qry_prov.list_queries()), 4)

    print(inst)
    # set up mock
    mock_driver = Mock(KqlDriver)
    mock_driver.query_with_results.return_value = "data", "success"
    monkeypatch.setattr(driver, "query_with_results", mock_driver.query_with_results)

    # Run query
    result = qry_prov.AppAuthCluster.scrubbeddata.list_host_processes(
        host_name="test", **qry_args
    )
    mock_qry_func = driver.query_with_results
    mock_qry_func.assert_called_once()
    check.equal(result, "data")
    check.is_in('DeviceName has "test"', mock_qry_func.call_args[0][0])
    check.is_in("where Timestamp >= datetime(2", mock_qry_func.call_args[0][0])
    for expected in _TEST_CON_STR:
        check.is_in(expected, driver.current_connection)


_TEST_CON_STR_INTEG = [
    "azure_data-Explorer://",
    "code;",
    "cluster='https://mstic.kusto.windows.net';",
    "database='scrubbeddata'",
]
_KUSTO_TESTS_INTEG = [
    ("no_params", {}),
    ("cluster_uri", {"cluster": "https://mstic.kusto.windows.net"}),
    ("cluster", {"cluster": "mstic"}),
    ("database", {"database": "scrubbeddata"}),
    (
        "both",
        {
            "cluster": "https://mstic.kusto.windows.net",
            "database": "scrubbeddata",
        },
    ),
    ("con_str", {"connection_str": "".join(_TEST_CON_STR_INTEG)}),
]


@pytest.mark.parametrize("inst, qry_args", _KUSTO_TESTS_INTEG)
def test_kusto_driver_integ_auth(inst, qry_args, monkeypatch, kusto_qry_prov):
    """Test class Kusto load and execute query driver."""
    qry_prov = kusto_qry_prov
    driver = qry_prov._query_provider
    check.is_instance(driver, KustoDriver)
    check.greater_equal(len(qry_prov.list_queries()), 4)

    print(inst)
    # set up mock
    mock_driver = Mock(KqlDriver)
    mock_driver.query_with_results.return_value = "data", "success"
    monkeypatch.setattr(driver, "query_with_results", mock_driver.query_with_results)

    # Run query
    result = qry_prov.IntegAuthCluster.scrubbeddata.list_host_processes(
        host_name="test", **qry_args
    )
    mock_qry_func = driver.query_with_results
    mock_qry_func.assert_called_once()
    check.equal(result, "data")
    check.is_in('DeviceName has "test"', mock_qry_func.call_args[0][0])
    check.is_in("where Timestamp >= datetime(2", mock_qry_func.call_args[0][0])
    for expected in _TEST_CON_STR_INTEG:
        check.is_in(expected, driver.current_connection)


@pytest.mark.parametrize("inst, qry_args", _KUSTO_TESTS)
def test_kusto_driver_params_fail(inst, qry_args, monkeypatch):
    """Test with parameters but missing config."""
    qry_path = str(get_test_data_path().joinpath("kusto"))
    msticpy_config = get_test_data_path().joinpath("msticpyconfig-nokusto.yaml")
    with custom_mp_config(msticpy_config):
        qry_prov = QueryProvider("Kusto", query_paths=[qry_path])
    driver = qry_prov._query_provider

    print(inst)
    # set up mock
    mock_driver = Mock(KqlDriver)
    mock_driver.query_with_results.return_value = "data", "success"
    monkeypatch.setattr(driver, "query_with_results", mock_driver.query_with_results)

    if inst == "con_str":
        # No configuration so only supplying full connection string should work
        result = qry_prov.AppAuthCluster.scrubbeddata.list_host_processes(
            host_name="test", **qry_args
        )
        mock_qry_func = driver.query_with_results
        mock_qry_func.assert_called_once()
        check.equal(result, "data")
        check.is_in('DeviceName has "test"', mock_qry_func.call_args[0][0])
        check.is_in("where Timestamp >= datetime(2", mock_qry_func.call_args[0][0])
        for expected in _TEST_CON_STR:
            check.is_in(expected, driver.current_connection)
    else:
        # Everything else should throw a configuration error.
        with pytest.raises(MsticpyUserConfigError):
            result = qry_prov.AppAuthCluster.scrubbeddata.list_host_processes(
                host_name="test", **qry_args
            )


@pytest.mark.parametrize("inst, qry_args", _KUSTO_TESTS)
def test_kusto_driver_query_fail(inst, qry_args, monkeypatch, kusto_qry_prov):
    """Test with queries + params with incomplete metadata."""
    qry_prov = kusto_qry_prov
    driver = qry_prov._query_provider
    check.is_instance(driver, KustoDriver)
    check.greater_equal(len(qry_prov.list_queries()), 4)

    check.is_true(hasattr(qry_prov.AppAuthClustera.scrubbeddata, "query_new_alias"))
    check.is_true(hasattr(qry_prov.scrubbeddata, "bad_query_fam_no_dot"))
    print(inst)
    # set up mock
    mock_driver = Mock(KqlDriver)
    mock_driver.query_with_results.return_value = "data", "success"
    monkeypatch.setattr(driver, "query_with_results", mock_driver.query_with_results)

    if inst in ("both", "cluster", "con_str", "cluster_uri"):
        # run query
        result = qry_prov.AppAuthCluster.scrubbeddata.bad_query_no_cluster(
            cmd_line="test", **qry_args
        )
        mock_qry_func = driver.query_with_results
        mock_qry_func.assert_called_once()
        check.equal(result, "data")
        check.is_in('ProcessCommandLine contains "test"', mock_qry_func.call_args[0][0])
        check.is_in("where Timestamp >= datetime(2", mock_qry_func.call_args[0][0])
        for expected in _TEST_CON_STR:
            check.is_in(expected, driver.current_connection)
    else:
        # Everything else should throw a parameter error.
        with pytest.raises(MsticpyParameterError):
            qry_prov.AppAuthCluster.scrubbeddata.bad_query_no_cluster(
                cmd_line="test", **qry_args
            )
