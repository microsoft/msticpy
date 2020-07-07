# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""datq query test class."""
from contextlib import redirect_stdout
import io
from unittest.mock import patch
import pytest
import pytest_check as check

import pandas as pd

from msticpy.common.exceptions import (
    MsticpyUserConfigError,
    MsticpyConnectionError,
    MsticpyNoDataSourceError,
)

from msticpy.data.drivers.splunk_driver import SplunkDriver, sp_client

from ...unit_test_lib import get_test_data_path

_TEST_DATA = get_test_data_path()

SPLUNK_CLI_PATCH = SplunkDriver.__module__ + ".sp_client"
SPLUNK_RESULTS_PATCH = SplunkDriver.__module__ + ".sp_results"


# pylint: disable=too-many-branches, too-many-return-statements
# pylint: disable=no-self-use


def cli_connect(**kwargs):
    """Return None if magic isn't == kql."""
    if kwargs.get("host") == "AuthError":
        raise sp_client.AuthenticationError(cause="test", message="test")
    if kwargs.get("host") == "HTTPError":
        raise sp_client.HTTPError
    return _MockSplunkService()


class _MockSplunkSearch:
    def __init__(self, name, search):
        self.name = name
        self.search = search

    def get(self, arg):
        del arg
        return self.search


class _MockAlert:
    def __init__(self, name, count):
        self.alert = name
        self.count = count


class _MockSplunkService:
    """IPython get_ipython mock."""

    def __init__(self):
        self.searches = [
            _MockSplunkSearch("query1", "get stuff from somewhere"),
            _MockSplunkSearch("query2", "get stuff from somewhere"),
        ]

    @property
    def saved_searches(self):
        return self.searches

    @property
    def fired_alerts(self):
        return [
            _MockAlert("alert1", 10),
            _MockAlert("alert2", 10),
            _MockAlert("alert3", 10),
            _MockAlert("alert4", 10),
        ]


@patch(SPLUNK_CLI_PATCH)
def test_splunk_connect_no_params(splunk_client):
    """Check loaded true."""
    splunk_client.connect = cli_connect

    sp_driver = SplunkDriver()
    check.is_true(sp_driver.loaded)

    with pytest.raises(MsticpyUserConfigError) as mp_ex:
        sp_driver.connect()
        check.is_false(sp_driver.connected)
    check.is_in("no Splunk connection parameters", mp_ex.value.args)


@patch(SPLUNK_CLI_PATCH)
def test_splunk_connect_req_params(splunk_client):
    """Check loaded true."""
    splunk_client.connect = cli_connect

    sp_driver = SplunkDriver()
    check.is_true(sp_driver.loaded)

    sp_driver.connect(host="localhost", username="ian", password="12345")
    check.is_true(sp_driver.connected)

    sp_cntn_str = "host='localhost'; username='ian'; password='12345'"
    sp_driver = SplunkDriver()

    sp_driver.connect(connection_str=sp_cntn_str)


@patch(SPLUNK_CLI_PATCH)
def test_splunk_connect_errors(splunk_client):
    """Check loaded true."""
    splunk_client.connect = cli_connect

    sp_driver = SplunkDriver()
    check.is_true(sp_driver.loaded)

    with pytest.raises(MsticpyConnectionError) as mp_ex:
        sp_driver.connect(host="AuthError", username="ian", password="12345")
        check.is_false(sp_driver.connected)
    check.is_in("Splunk connection", mp_ex.value.args)

    sp_driver = SplunkDriver()
    with pytest.raises(MsticpyConnectionError) as mp_ex:
        sp_driver.connect(host="HTTPError", username="ian", password="12345")
        check.is_false(sp_driver.connected)
    check.is_in("Splunk connection", mp_ex.value.args)


# @patch(GET_IPYTHON_PATCH)
# def test_kql_connect_no_cs(get_ipython):
#     """Check loaded true."""
#     get_ipython.return_value = _MockIPython()
#     kql_driver = KqlDriver()
#     check.is_true(kql_driver.loaded)
#     with pytest.raises(MsticpyKqlConnectionError) as mp_ex:
#         kql_driver.connect()
#     check.is_in("no connection string", mp_ex.value.args)


# @patch(GET_IPYTHON_PATCH)
# def test_kql_connect_kql_exceptions(get_ipython):
#     """Check loaded true."""
#     get_ipython.return_value = _MockIPython()
#     kql_driver = KqlDriver()

#     with pytest.raises(MsticpyKqlConnectionError) as mp_ex:
#         kql_driver.connect(connection_str="la://connection+KqlErrorUnk")
#     check.is_in("Kql response error", mp_ex.value.args)
#     check.is_false(kql_driver.connected)

#     with pytest.raises(MsticpyKqlConnectionError) as mp_ex:
#         kql_driver.connect(
#             connection_str="la://connection.workspace('1234').tenant(KqlErrorWS)"
#         )
#     check.is_in("unknown workspace", mp_ex.value.args)
#     check.is_false(kql_driver.connected)

#     with pytest.raises(MsticpyKqlConnectionError) as mp_ex:
#         kql_driver.connect(
#             connection_str="la://connection.workspace('1234').tenant(KqlEngineError)"
#         )
#     check.is_in("kql connection error", mp_ex.value.args)
#     check.is_false(kql_driver.connected)


# @patch(GET_IPYTHON_PATCH)
# def test_kql_connect_adal_exceptions(get_ipython):
#     """Check loaded true."""
#     get_ipython.return_value = _MockIPython()
#     kql_driver = KqlDriver()

#     with pytest.raises(MsticpyKqlConnectionError) as mp_ex:
#         kql_driver.connect(connection_str="la://connection+AdalErrorUnk")
#     check.is_in("could not authenticate to tenant", mp_ex.value.args)
#     check.is_false(kql_driver.connected)

#     with pytest.raises(MsticpyKqlConnectionError) as mp_ex:
#         kql_driver.connect(connection_str="la://connection+AdalErrorNR")
#     check.is_in("could not authenticate to tenant", mp_ex.value.args)
#     check.is_in("Full error", str(mp_ex.value.args))
#     check.is_false(kql_driver.connected)

#     with pytest.raises(MsticpyKqlConnectionError) as mp_ex:
#         kql_driver.connect(connection_str="la://connection+AdalErrorPoll")
#     check.is_in("authentication timed out", mp_ex.value.args)
#     check.is_false(kql_driver.connected)


# @patch(GET_IPYTHON_PATCH)
# def test_kql_connect_authn_exceptions(get_ipython):
#     """Check loaded true."""
#     get_ipython.return_value = _MockIPython()
#     kql_driver = KqlDriver()

#     with pytest.raises(MsticpyKqlConnectionError) as mp_ex:
#         kql_driver.connect(connection_str="la://connection+AuthenticationError")
#     check.is_in("authentication failed", mp_ex.value.args)
#     check.is_false(kql_driver.connected)


# @patch(GET_IPYTHON_PATCH)
# def test_kql_schema(get_ipython):
#     """Check loaded true."""
#     get_ipython.return_value = _MockIPython()
#     kql_driver = KqlDriver()
#     kql_driver.connect(connection_str="la://connection")

#     check.is_in("table1", kql_driver.schema)
#     check.is_in("table2", kql_driver.schema)
#     check.is_in("field1", kql_driver.schema["table1"])


# @patch(GET_IPYTHON_PATCH)
# def test_kql_query_not_connected(get_ipython):
#     """Check loaded true."""
#     get_ipython.return_value = _MockIPython()
#     kql_driver = KqlDriver()

#     with pytest.raises(MsticpyNotConnectedError) as mp_ex:
#         kql_driver.query("test")
#     check.is_in("not connected to a workspace.", mp_ex.value.args)
#     check.is_false(kql_driver.connected)


# @patch(GET_IPYTHON_PATCH)
# def test_kql_query_failed(get_ipython):
#     """Check loaded true."""
#     get_ipython.return_value = _MockIPython()
#     kql_driver = KqlDriver()
#     kql_driver.connect(connection_str="la://connection")

#     output = io.StringIO()
#     with redirect_stdout(output):
#         kql_driver.query("test query_failed")
#     check.is_in("Warning - query did", output.getvalue())


# @patch(GET_IPYTHON_PATCH)
# def test_kql_query_success(get_ipython):
#     """Check loaded true."""
#     get_ipython.return_value = _MockIPython()
#     kql_driver = KqlDriver()
#     kql_driver.connect(connection_str="la://connection")

#     result_df = kql_driver.query("test query")
#     check.is_instance(result_df, pd.DataFrame)


# @patch(GET_IPYTHON_PATCH)
# def test_kql_query_partial(get_ipython):
#     """Check loaded true."""
#     get_ipython.return_value = _MockIPython()
#     kql_driver = KqlDriver()
#     kql_driver.connect(connection_str="la://connection")

#     output = io.StringIO()
#     with redirect_stdout(output):
#         result_df = kql_driver.query("test query_partial")
#     check.is_instance(result_df, pd.DataFrame)
#     check.is_in("Warning - query returned partial", output.getvalue())


# @patch(GET_IPYTHON_PATCH)
# def test_kql_query_no_table(get_ipython):
#     """Check loaded true."""
#     get_ipython.return_value = _MockIPython()
#     kql_driver = KqlDriver()
#     kql_driver.connect(connection_str="la://connection")

#     with pytest.raises(MsticpyNoDataSourceError) as mp_ex:
#         query_source = {"args.table": "table3"}
#         kql_driver.query("test query", query_source=query_source)

#     check.is_in("table3 not found.", mp_ex.value.args)
