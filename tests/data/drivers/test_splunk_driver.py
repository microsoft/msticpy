# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""datq query test class."""
from contextlib import redirect_stdout
import io

from unittest.mock import patch, MagicMock
import pytest
import pytest_check as check

import pandas as pd

from msticpy.common.exceptions import (
    MsticpyUserConfigError,
    MsticpyConnectionError,
    MsticpyNoDataSourceError,
    MsticpyNotConnectedError,
)

from msticpy.data.drivers.splunk_driver import SplunkDriver, sp_client

from ...unit_test_lib import get_test_data_path

_TEST_DATA = get_test_data_path()

SPLUNK_CLI_PATCH = SplunkDriver.__module__ + ".sp_client"
SPLUNK_RESULTS_PATCH = SplunkDriver.__module__ + ".sp_results"


# pylint: disable=too-many-branches, too-many-return-statements
# pylint: disable=no-self-use, too-few-public-methods, protected-access


def cli_connect(**kwargs):
    """Return None if magic isn't == kql."""
    cause = MagicMock()
    cause.body = bytes("Test body stuff", encoding="utf-8")
    cause.status = 404
    cause.reason = "Page not found."
    cause.headers = "One Two Three"
    if kwargs.get("host") == "AuthError":
        raise sp_client.AuthenticationError(cause=cause, message="test AuthHeader")
    if kwargs.get("host") == "HTTPError":
        cause.body = io.BytesIO(cause.body)
        raise sp_client.HTTPError(response=cause, _message="test HTTPError")
    return _MockSplunkService()


class _MockSplunkSearch:
    def __init__(self, name, search):
        self.name = name
        self.search = search

    def get(self, arg):
        """Mock method."""
        del arg
        return self.search

    def __getitem__(self, key):
        """Mock method."""
        if key == "search":
            return self.search
        return "other"


class _MockAlert:
    def __init__(self, name, count):
        self.name = name
        self.count = count


class _MockSplunkService(MagicMock):
    """Splunk service mock."""

    def __init__(self):
        """Mock method."""
        super().__init__()
        self.searches = [
            _MockSplunkSearch("query1", "get stuff from somewhere"),
            _MockSplunkSearch("query2", "get stuff from somewhere"),
        ]
        self.jobs = MagicMock()
        self.jobs.oneshot = self._query_response

    @property
    def saved_searches(self):
        """Mock method."""
        return self.searches

    @property
    def fired_alerts(self):
        """Mock method."""
        return [
            _MockAlert("alert1", 10),
            _MockAlert("alert2", 10),
            _MockAlert("alert3", 10),
            _MockAlert("alert4", 10),
        ]

    @staticmethod
    def _query_response(query):
        return query


def _results_reader(query_result):
    """Mock Splunk results reader."""
    for i in range(10):
        yield f"""{{
            "row": {i}, "query": "{query_result}", "text": "test text"}}
            """


@patch(SPLUNK_CLI_PATCH)
def test_splunk_connect_no_params(splunk_client):
    """Check failure with no args."""
    splunk_client.connect = cli_connect

    sp_driver = SplunkDriver()
    check.is_true(sp_driver.loaded)

    with pytest.raises(MsticpyUserConfigError) as mp_ex:
        sp_driver.connect()
        check.is_false(sp_driver.connected)
    check.is_in("no Splunk connection parameters", mp_ex.value.args)


@patch(SPLUNK_CLI_PATCH)
def test_splunk_connect_req_params(splunk_client):
    """Check load/connect success with required params."""
    splunk_client.connect = cli_connect

    sp_driver = SplunkDriver()
    check.is_true(sp_driver.loaded)

    sp_driver.connect(host="localhost", username="ian", password="12345")  # nosec
    check.is_true(sp_driver.connected)

    sp_cntn_str = "host='localhost'; username='ian'; password='12345'"  # nosec
    sp_driver = SplunkDriver()

    sp_driver.connect(connection_str=sp_cntn_str)


@patch(SPLUNK_CLI_PATCH)
def test_splunk_connect_errors(splunk_client):
    """Check connect failure errors."""
    splunk_client.connect = cli_connect

    sp_driver = SplunkDriver()
    check.is_true(sp_driver.loaded)

    print("connected", sp_driver.connected)
    with pytest.raises(MsticpyConnectionError) as mp_ex:
        sp_driver.connect(host="AuthError", username="ian", password="12345")
        print("connected", sp_driver.connected)
        check.is_false(sp_driver.connected)
    check.is_in("Splunk connection", mp_ex.value.args)

    sp_driver = SplunkDriver()
    print("connected", sp_driver.connected)
    with pytest.raises(MsticpyConnectionError) as mp_ex:
        sp_driver.connect(host="HTTPError", username="ian", password="12345")
        print("connected", sp_driver.connected)
        check.is_false(sp_driver.connected)
    check.is_in("Splunk connection", mp_ex.value.args)


@patch(SPLUNK_CLI_PATCH)
def test_splunk_fired_alerts(splunk_client):
    """Check fired alerts."""
    splunk_client.connect = cli_connect
    sp_driver = SplunkDriver()

    # trying to get these before connecting should throw
    with pytest.raises(MsticpyNotConnectedError) as mp_ex:
        sp_driver._get_fired_alerts()
        check.is_false(sp_driver.connected)
        check.is_none(sp_driver._fired_alerts)
    check.is_in("not connected to a service.", mp_ex.value.args)
    sp_driver.connect(host="localhost", username="ian", password="12345")  # nosec
    check.is_true(sp_driver.connected)

    check.is_instance(sp_driver._fired_alerts, pd.DataFrame)
    for _, alert in sp_driver._fired_alerts.iterrows():
        check.is_true(alert["name"].startswith("alert"))
        check.equal(alert["count"], 10)


@patch(SPLUNK_CLI_PATCH)
def test_splunk_saved_searches(splunk_client):
    """Check saved searches."""
    splunk_client.connect = cli_connect
    sp_driver = SplunkDriver()

    # trying to get these before connecting should throw
    with pytest.raises(MsticpyNotConnectedError) as mp_ex:
        sp_driver._get_saved_searches()
        check.is_false(sp_driver.connected)
        check.is_none(sp_driver._saved_searches)
    check.is_in("not connected to a service.", mp_ex.value.args)

    sp_driver.connect(host="localhost", username="ian", password="12345")  # nosec
    check.is_true(sp_driver.connected)

    check.is_instance(sp_driver._saved_searches, pd.DataFrame)
    for _, search in sp_driver._saved_searches.iterrows():
        check.is_true(search["name"].startswith("query"))
        check.equal(search["query"], "get stuff from somewhere")

    queries, name = sp_driver.service_queries
    check.equal(name, "SavedSearches")
    check.is_instance(queries, dict)
    for name, query in queries.items():
        check.is_true(name.startswith("query"))
        check.equal(query, "get stuff from somewhere")


@patch(SPLUNK_RESULTS_PATCH)
@patch(SPLUNK_CLI_PATCH)
def test_splunk_query_success(splunk_client, splunk_results):
    """Check loaded true."""
    splunk_client.connect = cli_connect
    sp_driver = SplunkDriver()
    splunk_results.ResultsReader = _results_reader

    # trying to get these before connecting should throw
    with pytest.raises(MsticpyNotConnectedError) as mp_ex:
        sp_driver.query("some query")
        check.is_false(sp_driver.connected)
    check.is_in("not connected to Splunk.", mp_ex.value.args)

    sp_driver.connect(host="localhost", username="ian", password="12345")  # nosec
    check.is_true(sp_driver.connected)

    df_result = sp_driver.query("some query")
    check.is_instance(df_result, pd.DataFrame)
    check.equal(len(df_result), 10)


# TODO - read config
