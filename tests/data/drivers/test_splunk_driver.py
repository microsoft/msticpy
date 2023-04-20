# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""datq query test class."""
import io
from unittest.mock import MagicMock, patch

import pandas as pd
import pytest
import pytest_check as check

from msticpy.common.exceptions import (
    MsticpyConnectionError,
    MsticpyDataQueryError,
    MsticpyNotConnectedError,
    MsticpyUserConfigError,
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


class _MockAsyncResponse:
    stats = {
        "isDone": "0",
        "doneProgress": 0.0,
        "scanCount": 1,
        "eventCount": 100,
        "resultCount": 100,
    }

    def __init__(self, query):
        self.query = query

    def __getitem__(self, key):
        """Mock method."""
        return self.stats[key]

    def results(self, **kwargs):
        return self.query

    def is_done(self):
        return True

    def is_ready(self):
        return True

    @classmethod
    def set_done(cls):
        cls.stats["isDone"] = "1"
        cls.stats["doneProgress"] = 1


class _MockSplunkCall:
    def create(query, **kwargs):
        del kwargs
        return _MockAsyncResponse(query)

    def oneshot(query, **kwargs):
        del kwargs
        return query


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
        self.jobs = _MockSplunkCall

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
    def _query_response(query, **kwargs):
        del kwargs
        return query


def _results_reader(query_result):
    """Mock Splunk results reader."""
    if "zero query" in query_result:
        yield None
    else:
        for i in range(10):
            yield {"row": i, "query": query_result, "text": f"test text {i}"}


_FAKE_STRING = "42424"


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

    # [SuppressMessage("Microsoft.Security", "CS002:SecretInNextLine", Justification="Test code")]
    sp_driver.connect(host="localhost", username="ian", password=_FAKE_STRING)  # nosec
    check.is_true(sp_driver.connected)

    # [SuppressMessage("Microsoft.Security", "CS002:SecretInNextLine", Justification="Test code")]
    sp_cntn_str = (
        f"host='localhost'; username='ian'; password='{_FAKE_STRING}'"  # nosec
    )
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
        # [SuppressMessage("Microsoft.Security", "CS002:SecretInNextLine", Justification="Test code")]
        sp_driver.connect(
            host="AuthError", username="ian", password=_FAKE_STRING
        )  # nosec
        print("connected", sp_driver.connected)
        check.is_false(sp_driver.connected)
    check.is_in("Splunk connection", mp_ex.value.args)

    sp_driver = SplunkDriver()
    print("connected", sp_driver.connected)
    with pytest.raises(MsticpyConnectionError) as mp_ex:
        # [SuppressMessage("Microsoft.Security", "CS002:SecretInNextLine", Justification="Test code")]
        sp_driver.connect(
            host="HTTPError", username="ian", password=_FAKE_STRING
        )  # nosec
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
    check.is_in("not connected to Splunk.", mp_ex.value.args)

    # [SuppressMessage("Microsoft.Security", "CS002:SecretInNextLine", Justification="Test code")]
    sp_driver.connect(host="localhost", username="ian", password=_FAKE_STRING)  # nosec
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
    check.is_in("not connected to Splunk.", mp_ex.value.args)

    # [SuppressMessage("Microsoft.Security", "CS002:SecretInNextLine", Justification="Test code")]
    sp_driver.connect(host="localhost", username="ian", password=_FAKE_STRING)  # nosec
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
        check.equal(query, "search get stuff from somewhere")


@patch(SPLUNK_RESULTS_PATCH)
@patch(SPLUNK_CLI_PATCH)
def test_splunk_query_success(splunk_client, splunk_results):
    """Check loaded true."""
    splunk_client.connect = cli_connect
    sp_driver = SplunkDriver()
    splunk_results.ResultsReader = _results_reader
    splunk_results.JSONResultsReader = _results_reader

    # trying to get these before connecting should throw
    with pytest.raises(MsticpyNotConnectedError) as mp_ex:
        sp_driver.query("some query")
        check.is_false(sp_driver.connected)
    check.is_in("not connected to Splunk.", mp_ex.value.args)

    # [SuppressMessage("Microsoft.Security", "CS002:SecretInNextLine", Justification="Test code")]
    sp_driver.connect(host="localhost", username="ian", password=_FAKE_STRING)  # nosec
    check.is_true(sp_driver.connected)

    df_result = sp_driver.query("some query", oneshot=True)
    check.is_instance(df_result, pd.DataFrame)
    check.equal(len(df_result), 10)

    response = sp_driver.query("zero query", oneshot=True)
    check.is_not_instance(response, pd.DataFrame)
    check.equal(len(response), 0)

    with pytest.raises(MsticpyDataQueryError):
        df_result = sp_driver.query("some query", timeout=1)

    _MockAsyncResponse.set_done()
    df_result = sp_driver.query("some query")
    check.is_instance(df_result, pd.DataFrame)
    check.equal(len(df_result), 10)

    response = sp_driver.query("zero query")
    check.is_not_instance(response, pd.DataFrame)
    check.equal(len(response), 0)


# TODO - read config


@pytest.mark.skip
def test_live_connect():
    """Use this to do live testing."""
    sp_driver = SplunkDriver()
    www = "splunk-mstic.westus2.cloudapp.azure.com"
    # [SuppressMessage("Microsoft.Security", "CS002:SecretInNextLine", Justification="Test code")]
    sp_driver.connect(host=www, port=8089, username="admin", password="***")  # nosec

    query = """index="botsv2" earliest=08/25/2017:00:00:00 latest=08/26/2017:00:00:00
    source="WinEventLog:Microsoft-Windows-Sysmon/Operational"
    | table TimeCreated, host, EventID, EventDescription, User, process | head 10
    """
    res_df = sp_driver.query(query)
    check.is_not_none(res_df)

    query0 = """index="botsv2" earliest=08/25/2020:00:00:00
    + 'source="WinEventLog:Microsoft-Windows-Sysmon/Operational"
    | table TimeCreated, host, EventID, EventDescription, User, process | head 10
    """
    res_df = sp_driver.query(query0)
    check.is_instance(res_df, list)
    check.is_false(res_df)

    query1 = """
    index=blackhat sourcetype=network earliest=0 | table TimeGenerated, TotalBytesSent
    """
    res_df = sp_driver.query(query1)
    check.is_not_none(res_df)
