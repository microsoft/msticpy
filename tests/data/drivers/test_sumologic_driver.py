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
from requests.exceptions import ConnectionError as ConnError
from requests.exceptions import HTTPError

from msticpy.common.exceptions import (
    MsticpyConnectionError,
    MsticpyNotConnectedError,
    MsticpyUserConfigError,
)
from msticpy.data.drivers.sumologic_driver import SumologicDriver

# from ...unit_test_lib import TEST_DATA_PATH

# SUMOLOGIC_CLI_PATCH = SumologicDriver.__module__ + ".sp_client"
# SUMOLOGIC_RESULTS_PATCH = SumologicDriver.__module__ + ".sp_results"
SUMOLOGIC_SVC = SumologicDriver.__module__ + ".SumoLogic"

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
        raise ConnError(cause=cause, message="test AuthHeader")
    if kwargs.get("host") == "HTTPError":
        cause.body = io.BytesIO(cause.body)
        raise HTTPError(response=cause, _message="test HTTPError")
    return _MockSumologicService()


class _MockSumologicSearch:
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


class _MockSumologicService(MagicMock):
    """Sumologic service mock."""

    _JOB_STATUS = {
        "done": "DONE GATHERING RESULTS",
        "cancelled": "CANCELLED",
        "wating": "NO JOB",
    }

    def __init__(self, **kwargs):
        """Mock method."""
        super().__init__()
        self._cli_connect(**kwargs)
        self.status = self._JOB_STATUS["waiting"]
        # Sumologic has no saved searches
        self.searches = [
            _MockSumologicSearch("query1", "get stuff from somewhere"),
            _MockSumologicSearch("query2", "get stuff from somewhere"),
        ]
        self.jobs = MagicMock()
        self.jobs.oneshot = self._query_response

    def search_job(self, query, start_time, end_time, timezone, byreceipttime):
        """Create a search job."""
        # Need to create a mock job that emulates SL job
        raise NotImplementedError

    def search_job_status(self, search_job):
        """Return status of job."""
        del search_job
        # You can manually set the status from the test.
        return self.status

    def search_job_records(self, searchjob, limit=None):
        """Return the results."""
        # Need to implement a SL results object
        raise NotImplementedError


# Not used - this is a Splunk thing
def _results_reader(query_result):
    """Mock Sumologic results reader."""
    if "zero query" in query_result:
        yield None
    else:
        for i in range(10):
            yield {"row": i, "query": query_result, "text": f"test text {i}"}


@patch(SUMOLOGIC_SVC, _MockSumologicService)
def test_sumologic_connect_no_params():
    """Check failure with no args."""
    sumologic_driver = SumologicDriver()
    check.is_true(sumologic_driver.loaded)

    with pytest.raises(MsticpyUserConfigError) as mp_ex:
        sumologic_driver.connect()
    check.is_false(sumologic_driver.connected)
    check.is_in("no Sumologic connection parameters", mp_ex.value.args)


@patch(SUMOLOGIC_SVC, _MockSumologicService)
def test_sumologic_connect_req_params():
    """Check load/connect success with required params."""
    sumologic_driver = SumologicDriver()
    check.is_true(sumologic_driver.loaded)

    sumologic_driver.connect(
        connection_str="https://api.us2.sumologic.com/api/v1",
        accessid="***",
        accesskey="***",
    )  # nosec
    check.is_true(sumologic_driver.connected)


@patch(SUMOLOGIC_SVC, _MockSumologicService)
def test_sumologic_connect_errors():
    """Check connect failure errors."""
    sumologic_driver = SumologicDriver()
    check.is_true(sumologic_driver.loaded)

    print("connected", sumologic_driver.connected)
    with pytest.raises(MsticpyConnectionError) as mp_ex:
        sumologic_driver.connect(
            connection_str="AuthError", accessid="***", accesskey="***"
        )  # nosec
        print("connected", sumologic_driver.connected)
    check.is_false(sumologic_driver.connected)
    check.is_in("Sumologic connection", mp_ex.value.args)

    sumologic_driver = SumologicDriver()
    print("connected", sumologic_driver.connected)
    with pytest.raises(MsticpyConnectionError) as mp_ex:
        sumologic_driver.connect(
            connection_str="HTTPError", accessid="***", accesskey="***"
        )  # nosec
        print("connected", sumologic_driver.connected)
    check.is_false(sumologic_driver.connected)
    check.is_in("Sumologic connection", mp_ex.value.args)


@patch(SUMOLOGIC_SVC, _MockSumologicService)
def test_sumologic_query_success():
    """Check loaded true."""
    sumologic_driver = SumologicDriver()
    # sumologic_results.ResultsReader = _results_reader

    # trying to get these before connecting should throw
    with pytest.raises(MsticpyNotConnectedError) as mp_ex:
        sumologic_driver.query("some query")
    check.is_false(sumologic_driver.connected)
    check.is_in("not connected to Sumologic.", mp_ex.value.args)

    sumologic_driver.connect(
        url="https://api.us2.sumologic.com/api/v1", accessid="***", accesskey="***"
    )  # nosec
    check.is_true(sumologic_driver.connected)

    df_result = sumologic_driver.query("some query")
    check.is_instance(df_result, pd.DataFrame)
    check.equal(len(df_result), 10)

    response = sumologic_driver.query("zero query")
    check.is_not_instance(response, pd.DataFrame)
    check.equal(len(response), 0)


# @pytest.mark.skip
# def test_live_connect():
#     """Use this to do live testing."""
#     sumologic_driver = SumologicDriver()
#     www = "https://api.us2.sumologic.com/api/v1"
#     sumologic_driver.connect(url=www, accessid="***", accesskey="***")  # nosec

#     query = """_index=botsv2 source="WinEventLog:Microsoft-Windows-Sysmon/Operational"
#     | fields TimeCreated, host, EventID, EventDescription, User, process | head 10
#     """
#     res_df = sumologic_driver.query(
#         query, start_time="2017-08-25T00:00:00", end_time="2017-08-26T00:00:00"
#     )
#     check.is_not_none(res_df)

#     query0 = """_index=botsv2 source="WinEventLog:Microsoft-Windows-Sysmon/Operational"
#     | fields TimeCreated, host, EventID, EventDescription, User, process | head 10
#     """
#     res_df = sumologic_driver.query(query0, start_time="2020-08-25T00:00:00")
#     check.is_instance(res_df, list)
#     check.is_false(res_df)

#     query1 = """
#     index=blackhat sourcetype=network earliest=0 | fields TimeGenerated, TotalBytesSent
#     """
#     res_df = sumologic_driver.query(query1)
#     check.is_not_none(res_df)
