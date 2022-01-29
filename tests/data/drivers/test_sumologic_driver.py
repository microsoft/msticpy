# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""datq query test class."""
import io
import json
from datetime import datetime, timedelta
from pathlib import Path
from typing import Any, Dict
from unittest.mock import MagicMock, patch

import pandas as pd
import pytest
import httpx
import pytest_check as check

from msticpy.common.exceptions import (
    MsticpyConnectionError,
    MsticpyNotConnectedError,
    MsticpyUserConfigError,
    MsticpyUserError,
)
from msticpy.data.drivers.sumologic_driver import SumologicDriver

from ...unit_test_lib import TEST_DATA_PATH

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
        raise httpx.ConnectError("test AuthHeader")
    if kwargs.get("host") == "HTTPError":
        cause.body = io.BytesIO(cause.body)
        raise httpx.HTTPError("test HTTPError")
    return SumologicService()


class SumologicService(MagicMock):
    """Sumologic service mock."""

    JOB_STATUS = {
        "done_mssg": {"state": "DONE GATHERING RESULTS", "messageCount": 1},
        "done_rec": {
            "state": "DONE GATHERING RESULTS",
            "messageCount": 1,
            "recordCount": 10,
        },
        "cancelled": {"state": "CANCELLED"},
        "waiting": {"state": "NO JOB"},
        "pending": {"state": "PENDING"},
    }

    SEARCH_JOBS = {
        "Wait": 1,
        "Cancelled": 2,
        "Timeout": 3,
        "MessageSuccess": 21,
        "MessageFail": 20,
        "RecordSuccess": 31,
        "RecordSuccess | limit 10": 31,
        "RecordSuccess | count records": 32,
        "RecordFail": 30,
        "RecordFail | count records": 33,
        "FailJob": 50,
    }

    def __init__(self, **kwargs):
        """Mock method."""
        super().__init__()
        self._cli_connect(**kwargs)
        self.status = self.JOB_STATUS["waiting"]
        self.status_check_count = 0

        self.data = pd.read_csv(Path(TEST_DATA_PATH).joinpath("host_logons.csv")).head(
            10
        )
        self.data["map._count"] = 1

    def _cli_connect(self, **kwargs):
        if kwargs["endpoint"] == "AuthError":
            raise httpx.ConnectError
        if kwargs["endpoint"] == "HTTPError":
            raise httpx.HTTPError
        if kwargs["endpoint"] == "OtherError":
            raise Exception()

    def search_job(self, query, start_time, end_time, timezone, byreceipttime):
        """Create a search job."""
        del start_time, end_time, timezone, byreceipttime

        self.status_check_count = 0
        if query == "Failjob":
            raise httpx.HTTPError("job failed")
        return self.SEARCH_JOBS.get(query, 0)

    def search_job_status(self, search_job):
        """Return status of job."""
        if search_job in (20, 21):
            return self.JOB_STATUS["done_mssg"]
        if search_job in (30, 31, 32, 33):
            return self.JOB_STATUS["done_rec"]
        if search_job < 10:
            if self.status_check_count > 5:
                self.status_check_count = 0
                if search_job == self.SEARCH_JOBS["Wait"]:
                    return self.JOB_STATUS["done_rec"]
                if search_job == self.SEARCH_JOBS["Cancelled"]:
                    return self.JOB_STATUS["cancelled"]
            self.status_check_count += 1
            return self.JOB_STATUS["pending"]

        # You can manually set the status from the test.
        return self.status

    def search_job_records(self, searchjob, limit=None):
        """Return the record results."""
        del limit
        # Need to implement a SL results object
        if searchjob == self.SEARCH_JOBS["RecordSuccess | count records"]:
            return {"records": self._to_json_dict(self.data)}
        raise Exception("Record job failed")

    def search_job_messages(self, searchjob, limit):
        """Return the message results."""
        del limit
        if searchjob in (
            self.SEARCH_JOBS["MessageSuccess"],
            self.SEARCH_JOBS["Wait"],
            self.SEARCH_JOBS["Cancelled"],
        ):
            return {"messages": self._to_json_dict(self.data.head(1))}
        if searchjob == self.SEARCH_JOBS["RecordSuccess"]:
            return {"messages": self._to_json_dict(self.data)}
        raise Exception("Message job failed")

    @staticmethod
    def _to_json_dict(data):
        return json.loads(data.to_json(orient="records"))


@patch(SUMOLOGIC_SVC, SumologicService)
def test_sumologic_connect_no_params():
    """Check failure with no args."""
    sumologic_driver = SumologicDriver()
    check.is_true(sumologic_driver.loaded)

    with pytest.raises(MsticpyUserConfigError) as mp_ex:
        sumologic_driver.connect()
    check.is_false(sumologic_driver.connected)
    check.is_in("no Sumologic connection parameters", mp_ex.value.args)


@patch(SUMOLOGIC_SVC, SumologicService)
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


@patch(SUMOLOGIC_SVC, SumologicService)
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
    check.is_true(any(arg for arg in mp_ex.value.args if "Error" in arg))

    sumologic_driver = SumologicDriver()
    print("connected", sumologic_driver.connected)
    with pytest.raises(MsticpyConnectionError) as mp_ex:
        sumologic_driver.connect(
            connection_str="HTTPError", accessid="***", accesskey="***"
        )  # nosec
        print("connected", sumologic_driver.connected)
    check.is_false(sumologic_driver.connected)
    check.is_in("Sumologic connection", mp_ex.value.args)
    check.is_true(any(arg for arg in mp_ex.value.args if "Error" in arg))

    sumologic_driver = SumologicDriver()
    print("connected", sumologic_driver.connected)
    with pytest.raises(MsticpyConnectionError) as mp_ex:
        sumologic_driver.connect(
            connection_str="OtherError", accessid="***", accesskey="***"
        )  # nosec
        print("connected", sumologic_driver.connected)
    check.is_false(sumologic_driver.connected)
    check.is_in("Sumologic connection", mp_ex.value.args)
    check.is_true(any(arg for arg in mp_ex.value.args if "Error connecting" in arg))


@patch(SUMOLOGIC_SVC, SumologicService)
def test_sumologic_query_no_connect():
    """Check query fails when not connected true."""
    sumologic_driver = SumologicDriver()
    # trying to get these before connecting should throw
    with pytest.raises(MsticpyNotConnectedError) as mp_ex:
        sumologic_driver.query("some query")
    check.is_false(sumologic_driver.connected)
    check.is_in("not connected to Sumologic.", mp_ex.value.args)


@pytest.fixture
@patch(SUMOLOGIC_SVC, SumologicService)
def sumologic_drv():
    """Return sumologic driver with short timeout."""
    sumologic_driver = SumologicDriver()
    sumologic_driver.checkinterval = 0.1
    sumologic_driver.timeout = 1
    sumologic_driver.connect(
        connection_str="https://api.us2.sumologic.com/api/v1",
        accessid="***",
        accesskey="***",
    )  # nosec
    check.is_true(sumologic_driver.connected)
    return sumologic_driver


_QUERY_TESTS = [
    ("Wait", 1),
    ("Cancelled", 0),
    ("MessageSuccess", 1),
    ("MessageFail", "Failed to get job messages: Message job failed"),
    ("RecordSuccess", 10),
    ("RecordSuccess | count records", 10),
    ("RecordFail", "Failed to get job messages: Message job failed"),
    ("RecordFail | count records", "Failed to get search records: Record job failed"),
    ("Timeout", 0),
    ("Failjob", "Sumologic submit search_job"),
]


# pylint: disable=redefined-outer-name
@patch(SUMOLOGIC_SVC, SumologicService)
@pytest.mark.parametrize(("query", "expected"), _QUERY_TESTS)
def test_sumologic_query(sumologic_drv, query, expected):
    """Check queries with different outcomes."""
    end = datetime.utcnow()
    start = end - timedelta(1)
    if query in ("MessageFail", "RecordFail", "Failjob", "RecordFail | count records"):
        with pytest.raises(MsticpyConnectionError) as mp_ex:
            df_result = sumologic_drv.query(
                query, start=start, end=end, checkinterval=0.1, timeout=1, verbosity=4
            )
        check.is_in(expected, mp_ex.value.args)
    else:
        df_result = sumologic_drv.query(
            query, start=start, end=end, checkinterval=0.1, timeout=1
        )
        check.is_instance(df_result, pd.DataFrame)
        check.equal(len(df_result), expected)


_TIMEOUT_PARAMS: Dict[str, Any] = {
    "checkinterval": 0.1,
    "timeout": 1,
}

_DEF_DATE_PARAMS: Dict[str, Any] = {
    "start": datetime.utcnow() - timedelta(1),
    "end": datetime.utcnow(),
    **_TIMEOUT_PARAMS,
}


_PARAM_TESTS = [
    pytest.param({"days": 5, **_TIMEOUT_PARAMS}, True, id="days"),
    pytest.param(
        {"start": datetime.utcnow() - timedelta(1), **_TIMEOUT_PARAMS}, True, id="start"
    ),
    pytest.param(_DEF_DATE_PARAMS, True, id="start/end"),
    pytest.param(
        {"start_time": datetime.utcnow() - timedelta(1), "end_time": datetime.utcnow()},
        True,
        id="start_time/end_time",
    ),
    pytest.param(
        {
            **_DEF_DATE_PARAMS,
            "limit": 10,
            "timezone": "PST",
            "byreceipttime": True,
        },
        True,
        id="other params",
    ),
    pytest.param(
        {
            **_DEF_DATE_PARAMS,
            "normalize": False,
        },
        True,
        id="no_normalize",
    ),
    pytest.param({**_DEF_DATE_PARAMS, "verbosity": 0}, True, id="verbosity0"),
    pytest.param({**_DEF_DATE_PARAMS, "verbosity": 1}, True, id="verbosity1"),
    pytest.param({**_DEF_DATE_PARAMS, "verbosity": 2}, True, id="verbosity2"),
    pytest.param({**_DEF_DATE_PARAMS, "verbosity": 3}, True, id="verbosity3"),
    pytest.param({**_DEF_DATE_PARAMS, "verbosity": 5}, True, id="verbosity5"),
    pytest.param({}, False, id="no_date"),
]


@patch(SUMOLOGIC_SVC, SumologicService)
@pytest.mark.parametrize("params, expected", _PARAM_TESTS)
def test_sumologic_query_params(sumologic_drv, params, expected):
    """Check queries with different parameters."""
    if expected:
        df_result = sumologic_drv.query("RecordSuccess", **params)
        check.is_instance(df_result, pd.DataFrame)
        check.equal(len(df_result), 10)
    else:
        with pytest.raises(MsticpyUserError) as mp_ex:
            df_result = sumologic_drv.query("RecordSuccess", **params)
        check.is_in("Missing parameter.", mp_ex.value.args)


@patch(SUMOLOGIC_SVC, SumologicService)
@pytest.mark.parametrize("ext", ("xlsx", "csv"))
def test_sumologic_query_export(sumologic_drv, tmpdir, ext):
    """Check queries with different parameters."""
    exp_file = f"sumologic_test.{ext}"
    f_path = tmpdir.join(exp_file)
    params = {
        "exporting": True,
        "export_path": str(f_path),
        "verbosity": 5,
        **_DEF_DATE_PARAMS,
    }
    df_result = sumologic_drv.query("RecordSuccess", **params)
    check.is_instance(df_result, pd.DataFrame)
    check.equal(len(df_result), 10)
    check.is_true(Path(f_path).is_file())


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
