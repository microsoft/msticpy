# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""datq query test class."""
import io
import json
from datetime import UTC, datetime, timedelta
from pathlib import Path
from typing import Any, Dict
from unittest.mock import MagicMock, patch

import httpx
import pandas as pd
import pytest
import pytest_check as check

from msticpy.common.exceptions import (
    MsticpyConnectionError,
    MsticpyNotConnectedError,
    MsticpyUserConfigError,
    MsticpyUserError,
)
from msticpy.data.drivers.openobserve_driver import OpenObserveDriver

from ...unit_test_lib import TEST_DATA_PATH

# OPENOBSERVE_CLI_PATCH = OpenObserveDriver.__module__ + ".sp_client"
# OPENOBSERVE_RESULTS_PATCH = OpenObserveDriver.__module__ + ".sp_results"
OPENOBSERVE_SVC = OpenObserveDriver.__module__ + ".OpenObserve"

# pylint: disable=too-many-branches, too-many-return-statements
# pylint: disable=no-self-use, too-few-public-methods, protected-access
pytestmark = pytest.mark.filterwarnings("ignore::UserWarning")


def cli_connect(**kwargs):
    """Return None if magic isn't == kql."""
    cause = MagicMock()
    cause.body = bytes("Test body stuff", encoding="utf-8")
    cause.status = 404
    cause.reason = "Page not found."
    cause.headers = "One Two Three"
    if kwargs.get("connection_str") == "AuthError":
        raise httpx.ConnectError("test AuthHeader")
    if kwargs.get("connection_str") == "HTTPError":
        cause.body = io.BytesIO(cause.body)
        raise httpx.HTTPError("test HTTPError")
    return OpenObserveService()


class OpenObserveService(MagicMock):
    """OpenObserve service mock."""

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
        self.status_check_count = 0

        self.data = pd.read_csv(Path(TEST_DATA_PATH).joinpath("host_logons.csv")).head(
            10
        )
        self.data["map._count"] = 1

    def _cli_connect(self, **kwargs):
        # FIXME! pytest fails. openobserve_driver uses connection_str
        #       only underlying python-openobserve uses host
        #       kwargs only _new_name, nae, parent...
        if kwargs["host"] == "AuthError":
            raise httpx.ConnectError
        if kwargs["host"] == "HTTPError":
            raise httpx.HTTPError
        if kwargs["host"] == "OtherError":
            raise Exception()

    def search_job(self, query, start_time, end_time, timezone):
        """Create a search job."""
        del start_time, end_time, timezone

        self.status_check_count = 0
        if query == "Failjob":
            raise httpx.HTTPError("job failed")
        return self.SEARCH_JOBS.get(query, 0)

        # You can manually set the status from the test.
        return self.status

    def search_job_records(self, searchjob, limit=None, **kwargs):
        """Return the record results."""
        del limit, kwargs
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


@patch(OPENOBSERVE_SVC, OpenObserveService)
def test_openobserve_connect_no_params():
    """Check failure with no args."""
    openobserve_driver = OpenObserveDriver()
    check.is_true(openobserve_driver.loaded)

    with pytest.raises(MsticpyUserConfigError) as mp_ex:
        openobserve_driver.connect()
    check.is_false(openobserve_driver.connected)
    check.is_in("no OpenObserve connection parameters", mp_ex.value.args)


@patch(OPENOBSERVE_SVC, OpenObserveService)
def test_openobserve_connect_req_params():
    """Check load/connect success with required params."""
    openobserve_driver = OpenObserveDriver()
    check.is_true(openobserve_driver.loaded)

    openobserve_driver.connect(
        connection_str="https://localhost:5080",
        user="***",
        password="***",
    )  # nosec
    check.is_true(openobserve_driver.connected)


@patch(OPENOBSERVE_SVC, OpenObserveService)
def test_openobserve_connect_errors():
    """Check connect failure errors."""
    openobserve_driver = OpenObserveDriver()
    check.is_true(openobserve_driver.loaded)

    print("connected", openobserve_driver.connected)
    with pytest.raises(MsticpyConnectionError) as mp_ex:
        openobserve_driver.connect(
            connection_str="AuthError", user="***", password="***"
        )  # nosec
        print("connected", openobserve_driver.connected)
    check.is_false(openobserve_driver.connected)
    check.is_in("OpenObserve connection", mp_ex.value.args)
    check.is_true(any(arg for arg in mp_ex.value.args if "Error" in arg))

    openobserve_driver = OpenObserveDriver()
    print("connected", openobserve_driver.connected)
    with pytest.raises(MsticpyConnectionError) as mp_ex:
        openobserve_driver.connect(
            connection_str="HTTPError", user="***", password="***"
        )  # nosec
        print("connected", openobserve_driver.connected)
    check.is_false(openobserve_driver.connected)
    check.is_in("OpenObserve connection", mp_ex.value.args)
    check.is_true(any(arg for arg in mp_ex.value.args if "Error" in arg))

    openobserve_driver = OpenObserveDriver()
    print("connected", openobserve_driver.connected)
    with pytest.raises(MsticpyConnectionError) as mp_ex:
        openobserve_driver.connect(
            connection_str="OtherError", user="***", password="***"
        )  # nosec
        print("connected", openobserve_driver.connected)
    check.is_false(openobserve_driver.connected)
    check.is_in("OpenObserve connection", mp_ex.value.args)
    check.is_true(any(arg for arg in mp_ex.value.args if "Error connecting" in arg))


@patch(OPENOBSERVE_SVC, OpenObserveService)
def test_openobserve_query_no_connect():
    """Check query fails when not connected true."""
    openobserve_driver = OpenObserveDriver()
    # trying to get these before connecting should throw
    with pytest.raises(MsticpyNotConnectedError) as mp_ex:
        openobserve_driver.query("some query")
    check.is_false(openobserve_driver.connected)
    check.is_in("not connected to OpenObserve.", mp_ex.value.args)


@pytest.fixture
@patch(OPENOBSERVE_SVC, OpenObserveService)
def openobserve_drv():
    """Return openobserve driver with short timeout."""
    openobserve_driver = OpenObserveDriver()
    openobserve_driver.checkinterval = 0.1
    openobserve_driver.timeout = 1
    openobserve_driver.connect(
        connection_str="https://localhost:5080",
        user="***",
        password="***",
    )  # nosec
    check.is_true(openobserve_driver.connected)
    return openobserve_driver


_QUERY_TESTS = [
    ("Wait", 1),
    ("Cancelled", 0),
    ("MessageSuccess", 1),
    ("MessageFail", "Failed to get job messages: Message job failed"),
    ("RecordSuccess", 10),
    ("RecordSuccess | count records", 10),
    ("RecordFail", "Failed to get job messages: Message job failed"),
    (
        "RecordFail | count records",
        "Failed to get search records (paging i 0 / 10): Record job failed",
    ),
    ("Timeout", 0),
    ("Failjob", "OpenObserve submit search_job"),
]


# pylint: disable=redefined-outer-name
@patch(OPENOBSERVE_SVC, OpenObserveService)
@pytest.mark.parametrize(("query", "expected"), _QUERY_TESTS)
def test_openobserve_query(openobserve_drv, query, expected):
    """Check queries with different outcomes."""
    end = datetime.now(UTC)
    start = end - timedelta(1)
    if query in ("MessageFail", "RecordFail", "Failjob", "RecordFail | count records"):
        with pytest.raises(MsticpyConnectionError) as mp_ex:
            df_result = openobserve_drv.query(
                query, start=start, end=end, timeout=1, verbosity=4
            )
        check.is_in(expected, mp_ex.value.args)
    else:
        df_result = openobserve_drv.query(query, start=start, end=end, timeout=1)
        check.is_instance(df_result, pd.DataFrame)
        check.equal(len(df_result), expected)


_TIMEOUT_PARAMS: Dict[str, Any] = {
    "timeout": 1,
}

_DEF_DATE_PARAMS: Dict[str, Any] = {
    "start": datetime.now(UTC) - timedelta(1),
    "end": datetime.now(UTC),
    **_TIMEOUT_PARAMS,
}


_PARAM_TESTS = [
    pytest.param({"days": 5, **_TIMEOUT_PARAMS}, True, id="days"),
    pytest.param(
        {"start": datetime.now(UTC) - timedelta(1), **_TIMEOUT_PARAMS}, True, id="start"
    ),
    pytest.param(_DEF_DATE_PARAMS, True, id="start/end"),
    pytest.param(
        {"start_time": datetime.now(UTC) - timedelta(1), "end_time": datetime.now(UTC)},
        True,
        id="start_time/end_time",
    ),
    pytest.param(
        {
            **_DEF_DATE_PARAMS,
            "limit": 10,
            "timezone": "PST",
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


@patch(OPENOBSERVE_SVC, OpenObserveService)
@pytest.mark.parametrize("params, expected", _PARAM_TESTS)
def test_openobserve_query_params(openobserve_drv, params, expected):
    """Check queries with different parameters."""
    if expected:
        df_result = openobserve_drv.query("RecordSuccess", **params)
        check.is_instance(df_result, pd.DataFrame)
        check.equal(len(df_result), 10)
    else:
        with pytest.raises(MsticpyUserError) as mp_ex:
            df_result = openobserve_drv.query("RecordSuccess", **params)
        check.is_in("Missing parameter.", mp_ex.value.args)


@patch(OPENOBSERVE_SVC, OpenObserveService)
@pytest.mark.parametrize("ext", ("xlsx", "csv"))
def test_openobserve_query_export(openobserve_drv, tmpdir, ext):
    """Check queries with different parameters."""
    exp_file = f"openobserve_test.{ext}"
    f_path = tmpdir.join(exp_file)
    params = {
        "exporting": True,
        "export_path": str(f_path),
        "verbosity": 5,
        **_DEF_DATE_PARAMS,
    }
    df_result = openobserve_drv.query("RecordSuccess", **params)
    check.is_instance(df_result, pd.DataFrame)
    check.equal(len(df_result), 10)
    check.is_true(Path(f_path).is_file())


# @pytest.mark.skip
# def test_live_connect():
#     """Use this to do live testing."""
#     openobserve_driver = OpenObserveDriver()
#     www = "user"
#     openobserve_driver.connect(url=www, user="***", password="***")  # nosec

#     query = """SELECT _timestamp, host_name, EventID, EventDescription, User, process FROM botsv2 """
#             """WHERE source="WinEventLog:Microsoft-Windows-Sysmon/Operational" LIMIT 10"""
#     res_df = openobserve_driver.query(
#         query, start_time="2017-08-25T00:00:00", end_time="2017-08-26T00:00:00"
#     )
#     check.is_not_none(res_df)

#     query0 = """SELECT _timestamp, host_name, EventID, EventDescription, User, process FROM botsv2 """
#             """WHERE source="WinEventLog:Microsoft-Windows-Sysmon/Operational" LIMIT 10"""
#     """
#     res_df = openobserve_driver.query(query0, start_time="2020-08-25T00:00:00")
#     check.is_instance(res_df, list)
#     check.is_false(res_df)

#     query1 = """SELECT _timestamp, TotalBytesSent FROM blackhat """
#             """WHERE sourcetype=network LIMIT 10"""
#     res_df = openobserve_driver.query(query1)
#     check.is_not_none(res_df)
