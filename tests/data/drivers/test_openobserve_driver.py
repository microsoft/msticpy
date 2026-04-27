# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""OpenObserve query test class."""
# pylint: disable=missing-function-docstring,redefined-outer-name,unused-argument
from datetime import datetime, timedelta, timezone
from pathlib import Path
from unittest.mock import MagicMock

import pandas as pd
import pytest
import pytest_check as check

from msticpy.common.exceptions import (
    MsticpyConnectionError,
    MsticpyNotConnectedError,
    MsticpyUserConfigError,
    MsticpyUserError,
)

_OPEN_OBSERVE_NOT_LOADED = True
try:
    import httpx

    from msticpy.data.drivers.openobserve_driver import OpenObserveDriver

    _OPEN_OBSERVE_NOT_LOADED = False
except ImportError:
    pass

UTC = timezone.utc
OO_HOST = OO_USER = OO_PASS = "MOCK_INPUT"

_SEARCH_RESULT_DF = pd.DataFrame(
    [
        {
            "_p": "F",
            "_timestamp": 1674213225158000,
            "log": (
                "[2023-01-20T11:13:45Z INFO  actix_web::middleware::logger] "
                '10.2.80.192 "POST /api/demo/_bulk HTTP/1.1" 200 68 "-" '
                '"go-resty/2.7.0 (https://github.com/go-resty/resty)"'
                " 0.001074"
            ),
            "stream": "stderr",
        }
    ]
)


@pytest.mark.skipif(_OPEN_OBSERVE_NOT_LOADED, reason="OpenObserve driver not installed")
def test_openobserve_connect_no_params():
    """Check failure with no args."""
    openobserve_driver = OpenObserveDriver()
    check.is_true(openobserve_driver.loaded)

    with pytest.raises(MsticpyUserConfigError) as mp_ex:
        openobserve_driver.connect()
    check.is_false(openobserve_driver.connected)
    check.is_in("no OpenObserve connection parameters", mp_ex.value.args)


@pytest.mark.skipif(_OPEN_OBSERVE_NOT_LOADED, reason="OpenObserve driver not installed")
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


@pytest.mark.skipif(_OPEN_OBSERVE_NOT_LOADED, reason="OpenObserve driver not installed")
def test_openobserve_connect_errors():
    """Check connect failure errors."""
    openobserve_driver = OpenObserveDriver()
    check.is_true(openobserve_driver.loaded)
    openobserve_driver.connect(
        connection_str="invalid", user="***", password="***"
    )  # nosec
    openobserve_driver.service.search2df = MagicMock(
        side_effect=httpx.UnsupportedProtocol(
            "Request URL is missing an 'http://' or 'https://' protocol."
        ),
    )
    with pytest.raises(
        MsticpyConnectionError,
        match="Communication error connecting to OpenObserve:",
    ):
        openobserve_driver.query('select * from "default"', days=1)

    openobserve_driver = OpenObserveDriver()
    openobserve_driver.connect(
        connection_str="https://nonexistent.example.com",
        user="***",
        password="***",
    )  # nosec
    openobserve_driver.service.search2df = MagicMock(
        side_effect=httpx.ConnectError(
            "[Errno -5] No address associated with hostname"
        ),
    )
    with pytest.raises(
        MsticpyConnectionError,
        match="Communication error connecting to OpenObserve:",
    ):
        openobserve_driver.query('select * from "default"', days=1)


@pytest.mark.skipif(_OPEN_OBSERVE_NOT_LOADED, reason="OpenObserve driver not installed")
def test_openobserve_query_no_connect():
    """Check query fails when not connected true."""
    openobserve_driver = OpenObserveDriver()
    # trying to get these before connecting should throw
    with pytest.raises(MsticpyNotConnectedError) as mp_ex:
        openobserve_driver.query("some query")
    check.is_false(openobserve_driver.connected)
    check.is_in("not connected to OpenObserveDriver.", mp_ex.value.args)


@pytest.mark.skipif(_OPEN_OBSERVE_NOT_LOADED, reason="OpenObserve driver not installed")
def test_openobserve_query():
    """Check queries with different outcomes."""
    openobserve_drv = OpenObserveDriver()
    openobserve_drv.connect(connection_str=OO_HOST, user=OO_USER, password=OO_PASS)
    openobserve_drv.service.search2df = MagicMock(
        return_value=_SEARCH_RESULT_DF.copy()
    )
    end = datetime.now(UTC)
    start = end - timedelta(1)

    df_result = openobserve_drv.query(
        "RandomQuery", start=start, end=end, timeout=1, verbosity=4
    )
    check.is_instance(df_result, pd.DataFrame)
    check.equal(len(df_result), 1)


@pytest.mark.skipif(_OPEN_OBSERVE_NOT_LOADED, reason="OpenObserve driver not installed")
def test_openobserve_query_params():
    """Check queries with different parameters."""
    openobserve_drv = OpenObserveDriver()
    openobserve_drv.connect(connection_str=OO_HOST, user=OO_USER, password=OO_PASS)
    openobserve_drv.service.search2df = MagicMock(
        return_value=_SEARCH_RESULT_DF.copy()
    )

    df_result = openobserve_drv.query("RecordSuccess", days=1)
    check.is_instance(df_result, pd.DataFrame)
    check.equal(len(df_result), 1)

    with pytest.raises(MsticpyUserError) as mp_ex:
        df_result = openobserve_drv.query("RecordSuccess")
    check.is_in("Missing parameter.", mp_ex.value.args)


@pytest.mark.skipif(_OPEN_OBSERVE_NOT_LOADED, reason="OpenObserve driver not installed")
def test_openobserve_query_export(tmpdir):
    """Check queries with different parameters."""
    openobserve_drv = OpenObserveDriver()
    openobserve_drv.connect(connection_str=OO_HOST, user=OO_USER, password=OO_PASS)
    openobserve_drv.service.search2df = MagicMock(
        return_value=_SEARCH_RESULT_DF.copy()
    )
    ext = "csv"
    exp_file = f"openobserve_test.{ext}"
    f_path = tmpdir.join(exp_file)
    params = {
        "exporting": True,
        "export_path": str(f_path),
        "verbosity": 5,
    }
    df_result = openobserve_drv.query("RecordSuccess", days=1, **params)
    check.is_instance(df_result, pd.DataFrame)
    check.equal(len(df_result), 1)
    check.is_true(Path(f_path).is_file())
