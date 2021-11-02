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

from adal.adal_error import AdalError
from Kqlmagic.kql_response import KqlError
from Kqlmagic.kql_engine import KqlEngineError
from Kqlmagic.my_aad_helper import AuthenticationError

# from Kqlmagic import kql as kql_exec

from msticpy.data.drivers import kql_driver
from msticpy.common.exceptions import (
    MsticpyKqlConnectionError,
    MsticpyNotConnectedError,
    MsticpyNoDataSourceError,
    MsticpyDataQueryError,
)
from msticpy.data.drivers import import_driver
from msticpy.data.query_defns import DataEnvironment

KqlDriver = import_driver(DataEnvironment.AzureSentinel)

# from msticpy.data.drivers.kql_driver import KqlDriver
GET_IPYTHON_PATCH = KqlDriver.__module__ + ".get_ipython"


# pylint: disable=too-many-branches, too-many-return-statements
# pylint: disable=no-self-use, redefined-outer-name


class KqlResultTest:
    """Test Kql result class."""

    def __init__(self, code=0, partial=False, status="success"):
        """Create instance."""
        self.completion_query_info = {"StatusCode": code, "StatusDescription": status}
        self.is_partial_table = partial

    def to_dataframe(self):
        """Convert dataframe."""
        return pd.DataFrame()


class _MockIPython:
    """IPython get_ipython mock."""

    def find_magic(self, magic):
        """Return None if magic isn't == kql."""
        if magic == "kql":
            return "Kqlmagic"
        return None

    def run_line_magic(self, magic, line):
        """Mock run line magic."""
        return self._run_magic(magic, line)

    def run_cell_magic(self, magic, line, cell):
        """Mock run cell magic."""
        content = cell or line
        return self._run_magic(magic, content)

    @staticmethod  # noqa: MC0001
    def _run_magic(magic, content):
        if magic == "reload_ext":
            return None
        if magic == "config":
            if "=" in content:
                return "dummy_setting"
            return True

        check.equal(magic, "kql")
        return kql_exec(content)


def kql_exec(content):
    """Mock kql_exec function."""
    if "--config" in content:
        if "=" in content:
            conf_item, conf_value = content.replace("--config", "").strip().split("=")
            return {conf_item: conf_value}
        _, conf_item = content.split()
        return {conf_item: True}

    if "KqlErrorUnk" in content:
        resp = '{"error": {"code": "UnknownError"}}'
        raise KqlError(http_response=resp, message=resp)
    if "KqlErrorWS" in content:
        resp = '{"error": {"code": "WorkspaceNotFoundError"}}'
        raise KqlError(http_response=resp, message=resp)
    if "KqlEngineError" in content:
        raise KqlEngineError("Test Error")
    if "AdalErrorUnk" in content:
        resp = {"error_description": "unknown error"}
        raise AdalError("Test Error", error_response=resp)
    if "AdalErrorNR" in content:
        raise AdalError("Test Error")
    if "AdalErrorPoll" in content:
        raise AdalError("Unexpected polling state code_expired")
    if "AuthenticationError" in content:
        raise AuthenticationError("Test Error")

    if content == "--schema":
        return {
            "table1": {"field1": int, "field2": str},
            "table2": {"field1": int, "field2": str},
        }

    if "query_partial" in content:
        return KqlResultTest(code=0, partial=True, status="partial")
    if "query_failed" in content:
        return KqlResultTest(code=1, partial=False, status="failed")

    return KqlResultTest(code=0, partial=False, status="success")


KQL_EXEC_PATCH = (kql_driver, "kql_exec", kql_exec)


@patch(GET_IPYTHON_PATCH)
@patch.object(*KQL_EXEC_PATCH)
def test_kql_load(get_ipython):
    """Check loaded true."""
    get_ipython.return_value = _MockIPython()

    kql_driver = KqlDriver()
    check.is_true(kql_driver.loaded)

    kql_driver = KqlDriver(connection_str="la://connection")
    check.is_true(kql_driver.loaded)
    check.is_true(kql_driver.connected)


@patch(GET_IPYTHON_PATCH)
@patch.object(*KQL_EXEC_PATCH)
def test_kql_connect(get_ipython):
    """Check loaded true."""
    get_ipython.return_value = _MockIPython()
    kql_driver = KqlDriver()
    check.is_true(kql_driver.loaded)

    kql_driver.connect(connection_str="la://connection")
    check.is_true(kql_driver.connected)


@patch(GET_IPYTHON_PATCH)
@patch.object(*KQL_EXEC_PATCH)
def test_kql_connect_no_cs(get_ipython):
    """Check loaded true."""
    get_ipython.return_value = _MockIPython()
    kql_driver = KqlDriver()
    check.is_true(kql_driver.loaded)
    with pytest.raises(MsticpyKqlConnectionError) as mp_ex:
        kql_driver.connect()
    check.is_in("no connection string", mp_ex.value.args)


@patch(GET_IPYTHON_PATCH)
@patch.object(*KQL_EXEC_PATCH)
def test_kql_connect_kql_exceptions(get_ipython):
    """Check loaded true."""
    get_ipython.return_value = _MockIPython()
    kql_driver = KqlDriver()

    with pytest.raises(MsticpyKqlConnectionError) as mp_ex:
        kql_driver.connect(connection_str="la://connection+KqlErrorUnk")
    check.is_in("Kql response error", mp_ex.value.args)
    check.is_false(kql_driver.connected)

    with pytest.raises(MsticpyKqlConnectionError) as mp_ex:
        kql_driver.connect(
            connection_str="la://connection.workspace('1234').tenant(KqlErrorWS)"
        )
    check.is_in("unknown workspace", mp_ex.value.args)
    check.is_false(kql_driver.connected)

    with pytest.raises(MsticpyKqlConnectionError) as mp_ex:
        kql_driver.connect(
            connection_str="la://connection.workspace('1234').tenant(KqlEngineError)"
        )
    check.is_in("kql connection error", mp_ex.value.args)
    check.is_false(kql_driver.connected)


@patch(GET_IPYTHON_PATCH)
@patch.object(*KQL_EXEC_PATCH)
def test_kql_connect_adal_exceptions(get_ipython):
    """Check loaded true."""
    get_ipython.return_value = _MockIPython()
    kql_driver = KqlDriver()

    with pytest.raises(MsticpyKqlConnectionError) as mp_ex:
        kql_driver.connect(connection_str="la://connection+AdalErrorUnk")
    check.is_in("could not authenticate to tenant", mp_ex.value.args)
    check.is_false(kql_driver.connected)

    with pytest.raises(MsticpyKqlConnectionError) as mp_ex:
        kql_driver.connect(connection_str="la://connection+AdalErrorNR")
    check.is_in("could not authenticate to tenant", mp_ex.value.args)
    check.is_in("Full error", str(mp_ex.value.args))
    check.is_false(kql_driver.connected)

    with pytest.raises(MsticpyKqlConnectionError) as mp_ex:
        kql_driver.connect(connection_str="la://connection+AdalErrorPoll")
    check.is_in("authentication timed out", mp_ex.value.args)
    check.is_false(kql_driver.connected)


@patch(GET_IPYTHON_PATCH)
@patch.object(*KQL_EXEC_PATCH)
def test_kql_connect_authn_exceptions(get_ipython):
    """Check loaded true."""
    get_ipython.return_value = _MockIPython()
    kql_driver = KqlDriver()

    with pytest.raises(MsticpyKqlConnectionError) as mp_ex:
        kql_driver.connect(connection_str="la://connection+AuthenticationError")
    check.is_in("authentication failed", mp_ex.value.args)
    check.is_false(kql_driver.connected)


@patch(GET_IPYTHON_PATCH)
@patch.object(*KQL_EXEC_PATCH)
def test_kql_schema(get_ipython):
    """Check loaded true."""
    get_ipython.return_value = _MockIPython()
    kql_driver = KqlDriver()
    kql_driver.connect(connection_str="la://connection")

    check.is_in("table1", kql_driver.schema)
    check.is_in("table2", kql_driver.schema)
    check.is_in("field1", kql_driver.schema["table1"])


@patch(GET_IPYTHON_PATCH)
@patch.object(*KQL_EXEC_PATCH)
def test_kql_query_not_connected(get_ipython):
    """Check loaded true."""
    get_ipython.return_value = _MockIPython()
    kql_driver = KqlDriver()

    with pytest.raises(MsticpyNotConnectedError) as mp_ex:
        kql_driver.query("test")
    check.is_in("not connected to a Workspace", mp_ex.value.args)
    check.is_false(kql_driver.connected)


@patch(GET_IPYTHON_PATCH)
@patch.object(*KQL_EXEC_PATCH)
def test_kql_query_failed(get_ipython):
    """Check loaded true."""
    get_ipython.return_value = _MockIPython()
    kql_driver = KqlDriver()
    kql_driver.connect(connection_str="la://connection")

    with pytest.raises(MsticpyDataQueryError) as mp_ex:
        kql_driver.query("test query_failed")
    arg_str = "\n".join(str(arg) for arg in mp_ex.value.args)
    check.is_in("Query:", arg_str)
    check.is_in("test query_failed", arg_str)
    check.is_in("Query failed", arg_str)
    check.is_in(
        "https://msticpy.readthedocs.io/en/latest/DataAcquisition.html", arg_str
    )


@patch(GET_IPYTHON_PATCH)
@patch.object(*KQL_EXEC_PATCH)
def test_kql_query_success(get_ipython):
    """Check loaded true."""
    get_ipython.return_value = _MockIPython()
    kql_driver = KqlDriver()
    kql_driver.connect(connection_str="la://connection")

    result_df = kql_driver.query("test query")
    check.is_instance(result_df, pd.DataFrame)


@patch(GET_IPYTHON_PATCH)
@patch.object(*KQL_EXEC_PATCH)
def test_kql_query_partial(get_ipython):
    """Check loaded true."""
    get_ipython.return_value = _MockIPython()
    kql_driver = KqlDriver()
    kql_driver.connect(connection_str="la://connection")

    output = io.StringIO()
    with redirect_stdout(output):
        result_df = kql_driver.query("test query_partial")
    check.is_instance(result_df, pd.DataFrame)
    check.is_in("Warning - query returned partial", output.getvalue())


@patch(GET_IPYTHON_PATCH)
@patch.object(*KQL_EXEC_PATCH)
def test_kql_query_no_table(get_ipython):
    """Check loaded true."""
    get_ipython.return_value = _MockIPython()
    kql_driver = KqlDriver()
    kql_driver.connect(connection_str="la://connection")

    with pytest.raises(MsticpyNoDataSourceError) as mp_ex:
        query_source = {"args.table": "table3"}
        kql_driver.query("test query", query_source=query_source)

    check.is_in("table3 not found.", mp_ex.value.args)
