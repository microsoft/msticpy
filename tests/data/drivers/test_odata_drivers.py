# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Miscellaneous data provider driver tests."""
from unittest.mock import patch
from unittest.mock import Mock

import pytest
import pytest_check as check

from msticpy.data import DataEnvironment, QueryProvider
from msticpy.data.drivers import import_driver

from msticpy.data.drivers.mdatp_driver import MDATPDriver
from msticpy.data.drivers.security_graph_driver import SecurityGraphDriver

from ...unit_test_lib import get_test_data_path, custom_mp_config

_RGE_IMP_OK = False
try:
    from msticpy.data.drivers.resource_graph_driver import ResourceGraphDriver

    _RGE_IMP_OK = True
except ImportError:
    pass


MP_PATH = str(get_test_data_path().parent.joinpath("msticpyconfig-test.yaml"))
# pylint: disable=protected-access


_JSON_RESP = {
    "token_type": "Bearer",
    "expires_in": "3599",
    "ext_expires_in": "3599",
    "expires_on": "1582155956",
    "not_before": "1582152056",
    "resource": "https://api.securitycenter.windows.com",
    "access_token": None,
}

_MDEF_TESTS = [
    ("MDE", "https://api.securitycenter.microsoft.com/"),
    ("MDATP", "https://api.securitycenter.microsoft.com/"),
    ("M365D", "https://api.security.microsoft.com/"),
]


@pytest.mark.parametrize("env, api", _MDEF_TESTS)
def test_MDE_driver(env, api):
    """Test class MDE driver."""
    env_enum = DataEnvironment.parse(env)
    driver_cls = import_driver(env_enum)
    with custom_mp_config(MP_PATH):
        driver = driver_cls(data_environment=env_enum)
    check.is_instance(driver, MDATPDriver)
    check.equal(driver.api_root, api)

    with custom_mp_config(MP_PATH):
        qry_prov = QueryProvider(env)
    driver = qry_prov._query_provider
    check.is_instance(driver, MDATPDriver)
    check.greater_equal(len(qry_prov.list_queries()), 40)


# Test helper functions for MDE tests
def _mde_pre_checks(mde_drv, api):
    check.is_instance(mde_drv, MDATPDriver)
    check.is_false(mde_drv.connected)
    check.equal(mde_drv.api_root, f"https://api.{api}.microsoft.com/")


def _mde_post_checks(mde_drv, api, httpx):
    check.equal(mde_drv.aad_token, _AUTH_RESP["access_token"])
    httpx.post.assert_called_once()
    check.equal(
        httpx.post.call_args[1]["url"],
        "https://login.microsoftonline.com/8360dd21-0294-4240-9128-89611f415c53/oauth2/token",
    )
    check.is_instance(
        httpx.post.call_args[1]["content"],
        bytes,
    )
    check.equal(mde_drv.request_uri, f"https://api.{api}.microsoft.com/api")


def _mde_create_mock(httpx):
    response = Mock()
    response.json.return_value = _AUTH_RESP.copy()
    httpx.post.return_value = response


@pytest.mark.parametrize(
    "env, api",
    [("MDATP", "securitycenter"), ("MDE", "securitycenter"), ("M365D", "security")],
)
@patch("msticpy.data.drivers.odata_driver.httpx")
def test_mde_connect(httpx, env, api):
    """Test security graph driver."""
    driver_cls = import_driver(DataEnvironment.parse(env))
    with custom_mp_config(MP_PATH):
        mde_drv = driver_cls(data_environment=DataEnvironment.parse(env))
    _mde_pre_checks(mde_drv=mde_drv, api=api)
    _mde_create_mock(httpx)
    with custom_mp_config(MP_PATH):
        mde_drv.connect()
    _mde_post_checks(mde_drv, api, httpx)


_CONSTRING = "client_id=1234;tenant_id=8360dd21-0294-4240-9128-89611f415c53;client_secret=[PLACEHOLDER]"
_MDE_CONNECT_STR = [
    ("MDATP", "securitycenter", _CONSTRING),
    ("MDE", "securitycenter", _CONSTRING),
    ("M365D", "security", _CONSTRING),
]


@pytest.mark.parametrize("env, api, con_str", _MDE_CONNECT_STR)
@patch("msticpy.data.drivers.odata_driver.httpx")
def test_mde_connect_str(httpx, env, api, con_str):
    """Test security graph driver."""
    driver_cls = import_driver(DataEnvironment.parse(env))
    with custom_mp_config(MP_PATH):
        mde_drv = driver_cls(data_environment=DataEnvironment.parse(env))
    _mde_pre_checks(mde_drv=mde_drv, api=api)
    _mde_create_mock(httpx)
    with custom_mp_config(MP_PATH):
        mde_drv.connect(con_str)
    _mde_post_checks(mde_drv, api, httpx)


_PARAMS = {
    "client_id": "1234",
    "tenant_id": "8360dd21-0294-4240-9128-89611f415c53",
    "client_secret": "[PLACEHOLDER]",
}
_MDE_CONNECT_PARAMS = [
    ("MDATP", "securitycenter", _PARAMS),
    ("MDE", "securitycenter", _PARAMS),
    ("M365D", "security", _PARAMS),
]


@pytest.mark.parametrize("env, api, params", _MDE_CONNECT_PARAMS)
@patch("msticpy.data.drivers.odata_driver.httpx")
def test_mde_connect_params(httpx, env, api, params):
    """Test security graph driver."""
    driver_cls = import_driver(DataEnvironment.parse(env))
    with custom_mp_config(MP_PATH):
        mde_drv = driver_cls(data_environment=DataEnvironment.parse(env))
    _mde_pre_checks(mde_drv=mde_drv, api=api)
    _mde_create_mock(httpx)
    with custom_mp_config(MP_PATH):
        mde_drv.connect(**params)
    _mde_post_checks(mde_drv, api, httpx)


_AUTH_RESP = {"access_token": "123456789"}


@patch("msticpy.data.drivers.odata_driver.httpx")
def test_security_graph_connect(httpx):
    """Test security graph driver."""
    driver_cls = import_driver(DataEnvironment.SecurityGraph)
    with custom_mp_config(MP_PATH):
        sec_graph = driver_cls()
    assert isinstance(sec_graph, SecurityGraphDriver)
    check.is_false(sec_graph.connected)
    check.equal(sec_graph.api_root, "https://graph.microsoft.com/")

    response = Mock()
    response.json.return_value = _AUTH_RESP.copy()
    httpx.post.return_value = response
    with custom_mp_config(MP_PATH):
        sec_graph.connect()
    check.equal(sec_graph.aad_token, _AUTH_RESP["access_token"])
    httpx.post.assert_called_once()
    check.equal(
        httpx.post.call_args[1]["url"],
        "https://login.microsoftonline.com/8360dd21-0294-4240-9128-89611f415c53/oauth2/v2.0/token",
    )
    check.is_instance(
        httpx.post.call_args[1]["content"],
        bytes,
    )
    check.equal(sec_graph.request_uri, "https://graph.microsoft.com/v1.0")


@pytest.mark.skipif(not _RGE_IMP_OK, reason="Partial msticpy install")
def test_ResourceGraph():
    """Test resource graph driver."""
    driver_cls = import_driver(DataEnvironment.ResourceGraph)
    resource_graph = driver_cls()
    assert isinstance(resource_graph, ResourceGraphDriver)
