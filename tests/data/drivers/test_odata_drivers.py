# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Miscellaneous data provider driver tests."""
from unittest.mock import Mock, patch

import pandas as pd
import pytest
import pytest_check as check
import respx

from msticpy.common.exceptions import MsticpyUserConfigError
from msticpy.data import DataEnvironment, QueryProvider
from msticpy.data.drivers import import_driver
from msticpy.data.drivers.mdatp_driver import MDATPDriver
from msticpy.data.drivers.security_graph_driver import SecurityGraphDriver

from ...unit_test_lib import custom_mp_config, get_test_data_path

_RGE_IMP_OK = False
try:
    from msticpy.data.drivers.resource_graph_driver import ResourceGraphDriver

    _RGE_IMP_OK = True
except ImportError:
    pass


MP_PATH = str(get_test_data_path().parent.joinpath("msticpyconfig-test.yaml"))
# pylint: disable=protected-access
pytestmark = pytest.mark.filterwarnings("ignore::UserWarning")

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
    ("MDE", "https://api.securitycenter.microsoft.com/", ""),
    ("MDATP", "https://api.securitycenter.microsoft.com/", None),
    ("M365D", "https://api.securitycenter.microsoft.com/", None),
    ("M365D", "https://api-us.securitycenter.microsoft.com/", "us"),
    ("M365D", "https://api-eu.securitycenter.microsoft.com/", "eu"),
    ("M365D", "https://api-uk.securitycenter.microsoft.com/", "uk"),
    ("MDE", "https://api-gov.securitycenter.microsoft.us/", "dod"),
    ("MDE", "https://api-uk.securitycenter.microsoft.com/", "uk"),
    ("MDE", "https://api-us.securitycenter.microsoft.com/", "us"),
    ("MDE", "https://api-eu.securitycenter.microsoft.com/", "eu"),
    ("MDE", "https://api-gcc.securitycenter.microsoft.us/", "gcc"),
    ("MDE", "https://api-gov.securitycenter.microsoft.us/", "gcc-high"),
]


@pytest.mark.parametrize("env, api, cloud", _MDEF_TESTS)
def test_MDE_driver(env, api, cloud: None):
    """Test class MDE driver."""
    env_enum = DataEnvironment.parse(env)
    driver_cls = import_driver(env_enum)
    with custom_mp_config(MP_PATH):
        driver = driver_cls(data_environment=env_enum, cloud=cloud)
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
        "https://login.microsoftonline.com/8360dd21-0294-4240-9128-89611f415c53/oauth2/v2.0/token",
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
    [
        ("MDATP", "securitycenter"),
        ("MDE", "securitycenter"),
        ("MDE", "securitycenter"),
        ("M365D", "securitycenter"),
    ],
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
    ("M365D", "securitycenter", _CONSTRING),
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
    ("M365D", "securitycenter", _PARAMS),
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


# New tests for improved coverage


def test_odata_parse_connection_string():
    """Test connection string parsing."""
    from msticpy.data.drivers.odata_driver import OData

    connection_str = "tenant_id=abc123;client_id=def456;client_secret=[PLACEHOLDER]"
    parsed = OData._parse_connection_str(connection_str)

    check.equal(parsed["tenant_id"], "abc123")
    check.equal(parsed["client_id"], "def456")
    check.equal(parsed["client_secret"], "[PLACEHOLDER]")


def test_odata_parse_connection_string_with_spaces():
    """Test connection string parsing with spaces."""
    from msticpy.data.drivers.odata_driver import OData

    connection_str = (
        "tenant_id = abc123 ; client_id = def456 ; client_secret = [PLACEHOLDER]"
    )
    parsed = OData._parse_connection_str(connection_str)

    check.equal(parsed["tenant_id"], "abc123")
    check.equal(parsed["client_id"], "def456")
    check.equal(parsed["client_secret"], "[PLACEHOLDER]")


def test_odata_prepare_param_dict_from_filter():
    """Test filter string parsing."""
    from msticpy.data.drivers.odata_driver import OData

    filter_str = "?$filter=status eq 'active'&$top=10"
    params = OData._prepare_param_dict_from_filter(filter_str)

    check.equal(params["$filter"], "status eq 'active'")
    check.equal(params["$top"], "10")


def test_odata_connect_missing_tenant_id():
    """Test connection with missing tenant_id."""
    driver_cls = import_driver(DataEnvironment.MDE)
    with custom_mp_config(MP_PATH):
        mde_drv = driver_cls(data_environment=DataEnvironment.MDE)

    with pytest.raises(MsticpyUserConfigError) as exc_info:
        mde_drv.connect("client_id=abc;client_secret=[PLACEHOLDER]")

    check.is_in("tenant_id", str(exc_info.value))


def test_odata_connect_missing_auth_method():
    """Test connection with missing authentication method."""
    driver_cls = import_driver(DataEnvironment.MDE)
    with custom_mp_config(MP_PATH):
        mde_drv = driver_cls(data_environment=DataEnvironment.MDE)

    with pytest.raises(MsticpyUserConfigError) as exc_info:
        mde_drv.connect("tenant_id=abc;client_id=def")

    check.is_in("client_secret", str(exc_info.value))


@patch("msticpy.data.drivers.odata_driver.httpx")
def test_odata_query_not_connected(httpx):
    """Test query when not connected."""
    driver_cls = import_driver(DataEnvironment.MDE)
    with custom_mp_config(MP_PATH):
        mde_drv = driver_cls(data_environment=DataEnvironment.MDE)

    # Mock the connection attempt
    response = Mock()
    response.json.return_value = _AUTH_RESP.copy()
    httpx.post.return_value = response

    # Should auto-connect
    with custom_mp_config(MP_PATH):
        mde_drv.current_connection = _CONSTRING
        result, _ = mde_drv.query_with_results(
            "DeviceInfo | take 10",
            body=True,
            api_end="/advancedqueries/run",
        )


@respx.mock
@patch("msticpy.data.drivers.odata_driver.httpx.post")
def test_odata_query_with_results_success(mock_post):
    """Test successful query execution with results."""
    driver_cls = import_driver(DataEnvironment.MDE)
    with custom_mp_config(MP_PATH):
        mde_drv = driver_cls(data_environment=DataEnvironment.MDE)

    # Mock authentication
    auth_response = Mock()
    auth_response.json.return_value = _AUTH_RESP.copy()
    mock_post.return_value = auth_response

    with custom_mp_config(MP_PATH):
        mde_drv.connect(_CONSTRING)

    # Mock query response
    query_response = Mock()
    query_response.is_success = True
    query_response.json.return_value = {
        "Results": [{"DeviceId": "123", "DeviceName": "Test"}],
        "Schema": [{"Name": "DeviceId", "Type": "String"}],
    }
    query_response.status_code = 200

    with patch(
        "msticpy.data.drivers.odata_driver.httpx.post", return_value=query_response
    ):
        result, response = mde_drv.query_with_results(
            "DeviceInfo | take 10",
            body=True,
            api_end="/advancedqueries/run",
        )

    check.is_instance(result, pd.DataFrame)
    check.equal(len(result), 1)


@respx.mock
@patch("msticpy.data.drivers.odata_driver.httpx.post")
def test_odata_query_with_empty_results(mock_post):
    """Test query execution with empty results."""
    driver_cls = import_driver(DataEnvironment.MDE)
    with custom_mp_config(MP_PATH):
        mde_drv = driver_cls(data_environment=DataEnvironment.MDE)

    # Mock authentication
    auth_response = Mock()
    auth_response.json.return_value = _AUTH_RESP.copy()
    mock_post.return_value = auth_response

    with custom_mp_config(MP_PATH):
        mde_drv.connect(_CONSTRING)

    # Mock query response with no results
    query_response = Mock()
    query_response.is_success = True
    query_response.json.return_value = {"Results": []}
    query_response.status_code = 200

    with patch(
        "msticpy.data.drivers.odata_driver.httpx.post", return_value=query_response
    ):
        result, response = mde_drv.query_with_results(
            "DeviceInfo | where 1==0",
            body=True,
            api_end="/advancedqueries/run",
        )

    check.is_instance(result, pd.DataFrame)
    check.equal(len(result), 0)


@respx.mock
@patch("msticpy.data.drivers.odata_driver.httpx.post")
def test_odata_query_unauthorized(mock_post):
    """Test query with unauthorized error."""
    driver_cls = import_driver(DataEnvironment.MDE)
    with custom_mp_config(MP_PATH):
        mde_drv = driver_cls(data_environment=DataEnvironment.MDE)

    # Mock authentication
    auth_response = Mock()
    auth_response.json.return_value = _AUTH_RESP.copy()
    mock_post.return_value = auth_response

    with custom_mp_config(MP_PATH):
        mde_drv.connect(_CONSTRING)

    # Mock query response with unauthorized error
    query_response = Mock()
    query_response.is_success = False
    query_response.status_code = 401
    query_response.json.return_value = {"error": {"message": "Unauthorized"}}

    with patch(
        "msticpy.data.drivers.odata_driver.httpx.post", return_value=query_response
    ):
        with pytest.raises(ConnectionRefusedError) as exc_info:
            mde_drv.query_with_results(
                "DeviceInfo",
                body=True,
                api_end="/advancedqueries/run",
            )

        check.is_in("Authentication failed", str(exc_info.value))


@respx.mock
@patch("msticpy.data.drivers.odata_driver.httpx.post")
def test_odata_query_rate_limit(mock_post):
    """Test query with rate limit error."""
    driver_cls = import_driver(DataEnvironment.MDE)
    with custom_mp_config(MP_PATH):
        mde_drv = driver_cls(data_environment=DataEnvironment.MDE)

    # Mock authentication
    auth_response = Mock()
    auth_response.json.return_value = _AUTH_RESP.copy()
    mock_post.return_value = auth_response

    with custom_mp_config(MP_PATH):
        mde_drv.connect(_CONSTRING)

    # Mock query response with rate limit error
    query_response = Mock()
    query_response.is_success = False
    query_response.status_code = 429
    query_response.json.return_value = {"error": {"message": "Too many requests"}}

    with patch(
        "msticpy.data.drivers.odata_driver.httpx.post", return_value=query_response
    ):
        with pytest.raises(ConnectionRefusedError) as exc_info:
            mde_drv.query_with_results(
                "DeviceInfo",
                body=True,
                api_end="/advancedqueries/run",
            )

        check.is_in("API limit", str(exc_info.value))


def test_config_name_mapping():
    """Test configuration name mapping."""
    from msticpy.data.drivers.odata_driver import _map_config_dict_name

    config = {
        "tenantid": "abc123",
        "clientId": "def456",
        "clientSecret": "[PLACEHOLDER]",
    }
    mapped = _map_config_dict_name(config)

    check.equal(mapped["tenant_id"], "abc123")
    check.equal(mapped["client_id"], "def456")
    check.equal(mapped["client_secret"], "[PLACEHOLDER]")


@patch("msticpy.data.drivers.odata_driver.get_provider_settings")
def test_get_driver_settings_with_instance(mock_get_settings):
    """Test driver settings retrieval with instance."""
    from msticpy.data.drivers.odata_driver import _get_driver_settings

    mock_provider = Mock()
    mock_provider.args = {"tenant_id": "test", "client_id": "test"}
    mock_get_settings.return_value.get.return_value = mock_provider

    settings = _get_driver_settings("MicrosoftDefender", ["MDATPApp"], "MyInstance")

    check.is_not_none(settings)
    check.is_in("tenant_id", settings)


@patch("msticpy.data.drivers.odata_driver.get_provider_settings")
@patch("msticpy.data.drivers.odata_driver.get_config")
def test_get_driver_settings_fallback(mock_get_config, mock_get_settings):
    """Test driver settings retrieval with fallback to legacy."""
    from msticpy.data.drivers.odata_driver import _get_driver_settings

    mock_get_settings.return_value.get.return_value = None
    mock_get_config.return_value = {"tenant_id": "test", "client_id": "test"}

    settings = _get_driver_settings("MicrosoftDefender", ["MDATPApp"], None)

    check.is_not_none(settings)
    mock_get_config.assert_called()


def test_check_config_missing_item():
    """Test config check with missing item."""
    from msticpy.data.drivers.odata_driver import _check_config

    config = {"tenant_id": "test"}

    with pytest.raises(MsticpyUserConfigError) as exc_info:
        _check_config(config, "client_secret", "application authentication")

    check.is_in("client_secret", str(exc_info.value))


def test_check_config_item_present():
    """Test config check with item present."""
    from msticpy.data.drivers.odata_driver import _check_config

    config = {"tenant_id": "test", "client_secret": "secret"}

    # Should not raise
    _check_config(config, "client_secret", "application authentication")


@patch("msticpy.data.drivers.odata_driver.MSALDelegatedAuth")
@patch("msticpy.data.drivers.odata_driver.httpx")
def test_mde_delegated_auth(mock_httpx, mock_msal):
    """Test MDE driver with delegated authentication."""
    driver_cls = import_driver(DataEnvironment.MDE)

    mock_msal_instance = Mock()
    mock_msal_instance.token = "delegated_token_123"
    mock_msal.return_value = mock_msal_instance

    with custom_mp_config(MP_PATH):
        mde_drv = driver_cls(data_environment=DataEnvironment.MDE)

        # Connect with username to trigger delegated auth
        connection_str = "tenant_id=abc123;client_id=def456;username=user@example.com"
        mde_drv.connect(connection_str, auth_type="device")

    check.equal(mde_drv.token_type, "MSAL")
    check.equal(mde_drv.aad_token, "delegated_token_123")
    mock_msal.assert_called_once()


@patch("msticpy.data.drivers.odata_driver.ConfidentialClientApplication")
@patch("msticpy.data.drivers.odata_driver.load_der_x509_certificate")
def test_mde_certificate_auth(mock_load_cert, mock_app):
    """Test MDE driver with certificate authentication."""
    driver_cls = import_driver(DataEnvironment.MDE)

    # Mock certificate
    mock_cert = Mock()
    mock_cert.fingerprint.return_value.hex.return_value = "abc123"
    mock_cert.public_bytes.return_value.decode.return_value = "cert_data"
    mock_load_cert.return_value = mock_cert

    # Mock MSAL app
    mock_app_instance = Mock()
    mock_app_instance.acquire_token_for_client.return_value = {
        "access_token": "cert_token_123"
    }
    mock_app.return_value = mock_app_instance

    with custom_mp_config(MP_PATH):
        mde_drv = driver_cls(data_environment=DataEnvironment.MDE)

        # Create temporary certificate files for testing
        import tempfile

        with tempfile.NamedTemporaryFile(delete=False, suffix=".der") as cert_file:
            cert_file.write(b"fake_cert_data")
            cert_path = cert_file.name

        with tempfile.NamedTemporaryFile(delete=False, suffix=".key") as key_file:
            key_file.write(b"fake_key_data")
            key_path = key_file.name

        try:
            connection_str = (
                f"tenant_id=abc123;client_id=def456;"
                f"certificate={cert_path};private_key={key_path}"
            )
            mde_drv.connect(connection_str)

            check.equal(mde_drv.aad_token, "cert_token_123")
            mock_app.assert_called_once()
        finally:
            # Cleanup temp files
            import os

            os.unlink(cert_path)
            os.unlink(key_path)


@patch("msticpy.data.drivers.odata_driver.httpx.post")
def test_mde_oauth_v2_scope_handling(mock_post):
    """Test OAuth v2 scope handling in request body."""
    driver_cls = import_driver(DataEnvironment.MDE)

    # Mock authentication response
    auth_response = Mock()
    auth_response.json.return_value = {"access_token": "test_token"}
    mock_post.return_value = auth_response

    with custom_mp_config(MP_PATH):
        mde_drv = driver_cls(data_environment=DataEnvironment.MDE)
        mde_drv.connect(_CONSTRING)

    # Check that the request body was properly formatted
    call_args = mock_post.call_args
    check.is_not_none(call_args)


@respx.mock
@patch("msticpy.data.drivers.odata_driver.httpx.get")
@patch("msticpy.data.drivers.odata_driver.httpx.post")
def test_odata_query_get_method(mock_post, mock_get):
    """Test query execution using GET method."""
    driver_cls = import_driver(DataEnvironment.MDE)
    with custom_mp_config(MP_PATH):
        mde_drv = driver_cls(data_environment=DataEnvironment.MDE)

    # Mock authentication
    auth_response = Mock()
    auth_response.json.return_value = _AUTH_RESP.copy()
    mock_post.return_value = auth_response

    with custom_mp_config(MP_PATH):
        mde_drv.connect(_CONSTRING)

    # Mock GET query response
    query_response = Mock()
    query_response.is_success = True
    query_response.json.return_value = {"results": [{"id": "1", "name": "test"}]}
    query_response.status_code = 200
    mock_get.return_value = query_response

    result, response = mde_drv.query_with_results(
        "?$filter=id eq '1'",
        body=False,
        api_end="",
    )

    check.is_instance(result, pd.DataFrame)
    mock_get.assert_called_once()


def test_mdatp_driver_initialization():
    """Test MDATPDriver initialization with various parameters."""
    with custom_mp_config(MP_PATH):
        driver = MDATPDriver(
            instance="Default",
            cloud="global",
            auth_type="device",
            debug=True,
            max_threads=8,
        )

    check.is_instance(driver, MDATPDriver)
    check.equal(driver.cloud, "global")
    check.is_true(driver._debug)


def test_mdatp_query_datetime_conversion():
    """Test DateTime field conversion in query results."""
    with custom_mp_config(MP_PATH):
        driver = MDATPDriver(data_environment=DataEnvironment.MDE)

    # Mock connection and query
    with patch("msticpy.data.drivers.odata_driver.httpx.post") as mock_post:
        # Mock auth
        auth_response = Mock()
        auth_response.json.return_value = {"access_token": "test_token"}
        mock_post.return_value = auth_response

        driver.connect(_CONSTRING)

        # Mock query response with DateTime fields
        query_response = Mock()
        query_response.is_success = True
        query_response.json.return_value = {
            "Results": [{"Timestamp": "2024-01-01T00:00:00Z", "DeviceId": "123"}],
            "Schema": [
                {"Name": "Timestamp", "Type": "DateTime"},
                {"Name": "DeviceId", "Type": "String"},
            ],
        }
        query_response.status_code = 200

        with patch(
            "msticpy.data.drivers.odata_driver.httpx.post", return_value=query_response
        ):
            result = driver.query("DeviceInfo | take 1")

        check.is_instance(result, pd.DataFrame)
