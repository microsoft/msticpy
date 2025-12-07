"""Unit tests for MDATPDriver."""

from __future__ import annotations

import urllib.parse
from typing import Any

import pandas as pd
import pytest
import respx
from httpx import Response

from msticpy.data.core.query_defns import DataEnvironment  # type: ignore
from msticpy.data.drivers.mdatp_driver import (
    M365DConfiguration,
    MDATPDriver,
    _select_api,
)


@pytest.fixture
def dummy_conn_str() -> str:
    """Return a minimal client credential connection string."""
    return "tenant_id=tenant123;client_id=client123;client_secret=[PLACEHOLDER]"


@pytest.fixture
def mock_token_response() -> dict[str, Any]:
    """Return a minimal successful token response."""
    return {"access_token": "fake_token_value", "expires_in": 3600}


@pytest.fixture
def mock_query_response() -> dict[str, Any]:
    """Return a fake query response including Schema for datetime conversion."""
    return {
        "Schema": [{"Name": "Timestamp", "Type": "DateTime"}],
        "Results": [{"Timestamp": "2024-01-01T12:34:56Z"}],
    }


def test_select_api_mde() -> None:
    """Test API selection for MDE environment."""
    cfg = _select_api(DataEnvironment.MDE, "global")
    assert cfg.api_version == "api"
    assert cfg.api_endpoint == "/advancedqueries/run"
    assert cfg.scopes[0].endswith("/.default")
    assert cfg.oauth_v2 is True


def test_select_api_m365d() -> None:
    """Test API selection for M365 Defender unified environment."""
    # Note this now reverts to MDE parameters
    cfg = _select_api(DataEnvironment.M365D, "global")
    assert cfg.api_endpoint == "/advancedqueries/run"
    assert "/advancedqueries/run" in cfg.api_uri


def test_select_api_graph() -> None:
    """Test API selection for Microsoft Graph security hunting."""
    cfg = _select_api(DataEnvironment.M365DGraph, "global")
    assert cfg.api_version == "v1.0"
    assert cfg.api_endpoint == "/security/runHuntingQuery"
    assert urllib.parse.urlparse(cfg.resource_uri).hostname == "graph.microsoft.com"


@respx.mock
def test_driver_client_credentials_oauth_v2(
    dummy_conn_str, mock_token_response, mock_query_response
) -> None:
    """Test client credentials auth and query execution (OAuth v2)."""
    respx.post(r"https://login\.microsoftonline\.com/tenant123/oauth2/v2.0/token").mock(
        return_value=Response(200, json=mock_token_response)
    )
    # Instantiate driver (defaults to DataEnvironment.MDE)
    driver = MDATPDriver()

    # Mock token endpoint
    token_url = driver.oauth_url.format(tenantId="tenant123")
    respx.post(token_url).mock(return_value=Response(200, json=mock_token_response))
    driver.connect(connection_str=dummy_conn_str)

    # Mock query endpoint
    query_url = (
        f"{driver.api_root.rstrip('/')}/{driver.api_ver.strip('/')}{driver.api_suffix}"
    )
    respx.post(query_url).mock(return_value=Response(200, json=mock_query_response))

    df = driver.query("DeviceNetworkEvents | take 1")
    assert isinstance(df, pd.DataFrame)
    assert "Timestamp" in df.columns
    assert pd.api.types.is_datetime64_any_dtype(df["Timestamp"])
    assert driver.scopes and driver.scopes[0].endswith("/.default")


@respx.mock
def test_driver_legacy_oauth_v1_resource_param(
    monkeypatch, dummy_conn_str, mock_token_response
) -> None:
    """Test legacy OAuth v1 flow sets 'resource' instead of relying on scopes."""

    def legacy_select_api(env: DataEnvironment, cloud: str) -> M365DConfiguration:  # type: ignore
        resource_uri = "https://api.securitycenter.microsoft.com/"
        return M365DConfiguration(
            login_uri="https://login.microsoftonline.com/{tenantId}/oauth2/token",  # v1 endpoint (no v2.0)
            resource_uri=resource_uri,
            api_version="api",
            api_endpoint="/advancedqueries/run",
            api_uri="https://api.securitycenter.microsoft.com/api/advancedqueries/run",
            scopes=[
                f"{resource_uri.rstrip('/')}/.default"
            ],  # still populated but not used in v1 request body
        )

    monkeypatch.setattr(
        "msticpy.data.drivers.mdatp_driver._select_api", legacy_select_api
    )

    driver = MDATPDriver()
    assert driver._m365d_params.oauth_v2 is False
    assert "resource" in driver.req_body
    assert (
        urllib.parse.urlparse(driver.req_body["resource"]).hostname
        == "api.securitycenter.microsoft.com"
    )

    token_url = driver.oauth_url.format(tenantId="tenant123")
    captured_body: dict[str, str] = {}

    def token_side_effect(request) -> Response:
        decoded = urllib.parse.parse_qs(request.content.decode("utf-8"))
        captured_body.update({k: v[0] for k, v in decoded.items()})
        return Response(200, json=mock_token_response)

    respx.post(token_url).mock(side_effect=token_side_effect)
    driver.connect(connection_str=dummy_conn_str)

    # Force auth call
    # Re-run standard auth to capture body (connect already ran in __init__)
    driver._get_token_standard_auth(
        {},
        {  # type: ignore
            "tenant_id": "tenant123",
            "client_id": "client123",
            "client_secret": "[PLACEHOLDER]",
        },
    )

    assert "resource" in captured_body
    assert "scope" not in captured_body  # v1 flow should not send scope
    assert driver.aad_token == "fake_token_value"


@respx.mock
def test_request_uri_join_normalization(dummy_conn_str, mock_token_response) -> None:
    """Test request_uri does not duplicate slashes."""
    driver = MDATPDriver()
    token_url = driver.oauth_url.format(tenantId="tenant123")
    respx.post(token_url).mock(return_value=Response(200, json=mock_token_response))
    driver.connect(connection_str=dummy_conn_str)
    assert "//" not in driver.api_root.replace("://", "§")  # ignore scheme part
    assert "//" not in driver.api_ver
    assert not driver.api_root.endswith("//")
