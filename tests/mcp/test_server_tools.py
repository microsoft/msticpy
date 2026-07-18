# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Integration tests for the msticpy.mcp server tools via an in-memory client."""
from __future__ import annotations

import asyncio

import pytest

from mcp.shared.memory import (
    create_connected_server_and_client_session as client_connect,
)

from msticpy.mcp import server as mcp_server

__author__ = "Ian Hellen"


@pytest.fixture
def server_with_session(mcp_session):
    """Create a FastMCP server bound to the test session."""
    mcp_server.set_session(mcp_session)
    return mcp_server.create_server()


async def _call(client, name, args):
    """Call a tool and return its structured content."""
    result = await client.call_tool(name, args)
    return result.structuredContent


def _run(server, coro_func):
    """Run an async client interaction against the server and return its result."""

    async def _runner():
        async with client_connect(server._mcp_server) as client:
            return await coro_func(client)

    return asyncio.run(_runner())


def test_list_tools(server_with_session):
    """All seven tools are registered."""

    async def interaction(client):
        tools = await client.list_tools()
        return {t.name for t in tools.tools}

    names = _run(server_with_session, interaction)
    assert names == {
        "list_entities",
        "list_pivots",
        "describe_pivot",
        "run_pivot",
        "get_result",
        "set_timespan",
        "server_status",
    }


def test_list_entities_and_pivots(server_with_session):
    """list_entities and list_pivots return catalog data."""

    async def interaction(client):
        entities = await _call(client, "list_entities", {})
        pivots = await _call(
            client, "list_pivots", {"entity": "ip", "kind": "enrichment"}
        )
        return entities, pivots

    entities, pivots = _run(server_with_session, interaction)
    entity_names = {e["entity"] for e in entities["result"]}
    assert "IpAddress" in entity_names
    assert pivots["result"]
    assert all(p["kind"] == "enrichment" for p in pivots["result"])


def test_describe_pivot(server_with_session):
    """describe_pivot returns parameter metadata."""

    async def interaction(client):
        return await _call(
            client, "describe_pivot", {"entity": "ip", "pivot_path": "ip_type"}
        )

    desc = _run(server_with_session, interaction)
    assert desc["ok"] is True
    assert desc["kind"] == "enrichment"
    assert desc["primary_param"]


def test_describe_unknown_pivot(server_with_session):
    """describe_pivot returns a structured error for unknown pivots."""

    async def interaction(client):
        return await _call(
            client, "describe_pivot", {"entity": "ip", "pivot_path": "nope"}
        )

    desc = _run(server_with_session, interaction)
    assert desc["ok"] is False
    assert desc["error"] == "unknown_pivot"


def test_run_pivot_and_get_result(server_with_session):
    """run_pivot executes a local pivot and get_result pages the stored data."""

    async def interaction(client):
        run = await _call(
            client,
            "run_pivot",
            {"entity": "ip", "pivot_path": "ip_type", "value": "8.8.8.8"},
        )
        page = await _call(client, "get_result", {"result_id": run["result_id"]})
        return run, page

    run, page = _run(server_with_session, interaction)
    assert run["ok"] is True
    assert run["row_count"] == 1
    assert run["sample"][0]["result"] == "Public"
    assert page["ok"] is True
    assert page["rows"][0]["result"] == "Public"


def test_run_pivot_unknown(server_with_session):
    """run_pivot returns a structured error for unknown pivots."""

    async def interaction(client):
        return await _call(
            client,
            "run_pivot",
            {"entity": "ip", "pivot_path": "no_such", "value": "x"},
        )

    run = _run(server_with_session, interaction)
    assert run["ok"] is False
    assert run["error"] == "unknown_pivot"


def test_run_pivot_negative_max_rows(server_with_session):
    """run_pivot rejects a negative max_rows."""

    async def interaction(client):
        return await _call(
            client,
            "run_pivot",
            {
                "entity": "ip",
                "pivot_path": "ip_type",
                "value": "8.8.8.8",
                "max_rows": -1,
            },
        )

    run = _run(server_with_session, interaction)
    assert run["ok"] is False
    assert run["error"] == "invalid_argument"


def test_get_result_negative_bounds(server_with_session):
    """get_result rejects negative offset/limit."""

    async def interaction(client):
        run = await _call(
            client,
            "run_pivot",
            {"entity": "ip", "pivot_path": "ip_type", "value": "8.8.8.8"},
        )
        return await _call(
            client, "get_result", {"result_id": run["result_id"], "limit": -5}
        )

    page = _run(server_with_session, interaction)
    assert page["ok"] is False
    assert page["error"] == "invalid_argument"


def test_set_timespan_invalid(server_with_session):
    """set_timespan returns structured errors for bad input."""

    async def interaction(client):
        bad_days = await _call(client, "set_timespan", {"days": -1})
        bad_range = await _call(
            client,
            "set_timespan",
            {"start": "2026-02-01T00:00:00Z", "end": "2026-01-01T00:00:00Z"},
        )
        bad_date = await _call(client, "set_timespan", {"start": "nonsense"})
        return bad_days, bad_range, bad_date

    bad_days, bad_range, bad_date = _run(server_with_session, interaction)
    assert bad_days["error"] == "invalid_argument"
    assert bad_range["error"] == "invalid_timespan"
    assert bad_date["error"] == "invalid_timespan"


def test_set_timespan_and_status(server_with_session):
    """set_timespan updates the timespan and server_status reports it."""

    async def interaction(client):
        span = await _call(client, "set_timespan", {"days": 14})
        status = await _call(client, "server_status", {})
        return span, status

    span, status = _run(server_with_session, interaction)
    assert span["ok"] is True
    assert span["start"] and span["end"]
    assert status["pivot_count"] > 0
    assert status["entity_count"] > 0
    assert "timespan" in status


def test_list_pivots_unknown_entity(server_with_session):
    """list_pivots returns a structured error for an unknown entity."""

    async def interaction(client):
        return await _call(client, "list_pivots", {"entity": "not-an-entity"})

    result = _run(server_with_session, interaction)
    assert result["result"][0]["ok"] is False
    assert result["result"][0]["error"] == "unknown_entity"


def test_run_pivot_provider_not_connected(mcp_session):
    """run_pivot rejects a query pivot whose provider is not connected."""
    from msticpy.mcp import server as mcp_server
    from msticpy.mcp.catalog import PivotEntry
    from msticpy.mcp.session import ProviderState

    # Inject a synthetic query pivot bound to a disconnected provider.
    entry = PivotEntry(
        entity="IpAddress",
        pivot_path="sentinel_ip_query",
        description="synthetic query pivot",
        kind="query",
        func=lambda **kwargs: None,
        primary_param="ip_address",
        provider="sentinel",
    )
    mcp_session.catalog._entries[entry.key] = entry  # pylint: disable=protected-access
    state = ProviderState("sentinel", "MSSentinel")
    state.connected = False
    state.error = "no creds"
    mcp_session.providers["sentinel"] = state

    mcp_server.set_session(mcp_session)
    server = mcp_server.create_server()

    async def interaction(client):
        return await _call(
            client,
            "run_pivot",
            {
                "entity": "IpAddress",
                "pivot_path": "sentinel_ip_query",
                "value": "1.1.1.1",
            },
        )

    result = _run(server, interaction)
    assert result["ok"] is False
    assert result["error"] == "provider_not_connected"
