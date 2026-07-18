# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Unit tests for msticpy.mcp.session loaders and timespan handling."""
from __future__ import annotations

from unittest.mock import MagicMock

import pytest

from msticpy.mcp.config import (
    ComponentConfig,
    McpServerConfig,
    QueryProviderConfig,
)
from msticpy.mcp.session import MpMcpSession, _parse_dt

__author__ = "Ian Hellen"


def _session_with_pivot(config: McpServerConfig) -> MpMcpSession:
    """Return a session with a mock pivot (no full MSTICPy init)."""
    session = MpMcpSession(config=config)
    session.pivot = MagicMock()
    return session


def test_parse_dt_variants():
    """_parse_dt handles Z suffix, naive and None inputs."""
    assert _parse_dt(None) is None
    zulu = _parse_dt("2026-01-01T00:00:00Z")
    assert zulu is not None and zulu.tzinfo is not None
    naive = _parse_dt("2026-01-01T00:00:00")
    assert naive.tzinfo is not None  # coerced to UTC


def test_set_and_get_timespan_explicit():
    """Explicit start/end are stored on the pivot and returned."""
    session = _session_with_pivot(McpServerConfig())
    span = session.set_timespan(start="2026-01-01T00:00:00Z", end="2026-01-08T00:00:00Z")
    assert span["start"].startswith("2026-01-01")
    assert span["end"].startswith("2026-01-08")
    session.pivot.set_timespan.assert_called_once()


def test_set_timespan_days():
    """A relative 'days' window produces a start before the end."""
    session = _session_with_pivot(McpServerConfig())
    span = session.set_timespan(days=5)
    assert span["start"] < span["end"]


def test_set_timespan_inverted_raises():
    """An inverted explicit start/end raises ValueError."""
    session = _session_with_pivot(McpServerConfig())
    with pytest.raises(ValueError, match="must be before"):
        session.set_timespan(
            start="2026-01-08T00:00:00Z", end="2026-01-01T00:00:00Z"
        )


def test_set_timespan_bad_datetime_raises():
    """An unparseable datetime raises ValueError (surfaced as a tool error)."""
    session = _session_with_pivot(McpServerConfig())
    with pytest.raises(ValueError):
        session.set_timespan(start="not-a-date")


def test_get_timespan_no_pivot():
    """get_timespan tolerates an uninitialized pivot."""
    session = MpMcpSession(config=McpServerConfig())
    assert session.get_timespan() == {"start": None, "end": None}


def test_load_query_provider_init_failure():
    """A provider with a bogus environment is recorded as failed, not fatal."""
    config = McpServerConfig(
        QueryProviders={
            "bad": QueryProviderConfig(DataEnvironment="NotARealEnvironment")
        }
    )
    session = _session_with_pivot(config)
    session._load_query_providers()  # pylint: disable=protected-access
    state = session.providers["bad"]
    assert state.connected is False
    assert state.error and "init failed" in state.error


def test_load_query_provider_success_and_connect(monkeypatch):
    """A working provider connects and is attached to the pivot."""

    class _FakeProvider:
        def __init__(self, environment, **kwargs):
            self.environment = environment

        def connect(self, **kwargs):
            return True

    import msticpy.data.core.data_providers as mp_data  # pylint: disable=import-outside-toplevel

    monkeypatch.setattr(mp_data, "QueryProvider", _FakeProvider)
    config = McpServerConfig(
        QueryProviders={
            "good": QueryProviderConfig(DataEnvironment="MSSentinel", Connect=True)
        }
    )
    session = _session_with_pivot(config)
    session._load_query_providers()  # pylint: disable=protected-access
    state = session.providers["good"]
    assert state.connected is True
    session.pivot.add_query_provider.assert_called_once()


def test_load_query_provider_connect_failure(monkeypatch):
    """A provider that fails to connect is marked disconnected with an error."""

    class _FailingProvider:
        def __init__(self, environment, **kwargs):
            self.environment = environment

        def connect(self, **kwargs):
            raise ConnectionError("no creds")

    import msticpy.data.core.data_providers as mp_data  # pylint: disable=import-outside-toplevel

    monkeypatch.setattr(mp_data, "QueryProvider", _FailingProvider)
    config = McpServerConfig(
        QueryProviders={
            "flaky": QueryProviderConfig(DataEnvironment="MSSentinel", Connect=True)
        }
    )
    session = _session_with_pivot(config)
    session._load_query_providers()  # pylint: disable=protected-access
    state = session.providers["flaky"]
    assert state.connected is False
    assert "connect failed" in state.error


def test_load_component_import_failure():
    """A component with an unimportable module is recorded as failed."""
    config = McpServerConfig(
        Components={
            "bad": ComponentConfig(Module="no.such.module", Class="Nope")
        }
    )
    session = _session_with_pivot(config)
    session._load_components()  # pylint: disable=protected-access
    state = session.providers["bad"]
    assert state.connected is False
    assert "init failed" in state.error


def test_load_component_success_with_connect():
    """A valid component is created and connected."""
    config = McpServerConfig(
        Components={
            "coll": ComponentConfig(
                Module="collections", Class="OrderedDict", Connect=False
            )
        }
    )
    session = _session_with_pivot(config)
    session._load_components()  # pylint: disable=protected-access
    state = session.providers["coll"]
    # OrderedDict has no connect() so connected stays False but no error.
    assert state.error is None


def test_status_no_start():
    """status() works on a not-fully-started session."""
    session = MpMcpSession(config=McpServerConfig())
    session.pivot = MagicMock()
    session.pivot.start = None
    session.pivot.end = None
    status = session.status()
    assert status["pivot_count"] == 0
    assert status["providers"] == []
    assert "ti_providers" in status
