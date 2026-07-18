# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Tests for msticpy.mcp.config."""
from __future__ import annotations

import pytest

from msticpy.mcp.config import McpServerConfig

__author__ = "Ian Hellen"


def test_default_config():
    """Empty settings yield a default config."""
    config = McpServerConfig.from_msticpy_settings(None)
    assert config.query_providers == {}
    assert config.components == {}
    assert config.result_store.max_results == 50
    assert config.result_store.max_rows_per_result == 100_000
    assert config.defaults.timespan_days == 7.0
    assert config.defaults.sample_rows == 25


def test_config_from_section():
    """A populated MpMcpServer section parses into a config."""
    settings = {
        "MpMcpServer": {
            "QueryProviders": {
                "sentinel": {
                    "DataEnvironment": "MSSentinel",
                    "Connect": True,
                    "ConnectArgs": {"workspace": "MyWs"},
                }
            },
            "ResultStore": {"max_results": 10, "max_rows_per_result": 500},
            "Defaults": {"timespan_days": 3, "sample_rows": 5},
        }
    }
    config = McpServerConfig.from_msticpy_settings(settings)
    assert "sentinel" in config.query_providers
    prov = config.query_providers["sentinel"]
    assert prov.data_environment == "MSSentinel"
    assert prov.connect is True
    assert prov.connect_args == {"workspace": "MyWs"}
    assert config.result_store.max_results == 10
    assert config.defaults.timespan_days == 3


def test_config_accepts_direct_section():
    """A config passed as the section directly (no wrapper key) also parses."""
    section = {"Defaults": {"sample_rows": 99}}
    config = McpServerConfig.from_msticpy_settings(section)
    assert config.defaults.sample_rows == 99


@pytest.mark.parametrize("bad_settings", [{"MpMcpServer": "notadict"}, {}, None])
def test_config_bad_or_missing_section(bad_settings):
    """Missing or malformed sections fall back to defaults."""
    config = McpServerConfig.from_msticpy_settings(bad_settings)
    assert config.result_store.max_results == 50
