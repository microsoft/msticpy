# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Tests for the msticpy.mcp command-line entry point."""
from __future__ import annotations

from unittest.mock import patch

from msticpy.mcp.__main__ import main

__author__ = "Ian Hellen"


def test_main_default_transport():
    """main() runs the server with the stdio transport by default."""
    with patch("msticpy.mcp.server.run") as mock_run:
        rc = main([])
    assert rc == 0
    mock_run.assert_called_once_with(transport="stdio")


def test_main_custom_transport():
    """main() passes an explicit transport through to run()."""
    with patch("msticpy.mcp.server.run") as mock_run:
        rc = main(["--transport", "streamable-http", "--log-level", "INFO"])
    assert rc == 0
    mock_run.assert_called_once_with(transport="streamable-http")
