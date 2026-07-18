# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
MSTICPy MCP (Model Context Protocol) server subpackage.

Exposes MSTICPy pivot functionality to MCP-compatible AI agent hosts
(GitHub Copilot / VS Code, Claude Desktop, or any MCP client) through a
compact, generic set of tools.

Install the optional dependencies with ``pip install "msticpy[mcp]"`` and
run the server with the ``msticpy-mcp`` console command or
``python -m msticpy.mcp``.
"""

from .._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"
