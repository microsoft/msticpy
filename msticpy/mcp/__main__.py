# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Command-line entry point for the MSTICPy MCP server."""

from __future__ import annotations

import argparse
import logging
import sys

from .._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"


def main(argv: list[str] | None = None) -> int:
    """
    Run the MSTICPy MCP server from the command line.

    Parameters
    ----------
    argv : list[str] | None, optional
        Command-line arguments (defaults to ``sys.argv``).

    Returns
    -------
    int
        Process exit code.

    """
    parser = argparse.ArgumentParser(
        prog="msticpy-mcp",
        description="Run the MSTICPy pivot MCP (Model Context Protocol) server.",
    )
    parser.add_argument(
        "--transport",
        choices=["stdio", "sse", "streamable-http"],
        default="stdio",
        help="Transport to use (default: stdio).",
    )
    parser.add_argument(
        "--log-level",
        default="WARNING",
        help="Logging level (default: WARNING). Use INFO/DEBUG for troubleshooting.",
    )
    args = parser.parse_args(argv)

    logging.basicConfig(
        level=getattr(logging, args.log_level.upper(), logging.WARNING),
        stream=sys.stderr,
        format="%(asctime)s %(levelname)s %(name)s: %(message)s",
    )

    # Imported here so that --help works even without the mcp extra installed.
    from .server import run  # noqa: PLC0415  # pylint: disable=import-outside-toplevel

    run(transport=args.transport)
    return 0


if __name__ == "__main__":
    sys.exit(main())
