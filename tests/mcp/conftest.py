# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Shared fixtures for the msticpy.mcp tests."""
from __future__ import annotations

import warnings
from pathlib import Path

import pytest

from ..unit_test_lib import custom_mp_config

__author__ = "Ian Hellen"

_TEST_CONFIG = Path(__file__).parent.parent.joinpath("msticpyconfig-test.yaml")


@pytest.fixture(scope="module")
def mcp_session():
    """Return a started MpMcpSession using the test config."""
    with custom_mp_config(_TEST_CONFIG):
        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            from msticpy.mcp.session import MpMcpSession

            session = MpMcpSession().start()
        yield session


@pytest.fixture(scope="module")
def mcp_catalog(mcp_session):
    """Return the built pivot catalog from a started session."""
    return mcp_session.catalog
