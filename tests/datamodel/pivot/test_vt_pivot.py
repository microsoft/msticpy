# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Pivot VT import test."""
import pytest_check as check

from msticpy.datamodel import entities

# pylint: disable=unused-import, unused-argument, redefined-outer-name
from .pivot_fixtures import create_data_providers, create_pivot, data_providers


def test_import_vt_funcs(create_pivot):
    """Test VT Pivot functions loaded correctly."""
    check.is_in("VT", dir(entities.Url))
    check.is_in("VT", dir(entities.File))
    check.is_in("VT", dir(entities.IpAddress))
    check.is_in("VT", dir(entities.Dns))

    check.greater_equal(len(dir(entities.Url.VT())), 6)
    check.greater_equal(len(dir(entities.File.VT())), 6)
    check.greater_equal(len(dir(entities.IpAddress.VT())), 6)
    check.greater_equal(len(dir(entities.Dns.VT())), 6)
