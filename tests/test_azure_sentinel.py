# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Azure Sentinel unit tests."""
from pytest import raises

from msticpy.data.azure_sentinel import AzureSentinel


def test_azuresent_init():
    """Test class initalization."""
    azs = AzureSentinel()
    assert isinstance(azs, AzureSentinel)


def test_azuresent_connect_exp():
    """Test connect failure."""
    with raises(AttributeError):
        azs = AzureSentinel()
        azs.connect()
