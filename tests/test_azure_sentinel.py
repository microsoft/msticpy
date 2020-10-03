# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
import os
from pathlib import Path
from unittest.mock import patch
from collections import namedtuple

from pytest import raises

from msticpy.data.azure_sentinel import AzureSentinel
from msticpy.common.exceptions import MsticpyException
from msticpy.common import pkg_config
from msticpy.common.provider_settings import get_provider_settings

from .unit_test_lib import get_test_data_path, custom_mp_config

_TEST_DATA = get_test_data_path()


def test_azuresent_init():
    """Test class initalization."""
    azs = AzureSentinel()
    assert type(azs) == AzureSentinel

def test_azuresent_connect_exp():
    """Test connect failure."""
    with raises(AttributeError):
        azs = AzureSentinel()
        azs.connect()

