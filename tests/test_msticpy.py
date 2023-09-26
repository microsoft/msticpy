# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Test getattr and dir functionality of msticpy main __init__."""

import pytest_check as check

import msticpy
from msticpy import _LAZY_IMPORTS

__author__ = "Ian Hellen"


def test_getattr():
    """Test fetching and importing dynamic attributes."""

    for lazy_import in _LAZY_IMPORTS:
        _, _, lazy_attrib = lazy_import.rpartition(".")
        check.is_in(lazy_attrib, dir(msticpy))

        obj = getattr(msticpy, lazy_attrib)
        if isinstance(obj, type) or callable(obj):
            check.equal(obj.__name__, lazy_attrib)
