# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Unit test common utilities."""
import os

from ..msticpy.common.utility import export

__author__ = "Ian Hellen"


@export
def _get_test_data_path():
    _test_data_folders = [
        d for d, _, _ in os.walk(os.getcwd()) if d.endswith("/tests/testdata")
    ]
    if len(_test_data_folders) == 1:
        return _test_data_folders[0]
    return "./tests/testdata"


TEST_DATA_PATH = _get_test_data_path()
