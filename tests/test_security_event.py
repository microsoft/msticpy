# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Security Event test class."""
import unittest
import json
import os

import pandas as pd

from ..msticpy.sectools.eventcluster import *
from ..msticpy.sectools.eventcluster import (
    token_count_df,
    delim_count_df,
    char_ord_score_df,
)
from ..msticpy.nbtools.security_event import SecurityEvent


_test_data_folders = [
    d for d, _, _ in os.walk(os.getcwd()) if d.endswith("/tests/testdata")
]
if len(_test_data_folders) == 1:
    _TEST_DATA = _test_data_folders[0]
else:
    _TEST_DATA = "./tests/testdata"


class TestSecurity(unittest.TestCase):
    """Unit test class."""

    def setUp(self):
        input_file = os.path.join(_TEST_DATA, "processes_on_host.csv")
        self.input_df = pd.read_csv(input_file)

    def test_4688_events(self):

        for idx, row in self.input_df[0:5].iterrows():
            test_event = SecurityEvent(src_row=row)
            self.assertGreaterEqual(len(test_event.entities), 4)

            self.assertGreaterEqual(len(test_event.properties), 19)
