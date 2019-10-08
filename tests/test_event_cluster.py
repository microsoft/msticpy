# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Event cluster test class."""
import unittest
import json
import os

import pandas as pd

from ..msticpy.sectools.eventcluster import *
from ..msticpy.sectools.eventcluster import (
    token_count_df,
    delim_count_df,
    char_ord_score_df,
    crc32_hash_df,
)


_test_data_folders = [
    d for d, _, _ in os.walk(os.getcwd()) if d.endswith("/tests/testdata")
]
if len(_test_data_folders) == 1:
    _TEST_DATA = _test_data_folders[0]
else:
    _TEST_DATA = "./tests/testdata"


class TestEventCluster(unittest.TestCase):
    """Unit test class."""

    def setUp(self):
        input_file = os.path.join(_TEST_DATA, "processes_on_host.csv")
        self.input_df = pd.read_csv(input_file)

    def test_cluster_features(self):
        out_df = add_process_features(input_frame=self.input_df, path_separator="\\")

        # processName: the process file name (minus path)
        # commandlineLen: length of the command line
        # commandlineLogLen: log10 length of commandline
        # isSystemSession: 1 if session Id is 0x3e7 for Windows or -1 for Linux
        # commandlineTokensFull: counts number of token separators in commandline
        #     [\s\-\\/\.,"\'|&:;%$()]
        # pathScore: sum of ord() value of characters in path
        # commandlineScore: sum of ord() value of characters in commandline
        # commandlineLogScore:
        self.assertIn("processName", out_df.columns)
        self.assertIn("commandlineLen", out_df.columns)
        self.assertIn("commandlineLogLen", out_df.columns)
        self.assertIn("isSystemSession", out_df.columns)
        self.assertIn("commandlineTokensFull", out_df.columns)
        self.assertIn("pathScore", out_df.columns)
        self.assertIn("commandlineScore", out_df.columns)
        self.assertIn("commandlineTokensHash", out_df.columns)
        self.assertIn("pathHash", out_df.columns)

    def test_custom_features(self):

        input_str = (
            "The quick & sly (as all foxes might be/or not) fox, jumped over a frog."
        )
        test_df = pd.DataFrame(data=[input_str], columns=["input"], index=[0])

        test_df["tok_count"] = token_count_df(data=test_df, column="input")
        test_df["char_score"] = char_ord_score_df(data=test_df, column="input")
        test_df["delim_count"] = delim_count_df(data=test_df, column="input")
        test_df["crc32"] = crc32_hash_df(data=test_df, column="input")
        self.assertEqual(test_df["tok_count"].iloc[0], 15)
        self.assertEqual(test_df["char_score"].iloc[0], 6199.0)
        self.assertEqual(test_df["delim_count"].iloc[0], 20)
        self.assertEqual(test_df["crc32"].iloc[0], 2011081507)

        self.assertEqual(
            test_df.apply(lambda x: token_count(x.input), axis=1).iloc[0], 15
        )
        self.assertEqual(
            test_df.apply(lambda x: delim_count(x.input), axis=1).iloc[0], 20
        )
        self.assertEqual(
            test_df.apply(lambda x: char_ord_score(x.input), axis=1).iloc[0], 6199.0
        )
        self.assertEqual(
            test_df.apply(lambda x: crc32_hash(x.input), axis=1).iloc[0], 2011081507
        )
        self.assertEqual(
            test_df.apply(lambda x: delim_hash(x.input), axis=1).iloc[0], 2337396062
        )

    def test_clustering(self):
        out_df = add_process_features(input_frame=self.input_df, path_separator="\\")

        output = dbcluster_events(
            data=out_df,
            cluster_columns=["pathScore", "commandlineTokensFull", "isSystemSession"],
            verbose=False,
            normalize=True,
            time_column="TimeGenerated",
            max_cluster_distance=0.0001,
            min_cluster_samples=2,
        )
        out_df2, dbscan, model = output

        self.assertIsNotNone(out_df2)
        self.assertEqual(len(out_df2), 62)
        self.assertEqual(out_df2["ClusterSize"].max(), 71)
        self.assertEqual(out_df2["ClusterSize"].min(), 1)

        output = dbcluster_events(
            data=out_df,
            cluster_columns=["pathHash", "commandlineTokensHash", "isSystemSession"],
            verbose=False,
            normalize=True,
            time_column="TimeGenerated",
            max_cluster_distance=0.001,
            min_cluster_samples=2,
        )
        out_df3, dbscan, model = output

        self.assertIsNotNone(out_df3)
        self.assertEqual(len(out_df3), 121)
        self.assertEqual(out_df3["ClusterSize"].max(), 70)
        self.assertEqual(out_df3["ClusterId"].max(), 31)
        self.assertEqual(out_df3["ClusterSize"].min(), 1)
        self.assertEqual(len(out_df3[out_df3["ClusterId"] == -1]), 89)
