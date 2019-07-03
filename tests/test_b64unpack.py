# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Base64unpack test class."""
import unittest
import os
from os import path
import pandas as pd

from ..msticpy.sectools import base64unpack as b64


_test_data_folders = [
    d for d, _, _ in os.walk(os.getcwd()) if d.endswith("/tests/testdata")
]
if len(_test_data_folders) == 1:
    _TEST_DATA = _test_data_folders[0]
else:
    _TEST_DATA = "./tests/testdata"


class TestB64Unpack(unittest.TestCase):
    """Unit test class."""

    def test_archive_string(self):
        try:
            input_txt = None
            FILE_NAME = path.join(_TEST_DATA, "b64text_inzip.txt")
            with open(FILE_NAME, "r") as f_handle:
                input_txt = f_handle.read()

            self.assertIsNotNone(input_txt)

            result_str, result_df = b64.unpack_items(input_string=input_txt, trace=True)
            print(result_str)
            # assert result_df.shape == (2, 12)
            self.assertIsNotNone(result_str)
            self.assertIsNotNone(result_df)

            result_str, result_df = b64.unpack(input_string=input_txt, trace=True)
            print(result_str)
            # assert result_df.shape == (2, 12)
            self.assertIsNotNone(result_str)
            self.assertIsNotNone(result_df)

        except FileNotFoundError as ex:
            self.fail(msg="Exception {}".format(str(ex)))

    def test_nested_archive(self):
        try:
            input_txt = None
            FILE_NAME = path.join(_TEST_DATA, "base64msg.txt")
            with open(FILE_NAME, "r") as f_handle:
                input_txt = f_handle.read()

            self.assertIsNotNone(input_txt)

            result_str, result_df = b64.unpack_items(input_string=input_txt, trace=True)
            self.assertEqual(result_df.shape, (8, 12))
            self.assertIsNotNone(result_str)
            self.assertIsNotNone(result_df)

            result_str, result_df = b64.unpack(input_string=input_txt, trace=True)
            self.assertEqual(result_df.shape, (8, 12))
            self.assertIsNotNone(result_str)
            self.assertIsNotNone(result_df)

        except FileNotFoundError as ex:
            self.fail(msg="Exception {}".format(str(ex)))

    def test_nested_archive_df(self):
        try:
            input_txt = None
            FILE_NAME = path.join(_TEST_DATA, "base64msg.txt")
            with open(FILE_NAME, "r") as f_handle:
                input_txt = f_handle.read()

            self.assertIsNotNone(input_txt)
            # create datframe for input with 2 rows and add same data to both
            input_df = pd.DataFrame(data=["a", "b"], columns=["input"], index=[0, 1])
            input_df["input"] = input_txt
            result_df = b64.unpack_items(data=input_df, column="input", trace=True)
            # we should get 2x the rows as the previous test (since data is duplicated)
            # plus 2 added columns
            self.assertEqual(result_df.shape, (16, 14))
            self.assertIsNotNone(result_df)

            result_df = b64.unpack_df(data=input_df, column="input", trace=True)
            # we should get 2x the rows as the previous test (since data is duplicated)
            # plus 2 added columns
            self.assertEqual(result_df.shape, (16, 14))
            self.assertIsNotNone(result_df)

        except FileNotFoundError as ex:
            self.fail(msg="Exception {}".format(str(ex)))


if __name__ == "__main__":
    unittest.main()
    print("bye")
