# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""vtlookup test class."""
import unittest

from ..msticpy.nbtools import utils


class TestUtils(unittest.TestCase):
    """Unit test class."""

    def test_misc_funcs(self):
        self.assertTrue(utils.string_empty(None))
        self.assertTrue(utils.string_empty(""))

        self.assertFalse(utils.is_not_empty(None))
        self.assertFalse(utils.is_not_empty(""))
        self.assertFalse(utils.is_not_empty({}))

        self.assertEqual(utils.escape_windows_path("C:\\windows"), "C:\\\\Windows")
        self.assertEqual(utils.escape_windows_path("C:/windows"), "C:/Windows")

        self.assertEqual(utils.unescape_windows_path("C:\\\\Windows"), "C:\\windows")
        self.assertEqual(utils.unescape_windows_path("C:/windows"), "C:/Windows")

        with self.assertRaises(SystemExit):
            utils.check_py_version((4, 0))
        utils.check_py_version((3, 6))

        utils.resolve_pkg_path("c:/windows")
