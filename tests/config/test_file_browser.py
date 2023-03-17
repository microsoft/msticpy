# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Module docstring."""

import pytest_check as check

from msticpy.config.file_browser import FileBrowser

__author__ = "Ian Hellen"

# pylint: disable=protected-access, global-statement, invalid-name
file_name = ""


def test_file_browser():
    """Function_docstring."""
    f_brow = FileBrowser(".", select_cb=_callback)
    starting_folder = f_brow.current_folder
    check.greater(len(f_brow.select_file.options), 0)
    check.greater(len(f_brow.select_folder.options), 0)
    check.is_in("..", f_brow.select_folder.options)
    curr_files = f_brow.select_file.options
    check.equal(curr_files, f_brow.select_file.options)
    f_brow._open_folder(tgt_folder="msticpy")
    check.not_equal(curr_files, f_brow.select_file.options)

    f_brow.txt_path.value = str(starting_folder)
    f_brow._enter_folder(event=None)
    check.greater(len(f_brow.select_file.options), 0)
    f_brow.select_file.selected_index = 1
    f_brow._return_file(btn=None)
    check.equal(file_name, f_brow.file)

    f_brow.txt_search.value = "*.py"
    f_brow._search(f_brow.btn_search)
    check.greater(len(f_brow.select_search.options), 0)


def _callback(file):
    global file_name
    file_name = file
