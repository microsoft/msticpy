# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""process tree utils test class."""
import ast
import os
from pathlib import Path

import pandas as pd

from ..msticpy.sectools import process_tree_utils as ptutil

_test_data_folders = [
    d for d, _, _ in os.walk(os.getcwd()) if d.endswith("/tests/testdata")
]
if len(_test_data_folders) == 1:
    _TEST_DATA = _test_data_folders[0]
else:
    _TEST_DATA = "./tests/testdata"

testdf_win = pd.read_pickle(Path(_TEST_DATA).joinpath("win_proc_test.pkl"))
testdf_lx = pd.read_pickle(Path(_TEST_DATA).joinpath("linux_proc_test.pkl"))


def test_build_win_tree():
    p_tree = ptutil.build_process_tree(testdf_win, show_progress=True)
    assert ptutil.get_summary_info(p_tree) == {
        "Processes": 1010,
        "RootProcesses": 10,
        "LeafProcesses": 815,
        "BranchProcesses": 185,
        "IsolatedProcesses": 0,
        "LargestTreeDepth": 7,
    }


def test_build_lx_tree():
    p_tree_l = ptutil.build_process_tree(testdf_lx, show_progress=False)
    assert ptutil.get_summary_info(p_tree_l) == {
        "Processes": 1029,
        "RootProcesses": 29,
        "LeafProcesses": 497,
        "BranchProcesses": 503,
        "IsolatedProcesses": 0,
        "LargestTreeDepth": 5,
    }


def test_tree_utils_win():
    p_tree = ptutil.build_process_tree(testdf_win, show_progress=True)

    assert len(ptutil.get_roots(p_tree)) == 10
    t_root = ptutil.get_roots(p_tree).iloc[4]
    full_tree = ptutil.get_descendents(p_tree, t_root)
    assert len(full_tree) == 25
    children = ptutil.get_children(p_tree, t_root)
    assert len(children) == 13

    depth = full_tree["path"].str.count("/").max() + 1
    bottom_desc = full_tree[full_tree["path"].str.count("/") == depth - 1].iloc[0]

    assert len(ptutil.get_ancestors(p_tree, bottom_desc)) == 3

    assert isinstance(ptutil.get_parent(p_tree, bottom_desc), pd.Series)
    assert (
        ptutil.get_process(p_tree, bottom_desc.name).dropna() == bottom_desc.dropna()
    ).all()
    assert (
        ptutil.get_process(p_tree, bottom_desc).dropna() == bottom_desc.dropna()
    ).all()
    assert ptutil.build_process_key(bottom_desc) == bottom_desc.name

    assert (ptutil.get_root(p_tree, bottom_desc).dropna() == t_root.dropna()).all()

    children2 = ptutil.get_children(p_tree, t_root, include_source=False)
    assert len(children2) == len(
        ptutil.get_siblings(p_tree, children2.iloc[0], include_source=True)
    )
    assert len(children2) == (
        len(ptutil.get_siblings(p_tree, children2.iloc[0], include_source=False)) + 1
    )
    assert ptutil.get_summary_info(p_tree) == {
        "Processes": 1010,
        "RootProcesses": 10,
        "LeafProcesses": 815,
        "BranchProcesses": 185,
        "IsolatedProcesses": 0,
        "LargestTreeDepth": 7,
    }

    assert ptutil.infer_schema(p_tree) == ptutil.WIN_EVENT_SCH


def test_tree_utils_lx():
    p_tree_l = ptutil.build_process_tree(testdf_lx, show_progress=False)
    assert len(ptutil.get_roots(p_tree_l)) == 29
    t_root = ptutil.get_roots(p_tree_l).iloc[0]
    full_tree = ptutil.get_descendents(p_tree_l, t_root)
    assert len(full_tree) == 871
    children = ptutil.get_children(p_tree_l, t_root)
    assert len(children) == 436

    depth = full_tree["path"].str.count("/").max() + 1
    bottom_desc = full_tree[full_tree["path"].str.count("/") == depth - 1].iloc[0]

    assert len(ptutil.get_ancestors(p_tree_l, bottom_desc)) == 3

    assert isinstance(ptutil.get_parent(p_tree_l, bottom_desc), pd.Series)
    assert (
        ptutil.get_process(p_tree_l, bottom_desc.name).dropna() == bottom_desc.dropna()
    ).all()
    assert (
        ptutil.get_process(p_tree_l, bottom_desc).dropna() == bottom_desc.dropna()
    ).all()
    assert ptutil.build_process_key(bottom_desc) == bottom_desc.name

    assert (ptutil.get_root(p_tree_l, bottom_desc).dropna() == t_root.dropna()).all()

    children2 = ptutil.get_children(p_tree_l, t_root, include_source=False)
    assert len(children2) == len(
        ptutil.get_siblings(p_tree_l, children2.iloc[0], include_source=True)
    )
    assert len(children2) == (
        len(ptutil.get_siblings(p_tree_l, children2.iloc[0], include_source=False)) + 1
    )
    assert ptutil.get_summary_info(p_tree_l) == {
        "Processes": 1029,
        "RootProcesses": 29,
        "LeafProcesses": 497,
        "BranchProcesses": 503,
        "IsolatedProcesses": 0,
        "LargestTreeDepth": 5,
    }

    assert ptutil.infer_schema(p_tree_l) == ptutil.LX_EVENT_SCH
