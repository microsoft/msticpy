# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""process tree utils test class."""
import ast
import os
from pathlib import Path

import nbformat
from nbconvert.preprocessors import ExecutePreprocessor, CellExecutionError
import pandas as pd
import pytest

from msticpy.nbtools.process_tree import build_and_show_process_tree
from msticpy.sectools import process_tree_utils as pt_util
from msticpy.sectools import proc_tree_builder as pt_build

_test_data_folders = [
    d for d, _, _ in os.walk(os.getcwd()) if d.endswith("/tests/testdata")
]
if len(_test_data_folders) == 1:
    _TEST_DATA = _test_data_folders[0]
else:
    _TEST_DATA = "./tests/testdata"

testdf_win = pd.read_pickle(Path(_TEST_DATA).joinpath("win_proc_test.pkl"))
testdf_lx = pd.read_pickle(Path(_TEST_DATA).joinpath("linux_proc_test.pkl"))
testdf_win_mde = pd.read_csv(
    Path(_TEST_DATA).joinpath("mde_proc_cs1.csv"),
    parse_dates=[
        "CreatedProcessCreationTime",
        "CreatedProcessFileCreationTime",
        "InitiatingProcessCreationTime",
        "InitiatingProcessParentCreationTime",
        "CreatedProcessParentCreationTimeUtc",
        "ReportTime",
        "ReportArrivalTimeUtc",
        "CreatedProcessReparentingProcessCreationTimeUtc",
        "CreatedProcessParentCreationTimeUtc",
        "InitiatingProcessImagePeTimestampUtc",
        "InitiatingProcessImageLastWriteTimeUtc",
        "InitiatingProcessImageLastAccessTimeUtc",
        "InitiatingProcessImageCreationTimeUtc",
    ],
)

# MDE Schema
# CreatedProcessIsElevated,
# CreatedProcessIntegrityLevel,
# CreatedProcessAccountSid,
# CreatedProcessAccountName,
# CreatedProcessAccountDomainName,
# CreatedProcessTokenElevationType,
# CreatedProcessFileMarkOfTheWeb,
# CreatedProcessCreationTime,
# CreatedProcessId,
# CreatedProcessName,
# CreatedProcessCommandLine,
# CreatedProcessFileType,
# CreatedProcessFileCreationTime,
# CreatedProcessFilePath,
# CreatedProcessFileSize,
# CreatedProcessFileMd5,
# CreatedProcessFileSha256,
# CreatedProcessFileSha1,
# InitiatingProcessImageMd5,
# InitiatingProcessImageSha256,
# InitiatingProcessImageSha1,
# InitiatingProcessAccountSid,
# InitiatingProcessAccountDomainName,
# InitiatingProcessAccountName,
# InitiatingProcessCreationTime,
# InitiatingProcessId,
# InitiatingProcessName,
# Process_CommandLine,
# IsElevatedProcess,
# InitiatingProcessParentCreationTime,
# InitiatingProcessParentProcessId,
# InitiatingProcessParentProcessName,
# InitiatingProcessIntegrityLevel,
# InitiatingProcessTokenElevationType,
# WcdMachineId,
# SenseMachineGuid,
# ReportTime,
# ReportArrivalTimeUtc,
# ReportGuid,
# ComputerDnsName,
# ReportIndex,
# IsLastInQuota,
# ClientVersion,
# IsTestOrg,
# InitiatingProcessStartKey,
# ContainerId,
# CreatedProcessStartKey,
# CreatedProcessReparentingProcessCreationTimeUtc,
# CreatedProcessReparentingProcessId,
# CreatedProcessParentCreationTimeUtc,
# CreatedProcessParentName,
# CreatedProcessParentId,
# CreatedProcessAttributes,
# InitiatingProcessSource,
# InitiatingProcessImageFilePath,
# InitiatingProcessImageFileSizeInBytes,
# InitiatingProcessImagePeTimestampUtc,
# InitiatingProcessImageLastWriteTimeUtc,
# InitiatingProcessImageLastAccessTimeUtc,
# InitiatingProcessImageCreationTimeUtc,
# InitiatingProcessAttributes,
# TruncationPolicy,
# RbacGroupId,
# OsVersionK,
# OsVersion,
# IsMalformed,
# InitiatingProcessAccountUpn,
# InitiatingProcessAccountAzureADId,
# CreatedProcessAccountUpn,
# CreatedProcessAccountAzureADId,
# IsMtpEnabled,
# TenantId,
# InitiatingProcessLogonId,
# LogonId,
# FirstSeen,
# InitiatingProcessShowWindow,
# InitiatingProcessStartupFlags,
# CreatedProcessShowWindow,
# CreatedProcessStartupFlags,
# InitiatingProcessCurrentWorkingDirectory,
# InitiatingProcessPosixProcessGroupId,
# InitiatingProcessPosixSessionId,
# InitiatingProcessPosixEffectiveUser,
# InitiatingProcessPosixEffectiveGroup,
# InitiatingProcessPosixAttachedTerminal,
# InitiatingProcessSignatureStatus,
# InitiatingProcessSignerType,
# CreatedProcessSignatureStatus,
# CreatedProcessSignerType,
# InitiatingProcessVersionInfoCompanyName,
# InitiatingProcessVersionInfoProductName,
# InitiatingProcessVersionInfoProductVersion,
# InitiatingProcessVersionInfoInternalFileName,
# InitiatingProcessVersionInfoOriginalFileName,
# InitiatingProcessVersionInfoFileDescription,
# CreatedProcessVersionInfoCompanyName,
# CreatedProcessVersionInfoProductName,
# CreatedProcessVersionInfoProductVersion,
# CreatedProcessVersionInfoInternalFileName,
# CreatedProcessVersionInfoOriginalFileName,
# CreatedProcessVersionInfoFileDescription


def test_build_win_tree():
    p_tree = pt_build.build_process_tree(testdf_win, show_summary=True, debug=True)
    assert pt_util.get_summary_info(p_tree) == {
        "Processes": 1010,
        "RootProcesses": 10,
        "LeafProcesses": 815,
        "BranchProcesses": 185,
        "IsolatedProcesses": 0,
        "LargestTreeDepth": 7,
    }


def test_build_lx_tree():
    p_tree_l = pt_build.build_process_tree(testdf_lx, show_summary=False, debug=True)
    assert pt_util.get_summary_info(p_tree_l) == {
        "Processes": 1029,
        "RootProcesses": 29,
        "LeafProcesses": 497,
        "BranchProcesses": 503,
        "IsolatedProcesses": 0,
        "LargestTreeDepth": 5,
    }


def test_build_win_tree_dict_schema():
    schema = dict(
        time_stamp="TimeGenerated",
        process_name="NewProcessName",
        process_id="NewProcessId",
        parent_name="ParentProcessName",
        parent_id="ProcessId",
        logon_id="SubjectLogonId",
        target_logon_id="TargetLogonId",
        cmd_line="CommandLine",
        user_name="SubjectUserName",
        path_separator="\\",
        user_id="SubjectUserSid",
        event_id_column="EventID",
        event_id_identifier=4688,
        host_name_column="Computer",
    )
    p_tree = pt_build.build_process_tree(
        testdf_win, schema=schema, show_summary=True, debug=True
    )
    assert pt_util.get_summary_info(p_tree) == {
        "Processes": 1010,
        "RootProcesses": 10,
        "LeafProcesses": 815,
        "BranchProcesses": 185,
        "IsolatedProcesses": 0,
        "LargestTreeDepth": 7,
    }


def test_tree_utils_win():
    p_tree = pt_build.build_process_tree(testdf_win, show_summary=True, debug=True)

    assert len(pt_util.get_roots(p_tree)) == 10
    t_root = pt_util.get_roots(p_tree).iloc[4]
    full_tree = pt_util.get_descendents(p_tree, t_root)
    assert len(full_tree) == 25
    children = pt_util.get_children(p_tree, t_root)
    assert len(children) == 13

    depth = full_tree["path"].str.count("/").max() + 1
    bottom_desc = full_tree[full_tree["path"].str.count("/") == depth - 1].iloc[0]

    assert len(pt_util.get_ancestors(p_tree, bottom_desc)) == 3

    assert isinstance(pt_util.get_parent(p_tree, bottom_desc), pd.Series)
    assert (
        pt_util.get_process(p_tree, bottom_desc.name).dropna() == bottom_desc.dropna()
    ).all()
    assert (
        pt_util.get_process(p_tree, bottom_desc).dropna() == bottom_desc.dropna()
    ).all()
    # assert ptutil.build_process_key(bottom_desc) == bottom_desc.name

    assert (pt_util.get_root(p_tree, bottom_desc).dropna() == t_root.dropna()).all()

    children2 = pt_util.get_children(p_tree, t_root, include_source=False)
    assert len(children2) == len(
        pt_util.get_siblings(p_tree, children2.iloc[0], include_source=True)
    )
    assert len(children2) == (
        len(pt_util.get_siblings(p_tree, children2.iloc[0], include_source=False)) + 1
    )
    assert pt_util.get_summary_info(p_tree) == {
        "Processes": 1010,
        "RootProcesses": 10,
        "LeafProcesses": 815,
        "BranchProcesses": 185,
        "IsolatedProcesses": 0,
        "LargestTreeDepth": 7,
    }

    assert pt_build.infer_schema(p_tree) == pt_build.WIN_EVENT_SCH


def test_tree_utils_lx():
    p_tree_l = pt_build.build_process_tree(testdf_lx, show_summary=False, debug=True)
    assert len(pt_util.get_roots(p_tree_l)) == 29
    t_root = pt_util.get_roots(p_tree_l).iloc[0]
    full_tree = pt_util.get_descendents(p_tree_l, t_root)
    assert len(full_tree) == 901
    children = pt_util.get_children(p_tree_l, t_root)
    assert len(children) == 452

    depth = full_tree["path"].str.count("/").max() + 1
    bottom_desc = full_tree[full_tree["path"].str.count("/") == depth - 1].iloc[0]

    assert len(pt_util.get_ancestors(p_tree_l, bottom_desc)) == 3

    assert isinstance(pt_util.get_parent(p_tree_l, bottom_desc), pd.Series)
    assert (
        pt_util.get_process(p_tree_l, bottom_desc.name).dropna() == bottom_desc.dropna()
    ).all()
    assert (
        pt_util.get_process(p_tree_l, bottom_desc).dropna() == bottom_desc.dropna()
    ).all()
    # assert ptutil.build_process_key(bottom_desc) == bottom_desc.name

    assert (pt_util.get_root(p_tree_l, bottom_desc).dropna() == t_root.dropna()).all()

    children2 = pt_util.get_children(p_tree_l, t_root, include_source=False)
    assert len(children2) == len(
        pt_util.get_siblings(p_tree_l, children2.iloc[0], include_source=True)
    )
    assert len(children2) == (
        len(pt_util.get_siblings(p_tree_l, children2.iloc[0], include_source=False)) + 1
    )
    assert pt_util.get_summary_info(p_tree_l) == {
        "Processes": 1029,
        "RootProcesses": 29,
        "LeafProcesses": 497,
        "BranchProcesses": 503,
        "IsolatedProcesses": 0,
        "LargestTreeDepth": 5,
    }

    assert pt_build.infer_schema(p_tree_l) == pt_build.LX_EVENT_SCH


def test_build_process_tree():
    build_and_show_process_tree(testdf_win, legend_col="NewProcessName")
    build_and_show_process_tree(testdf_win, legend_col="NewProcessName")


def test_build_mde_win_tree_dict_schema():
    schema = dict(
        time_stamp="CreatedProcessCreationTime",
        process_name="CreatedProcessName",
        process_id="CreatedProcessId",
        parent_name="ParentProcessName",
        parent_id="CreatedProcessParentId",
        logon_id="InitiatingProcessLogonId",
        target_logon_id="LogonId",
        cmd_line="CreatedProcessCommandLine",
        user_name="CreatedProcessAccountName",
        path_separator="\\",
        user_id="CreatedProcessAccountSid",
        # event_id_column="EventID",
        # event_id_identifier=4688,
        host_name_column="ComputerDnsName",
    )
    p_tree = pt_build.build_process_tree(
        testdf_win_mde, schema=schema, show_summary=True, debug=True
    )
    assert pt_util.get_summary_info(p_tree) == {
        "Processes": 1642,
        "RootProcesses": 9,
        "LeafProcesses": 1177,
        "BranchProcesses": 456,
        "IsolatedProcesses": 0,
        "LargestTreeDepth": 16,
    }


_NB_FOLDER = "docs/notebooks"
_NB_NAME = "ProcessTree.ipynb"


@pytest.mark.skipif(
    not os.environ.get("MSTICPY_TEST_NOSKIP"), reason="Skipped for local tests."
)
def test_process_tree_notebook():
    nb_path = Path(_NB_FOLDER).joinpath(_NB_NAME)
    abs_path = Path(_NB_FOLDER).absolute()
    with open(nb_path) as f:
        nb = nbformat.read(f, as_version=4)
    ep = ExecutePreprocessor(timeout=600, kernel_name="python3")

    try:
        ep.preprocess(nb, {"metadata": {"path": abs_path}})
    except CellExecutionError:
        nb_err = str(nb_path).replace(".ipynb", "-err.ipynb")
        msg = f"Error executing the notebook '{nb_path}'.\n"
        msg += f"See notebook '{nb_err}' for the traceback."
        print(msg)
        with open(nb_err, mode="w", encoding="utf-8") as f:
            nbformat.write(nb, f)
        raise
