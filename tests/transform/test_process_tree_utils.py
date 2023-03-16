# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""process tree utils test class."""
import os
from pathlib import Path

import pandas as pd
import pytest

from msticpy.transform import proc_tree_builder as pt_build
from msticpy.transform import process_tree_utils as pt_util
from msticpy.transform.proc_tree_schema import LX_EVENT_SCH, WIN_EVENT_SCH
from msticpy.vis.process_tree import build_and_show_process_tree

from ..unit_test_lib import (
    TEST_DATA_PATH,
    custom_mp_config,
    exec_notebook,
    get_test_data_path,
)

testdf_win = pd.read_pickle(Path(TEST_DATA_PATH).joinpath("win_proc_test.pkl"))
testdf_lx = pd.read_pickle(Path(TEST_DATA_PATH).joinpath("linux_proc_test.pkl"))
testdf_win_min = pd.read_csv(
    Path(TEST_DATA_PATH).joinpath("winproctree-minimal.csv"),
    index_col=0,
    parse_dates=["CreateTime"],
)
testdf_mde_pub = pd.read_pickle(Path(TEST_DATA_PATH).joinpath("mde_proc_pub.pkl"))
testdf_win_mde = pd.read_csv(
    Path(TEST_DATA_PATH).joinpath("mde_proc_cs1.csv"),
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
    """Test building process tree - no plotting."""
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
    """Test building process tree - no plotting."""
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
    """Test building process tree with custom schema - no plotting."""
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


def test_build_win_tree_dict_schema_no_logon_id():
    """Test building process tree with custom schema - no plotting."""
    schema = dict(
        time_stamp="TimeGenerated",
        process_name="NewProcessName",
        process_id="NewProcessId",
        parent_name="ParentProcessName",
        parent_id="ProcessId",
        logon_id=None,
        target_logon_id=None,
        cmd_line="CommandLine",
        user_name="SubjectUserName",
        path_separator="\\",
        user_id="SubjectUserSid",
        event_id_column="EventID",
        event_id_identifier=4688,
        host_name_column="Computer",
    )
    testdf_win_no_logonid = testdf_win.drop(columns=["SubjectLogonId", "TargetLogonId"])
    p_tree = pt_build.build_process_tree(
        testdf_win_no_logonid, schema=schema, show_summary=True, debug=True
    )
    assert pt_util.get_summary_info(p_tree) == {
        "Processes": 1010,
        "RootProcesses": 10,
        "LeafProcesses": 815,
        "BranchProcesses": 185,
        "IsolatedProcesses": 0,
        "LargestTreeDepth": 7,
    }


def test_build_tree_minimal():
    """Test build tree with minimal columns."""
    cust_win_schema = {
        "process_name": "ImageFileName",
        "process_id": "PID",
        "parent_id": "PPID",
        "time_stamp": "CreateTime",
        "target_logon_id": "TargetLogonId",
        "user_id": "SubjectUserSid",
    }
    p_tree = pt_build.build_process_tree(
        testdf_win_min, schema=cust_win_schema, show_summary=True, debug=True
    )
    summary = pt_util.get_summary_info(p_tree)
    assert summary == {
        "Processes": 126,
        "RootProcesses": 10,
        "LeafProcesses": 97,
        "BranchProcesses": 19,
        "IsolatedProcesses": 0,
        "LargestTreeDepth": 7,
    }


def test_tree_utils_win():
    """Test process tree utils."""
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

    assert pt_build.infer_schema(p_tree) == WIN_EVENT_SCH


def test_tree_utils_lx():
    """Test process tree utils."""
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

    assert pt_build.infer_schema(p_tree_l) == LX_EVENT_SCH


def test_build_and_plot_process_tree_win():
    """Test build and plot process tree."""
    build_and_show_process_tree(testdf_win, legend_col="NewProcessName")


def test_build_and_plot_process_tree_lx():
    """Test build and plot process tree."""
    build_and_show_process_tree(testdf_lx, legend_col="exe")


def test_build_and_plot_process_tree_mde():
    """Test build and plot process tree."""
    build_and_show_process_tree(testdf_mde_pub, legend_col="FileName")


def test_build_and_plot_min_data_tree():
    """Test that minimal data source plots without error."""

    cust_win_schema = {
        "process_name": "ImageFileName",
        "process_id": "PID",
        "parent_id": "PPID",
        "time_stamp": "CreateTime",
    }
    build_and_show_process_tree(testdf_win_min, schema=cust_win_schema)
    # add non-existing ID cols and re-run with minimal schema
    cust_win_schema.update(
        {
            "target_logon_id": "TargetLogonId",
            "user_id": "SubjectUserSid",
        }
    )
    build_and_show_process_tree(
        testdf_win_min,
        schema=cust_win_schema,
    )


def test_build_mde_win_tree_dict_schema():
    """Test build MDE process tree."""
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


def test_text_process_tree():
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
    tree_txt = pt_util.tree_to_text(p_tree, schema=schema)
    assert len(tree_txt.split("\n")) == 5028


_NB_FOLDER = "docs/notebooks"
_NB_NAME = "ProcessTree.ipynb"
_MP_CONFIG_PATH = get_test_data_path().parent.joinpath("msticpyconfig-test.yaml")


@pytest.mark.skipif(
    not os.environ.get("MSTICPY_TEST_NOSKIP"), reason="Skipped for local tests."
)
def test_process_tree_notebook():
    """Run process tree notebook."""
    nb_path = Path(_NB_FOLDER).joinpath(_NB_NAME)

    exec_notebook(nb_path=nb_path, mp_config=_MP_CONFIG_PATH)
