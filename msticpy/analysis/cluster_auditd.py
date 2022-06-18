# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Auditd cluster function."""
import pandas as pd

from .._version import VERSION
from .eventcluster import add_process_features, dbcluster_events

__version__ = VERSION
__author__ = "Ian Hellen"


def cluster_auditd_processes(audit_data: pd.DataFrame, app: str = None) -> pd.DataFrame:
    """
    Clusters process data into specific processes.

    Parameters
    ----------
    audit_data : pd.DataFrame
        The Audit data containing process creation events
    app: str, optional
        The name of a specific app you wish to cluster

    Returns
    -------
    pd.DataFrame
        Details of the clustered process

    """
    if app is not None:
        processes = audit_data[audit_data["exe"].str.contains(app, na=False)]
    else:
        processes = audit_data
    processes = processes.rename(
        columns={
            "acct": "SubjectUserName",
            "uid": "SubjectUserSid",
            "user": "SubjectUserName",
            "ses": "SubjectLogonId",
            "pid": "NewProcessId",
            "exe": "NewProcessName",
            "ppid": "ProcessId",
            "cmdline": "CommandLine",
        }
    )
    req_cols = [
        "cwd",
        "SubjectUserName",
        "SubjectUserSid",
        "SubjectUserName",
        "SubjectLogonId",
        "NewProcessId",
        "NewProcessName",
        "ProcessId",
        "CommandLine",
    ]
    for col in req_cols:
        if col not in processes:
            processes[col] = ""

    feature_procs_h1 = add_process_features(input_frame=processes)

    clus_events, _, _ = dbcluster_events(
        data=feature_procs_h1,
        cluster_columns=["pathScore", "SubjectUserSid"],
        time_column="TimeGenerated",
        max_cluster_distance=0.0001,
    )
    (
        clus_events.sort_values("TimeGenerated")[
            [
                "TimeGenerated",
                "LastEventTime",
                "NewProcessName",
                "CommandLine",
                "SubjectLogonId",
                "SubjectUserSid",
                "pathScore",
                "isSystemSession",
                "ProcessId",
                "ClusterSize",
            ]
        ].sort_values("ClusterSize", ascending=True)
    )

    procs = clus_events[
        [
            "TimeGenerated",
            "NewProcessName",
            "CommandLine",
            "NewProcessId",
            "SubjectUserSid",
            "cwd",
            "ClusterSize",
            "ProcessId",
        ]
    ]
    procs = procs.rename(columns={"NewProcessId": "pid", "ProcessId": "ppid"})

    return procs
