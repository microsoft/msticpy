# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------

import os
import pandas as pd
from ..msticpy.nbtools.entityschema import Host
from ..msticpy.sectools import syslog_utils as ls
from ..msticpy.sectools import cmd_line as cl

_test_data_folders = [
    d for d, _, _ in os.walk(os.getcwd()) if d.endswith("/tests/testdata")
]
if len(_test_data_folders) == 1:
    _TEST_DATA = _test_data_folders[0]
else:
    _TEST_DATA = "./tests/testdata"


def test_cluster_syslog_logons_df():
    input_file = os.path.join(_TEST_DATA, "linux_logons.csv")
    input_df = pd.read_csv(input_file, parse_dates=["TimeGenerated"])
    output = ls.cluster_syslog_logons_df(input_df)
    assert len(output.index) >= 1  # nosec


def test_host_data():
    syslog_file = os.path.join(_TEST_DATA, "syslog_data.csv")
    syslog_df = pd.read_csv(syslog_file, parse_dates=["TimeGenerated"])
    heartbeat_file = os.path.join(_TEST_DATA, "host_hb.csv")
    heartbeat_df = pd.read_csv(heartbeat_file)
    az_net_file = os.path.join(_TEST_DATA, "az_net.csv")
    az_net_df = pd.read_csv(az_net_file)
    host_record = ls.create_host_record(syslog_df, heartbeat_df, az_net_df)
    assert type(host_record) == Host  # nosec
    assert host_record.OSType == "Linux"  # nosec


def test_cluster_sudo_sessions():
    input_file = os.path.join(_TEST_DATA, "sudo_events.csv")
    input_df = pd.read_csv(input_file, parse_dates=["TimeGenerated"])
    output = ls.cluster_syslog_logons_df(input_df)
    assert len(output.index) >= 1  # nosec


def test_risky_sudo_sessions():
    input_file = os.path.join(_TEST_DATA, "sudo_session_test.csv")
    sudo_events = pd.read_csv(input_file, parse_dates=["TimeGenerated"])
    risky_actions = cl.risky_cmd_line(events=sudo_events, log_type="Syslog")
    suspicious_events = cl.cmd_speed(
        cmd_events=sudo_events, cmd_field="Command", time=60, events=2
    )
    sudo_sessions = ls.cluster_syslog_logons_df(logon_events=sudo_events)
    output = ls.risky_sudo_sessions(
        risky_actions=risky_actions,
        suspicious_actions=suspicious_events,
        sudo_sessions=sudo_sessions,
    )
    assert len(output) == 2  # nosec
    assert type(output) == dict  # nosec
