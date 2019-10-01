# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------

import os
import pandas as pd
from ..msticpy.nbtools.entityschema import Host
from ..msticpy.sectools import syslog_utils as ls

_test_data_folders = [
    d for d, _, _ in os.walk(os.getcwd()) if d.endswith("/tests/testdata")
]
if len(_test_data_folders) == 1:
    _TEST_DATA = _test_data_folders[0]
else:
    _TEST_DATA = "./tests/testdata"


def test_cluster_syslog_logons():
    input_file = os.path.join(_TEST_DATA, "linux_logons.csv")
    input_df = pd.read_csv(input_file, parse_dates=["TimeGenerated"])
    output = ls.cluster_syslog_logons(input_df)
    assert len(output) >= 1


def test_cluster_syslog_logons_df():
    input_file = os.path.join(_TEST_DATA, "linux_logons.csv")
    input_df = pd.read_csv(input_file, parse_dates=["TimeGenerated"])
    output = ls.cluster_syslog_logons_df(input_df)
    assert len(output.index) >= 1


def test_host_data():
    syslog_file = os.path.join(_TEST_DATA, "syslog_data.csv")
    syslog_df = pd.read_csv(syslog_file, parse_dates=["TimeGenerated"])
    heartbeat_file = os.path.join(_TEST_DATA, "host_hb.csv")
    heartbeat_df = pd.read_csv(heartbeat_file)
    az_net_file = os.path.join(_TEST_DATA, "az_net.csv")
    az_net_df = pd.read_csv(az_net_file)
    host_record = ls.create_host_record(syslog_df, heartbeat_df, az_net_df)
    assert type(host_record) == Host
    assert host_record.OSType == "Linux"


def test_sudo_risky_actions_default():
    input_file = os.path.join(_TEST_DATA, "sudo_data.csv")
    input_df = pd.read_csv(input_file)
    output = ls.risky_cmd_line(input_df)
    assert len(output) >= 1
    assert type(output) == dict
    assert output["2019-07-05T18:19:52.873Z"] == "/bin/bash"


def test_speed():
    input_file = os.path.join(_TEST_DATA, "sudo_data_speed.csv")
    input_df = pd.read_csv(input_file, parse_dates=["TimeGenerated"])
    output = ls.sudo_actions_speed(input_df)
    assert len(output) >= 1
    assert type(output[0]) == dict


def test_cluster_sudo_sessions():
    input_file = os.path.join(_TEST_DATA, "sudo_events.csv")
    input_df = pd.read_csv(input_file, parse_dates=["TimeGenerated"])
    output = ls.cluster_syslog_logons(input_df)
    assert len(output) >= 1


def test_risky_sudo_sessions():
    input_file = os.path.join(_TEST_DATA, "sudo_session_test.csv")
    sudo_events = pd.read_csv(input_file, parse_dates=["TimeGenerated"])
    risky_actions = ls.risky_cmd_line(events=sudo_events)
    suspicious_events = ls.sudo_actions_speed(
        sudo_events=sudo_events, time=60, events=2
    )
    sudo_sessions = ls.cluster_syslog_logons(logon_events=sudo_events)
    output = ls.risky_sudo_sessions(
        risky_actions=risky_actions,
        suspicious_actions=suspicious_events,
        sudo_sessions=sudo_sessions,
    )
    assert len(output) == 2
    assert type(output) == dict


def test_syslog_risky_actions():
    input_file = os.path.join(_TEST_DATA, "syslog_data.csv")
    input_df = pd.read_csv(input_file)
    risky_stuff = os.path.join(_TEST_DATA, "risky_stuff_custom.json")
    output = ls.risky_cmd_line(input_df, risky_stuff)
    assert len(output) >= 1
    assert type(output) == dict
