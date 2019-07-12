# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------

import os
import pandas as pd
from ..msticpy.data.data_providers import QueryProvider
from ..msticpy.nbtools.entityschema import Host
from ..msticpy.sectools import linuxsyslog as ls

_test_data_folders = [d for d, _, _ in os.walk(os.getcwd()) if d.endswith('/tests/testdata')]
if len(_test_data_folders) == 1:
    _TEST_DATA = _test_data_folders[0]
else:
    _TEST_DATA = './tests/testdata'

def test_cluster_syslog_logons():
    #How to get realistic test data (with correct time stamps)?
    input_file = os.path.join(_TEST_DATA, 'linux_logons.csv')
    input_df = pd.read_csv(input_file)
    output = ls.cluster_syslog_logons(input_df)
    assert len(output) >= 1 

def test_host_data():
    syslog_file = os.path.join(_TEST_DATA, 'syslog_data.csv')
    syslog_df = pd.read_csv(syslog_file)
    heartbeat_file = os.path.join(_TEST_DATA, 'host_hb.csv')
    heartbeat_df = pd.read_csv(heartbeat_file)
    az_net_file = os.path.join(_TEST_DATA, 'az_net.csv')
    az_net_df = pd.read_csv(az_net_file)
    host_record = ls.create_host_record(syslog_df, heartbeat_df, az_net_df)
    assert type(host_record) == Host
    assert host_record.OSType == "Linux"


def test_sudo_risk_actions():
    input_file = os.path.join(_TEST_DATA, 'sudo_data.csv')
    risky_stuff = os.path.join(_TEST_DATA, 'risky_stuff_custom.txt')
    input_df = pd.read_csv(input_file)
    output = ls.sudo_risk_actions(input_df, risky_stuff)
    assert len(output) >= 1
    assert type(output[1]) == str
    assert output[0] == '/usr/sbin/squid'

def test_sudo_risk_actions_default():
    input_file = os.path.join(_TEST_DATA, 'sudo_data.csv')
    input_df = pd.read_csv(input_file)
    output = ls.sudo_risk_actions(input_df)
    assert len(output) >= 1
    assert type(output[0]) == str
    assert output[0] == '/bin/bash'

def test_speed():
    input_file = os.path.join(_TEST_DATA, 'sudo_data_speed.csv')
    input_df = pd.read_csv(input_file)
    output = ls.sudo_actions_speed(input_df)
    assert len(output) >= 1
    assert type(output[0]) == dict

def cluster_sudo_sessions():
    input_file = os.path.join(_TEST_DATA, 'sudo_events.csv')
    input_df = pd.read_csv(input_file)
    output = ls.cluster_syslog_logons(input_df)
    assert len(output) >= 1 