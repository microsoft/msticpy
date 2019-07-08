# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------

import pandas as pd
import os
from  ..msticpy.sectools.linuxsyslog import create_host_record, cluster_syslog_logons
from ..msticpy.nbtools.entityschema import Host
from ..msticpy.data.data_providers import QueryProvider



_test_data_folders = [d for d, _, _ in os.walk(os.getcwd()) if d.endswith('/tests/testdata')]
if len(_test_data_folders) == 1:
    _TEST_DATA = _test_data_folders[0]
else:
    _TEST_DATA = './tests/testdata'

def test_cluster_syslog_logons():
    #How to get realistic test data (with correct time stamps)?
    input_file = os.path.join(_TEST_DATA, 'linux_logons.csv')
    input_df = pd.read_csv(input_file)
    output = cluster_syslog_logons(input_df)
    assert len(output) >= 1 

def test_host_data():
    syslog_file = os.path.join(_TEST_DATA, 'syslog_data.csv')
    syslog_df = pd.read_csv(syslog_file)
    heartbeat_file = os.path.join(_TEST_DATA, 'host_hb.csv')
    heartbeat_df = pd.read_csv(heartbeat_file)
    az_net_file = os.path.join(_TEST_DATA, 'az_net.csv')
    az_net_df = pd.read_csv(az_net_file)
    host_record = create_host_record(syslog_df, heartbeat_df, az_net_df)
    assert type(host_record) == Host
    assert host_record.OSType == "Linux"