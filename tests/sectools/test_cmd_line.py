# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
import os

import pandas as pd

from pytest import raises

from msticpy.common.exceptions import MsticpyException
from msticpy.analysis.data import cmd_line as cl

from ..unit_test_lib import TEST_DATA_PATH


def test_risky_cmd_line():
    input_file = os.path.join(TEST_DATA_PATH, "sudo_data.csv")
    input_df = pd.read_csv(input_file)
    output = cl.risky_cmd_line(events=input_df, log_type="Syslog")
    assert len(output) >= 1  # nosec
    assert type(output) == dict  # nosec
    assert output["2019-07-05T18:19:52.873Z"] == "/bin/bash"  # nosec
    with raises(MsticpyException):
        cl.risky_cmd_line(events=input_df, log_type="Syslog", cmd_field="Test")


def test_cmd_speed():
    input_file = os.path.join(TEST_DATA_PATH, "sudo_data_speed.csv")
    input_df = pd.read_csv(input_file, parse_dates=["TimeGenerated"])
    output = cl.cmd_speed(cmd_events=input_df, cmd_field="Command")
    assert len(output) >= 1  # nosec
    assert type(output[0]) == dict  # nosec
    with raises(MsticpyException):
        output = cl.cmd_speed(cmd_events=input_df, cmd_field="Test")


def test_syslog_risky_actions():
    input_file = os.path.join(TEST_DATA_PATH, "syslog_data.csv")
    input_df = pd.read_csv(input_file)
    risky_stuff = os.path.join(TEST_DATA_PATH, "risky_stuff_custom.json")
    output = cl.risky_cmd_line(
        events=input_df,
        log_type="Syslog",
        cmd_field="SyslogMessage",
        detection_rules=risky_stuff,
    )
    assert len(output) >= 1  # nosec
    assert type(output) == dict  # nosec
