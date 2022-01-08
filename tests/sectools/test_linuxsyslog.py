# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
import os
from pathlib import Path
import warnings

import pandas as pd
import pytest
from pytest import raises

from msticpy.datamodel.entities import Host
from msticpy.sectools import syslog_utils as ls
from msticpy.common.exceptions import MsticpyException
from msticpy.sectools import cmd_line as cl
from msticpy.common.provider_settings import get_provider_settings
from msticpy.sectools.geoip import GeoIPDatabaseException

from ..unit_test_lib import get_test_data_path, custom_mp_config

_TEST_DATA = get_test_data_path()


def test_cluster_syslog_logons_df():
    input_file = os.path.join(_TEST_DATA, "linux_logons.csv")
    input_df = pd.read_csv(input_file, parse_dates=["TimeGenerated"])
    output = ls.cluster_syslog_logons_df(input_df)
    assert len(output.index) >= 1  # nosec
    with raises(MsticpyException):
        empty_logons = pd.DataFrame(columns=["TimeGenerated", "SyslogMessage"])
        ls.cluster_syslog_logons_df(logon_events=empty_logons)


@pytest.mark.skipif(
    os.environ.get("MSTICPY_BUILD_SOURCE").casefold() == "fork", reason="External fork."
)
def test_host_data():
    test_config1 = Path(_TEST_DATA).parent.joinpath("msticpyconfig-test.yaml")

    with custom_mp_config(test_config1):
        with warnings.catch_warnings():
            # We want to ignore warnings from missing config
            warnings.simplefilter("ignore", category=UserWarning)

            syslog_file = os.path.join(_TEST_DATA, "syslog_data.csv")
            syslog_df = pd.read_csv(syslog_file, parse_dates=["TimeGenerated"])
            heartbeat_file = os.path.join(_TEST_DATA, "host_hb.csv")
            heartbeat_df = pd.read_csv(heartbeat_file)
            az_net_file = os.path.join(_TEST_DATA, "az_net.csv")
            az_net_df = pd.read_csv(az_net_file)
            try:
                host_record = ls.create_host_record(syslog_df, heartbeat_df, az_net_df)
                assert type(host_record) == Host  # nosec
                assert host_record.OSType == "Linux"  # nosec

            except GeoIPDatabaseException:
                # test will fail if no GeoIP database exists or can be downloaded
                other_provider_settings = get_provider_settings(
                    config_section="OtherProviders"
                ).get("GeoIPLite", {})
                geolite_key = None
                if other_provider_settings:
                    geolite_key = other_provider_settings.args.get("AuthKey")
                if not geolite_key:
                    warnings.resetwarnings()
                    warnings.warn(
                        message=(
                            "No configuration value found for GeoLite key. "
                            + "Test test_host_data skipped.",
                        )
                    )
                    return
                assert False


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
    with raises(MsticpyException):
        ls.risky_sudo_sessions(sudo_sessions=sudo_sessions)
