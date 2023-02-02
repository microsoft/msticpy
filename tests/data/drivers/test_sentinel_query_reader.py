# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Github Sentinel Query repo import class and helpers tests"""

import os
from datetime import datetime
from pathlib import Path

import pytest
import pytest_check as check

from msticpy.data.drivers.sentinel_query_reader import (
    SentinelQuery,
    _create_queryfile_metadata,
    _format_query_name,
    _import_sentinel_query,
    _organize_query_list_by_folder,
    get_sentinel_queries_from_github,
    import_sentinel_queries,
    read_yaml_files,
    write_to_yaml,
)

# variables used throughout tests
DEF_PATH = Path.joinpath(Path(os.getcwd()))
BASE_DIR = str(DEF_PATH) + "/Azure-Sentinel-master"


def test_get_sentinel_queries_from_github():
    assert get_sentinel_queries_from_github(outputdir=os.getcwd())


def test_read_yaml_files():
    yaml_files = read_yaml_files(parent_dir=BASE_DIR, child_dir="Detections")
    assert yaml_files[BASE_DIR + "/Detections\\Anomalies\\UnusualAnomaly.yaml"]


def test__import_sentinel_query():
    yaml_files = read_yaml_files(parent_dir=BASE_DIR, child_dir="Detections")
    query_type = "Detections"
    yaml_path = BASE_DIR + "/Detections\\Anomalies\\UnusualAnomaly.yaml"
    yaml_text = yaml_files[yaml_path]
    sample_query = SentinelQuery(
        id="d0255b5f-2a3c-4112-8744-e6757af3283a",
        name="Unusual Anomaly",
        description="'Anomaly Rules generate events in the Anomalies table. This scheduled rule tries to detect Anomalies that are not usual, they could be a type of Anomaly that has recently been activated, or an infrequent type. The detected Anomaly should be reviewed, if it is relevant enough, eventually a separate scheduled Analytics Rule could be created specifically for that Anomaly Type, so an alert and/or incident is generated everytime that type of Anomaly happens.'\n",
        severity="Medium",
        tags=[],
        requiredDataConnectors=[],
        queryFrequency="1h",
        queryPeriod="4d",
        triggerOperator="gt",
        triggerThreshold=0,
        tactics=[],
        relevantTechniques=[],
        query='// You can leave out Anomalies that are already monitored through other Analytics Rules\n//let _MonitoredRules = dynamic(["TestAlertName"]);\nlet query_frequency = 1h;\nlet query_lookback = 3d;\nAnomalies\n| where TimeGenerated > ago(query_frequency)\n//| where not(RuleName has_any (_MonitoredRules))\n| join kind = leftanti (\n    Anomalies\n    | where TimeGenerated between (ago(query_frequency + query_lookback)..ago(query_frequency))\n    | distinct RuleName\n) on RuleName\n',
        entityMappings={},
        customDetails={},
        alertDetailsOverride={},
        version="1.0.1",
        kind="Scheduled",
        folder_name="Anomalies",
        source_file_name=yaml_path,
        query_type="Detections",
    )
    assert _import_sentinel_query(yaml_path, yaml_text, query_type) == sample_query


def test_import_sentinel_query():
    yaml_files = read_yaml_files(parent_dir=BASE_DIR, child_dir="Detections")
    yaml_path = BASE_DIR + "/Detections\\Anomalies\\UnusualAnomaly.yaml"
    sample_query = SentinelQuery(
        id="d0255b5f-2a3c-4112-8744-e6757af3283a",
        name="Unusual Anomaly",
        description="'Anomaly Rules generate events in the Anomalies table. This scheduled rule tries to detect Anomalies that are not usual, they could be a type of Anomaly that has recently been activated, or an infrequent type. The detected Anomaly should be reviewed, if it is relevant enough, eventually a separate scheduled Analytics Rule could be created specifically for that Anomaly Type, so an alert and/or incident is generated everytime that type of Anomaly happens.'\n",
        severity="Medium",
        tags=[],
        requiredDataConnectors=[],
        queryFrequency="1h",
        queryPeriod="4d",
        triggerOperator="gt",
        triggerThreshold=0,
        tactics=[],
        relevantTechniques=[],
        query='// You can leave out Anomalies that are already monitored through other Analytics Rules\n//let _MonitoredRules = dynamic(["TestAlertName"]);\nlet query_frequency = 1h;\nlet query_lookback = 3d;\nAnomalies\n| where TimeGenerated > ago(query_frequency)\n//| where not(RuleName has_any (_MonitoredRules))\n| join kind = leftanti (\n    Anomalies\n    | where TimeGenerated between (ago(query_frequency + query_lookback)..ago(query_frequency))\n    | distinct RuleName\n) on RuleName\n',
        entityMappings={},
        customDetails={},
        alertDetailsOverride={},
        version="1.0.1",
        kind="Scheduled",
        folder_name="Anomalies",
        source_file_name=yaml_path,
        query_type="Detections",
    )
    assert (
        import_sentinel_queries(yaml_files, query_type="Detections")[0] == sample_query
    )


@pytest.mark.parametrize(
    "initial_str, expected_result",
    [
        (
            "Discord download invoked from cmd line (ASIM Version)",
            "Discord_download_invoked_from_cmd_line_ASIM_Version",
        ),
        ("Unusual Anomaly", "Unusual_Anomaly"),
        (
            "Dev-0056 Command Line Activity November 2021 (ASIM Version)",
            "Dev0056_Command_Line_Activity_November_2021_ASIM_Version",
        ),
    ],
)
def test__format_query_name(initial_str, expected_result):
    assert _format_query_name(initial_str) == expected_result


@pytest.mark.parametrize(
    "dict_section, expected_result",
    [
        (
            "keys",
            [
                "Anomalies",
                "ASimAuthentication",
                "ASimDNS",
                "ASimFileEvent",
                "ASimNetworkSession",
                "ASimProcess",
                "ASimWebSession",
                "AuditLogs",
                "AWSCloudTrail",
                "AWSGuardDuty",
                "AzureActivity",
                "AzureAppServices",
                "AzureDevOpsAuditing",
                "AzureDiagnostics",
                "AzureFirewall",
                "AzureWAF",
                "CiscoUmbrella",
                "CommonSecurityLog",
                "DeviceEvents",
                "DeviceFileEvents",
                "DeviceNetworkEvents",
                "DeviceProcessEvents",
                "DnsEvents",
                "DuoSecurity",
                "Dynamics365Activity",
                "GitHub",
                "Heartbeat",
                "http_proxy_oab_CL",
                "LAQueryLogs",
                "MultipleDataSources",
                "OfficeActivity",
                "ProofpointPOD",
                "PulseConnectSecure",
                "QualysVM",
                "QualysVMV2",
                "SecurityAlert",
                "SecurityEvent",
                "SecurityNestedRecommendation",
                "SigninLogs",
                "Syslog",
                "ThreatIntelligenceIndicator",
                "W3CIISLog",
                "WindowsEvents",
                "ZoomLogs",
            ],
        ),
        (
            "Anomalies",
            [
                SentinelQuery(
                    id="d0255b5f-2a3c-4112-8744-e6757af3283a",
                    name="Unusual Anomaly",
                    description="'Anomaly Rules generate events in the Anomalies table. This scheduled rule tries to detect Anomalies that are not usual, they could be a type of Anomaly that has recently been activated, or an infrequent type. The detected Anomaly should be reviewed, if it is relevant enough, eventually a separate scheduled Analytics Rule could be created specifically for that Anomaly Type, so an alert and/or incident is generated everytime that type of Anomaly happens.'\n",
                    severity="Medium",
                    tags=[],
                    requiredDataConnectors=[],
                    queryFrequency="1h",
                    queryPeriod="4d",
                    triggerOperator="gt",
                    triggerThreshold=0,
                    tactics=[],
                    relevantTechniques=[],
                    query='// You can leave out Anomalies that are already monitored through other Analytics Rules\n//let _MonitoredRules = dynamic(["TestAlertName"]);\nlet query_frequency = 1h;\nlet query_lookback = 3d;\nAnomalies\n| where TimeGenerated > ago(query_frequency)\n//| where not(RuleName has_any (_MonitoredRules))\n| join kind = leftanti (\n    Anomalies\n    | where TimeGenerated between (ago(query_frequency + query_lookback)..ago(query_frequency))\n    | distinct RuleName\n) on RuleName\n',
                    entityMappings={},
                    customDetails={},
                    alertDetailsOverride={},
                    version="1.0.1",
                    kind="Scheduled",
                    folder_name="Anomalies",
                    source_file_name="C:\\Users\\jannieli\\OneDrive - Microsoft\\Documents\\msticpy\\tests\\data\\drivers/Azure-Sentinel-master/Detections\\Anomalies\\UnusualAnomaly.yaml",
                    query_type="Detections",
                )
            ],
        ),
    ],
)
def test__organize_query_list_by_folder(dict_section, expected_result):
    yaml_files = read_yaml_files(parent_dir=BASE_DIR, child_dir="Detections")
    query_list = import_sentinel_queries(yaml_files=yaml_files, query_type="Detections")
    if dict_section == "keys":
        assert (
            list(_organize_query_list_by_folder(query_list=query_list).keys())
            == expected_result
        )
    else:
        assert (
            _organize_query_list_by_folder(query_list=query_list)[dict_section]
            == expected_result
        )


def test__create_queryfile_metadata():
    assert _create_queryfile_metadata(folder_name="Detections")["metadata"] == {
        "version": 1,
        "description": "Sentinel Alert Queries - Detections",
        "data_environments": ["MSSentinel"],
        "data_families": "Detections",
        "last_updated": str(datetime.now()),
    }


def test_write_to_yaml():
    yaml_files = read_yaml_files(parent_dir=BASE_DIR, child_dir="Detections")
    query_list = import_sentinel_queries(yaml_files=yaml_files, query_type="Detections")
    query_list = [l for l in query_list if l is not None]
    write_to_yaml(
        query_list=query_list,
        query_type="Detections",
        output_folder="github_test_yamls",
    )
    assert os.path.isdir(os.getcwd() + "/github_test_yamls")
