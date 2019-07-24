# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""security_alert test class."""
import sys
import unittest
import pandas as pd

from ..msticpy.nbtools.security_alert import SecurityAlert
from ..msticpy.nbtools.security_alert_graph import create_alert_graph
from ..msticpy.nbtools.nbdisplay import display_alert
from ..msticpy.nbtools.query_defns import DataFamily, DataEnvironment


sample_alert = {
    "StartTimeUtc": "2018-09-27 16:59:16",
    "EndTimeUtc": "2018-09-27 16:59:16",
    "ProviderAlertId": "b6329e79-0a94-4035-beee-c2e2657b71e3",
    "SystemAlertId": "2518642332435550951_b6329e79-0a94-4035-beee-c2",
    "ProviderName": "Detection",
    "VendorName": "Microsoft",
    "AlertType": "RegistryPersistence",
    "AlertDisplayName": "Windows registry persistence method detected",
    "Severity": "Low",
    "IsIncident": False,
    "ExtendedProperties": {
        "resourceType": "Non-Azure Resource",
        "enrichment_tas_threat__reports": '{"Kind":"MultiLink","DisplayValueToUrlDictionary":{"Report: Registry Persistence":"https://iflowreportsproda.blob.core.windows.net/reports/MSTI-TS-Registry-Persistence.pdf"}}',
        "parent process id": "0x940",
        "account logon id": "0x3e7",
        "User SID": "S-1-5-18",
        "command line": 'reg  add hkey_current_user\\software\\microsoft\\windows\\currentversion\\run /v cclientcleandll /t reg_sz /d "c:\\windows\\temp\\epdeldll.bat" /f',
        "process name": "c:\\windows\\system32\\reg.exe",
        "process id": "0x1670",
        "domain name": "DOM",
        "user name": "DOM\\TESTHOST$",
        "parent process": "cmd.exe",
        "Persisted Process": "c:\\windows\\temp\\epdeldll.bat",
    },
    "CompromisedEntity": "TESTHOST",
    "Entities": [
        {
            "Type": "host",
            "$id": "1",
            "HostName": "TESTHOST",
            "DnsDomain": "DOM.CONTOSO.COM",
            "IsDomainJoined": True,
            "NTDomain": "DOM",
            "NetBiosName": "TESTHOST",
            "OsVersion": None,
            "OSFamily": "Windows",
        },
        {
            "Type": "file",
            "$id": "2",
            "Directory": "c:\\windows\\system32",
            "Name": "cmd.exe",
        },
        {
            "Type": "process",
            "$id": "3",
            "CommandLine": "",
            "Host": {"$ref": "1"},
            "ProcessId": "0x940",
            "ImageFile": {"$ref": "2"},
        },
        {
            "Type": "account",
            "$id": "4",
            "Name": "TESTHOST$",
            "IsDomainJoined": True,
            "NTDomain": "DOM",
            "Host": {"$ref": "1"},
            "LogonId": "0x3e7",
            "Sid": "S-1-5-18",
        },
        {
            "Type": "file",
            "$id": "5",
            "Directory": "c:\\windows\\system32",
            "Name": "reg.exe",
        },
        {
            "Type": "process",
            "$id": "6",
            "CommandLine": 'reg  add hkey_current_user\\software\\microsoft\\windows\\currentversion\\run /v cclientcleandll /t reg_sz /d "c:\\windows\\temp\\epdeldll.bat" /f',
            "Host": {"$ref": "1"},
            "ProcessId": "0x1670",
            "ImageFile": {"$ref": "5"},
            "CreationTimeUtc": "2018-09-27T16:59:16.4449048Z",
            "ParentProcess": {"$ref": "3"},
            "Account": {"$ref": "4"},
            "ElevationToken": "Default",
        },
        {
            "Type": "registry-key",
            "$id": "7",
            "Key": "software\\microsoft\\windows\\currentversion\\run",
            "Hive": "HKEY_CURRENT_USER",
        },
        {
            "Type": "registry-value",
            "$id": "8",
            "ValueType": "Unknown",
            "Key": {"$ref": "7"},
        },
        {
            "Type": "registry-value",
            "$id": "9",
            "Name": "cclientcleandll",
            "ValueType": "String",
            "Key": {"$ref": "7"},
            "Value": "System.Byte[]",
        },
        {
            "$id": "10",
            "Algorithm": "SHA256",
            "Value": "D41D122374906FE97D7185DBB2C767B8D98DF9DEC564C4A204028DFD892496BA",
            "Type": "filehash",
        },
        {
            "$id": "11",
            "Directory": "%OSDRIVE%\\WINDOWSAZURE\\SECAGENT",
            "Name": "WASECAGENTPROV.EXE",
            "Host": {"$ref": "1"},
            "FileHashes": [{"$ref": "10"}],
            "Type": "file",
        },
    ],
    "ConfidenceLevel": "Unknown",
    "ConfidenceScore": None,
    "ConfidenceReasons": None,
    "Intent": "Persistence",
    "ExtendedLinks": None,
    "AzureResourceId": None,
    "AzureResourceSubscriptionId": None,
    "TenantId": "b6329e79-0a94-4035-beee-c2e2657b71e3",
    "WorkspaceId": "b6329e79-0a94-4035-beee-c2e2657b71e3",
    "AgentId": "b6329e79-0a94-4035-beee-c2e2657b71e3",
    "SourceComputerId": "b6329e79-0a94-4035-beee-c2e2657b71e3",
    "SystemSource": "Non-Azure",
    "WorkspaceSubscriptionId": "b6329e79-0a94-4035-beee-c2e2657b71e3",
    "WorkspaceResourceGroup": "test-east-us",
    "TimeGeneratedUtc": "2018-09-27 16:59:47",
}


class TestSecurityAlert(unittest.TestCase):
    def setUp(self):
        self.raw_alert = pd.Series(sample_alert)
        self.raw_alert["StartTimeUtc"] = pd.to_datetime(self.raw_alert["StartTimeUtc"])
        self.raw_alert["EndTimeUtc"] = pd.to_datetime(self.raw_alert["EndTimeUtc"])
        self.raw_alert["TimeGeneratedUtc"] = pd.to_datetime(
            self.raw_alert["TimeGeneratedUtc"]
        )

    def test_alert_import(self):
        alert = SecurityAlert(self.raw_alert)

        str_alert = str(alert)
        self.assertIsNotNone(str_alert)
        self.assertIsNotNone(alert)
        self.assertGreaterEqual(len(alert.entities), 0)
        self.assertGreaterEqual(len(alert.ExtendedProperties), 0)
        self.assertGreaterEqual(len(alert["ExtendedProperties"]), 0)
        self.assertIn("StartTimeUtc", alert)
        self.assertIn("EndTimeUtc", alert)
        self.assertIn("SystemAlertId", alert)
        self.assertIn("SystemAlertId", alert)
        self.assertIn("ProviderName", alert)
        self.assertIn("VendorName", alert)
        self.assertIn("AlertType", alert)
        self.assertIn("AlertDisplayName", alert)
        self.assertIn("Severity", alert)
        self.assertIn("IsIncident", alert)

        str_alert = str(alert)
        self.assertIsNotNone(str_alert)
        repr_alert = repr(alert)
        self.assertIsNotNone(repr_alert)

        #
        self.assertIsNotNone(alert.primary_host)
        self.assertEqual("TESTHOST", alert.primary_host.HostName)
        self.assertIsNotNone(alert.primary_process)
        self.assertIsNotNone(alert.primary_process.ProcessFilePath)
        self.assertEqual(
            "c:\\windows\\system32\\reg.exe", alert.primary_process.ProcessFilePath
        )
        self.assertIsNotNone(alert.primary_account)
        self.assertEqual("TESTHOST$", alert.primary_account.Name)
        self.assertEqual("DOM\\TESTHOST$", alert.primary_account.qualified_name)
        self.assertEqual("0x3e7", alert.get_logon_id())

        self.assertIn("Computer", alert.host_filter(operator="=="))
        self.assertTrue(alert.is_in_log_analytics)
        self.assertTrue(alert.is_in_workspace)
        self.assertFalse(alert.is_in_azure_sub)
        self.assertIsNotNone(alert.host_filter(operator="=="))
        self.assertIn("true", alert.subscription_filter(operator="=="))

        self.assertEqual(3, len(alert.get_entities_of_type(entity_type="file")))
        self.assertEqual(2, len(alert.get_entities_of_type(entity_type="process")))
        self.assertEqual(1, len(alert.get_entities_of_type(entity_type="filehash")))
        self.assertEqual(
            2, len(alert.get_entities_of_type(entity_type="registryvalue"))
        )
        self.assertEqual(1, len(alert.get_entities_of_type(entity_type="registrykey")))
        self.assertEqual(1, len(alert.get_entities_of_type(entity_type="account")))
        self.assertEqual(1, len(alert.get_entities_of_type(entity_type="host")))

        self.assertGreater(len(alert.query_params), 5)
        self.assertEqual(alert.data_family, DataFamily.WindowsSecurity)
        self.assertEqual(alert.data_environment, DataEnvironment.LogAnalytics)

    def test_alert_display(self):

        alert = SecurityAlert(self.raw_alert)
        entity_str = ", ".join([str(e) for e in alert.entities])
        self.assertIsNotNone(entity_str)
        alert_html = alert.to_html(show_entities=True)
        self.assertIsNotNone(alert_html)
        alert_html = alert.to_html(show_entities=False)
        self.assertIsNotNone(alert_html)

    def test_alert_graph(self):
        alert = SecurityAlert(self.raw_alert)
        alert_graph = create_alert_graph(alert)
        self.assertIsNotNone(alert_graph)
        self.assertLessEqual(5, len(alert_graph.nodes))

    def test_alert_entities(self):
        alert = SecurityAlert(self.raw_alert)
        for ent in alert.entities:
            self.assertIsNotNone(ent.description_str)
