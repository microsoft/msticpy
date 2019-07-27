# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""query_schema test class."""
import unittest
import json
import os

from ..msticpy.nbtools.entityschema import (
    Entity,
    Account,
    Host,
    Process,
    File,
    IpAddress,
)
from ..msticpy.nbtools.wsconfig import WorkspaceConfig
from ..msticpy.nbtools.query_defns import DataFamily, DataEnvironment


_test_data_folders = [
    d for d, _, _ in os.walk(os.getcwd()) if d.endswith("/tests/testdata")
]
if len(_test_data_folders) == 1:
    _TEST_DATA = _test_data_folders[0]
else:
    _TEST_DATA = "./tests/testdata"


class Testnbtools(unittest.TestCase):
    """Unit test class."""

    def test_entity_creation(self):
        try:
            file = input_file = os.path.join(_TEST_DATA, "entities.json")
            with open(file, "r") as file_handle:
                txt = file_handle.read()
                entity_dict = json.loads(txt)

            parsed_entities = []
            for _, entity in entity_dict.items():
                e = Entity.instantiate_entity(entity)
                self.assertIsInstance(e, Entity)

                if e["Type"] == "account":
                    self.assertIsInstance(e, Account)
                    self.assertTrue("Name" in e)
                    self.assertGreater(len(e.Name), 0)
                elif e["Type"] == "host":
                    self.assertIsInstance(e, Host)
                    self.assertTrue("HostName" in e)
                    self.assertGreater(len(e.HostName), 0)
                elif e["Type"] == "process":
                    self.assertIsInstance(e, Process)
                    self.assertTrue("ProcessId" in e)
                    self.assertGreater(len(e.ProcessId), 0)
                elif e["Type"] == "file":
                    self.assertIsInstance(e, File)
                    self.assertTrue("Name" in e)
                    self.assertGreater(len(e.Name), 0)
                elif e["Type"] == "ipaddress":
                    self.assertIsInstance(e, IpAddress)
                    self.assertTrue("Address" in e)
                    self.assertGreater(len(e.Address), 0)

                parsed_entities.append(e)

            self.assertGreaterEqual(len(parsed_entities), 7)

        except Exception as ex:
            self.fail(msg="Exception {}".format(str(ex)))

    def test_wsconfig(self):
        file = input_file = os.path.join(_TEST_DATA, "config.json")

        ws_conf = WorkspaceConfig(file)
        self.assertEqual(ws_conf["tenant_id"], "My Tenant Id")
        self.assertEqual(ws_conf["subscription_id"], "My Sub Id")
        self.assertEqual(ws_conf["resource_group"], "OMSWorkspaceRG")
        self.assertEqual(ws_conf["workspace_id"], "My Workspace Id")
        self.assertEqual(ws_conf["workspace_name"], "OMSWorkspace")

        ws_conf["workspace_name"] = "My other workspace"
        self.assertEqual(ws_conf["workspace_name"], "My other workspace")

    def test_query_defns(self):

        # WindowsSecurity = 1
        # LinuxSecurity = 2
        # SecurityAlert = 3
        self.assertEqual(
            DataFamily.WindowsSecurity, DataFamily.parse("WindowsSecurity")
        )
        self.assertEqual(DataFamily.LinuxSecurity, DataFamily.parse("LinuxSecurity"))
        self.assertEqual(DataFamily.SecurityAlert, DataFamily.parse("SecurityAlert"))
        self.assertEqual(DataFamily.WindowsSecurity, DataFamily.parse(1))
        self.assertEqual(DataFamily.LinuxSecurity, DataFamily.parse(2))
        self.assertEqual(DataFamily.SecurityAlert, DataFamily.parse(3))

        # LogAnalytics = 1
        # Kusto = 2
        self.assertEqual(
            DataEnvironment.LogAnalytics, DataEnvironment.parse("LogAnalytics")
        )
        self.assertEqual(DataEnvironment.Kusto, DataEnvironment.parse("Kusto"))
        self.assertEqual(DataEnvironment.LogAnalytics, DataEnvironment.parse(1))
        self.assertEqual(DataEnvironment.Kusto, DataEnvironment.parse(2))


if __name__ == "__main__":
    unittest.main()
    print("bye")
