# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""query_schema test class."""
import unittest
import warnings

from ..msticpy.nbtools.query_schema import DataSchema
from ..msticpy.nbtools.query_defns import DataFamily, DataEnvironment


class TestQuerySchema(unittest.TestCase):
    """Unit test class."""

    def setUp(self):
        with warnings.catch_warnings():
            # We want to ignore warnings from our own deprecations
            warnings.simplefilter("ignore", category=DeprecationWarning)
            self.schema = DataSchema(
                environment="LogAnalytics",
                data_family="WindowsSecurity",
                data_source="process_create",
            )

    def test_global_properties(self):
        with warnings.catch_warnings():
            # We want to ignore warnings from our own deprecations
            warnings.simplefilter("ignore", category=DeprecationWarning)
            self.assertGreaterEqual(len(self.schema.data_environments), 2)
            self.assertGreaterEqual(len(self.schema.data_families), 2)
            self.assertGreaterEqual(len(self.schema.data_source_types), 3)

    def test_SecurityAlert(self):
        with warnings.catch_warnings():
            # We want to ignore warnings from our own deprecations
            warnings.simplefilter("ignore", category=DeprecationWarning)
            for src in ["security_alert"]:
                schema = DataSchema(
                    environment="LogAnalytics",
                    data_family="SecurityAlert",
                    data_source=src,
                )
                self.assertTrue("table" in schema)
                self.assertTrue("query_project" in schema)
                if src == "security_alert":
                    self.assertGreaterEqual(len(schema["query_project"].split(",")), 23)
                    self.assertEqual(schema["table"], "SecurityAlert")

    def test_WindowsSecurity(self):
        with warnings.catch_warnings():
            # We want to ignore warnings from our own deprecations
            warnings.simplefilter("ignore", category=DeprecationWarning)
            for src in ["process_create", "account_logon"]:
                schema = DataSchema(
                    environment="LogAnalytics",
                    data_family="WindowsSecurity",
                    data_source=src,
                )
                self.assertTrue("table" in schema)
                self.assertTrue("query_project" in schema)
                if src == "proc_create":
                    self.assertGreaterEqual(len(schema["query_project"].split(",")), 17)
                    self.assertEqual(
                        schema["table"], "SecurityEvent | where EventID == 4688"
                    )
                elif src == "account_logon":
                    self.assertGreaterEqual(len(schema["query_project"].split(",")), 19)
                    self.assertEqual(
                        schema["table"], "SecurityEvent | where EventID == 4624"
                    )

    def test_LinuxSecurity(self):
        for src in ["process_create", "account_logon"]:
            schema = DataSchema(
                environment="LogAnalytics", data_family="LinuxSecurity", data_source=src
            )
            self.assertTrue("table" in schema)
            self.assertTrue("query_project" in schema)
            if src == "proc_create":
                self.assertGreaterEqual(len(schema["query_project"].split(",")), 17)
                self.assertEqual(
                    schema["table"], "LinuxAuditD | where EventID == 14688"
                )
            elif src == "account_logon":
                self.assertGreaterEqual(len(schema["query_project"].split(",")), 19)
                self.assertEqual(
                    schema["table"],
                    "LinuxAuditD | where EventID == 1100 or EventID == 1112",
                )

    def test_default_schemas(self):
        schemas = DataSchema.default_schemas(
            environment=DataEnvironment.LogAnalytics,
            data_family=DataFamily.WindowsSecurity,
        )
        self.assertIsNotNone(schemas)
        self.assertGreaterEqual(len(schemas), 2)
        schemas = DataSchema.default_schemas(
            environment=DataEnvironment.LogAnalytics,
            data_family=DataFamily.LinuxSecurity,
        )
        self.assertIsNotNone(schemas)
        self.assertGreaterEqual(len(schemas), 2)
        schemas = DataSchema.default_schemas(
            environment=DataEnvironment.LogAnalytics,
            data_family=DataFamily.SecurityAlert,
        )
        self.assertIsNotNone(schemas)
        self.assertGreaterEqual(len(schemas), 1)
