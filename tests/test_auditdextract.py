# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""auditd extract test class."""
import ast
import unittest
import os

import pandas as pd

from ..msticpy.sectools.auditdextract import (
    extract_events_to_df,
    get_event_subset,
    generate_process_tree,
    cluster_auditd_processes,
    read_from_file,
)

from ..msticpy.sectools.process_tree_utils import get_summary_info

_test_data_folders = [
    d for d, _, _ in os.walk(os.getcwd()) if d.endswith("/tests/testdata")
]
if len(_test_data_folders) == 1:
    _TEST_DATA = _test_data_folders[0]
else:
    _TEST_DATA = "./tests/testdata"


class TestAuditdExtract(unittest.TestCase):
    """Unit test class."""

    def test_extract_from_log_analytics(self):
        input_file = os.path.join(_TEST_DATA, "linux_events.csv")
        input_df = pd.read_csv(input_file)

        import_time = input_df["TimeGenerated"].iloc[0]
        self.assertGreaterEqual(input_df.shape[0], 100)

        input_df["AuditdMessage"] = input_df.apply(
            lambda x: ast.literal_eval(x.AuditdMessage), axis=1
        )

        output_df = extract_events_to_df(data=input_df)
        self.assertGreaterEqual(output_df.shape[0], 100, "Expected >=100 rows")
        self.assertGreaterEqual(output_df.shape[1], 32, "Expected >=32 colums")
        event_types = output_df["EventType"].drop_duplicates().to_list()
        self.assertIn("USER_END", event_types)
        self.assertIn("CRED_DISP", event_types)
        self.assertIn("SYSCALL_EXECVE", event_types)
        self.assertIn("USER_ACCT", event_types)
        self.assertIn("CRED_ACQ", event_types)
        self.assertIn("LOGIN", event_types)
        self.assertIn("USER_START", event_types)
        self.assertIn("USER_CMD", event_types)
        self.assertIn("CRED_REFR", event_types)

        self.assertGreaterEqual(
            len(output_df[output_df["EventType"] == "SYSCALL_EXECVE"]),
            82,
            "Expected EXECVE events",
        )
        self.assertGreaterEqual(
            len(output_df[output_df["EventType"] == "LOGIN"]),
            1,
            "Expected LOGIN events",
        )
        self.assertGreaterEqual(
            len(output_df[output_df["EventType"] == "USER_CMD"]),
            2,
            "Expected USER_CMD events",
        )
        self.assertGreaterEqual(
            len(output_df[output_df["EventType"] == "USER_ACCT"]),
            1,
            "Expected USER_ACCT events",
        )

        for _, execve_event in output_df[output_df["EventType"] == "SYSCALL_EXECVE"][
            1:20
        ].iterrows():
            self.assertIsNotNone(execve_event.Computer)
            self.assertIsNotNone(execve_event.TimeGenerated)
            self.assertNotEqual(import_time, execve_event.TimeGenerated)
            self.assertIsNotNone(execve_event.exe)
            self.assertIsNotNone(execve_event.cmdline)
            self.assertIsNotNone(execve_event.cwd)

        for _, login_event in output_df[output_df["EventType"] == "LOGIN"].iterrows():
            self.assertIsNotNone(login_event.Computer)
            self.assertIsNotNone(login_event.acct)
            self.assertIsNotNone(login_event.TimeGenerated)
            self.assertNotEqual(import_time, login_event.TimeGenerated)

    def test_auditd_from_file(self):
        input_file = os.path.join(_TEST_DATA, "auditd_log.txt")
        parsed_events = read_from_file(input_file)
        self.assertIsNotNone(parsed_events)
        self.assertEqual(parsed_events.shape, (381, 97))

        proc_events = get_event_subset(parsed_events, event_type="SYSCALL_EXECVE")
        self.assertIsNotNone(proc_events)
        self.assertEqual(proc_events.shape, (78, 20))

    def test_auditd_utils(self):
        input_file = os.path.join(_TEST_DATA, "linux_events.csv")
        input_df = pd.read_csv(input_file)

        input_df["AuditdMessage"] = input_df.apply(
            lambda x: ast.literal_eval(x.AuditdMessage), axis=1
        )
        output_df = extract_events_to_df(data=input_df)
        proc_events = get_event_subset(output_df, event_type="SYSCALL_EXECVE")

        proc_tree = generate_process_tree(proc_events)
        pt_summary = get_summary_info(proc_tree)
        self.assertGreaterEqual(len(proc_tree), 85)
        self.assertEqual(pt_summary["LargestTreeDepth"], 5)

        proc_events2 = proc_events.copy()
        clustered_procs = cluster_auditd_processes(proc_events2, app=None)
        self.assertIsNotNone(clustered_procs)
        self.assertEqual(len(clustered_procs), 2)


if __name__ == "__main__":
    unittest.main()
    print("bye")
