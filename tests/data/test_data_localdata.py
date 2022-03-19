# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Local data driver test class."""
import unittest
from pathlib import Path

import pandas as pd

from msticpy.data.core.data_providers import QueryProvider

from ..unit_test_lib import get_test_data_path


class TestLocalDataQuery(unittest.TestCase):
    """Test class for local data provider."""

    def test_create_provider(self):
        """Test method."""
        qry_prov = QueryProvider("LocalData")
        qry_prov.connect()
        self.assertTrue(qry_prov.connected)
        queries = qry_prov.list_queries()
        self.assertGreaterEqual(len(queries), 8)
        self.assertIn("SecurityAlert.list_alerts", queries)
        self.assertIn("WindowsSecurity.list_host_events", queries)
        self.assertIn("Network.list_azure_network_flows_by_ip", queries)

    def test_queries(self):
        """Test method."""
        data_path = Path(get_test_data_path()) / "localdata"
        qry_prov = QueryProvider("LocalData", data_paths=[str(data_path)])

        queries = qry_prov.list_queries()
        for query in queries:
            qry_func = getattr(qry_prov, query)
            d_frame = qry_func()
            self.assertIsInstance(d_frame, pd.DataFrame)
            self.assertGreater(len(d_frame), 1)

        schema = qry_prov.schema
        for cols in schema.values():
            self.assertIsInstance(cols, dict)
            self.assertGreater(len(cols), 10)

    def test_additional_queries(self):
        """Test method."""
        data_path = get_test_data_path()
        query_path = str(Path(get_test_data_path()) / "localdata")
        qry_prov = QueryProvider(
            "LocalData", data_paths=[str(data_path)], query_paths=[query_path]
        )
        queries = qry_prov.list_queries()
        self.assertGreaterEqual(len(queries), 11)

        qry_params = {
            "start": -1,
            "end": 0,
            "ip_address_list": ["test"],
            "host_name": "test",
            "account_name": "test",
        }
        for query in queries:
            qry_func = getattr(qry_prov, query)
            d_frame = qry_func(**qry_params)
            self.assertIsInstance(d_frame, pd.DataFrame)
            self.assertGreaterEqual(len(d_frame), 1)
