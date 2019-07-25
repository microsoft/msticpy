# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""datq query test class."""
import unittest
from functools import partial
import json
import os
from pathlib import Path
from typing import Union, Any, Tuple

import pandas as pd

from ..msticpy.data.data_providers import QueryProvider, DriverBase


class UTDataDriver(DriverBase):
    def __init__(self, **kwargs):
        """Initialize new instance."""
        self._kwargs = kwargs
        self._loaded = True
        self._connected = False

    def connect(self, connection_str: str, **kwargs):
        del connection_str
        self._connected = True

    def query(self, query: str) -> Union[pd.DataFrame, Any]:
        return pd.DataFrame(data=query, index=[0], columns=["query"])

    def query_with_results(self, query: str) -> Tuple[pd.DataFrame, Any]:
        return (pd.DataFrame(data=query, index=[0], columns=["query"]), query)


_test_data_folders = [
    d for d, _, _ in os.walk(os.getcwd()) if d.endswith("/tests/testdata")
]
if len(_test_data_folders) == 1:
    _TEST_DATA = _test_data_folders[0]
else:
    _TEST_DATA = "./tests/testdata"


class TestDataQuery(unittest.TestCase):
    """Unit test class."""

    provider = None

    def setUp(self):
        provider = UTDataDriver()
        self.assertTrue(provider.loaded)
        provider.connect("testuri")
        self.assertTrue(provider.connected)
        self.provider = provider

    def test_load_kql_query_defs(self):

        la_provider = QueryProvider(
            data_environment="LogAnalytics", driver=self.provider
        )
        # graph_provider = QueryProvider(data_environment = 'SecurityGraph',
        #                           la_provider     driver='dummy')

        # Did we read and process the query definitions OK
        q_sources = la_provider._query_store.data_families
        self.assertGreaterEqual(len(q_sources["WindowsSecurity"]), 9)
        self.assertGreaterEqual(len(q_sources["SecurityAlert"]), 5)

        # pick one item and check properties
        get_alert_q = q_sources["SecurityAlert"]["get_alert"]
        self.assertEqual(len(get_alert_q.default_params), 7)
        self.assertEqual(len(get_alert_q.params), 8)
        self.assertEqual(len(get_alert_q.required_params), 1)
        self.assertEqual(len(get_alert_q.metadata), 6)
        self.assertIn("data_families", get_alert_q.metadata)
        self.assertIn("data_environments", get_alert_q.metadata)
        self.assertEqual(len(get_alert_q.data_families), 1)
        self.assertEqual(get_alert_q.name, "get_alert")
        self.assertIn("Retrieves", get_alert_q.description)

    def test_query_create_funcs(self):

        la_provider = QueryProvider(
            data_environment="LogAnalytics", driver=self.provider
        )
        # graph_provider = QueryProvider(data_environment = 'SecurityGraph',
        #                           la_provider     driver='dummy')

        all_queries = [
            q for q in dir(la_provider.all_queries) if not q.startswith("__")
        ]
        winsec_queries = [
            q for q in dir(la_provider.WindowsSecurity) if not q.startswith("__")
        ]
        alert_queries = [
            q for q in dir(la_provider.SecurityAlert) if not q.startswith("__")
        ]
        self.assertGreaterEqual(len(all_queries), 14)
        self.assertGreaterEqual(len(winsec_queries), 9)
        self.assertGreaterEqual(len(alert_queries), 5)

        # Test that function attributes have been created properly
        for func_name, func in la_provider.all_queries:
            self.assertIsInstance(func, partial)
            self.assertTrue(len(func.__doc__))
            self.assertIn("Parameters", func.__doc__)

    def test_load_query_exec(self):

        la_provider = QueryProvider(
            data_environment="LogAnalytics", driver=self.provider
        )
        # graph_provider = QueryProvider(data_environment = 'SecurityGraph',
        #                           la_provider     driver='dummy')

        df = la_provider.all_queries.get_alert("help")
        self.assertIsNone(df)

        with self.assertRaises(ValueError) as cm:
            df = la_provider.all_queries.get_alert()
            self.assertIn("system_alert_id", str(cm.exception))

        df = la_provider.all_queries.get_alert(system_alert_id="foo")
        self.assertEqual(len(df), 1)
        self.assertIn('SystemAlertId == "foo"', df["query"].iloc[0])

    def test_load_graph_query_defs(self):
        provider = QueryProvider(data_environment="SecurityGraph", driver=self.provider)

        # Did we read and process the query definitions OK
        q_sources = provider._query_store.data_families
        self.assertGreaterEqual(len(q_sources["SecurityGraphAlert"]), 7)

        # pick one item and check properties
        get_alert_q = q_sources["SecurityGraphAlert"]["get_alert"]
        self.assertEqual(len(get_alert_q.default_params), 6)
        self.assertEqual(len(get_alert_q.params), 7)
        self.assertEqual(len(get_alert_q.required_params), 1)
        self.assertEqual(len(get_alert_q.metadata), 6)
        self.assertIn("data_families", get_alert_q.metadata)
        self.assertIn("data_environments", get_alert_q.metadata)
        self.assertEqual(len(get_alert_q.data_families), 1)
        self.assertEqual(get_alert_q.name, "get_alert")
        self.assertIn("Retrieves", get_alert_q.description)

    def test_graph_query_create_funcs(self):

        provider = QueryProvider(data_environment="SecurityGraph", driver=self.provider)

        all_queries = [q for q in dir(provider.all_queries) if not q.startswith("__")]
        alert_queries = [
            q for q in dir(provider.SecurityGraphAlert) if not q.startswith("__")
        ]
        self.assertGreaterEqual(len(all_queries), 7)
        self.assertGreaterEqual(len(alert_queries), 7)

        # Test that function attributes have been created properly
        for func_name, func in provider.all_queries:
            self.assertIsInstance(func, partial)
            self.assertTrue(len(func.__doc__))
            self.assertIn("Parameters", func.__doc__)

    def test_graph_load_query_exec(self):

        provider = QueryProvider(data_environment="SecurityGraph", driver=self.provider)
        df = provider.all_queries.get_alert("help")
        self.assertIsNone(df)

        with self.assertRaises(ValueError) as cm:
            df = provider.all_queries.get_alert()
            self.assertIn("alert_id", str(cm.exception))

        df = provider.all_queries.get_alert(alert_id="foo")
        self.assertEqual(len(df), 1)
        self.assertIn("/foo", df["query"].iloc[0])

    def test_load_yaml_def(self):
        la_provider = QueryProvider(
            data_environment="LogAnalytics", driver=self.provider
        )
        with self.assertRaises((ImportError, ValueError)) as cm:
            file_path = Path(_TEST_DATA, "data_q_meta_fail.yaml")
            la_provider.import_query_file(query_file=file_path)
            self.assertIn("no data families defined", str(cm.exception))

        with self.assertRaises((ImportError, ValueError)) as cm:
            file_path = Path(_TEST_DATA, "data_q_source_fail_param.yaml")
            la_provider.import_query_file(query_file=file_path)
            self.assertIn("Missing parameters are", str(cm.exception))

        with self.assertRaises((ImportError, ValueError)) as cm:
            file_path = Path(_TEST_DATA, "data_q_source_fail_type.yaml")
            la_provider.import_query_file(query_file=file_path)
            self.assertIn("Parameters with missing types", str(cm.exception))

        before_queries = len(list(la_provider.list_queries()))
        file_path = Path(_TEST_DATA, "data_q_success.yaml")
        la_provider.import_query_file(query_file=file_path)

        self.assertEqual(before_queries + 3, len(list(la_provider.list_queries())))
