# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""dataprovider query test class."""
import contextlib
import io
import unittest
import warnings
from copy import deepcopy
from datetime import datetime
from functools import partial
from pathlib import Path
from typing import Any, Dict, Iterable, Optional, Tuple, Union

import pandas as pd
import pytest
import pytest_check as check

from msticpy.common import pkg_config
from msticpy.common.exceptions import MsticpyException
from msticpy.data.core.data_providers import QueryProvider
from msticpy.data.core.query_container import QueryContainer
from msticpy.data.core.query_provider_connections_mixin import _calc_split_ranges
from msticpy.data.core.query_source import QuerySource
from msticpy.data.drivers.driver_base import DriverBase, DriverProps

from ..unit_test_lib import get_test_data_path

_TEST_DATA = get_test_data_path()


# pylint: disable=protected-access, invalid-name, redefined-outer-name
class UTDataDriver(DriverBase):
    """Test class."""

    _TEST_ATTRIB = "CustomAttrib"

    def __init__(self, **kwargs):
        """Initialize new instance."""
        super().__init__(**kwargs)
        self._kwargs = kwargs
        self._loaded = True
        self._connected = False
        self.set_driver_property(DriverProps.PUBLIC_ATTRS, {"test": self._TEST_ATTRIB})
        self.svc_queries = {}
        self.has_driver_queries = True

    def connect(self, connection_str: Optional[str] = None, **kwargs):
        """Test method."""
        del connection_str
        self._connected = True

    def query(
        self, query: str, query_source: QuerySource = None, **kwargs
    ) -> Union[pd.DataFrame, Any]:
        """Test method."""
        del query_source, kwargs
        return pd.DataFrame(data=query, index=[0], columns=["query"])

    def query_with_results(self, query: str, **kwargs) -> Tuple[pd.DataFrame, Any]:
        """Test method."""
        return (pd.DataFrame(data=query, index=[0], columns=["query"]), query)

    @property
    def driver_queries(self) -> Iterable[Dict[str, str]]:
        """Return dynamic queries available on connection to service."""
        return self.svc_queries


_TEST_QUERIES = [
    {
        "name": "test_query1",
        "query": "Select * from test",
        "query_container": "SavedSearches",
        "description": "Test 1",
    },
    {
        "name": "test_query2",
        "query": "Select * from test2",
        "query_container": "SavedSearches",
        "description": "Test 2",
    },
    {
        "name": "test.query3",
        "query": "Select * from test3",
        "query_container": "SavedSearches",
        "description": "Test 3",
    },
]


class TestDataQuery(unittest.TestCase):
    """Unit test class."""

    provider = None

    def setUp(self):
        """Test initialization."""
        provider = UTDataDriver()
        self.assertTrue(provider.loaded)
        provider.connect("testuri")
        self.assertTrue(provider.connected)
        self.provider = provider
        with warnings.catch_warnings():
            warnings.simplefilter("ignore", category=UserWarning)
            self.la_provider = QueryProvider(
                data_environment="LogAnalytics", driver=self.provider
            )

    def test_load_kql_query_defs(self):
        """Test loading query definitions."""
        la_provider = self.la_provider
        la_provider.connect(connection_str="test")
        self.assertEqual(getattr(la_provider, "test"), "CustomAttrib")

        # Did we read and process the query definitions OK
        q_sources = la_provider.query_store.data_families
        self.assertGreaterEqual(len(q_sources["WindowsSecurity"]), 9)
        self.assertGreaterEqual(len(q_sources["SecurityAlert"]), 5)
        self.assertGreaterEqual(len(q_sources["LinuxSyslog"]), 5)

        # pick one item and check properties
        get_alert_q = q_sources["SecurityAlert"]["get_alert"]
        self.assertEqual(len(get_alert_q.default_params), 3)
        self.assertEqual(len(get_alert_q.params), 4)
        self.assertEqual(len(get_alert_q.required_params), 1)
        self.assertEqual(len(get_alert_q.metadata), 6)
        self.assertIn("data_families", get_alert_q.metadata)
        self.assertIn("data_environments", get_alert_q.metadata)
        self.assertEqual(len(get_alert_q.data_families), 1)
        self.assertEqual(get_alert_q.name, "get_alert")
        self.assertIn("Retrieves", get_alert_q.description)

    def test_query_create_funcs(self):
        """Test create partial functions."""
        la_provider = self.la_provider
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
        for _, func in la_provider.all_queries:
            self.assertIsInstance(func, partial)
            self.assertTrue(len(func.__doc__))
            self.assertIn("Parameters", func.__doc__)

    def test_load_query_exec(self):
        """Test run query."""
        la_provider = self.la_provider
        df = la_provider.all_queries.get_alert("help")
        self.assertIsNone(df)

        with self.assertRaises(ValueError) as cm:
            df = la_provider.all_queries.get_alert()
            self.assertIn("system_alert_id", str(cm.exception))

        df = la_provider.all_queries.get_alert(system_alert_id="foo")
        self.assertEqual(len(df), 1)
        self.assertIn('SystemAlertId == "foo"', df["query"].iloc[0])

    def test_load_graph_query_defs(self):
        """Test Security graph query load."""
        provider = QueryProvider(data_environment="SecurityGraph", driver=self.provider)

        # Did we read and process the query definitions OK
        q_sources = provider.query_store.data_families
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
        """Test Security Graph create partial functions."""
        provider = QueryProvider(data_environment="SecurityGraph", driver=self.provider)

        all_queries = [q for q in dir(provider.all_queries) if not q.startswith("__")]
        alert_queries = [
            q for q in dir(provider.SecurityGraphAlert) if not q.startswith("__")
        ]
        self.assertGreaterEqual(len(all_queries), 7)
        self.assertGreaterEqual(len(alert_queries), 7)

        # Test that function attributes have been created properly
        for _, func in provider.all_queries:
            self.assertIsInstance(func, partial)
            self.assertTrue(len(func.__doc__))
            self.assertIn("Parameters", func.__doc__)

    def test_graph_load_query_exec(self):
        """Test Security graph run query."""
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
        """Test query loader rejecting badly formed query files."""
        la_provider = self.la_provider
        with self.assertRaises((MsticpyException, ValueError, KeyError)) as cm:
            file_path = Path(_TEST_DATA, "data_q_meta_fail.yaml")
            la_provider.import_query_file(query_file=file_path)
            self.assertIn("no data families defined", str(cm.exception))

        with self.assertRaises((MsticpyException, ValueError)) as cm:
            file_path = Path(_TEST_DATA, "data_q_source_fail_param.yaml")
            la_provider.import_query_file(query_file=file_path)
            self.assertIn("Missing parameters are", str(cm.exception))

        with self.assertRaises((MsticpyException, ValueError)) as cm:
            file_path = Path(_TEST_DATA, "data_q_source_fail_type.yaml")
            la_provider.import_query_file(query_file=file_path)
            self.assertIn("Parameters with missing types", str(cm.exception))

        before_queries = len(list(la_provider.list_queries()))
        file_path = Path(_TEST_DATA, "data_q_success.yaml")
        la_provider.import_query_file(query_file=file_path)

        self.assertEqual(before_queries + 3, len(list(la_provider.list_queries())))

    def test_load_hierarchical_q_paths(self):
        """Test use of hierarchical query paths."""
        la_provider = self.la_provider
        file_path = Path(_TEST_DATA, "data_q_hierarchy.yaml")
        la_provider.import_query_file(query_file=file_path)

        self.assertIsNotNone(la_provider.Alerts)
        self.assertEqual(len(la_provider.Alerts), 3)

        for _, item in la_provider.Alerts:
            self.assertTrue(isinstance(item, (partial, QueryContainer)))
            if isinstance(item, QueryContainer):
                self.assertTrue(repr(item).startswith("query"))
                self.assertIn("(query)", repr(item))

        self.assertIsInstance(la_provider.Alerts.type1.query1, partial)
        self.assertIsInstance(la_provider.Alerts.type2.query2, partial)
        self.assertIsInstance(la_provider.Alerts.type3.query3, partial)

    def test_query_store_get(self):
        """Test QueryStore get function."""
        la_provider = self.la_provider
        file_path = Path(_TEST_DATA, "data_q_hierarchy.yaml")
        la_provider.import_query_file(query_file=file_path)
        q_store = la_provider.query_store

        q_src = q_store.get_query("Alerts.type1.query1")
        self.assertIsInstance(q_src, QuerySource)
        self.assertEqual(q_src.name, "query1")
        q_src2 = q_store.get_query("Alerts.type1.query1", query_path="Alerts")
        self.assertIsInstance(q_src2, QuerySource)
        self.assertIs(q_src, q_src2)
        q_src3 = q_store.get_query("type1.query1", query_path="Alerts")
        self.assertIsInstance(q_src3, QuerySource)
        self.assertIs(q_src, q_src3)

        with self.assertRaises(LookupError):
            q_src = q_store.get_query("Alerts.type1.query2")

    def test_query_store_find(self):
        """Test QueryStore find query function."""
        la_provider = self.la_provider
        file_path = Path(_TEST_DATA, "data_q_hierarchy.yaml")
        la_provider.import_query_file(query_file=file_path)
        q_store = la_provider.query_store

        result = list(q_store.find_query("query1"))
        self.assertGreaterEqual(len(result), 1)

        result = list(q_store.find_query("missing_query1"))
        self.assertEqual(len(result), 0)

    def test_connect_queries(self):
        """Test queries provided at connect time."""
        ut_provider = UTDataDriver()
        ut_provider.svc_queries = _TEST_QUERIES

        data_provider = QueryProvider(
            data_environment="LogAnalytics", driver=ut_provider
        )
        data_provider.connect("testuri")

        # Check that we have expected attributes
        self.assertTrue(hasattr(data_provider, "SavedSearches"))
        saved_searches = getattr(data_provider, "SavedSearches")
        for attr in _TEST_QUERIES:
            attr = attr["name"].split(".")[0]
            self.assertTrue(hasattr(saved_searches, attr))
            self.assertTrue(
                isinstance(getattr(saved_searches, attr), (partial, QueryContainer))
            )

        # Check that we have expected query text
        q_store = data_provider.query_store
        q_src = q_store.get_query("SavedSearches.test.query3")
        self.assertEqual(q_src.query, _TEST_QUERIES[2]["query"])

    def test_connect_queries_dotted(self):
        """Test queries provided at connect time."""
        # Same test as above but with dotted container
        ut_provider = UTDataDriver()
        dotted_container_qs = _TEST_QUERIES.copy()
        for query in dotted_container_qs:
            query["query_container"] = "Saved.Searches"
        ut_provider.svc_queries = dotted_container_qs
        data_provider = QueryProvider(
            data_environment="LogAnalytics", driver=ut_provider
        )
        data_provider.connect("testuri")

        self.assertTrue(hasattr(data_provider, "Saved"))
        saved_searches = getattr(data_provider, "Saved")
        saved_searches = getattr(saved_searches, "Searches")
        for attr in dotted_container_qs:
            attr = attr["name"].split(".")[0]
            self.assertTrue(hasattr(saved_searches, attr))
            self.assertTrue(
                isinstance(getattr(saved_searches, attr), (partial, QueryContainer))
            )

        q_store = data_provider.query_store
        q_src = q_store.get_query("Saved.Searches.test.query3")
        self.assertEqual(q_src.query, dotted_container_qs[2]["query"])

    def test_split_ranges(self):
        """Test time range split logic."""
        start = datetime.utcnow() - pd.Timedelta("5H")
        end = datetime.utcnow() + pd.Timedelta("5min")
        delta = pd.Timedelta("1H")

        ranges = _calc_split_ranges(start, end, delta)
        self.assertEqual(len(ranges), 5)
        self.assertEqual(ranges[0][0], start)
        self.assertEqual(ranges[-1][1], end)

        st_times = [start_tm[0] for start_tm in ranges]
        for end_time in (end_tm[1] for end_tm in ranges):
            self.assertNotIn(end_time, st_times)

        end = end + pd.Timedelta("20min")
        ranges = _calc_split_ranges(start, end, delta)
        self.assertEqual(len(ranges), 5)
        self.assertEqual(ranges[0][0], start)
        self.assertEqual(ranges[-1][1], end)

    def test_split_queries(self):
        """Test queries split into time segments."""
        la_provider = self.la_provider

        start = datetime.utcnow() - pd.Timedelta("5H")
        end = datetime.utcnow() + pd.Timedelta("5min")
        delta = pd.Timedelta("1H")

        ranges = _calc_split_ranges(start, end, delta)
        result_queries = la_provider.all_queries.list_alerts(
            "print", start=start, end=end, split_query_by="1H"
        )
        queries = result_queries.split("\n\n")
        self.assertEqual(len(queries), 5)

        for idx, (st_time, e_time) in enumerate(ranges):
            self.assertIn(st_time.isoformat(sep="T") + "Z", queries[idx])
            self.assertIn(e_time.isoformat(sep="T") + "Z", queries[idx])
        self.assertIn(start.isoformat(sep="T") + "Z", queries[0])
        self.assertIn(end.isoformat(sep="T") + "Z", queries[-1])

    def test_split_queries_err(self):
        """Test queries split error conditions."""
        la_provider = self.la_provider

        mssg = io.StringIO()
        with contextlib.redirect_stdout(mssg):
            result_queries = la_provider.all_queries.get_alert(
                "print", system_alert_id="test", split_query_by="1H"
            )
        queries = result_queries.split("\n\n")
        # if no start and end - provider prints message and returns None
        self.assertEqual(len(queries), 1)
        self.assertIn("Cannot split a query", mssg.getvalue())

        # With invalid split_query_by value it will default to 1D
        start = datetime.utcnow() - pd.Timedelta("5D")
        end = datetime.utcnow() + pd.Timedelta("5min")

        result_queries = la_provider.all_queries.list_alerts(
            "print", start=start, end=end, split_query_by="Invalid"
        )
        queries = result_queries.split("\n\n")
        self.assertEqual(len(queries), 5)


_LOCAL_DATA_PATHS = [str(get_test_data_path().joinpath("localdata"))]


def test_multiple_connections():
    """Test adding connection instance to provider."""
    prov_args = dict(query_paths=_LOCAL_DATA_PATHS, data_paths=_LOCAL_DATA_PATHS)
    # create local provider and run a query
    local_prov = QueryProvider("LocalData", **prov_args)
    single_results = local_prov.Azure.list_all_signins_geo()

    # add another connection (to same folder)
    local_prov.add_connection(alias="SecondInst", **prov_args)
    connections = local_prov.list_connections()
    # verify second connection is listed
    check.equal(len(connections), 2)
    check.is_in("Default:", connections[0])
    check.is_in("SecondInst:", connections[1])

    # run query again
    multi_results = local_prov.Azure.list_all_signins_geo()
    # verify len of result is 2x single_result
    check.equal(single_results.shape[0] * 2, multi_results.shape[0])
    # verify columns/schema is the same.
    check.equal(list(single_results.columns), list(multi_results.columns))


def test_query_prov_properties():
    """Test basic properties of QueryProvider instance."""
    prov_args = dict(query_paths=_LOCAL_DATA_PATHS, data_paths=_LOCAL_DATA_PATHS)
    # create local provider
    local_prov = QueryProvider("LocalData", **prov_args)
    local_prov.connect()

    check.is_not_none(local_prov.connection_string)
    check.is_not_none(local_prov.schema_tables)

    qry_help = io.StringIO()
    with contextlib.redirect_stdout(qry_help):
        local_prov.query_help("LinuxSyslog.linux_events")
    check.is_in("List Linux Events", qry_help.getvalue())
    check.is_not_none(local_prov.browse_queries())
    check.is_not_none(local_prov.query_time)

    data_envs = QueryProvider.list_data_environments()
    check.greater(len(data_envs), 5)
    check.is_in("M365D", data_envs)
    check.is_in("LocalData", data_envs)
    check.is_in("ResourceGraph", data_envs)


def test_add_query():
    """Test adding a query dynamically."""
    qry_prov = QueryProvider("MSSentinel")

    # define a query
    query = """
    SecurityEvent
    | where EventID == {event_id}
    | where TimeGenerated between (datetime({start}) .. datetime({end}))
    | where Computer has "{host_name}"
    """
    # define the query parameters
    # (these can also be passed as a list of raw tuples)
    qp_host = qry_prov.create_param("host_name", "str", "Name of Host")
    qp_start = qry_prov.create_param("start", "datetime")
    qp_end = qry_prov.create_param("end", "datetime")
    qp_evt = qry_prov.create_param("event_id", "int", None, 4688)

    # add the query
    qry_prov.add_custom_query(
        name="get_host_events",
        query=query,
        description="Get events of type from host",
        family="Custom",
        parameters=[qp_host, qp_start, qp_end, qp_evt],
    )

    check.is_true(hasattr(qry_prov, "Custom"))
    check.is_true(hasattr(qry_prov.Custom, "get_host_events"))
    check.is_true(callable(qry_prov.Custom.get_host_events))
    check.is_in("Get events of type", qry_prov.Custom.get_host_events.__doc__)


_SEARCH_TESTS = [
    ((None, None, None), 0),
    (("syslog", None, None), 15),
    (("syslog", "syslog", None), 15),
    ((None, "syslog", None), 15),
    ((None, "sys", None), 15),
    ((None, "sys.*", None), 15),
    ((None, None, "ip_address"), 25),
    ((None, "syslog", "ip_address"), 2),
    ((None, ["syslog", "signinlogs"], None), 15),
    ((None, ["syslog", "signinlogs"], ["(ip.*)|(host.*)", "account.*"]), 15),
    ((None, "syslog", None, False), 0),
]


@pytest.fixture(scope="module")
def sentinel_qry_prov():
    """Return initialized query provider."""
    driver = UTDataDriver()
    with warnings.catch_warnings():
        warnings.simplefilter("ignore", category=UserWarning)
        qry_prov = QueryProvider(data_environment="MSSentinel", driver=driver)
    qry_prov.connect()
    check.greater_equal(len(qry_prov.list_queries()), 30)
    return qry_prov


_SEARCH_IDS = [", ".join([str(i) for i in item[0]]) for item in _SEARCH_TESTS]


@pytest.mark.parametrize("search, expected", _SEARCH_TESTS, ids=_SEARCH_IDS)
def test_query_search(search, expected, sentinel_qry_prov):
    """Test query search."""
    results = sentinel_qry_prov.search(*search)
    print(search, len(results))
    print(results[:5])
    check.greater_equal(len(results), expected)


@pytest.mark.parametrize("mode", ["abs", "rel"])
def test_query_paths(mode):
    """Test correctly loading queries from custom paths."""

    if mode == "abs":
        query_paths = [str(get_test_data_path().joinpath("kusto"))]
    else:
        query_paths = ["tests/testdata/kusto"]

    qry_prov = QueryProvider("Kusto", query_paths=query_paths)
    check.greater_equal(len(qry_prov.list_queries()), 13)
    for data_family in (
        "AppAuthCluster",
        "AppAuthClustera",
        "IntegAuthCluster",
        "IntegAuthCluster2",
    ):
        check.is_true(hasattr(qry_prov, data_family))

    # Custom paths in settings
    current_settings = deepcopy(pkg_config._settings.get("QueryDefinitions"))
    if not current_settings:
        pkg_config._settings["QueryDefinitions"] = {"Custom": query_paths}
    elif not current_settings.get("Custom"):
        pkg_config._settings["QueryDefinitions"]["Custom"] = query_paths
    else:
        pkg_config._settings["QueryDefinitions"]["Custom"].extend(query_paths)

    check.greater_equal(len(pkg_config._settings["QueryDefinitions"]["Custom"]), 1)

    qry_prov = QueryProvider("Kusto")
    check.greater_equal(len(qry_prov.list_queries()), 13)
    for data_family in (
        "AppAuthCluster",
        "AppAuthClustera",
        "IntegAuthCluster",
        "IntegAuthCluster2",
    ):
        check.is_true(hasattr(qry_prov, data_family))
    pkg_config._settings["QueryDefinitions"] = current_settings
