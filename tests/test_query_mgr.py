# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""query_mgr test class."""
import unittest
from datetime import datetime

from ..msticpy.nbtools import query_builtin_queries as queries
from ..msticpy.nbtools import kql
from ..msticpy.nbtools.query_mgr import replace_query_params, add_query, required_params
from ..msticpy.nbtools.query_defns import (
    DataFamily,
    DataEnvironment,
    KqlQuery,
    QueryParamProvider,
)


class QPTest(QueryParamProvider):
    """Unit test class."""

    def __init__(self, p_dict):
        self._p_dict = p_dict

    @property
    def query_params(self):
        """Return dict of query parameters."""
        return self._p_dict


class TestQueryManager(unittest.TestCase):
    """Unit test class."""

    def test_nbqueries(self):

        self.assertGreaterEqual(len(queries.query_definitions), 5)
        # try:
        #     subst_query = replace_query_params('list_alerts_counts')
        # except ValueError as val_err:
        self.assertRaises(ValueError, replace_query_params, "list_alerts_counts")
        before_alerts = len(queries.query_definitions)
        add_query(
            KqlQuery(
                name="dummy_query",
                query="""
                                {table}
                                {query_project}
                                | where StartTimeUtc >= datetime({start})
                                | where StartTimeUtc <= datetime({end})
                                | summarize alertCount=count(), firstAlert=min(StartTimeUtc),
                                lastAlert=max(StartTimeUtc) by AlertName
                                | order by alertCount desc
                                """,
                description="Retrieves summary of current alerts",
                data_source="security_alert",
                data_families=[DataFamily.WindowsSecurity],
                data_environments=[DataEnvironment.LogAnalytics],
            )
        )
        add_query(
            name="dummy_query2",
            query="""
                        {table}
                        {query_project}
                        | where StartTimeUtc >= datetime({start})
                        | where StartTimeUtc <= datetime({end})
                        | summarize alertCount=count(), firstAlert=min(StartTimeUtc),
                        lastAlert=max(StartTimeUtc) by AlertName
                        | order by alertCount desc
                        """,
            description="Retrieves summary of current alerts",
            data_source="security_alert",
            data_families=[DataFamily.WindowsSecurity],
            data_environments=[DataEnvironment.LogAnalytics],
        )
        self.assertEqual(before_alerts + 2, len(queries.query_definitions))

        self.assertIn("get_alert", kql.__dict__)
        self.assertIn("get_process_parent", kql.__dict__)

        # This returns None and prints output but should execute with error
        self.assertRaises(LookupError, kql.get_process_parent)

        # from  datetime import datetime, timedelta
        # end_time = datetime.utcnow()
        # start_time = end_time - timedelta(hours=4)
        # try:
        #     nbqueries.list_alerts_counts(data_family='LinuxSecurity',
        # start=start_time, end=end_time)
        # except Exception as ex_type:
        #     pass

    def test_query_params(self):

        start_end = {
            "start": datetime(2018, 11, 23, 23, 36, 2, 894637),
            "end": datetime(2018, 11, 29, 23, 36, 2, 894637),
        }
        qptest1 = QPTest(start_end)

        self.assertIn("list_alerts_counts", queries.query_definitions)
        q_result = replace_query_params("list_alerts_counts", qptest1)
        self.assertIsNotNone(q_result)
        self.assertIn("SecurityAlert", q_result)
        self.assertIn("project", q_result)
        self.assertIn("2018-11-23", q_result)

        # Try with different query that expects an additional
        self.assertIn("get_alert", queries.query_definitions)
        try:
            q_result = replace_query_params("get_alert", qptest1)
        except:
            q_result = None
            pass
        self.assertIsNone(q_result)

        alertid = {"system_alert_id": "{some guid}"}

        qptest2 = QPTest(alertid)
        # Try with different query that expects an additional
        self.assertIn("get_alert", queries.query_definitions)
        q_result = replace_query_params("get_alert", qptest1, qptest2)
        self.assertIsNotNone(q_result)
        self.assertIn("SecurityAlert", q_result)
        self.assertIn("project", q_result)
        self.assertIn("2018-11-23", q_result)
        self.assertIn("SystemAlertId", q_result)
        self.assertIn("{some guid}", q_result)

        q_result3 = kql.get_alert(provs=[qptest1, qptest2])
        self.assertIsNotNone(q_result3)
        self.assertIn("SecurityAlert", q_result3)
        self.assertIn("project", q_result3)
        self.assertIn("2018-11-23", q_result3)
        self.assertIn("SystemAlertId", q_result3)
        self.assertIn("{some guid}", q_result3)

    def test_builtin_query_params(self):
        for _, kquery in queries.query_definitions.items():
            for param in required_params(kquery):
                self.assertIn(param, queries.KNOWN_PARAM_NAMES)


if __name__ == "__main__":
    unittest.main()
    print("bye")
