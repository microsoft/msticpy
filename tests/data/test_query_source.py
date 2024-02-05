# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""datq query test class."""
import os
import unittest
import warnings
from datetime import datetime, timedelta
from typing import Any, Optional, Tuple, Union

import pandas as pd
import pytest
import pytest_check as check

from msticpy.data.core.data_providers import DriverBase, QueryProvider
from msticpy.data.core.query_source import QuerySource

_SPLUNK_IMP_OK = False
try:
    from msticpy.data.drivers import splunk_driver

    _SPLUNK_IMP_OK = True
except ImportError:
    pass

# pylint: disable=protected-access, invalid-name


class UTDataDriver(DriverBase):
    """Test class."""

    def __init__(self, **kwargs):
        """Initialize new instance."""
        super().__init__(**kwargs)
        self._kwargs = kwargs
        self._loaded = True
        self._connected = False

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


_test_data_folders = [
    d for d, _, _ in os.walk(os.getcwd()) if d.endswith("/tests/testdata")
]
if len(_test_data_folders) == 1:
    _TEST_DATA = _test_data_folders[0]
else:
    _TEST_DATA = "./tests/testdata"


class TestQuerySource(unittest.TestCase):
    """Unit test class."""

    provider = None

    def setUp(self):
        """Intialize test."""
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
        self.query_sources = self.la_provider.query_store.data_families

    def test_date_formatters_datetime(self):
        """Test date formatting standard date."""
        # standard date
        test_end = datetime.utcnow()
        test_start = test_end - timedelta(days=1)
        check_dt_str = test_start.isoformat(sep="T") + "Z"
        q_src = self.query_sources["SecurityAlert"]["list_related_alerts"]
        query = q_src.create_query(start=test_start, end=test_end)
        self.assertIn(check_dt_str, query)

    def test_date_formatters_datestring(self):
        """Test date formatting ISO date string."""
        test_end = datetime.utcnow()
        test_start = test_end - timedelta(days=1)
        check_dt_str = test_start.isoformat(sep="T") + "Z"
        start = test_start.isoformat()
        q_src = self.query_sources["SecurityAlert"]["list_related_alerts"]
        query = q_src.create_query(start=start, end=test_end)
        self.assertIn(check_dt_str, query)

        start = str(test_start)
        query = q_src.create_query(start=start, end=test_end)
        self.assertIn(check_dt_str, query)

    def test_date_formatters_off_1day(self):
        """Test date formatting Offset -1 day."""
        test_end = datetime.utcnow()
        q_src = self.query_sources["SecurityAlert"]["list_related_alerts"]
        query = q_src.create_query(start=-1, end=0)
        check_date = test_end - timedelta(1)
        check_date_str = check_date.isoformat(sep="T") + "Z"
        # check that date + hours are correct
        check_date_str = check_date_str.split(":", 1)[0]
        self.assertIn(check_date_str, query)

    def test_date_formatters_off_1day_str(self):
        """Test date formatting Offset -1 day as string."""
        test_dt = datetime.utcnow()
        q_src = self.query_sources["SecurityAlert"]["list_related_alerts"]
        query = q_src.create_query(start="-1d", end=test_dt)
        check_date = test_dt - timedelta(1)
        check_date_str = check_date.isoformat(sep="T") + "Z"
        check_date_str = check_date_str.split(":", 1)[0]
        self.assertIn(check_date_str, query)

    def test_date_formatters_off_1week_str(self):
        """Test date formatting Offset -1 week."""
        test_dt = datetime.utcnow()
        q_src = self.query_sources["SecurityAlert"]["list_related_alerts"]
        query = q_src.create_query(start="-1w", end=test_dt)
        check_date = test_dt - timedelta(7)
        check_date_str = check_date.isoformat(sep="T") + "Z"
        check_date_str = check_date_str.split(":", 1)[0]
        self.assertIn(check_date_str, query)

    def test_date_formatters_off_1wk_rnd_dn(self):
        """Test date formatting Offset -1 week rounded to day."""
        test_dt = datetime.utcnow()
        q_src = self.query_sources["SecurityAlert"]["list_related_alerts"]
        query = q_src.create_query(start="-1w@d", end=test_dt)
        check_date = test_dt - timedelta(7)
        check_date_str = check_date.isoformat(sep="T") + "Z"
        check_date_str = check_date_str.split("T", 1)[0] + "T00:00:00"
        self.assertIn(check_date_str, query)

    def test_date_formatters_off_1wk_rnd_up(self):
        """Test date formatting Offset +1 week rounded to day."""
        test_dt = datetime.utcnow()
        q_src = self.query_sources["SecurityAlert"]["list_related_alerts"]
        query = q_src.create_query(start="1w@d", end=test_dt)
        check_date = test_dt + timedelta(7 + 1)
        check_date_str = check_date.isoformat(sep="T") + "Z"
        check_date_str = check_date_str.split("T", 1)[0] + "T00:00:00"
        self.assertIn(check_date_str, query)

    def test_list_formatter(self):
        """Test for default list formatting."""
        test_end = datetime.utcnow()
        test_start = test_end - timedelta(days=1)
        q_src = self.query_sources["Azure"]["list_azure_activity_for_ip"]
        ip_address_list = ["192.168.0.1", "192.168.0.2", "192.168.0.3"]
        query = q_src.create_query(
            ip_address_list=ip_address_list, start=test_start, end=test_end
        )

        check_list = ", ".join([f"'{ip}'" for ip in ip_address_list])
        self.assertIn(check_list, query)

        ip_address_list = "192.168.0.1, 192.168.0.2, 192.168.0.3"
        query = q_src.create_query(
            ip_address_list=ip_address_list, start=test_start, end=test_end
        )
        self.assertIn(check_list, query)

        int_list = [1, 2, 3, 4]
        query = q_src.create_query(
            ip_address_list=int_list, start=test_start, end=test_end
        )
        check_list = ", ".join([str(i) for i in int_list])
        self.assertIn(check_list, query)


@pytest.mark.skipif(not _SPLUNK_IMP_OK, reason="Partial msticpy install")
def test_cust_formatters_splunk():
    """Test SplunkDriver formatting."""
    provider = UTDataDriver()
    provider.connect("testuri")
    la_provider = QueryProvider(data_environment="LogAnalytics", driver=provider)
    query_sources = la_provider.query_store.data_families

    splunk_provider = QueryProvider(data_environment="Splunk", driver=provider)
    splunk_query_sources = splunk_provider.query_store.data_families

    splunk_fmt = {
        "datetime": splunk_driver.SplunkDriver._format_datetime,
        "list": splunk_driver.SplunkDriver._format_list,
    }

    test_end = datetime.utcnow()
    test_start = test_end - timedelta(days=1)
    ip_address_list = "192.168.0.1, 192.168.0.2, 192.168.0.3"

    check_dt_str = test_start.isoformat(sep=" ")
    # Using an Azure Sentinel query here since we want something
    # that requires a list parameter
    q_src = query_sources["Azure"]["list_azure_activity_for_ip"]
    query = q_src.create_query(
        formatters=splunk_fmt,
        start=test_start,
        end=test_end,
        ip_address_list=ip_address_list,
    )
    check.is_in(check_dt_str, query)

    query = q_src.create_query(
        formatters=splunk_fmt,
        start=test_start,
        end=test_end,
        ip_address_list=ip_address_list,
    )
    # Double-quote list elements
    check_list = ",".join([f'"{ip.strip()}"' for ip in ip_address_list.split(",")])
    check.is_in(check_list, query)

    int_list = [1, 2, 3, 4]
    query = q_src.create_query(
        formatters=splunk_fmt, start=test_start, end=test_end, ip_address_list=int_list
    )
    # Always quoted strings
    check_list = ",".join([f'"{i}"' for i in int_list])
    check.is_in(check_list, query)

    # Use a splunk query to verify timeformat parameter and datetime formatting
    q_src = splunk_query_sources["SplunkGeneral"]["get_events_parameterized"]
    query = q_src.create_query(formatters=splunk_fmt, start=test_start, end=test_end)
    check.is_in('timeformat="%Y-%m-%d %H:%M:%S.%6N"', query)
    check.is_in(f'earliest="{check_dt_str}"', query)
