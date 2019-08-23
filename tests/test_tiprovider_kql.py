# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""TIProviders test class."""
import unittest
import os
from datetime import datetime
from pathlib import Path
from typing import Union, Any, Tuple, Dict

import pandas as pd

from ..msticpy.data import QueryProvider
from ..msticpy.data.data_providers import DriverBase
from ..msticpy.nbtools import pkg_config
from ..msticpy.sectools.iocextract import IoCExtract
from ..msticpy.sectools.tilookup import TILookup
from ..msticpy.sectools.tiproviders import (
    TIProviderSettings,
    get_provider_settings,
    preprocess_observable,
    LookupResult,
    AzSTI,
)

_test_data_folders = [
    d for d, _, _ in os.walk(os.getcwd()) if d.endswith("/tests/testdata")
]
if len(_test_data_folders) == 1:
    _TEST_DATA = _test_data_folders[0]
else:
    _TEST_DATA = "./tests/testdata"


class KqlTestDriver(DriverBase):
    """KqlTestDriver class to execute kql queries."""

    def __init__(self, connection_str: str = None, **kwargs):
        super().__init__()

        self._loaded = True
        self._connected = True
        self._schema: Dict[str, Any] = {}

        indicator_file = Path(_TEST_DATA).joinpath("as_threatintel")
        self.test_df = pd.read_pickle(indicator_file)
        self.ip_df = self.test_df[self.test_df["NetworkIP"].str.len() > 0].copy()
        self.ip_df["IoC"] = self.ip_df["NetworkIP"].str.lower()
        self.url_df = self.test_df[self.test_df["Url"].str.len() > 0].copy()
        self.url_df["IoC"] = self.url_df["Url"].str.lower()

    def connect(self, connection_str: str, **kwargs):
        self._connected = True
        return None

    @property
    def schema(self) -> Dict[str, Dict]:
        return self._schema

    def query(self, query: str) -> Union[pd.DataFrame, Any]:

        query_toks = [tok.lower() for tok in query.split("'") if tok != ","]
        if "where NetworkIP" in query:
            result_df = self.ip_df[self.ip_df["IoC"].isin(query_toks)]
            return result_df

        if "where Url" in query:
            result_df = self.url_df[self.url_df["IoC"].isin(query_toks)]
            return result_df

    def query_with_results(self, query: str) -> Tuple[pd.DataFrame, Any]:
        pass


test_data_provider = KqlTestDriver()
qry_prov = QueryProvider(data_environment="LogAnalytics", driver=test_data_provider)


class TestASKqlTIProvider(unittest.TestCase):
    """Unit test class."""

    def test_ti_config_and_load(self):
        test_config1 = Path(_TEST_DATA).joinpath("msticpyconfig-askql.yaml")
        os.environ[pkg_config._CONFIG_ENV_VAR] = str(test_config1)

        pkg_config.refresh_config()

        ti_settings = get_provider_settings()

        self.assertIsInstance(ti_settings, dict)
        self.assertGreaterEqual(1, len(ti_settings))

        # Try to load TIProviders - should throw a warning on
        # missing provider class
        as_byoti_prov = AzSTI(query_provider=qry_prov)
        ti_lookup = TILookup(primary_providers=[as_byoti_prov])

        # should have 2 succesfully loaded providers
        self.assertGreaterEqual(1, len(ti_lookup.loaded_providers))
        self.assertGreaterEqual(1, len(ti_lookup.provider_status))

    @staticmethod
    def load_ti_lookup():
        test_config1 = Path(_TEST_DATA).joinpath("msticpyconfig-askql.yaml").resolve()
        os.environ[pkg_config._CONFIG_ENV_VAR] = str(test_config1)

        pkg_config.refresh_config()
        as_byoti_prov = AzSTI(query_provider=qry_prov)
        return TILookup(primary_providers=[as_byoti_prov])

    def test_ASByoti_provider(self):
        ti_lookup = self.load_ti_lookup()

        end = datetime(2019, 8, 5, 22, 59, 59, 809000)
        start = datetime(2019, 8, 5, 22, 16, 16, 574000)
        ioc_url = "http://ajaraheritage.ge/g7cberv"
        ioc_urls = [
            "http://cheapshirts.us/zVnMrG.php",
            "http://chinasymbolic.com/i9jnrc",
            "http://cetidawabi.com/468fd",
            "http://append.pl/srh9xsz",
            "http://aiccard.co.th/dvja1te",
            "http://ajaraheritage.ge/g7cberv",
            "http://cic-integration.com/hjy93JNBasdas",
            "https://google.com",  # benign
            "https://microsoft.com",  # benign
            "https://python.org",  # benign
        ]
        ioc_ip = "91.219.31.18"
        ioc_ips = [
            "185.92.220.35",
            "213.159.214.86",
            "77.222.54.202",
            "91.219.29.81",
            "193.9.28.254",
            "89.108.83.196",
            "91.219.28.44",
            "188.127.231.124",
            "192.42.116.41",
            "91.219.31.18",
            "46.4.239.76",
            "188.166.168.250",
            "195.154.241.208",
            "51.255.172.55",
            "93.170.169.52",
            "104.215.148.63",
            "13.77.161.179",
            "40.76.4.15",  # benign
            "40.112.72.205",
            "40.113.200.201",  # benign
        ]

        result = ti_lookup.lookup_ioc(observable=ioc_url, start=start, end=end)
        self.assertIsNotNone(result)
        ioc_lookups = result[1]

        self.assertGreaterEqual(1, len(ioc_lookups))
        self.assertEqual(ioc_lookups[0][0], "AzSTI")
        azs_result = ioc_lookups[0][1]
        self.assertEqual(azs_result.ioc.lower(), ioc_url.lower())
        self.assertEqual(azs_result.ioc_type, "url")
        self.assertIn("alert", azs_result.details["Action"])
        self.assertIn(True, azs_result.details["Active"])
        self.assertIn(100, azs_result.details["ConfidenceScore"])
        self.assertIn("Malware", azs_result.details["ThreatType"])

        res_df = azs_result.raw_result
        self.assertIsInstance(res_df, pd.DataFrame)
        self.assertIsInstance(azs_result.reference, str)
        self.assertTrue("ThreatIntelligenceIndicator  | where" in azs_result.reference)

        # Bulk URL Lookups
        results = ti_lookup.lookup_iocs(data=ioc_urls, start=start, end=end)
        self.assertIsNotNone(results)
        self.assertEqual(10, len(ioc_urls))
        self.assertEqual(7, len(results[results["Result"] == True]))

        # IP Lookups
        result = ti_lookup.lookup_ioc(observable=ioc_ip, start=start, end=end)
        self.assertIsNotNone(result)
        ioc_lookups = result[1]

        self.assertGreaterEqual(1, len(ioc_lookups))
        self.assertEqual(ioc_lookups[0][0], "AzSTI")
        azs_result = ioc_lookups[0][1]
        self.assertEqual(azs_result.ioc, ioc_ip)
        self.assertEqual(azs_result.ioc_type, "ipv4")
        self.assertIn("alert", azs_result.details["Action"])
        self.assertIn(True, azs_result.details["Active"])
        self.assertIn(70, azs_result.details["ConfidenceScore"])
        self.assertIn("Malware", azs_result.details["ThreatType"])

        # Bulk IP Lookups
        results = ti_lookup.lookup_iocs(data=ioc_ips, start=start, end=end)
        self.assertIsNotNone(results)
        self.assertEqual(20, len(ioc_ips))
        self.assertEqual(15, len(results[results["Result"] == True]))
