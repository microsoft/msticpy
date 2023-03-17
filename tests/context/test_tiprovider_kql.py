# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""TIProviders test class."""
from datetime import datetime
from pathlib import Path
from typing import Any, Dict, Optional, Tuple, Union

import pandas as pd
import pytest
import pytest_check as check

from msticpy.common.provider_settings import get_provider_settings
from msticpy.context.lookup_result import LookupStatus
from msticpy.context.tilookup import TILookup
from msticpy.context.tiproviders.azure_sent_byoti import AzSTI
from msticpy.data import QueryProvider
from msticpy.data.core.data_providers import DriverBase

from ..unit_test_lib import custom_mp_config, get_test_data_path

_TEST_DATA = get_test_data_path()


class Kql_Result:
    """Mock result."""


class KqlTestDriver(DriverBase):
    """KqlTestDriver class to execute kql queries."""

    def __init__(self, connection_str: str = None, **kwargs):
        """Initialize class."""
        del connection_str
        super().__init__(**kwargs)

        self._loaded = True
        self._connected = True
        self._schema: Dict[str, Any] = {"ThreatIntelligenceIndicator": {}}

        indicator_file = Path(_TEST_DATA).joinpath("as_threatintel")
        self.test_df = pd.read_pickle(indicator_file)
        self.ip_df = self.test_df[self.test_df["NetworkIP"].str.len() > 0].copy()
        self.ip_df["IoC"] = self.ip_df["NetworkIP"].str.lower()
        self.url_df = self.test_df[self.test_df["Url"].str.len() > 0].copy()
        self.url_df["IoC"] = self.url_df["Url"].str.lower()

    def connect(self, connection_str: Optional[str] = None, **kwargs):
        """Mock connect function."""
        self._connected = True
        return None

    @property
    def schema(self) -> Dict[str, Dict]:
        """Mock schema property."""
        return self._schema

    def query(
        self, query: str, query_source: Any = None, **kwargs
    ) -> Union[pd.DataFrame, Any]:
        """Mock query function."""
        del query_source, kwargs

        query_toks = [tok.lower() for tok in query.split("'") if tok != ","]
        if "where NetworkIP" in query:
            result_df = self.ip_df[self.ip_df["IoC"].isin(query_toks)]
            return result_df

        if "where Url" in query:
            result_df = self.url_df[self.url_df["IoC"].isin(query_toks)]
            return result_df

        if "empty_result" in query:
            return pd.DataFrame()

        if "failed_query" in query:
            query_result = Kql_Result()
            # pylint: disable=attribute-defined-outside-init
            query_result.completion_query_info = {"StatusCode": 1}  # type: ignore
            query_result.records_count = 0  # type: ignore
            return query_result

        if "no_dataframe" in query:
            return Kql_Result()
        return None

    def query_with_results(self, query: str, **kwargs) -> Tuple[pd.DataFrame, Any]:
        """Mock query_with_results."""
        del query, kwargs
        return (pd.DataFrame, None)


class IPython:
    """Mock IPython."""

    def run_cell_magic(self, *args, **kwargs):
        """Mock run_cell_magic."""
        del args, kwargs

    def run_line_magic(self, *args, **kwargs):
        """Mock run_line_magic."""
        del args
        if kwargs.get("line") == "--schema":
            return {}

    def find_magic(self, *args, **kwargs):
        """Mock find_magic."""
        del args, kwargs
        return True


# pylint: disable=redefined-outer-name
@pytest.fixture(scope="module")
def data_provider():
    """Return mock Kql driver."""
    return KqlTestDriver()


@pytest.fixture(scope="module")
def query_provider(data_provider):
    """Query provider."""
    return QueryProvider(data_environment="MSSentinel", driver=data_provider)


@pytest.fixture
def ti_lookup(query_provider):
    """Return TI Lookup."""
    test_config1 = Path(_TEST_DATA).joinpath("msticpyconfig-askql.yaml").resolve()
    with custom_mp_config(test_config1):
        as_byoti_prov = AzSTI(query_provider=query_provider)
        return TILookup(primary_providers=[as_byoti_prov])


def test_ti_config_and_load(query_provider):
    """Test loading settings and TI Lookup."""
    test_config1 = Path(_TEST_DATA).joinpath("msticpyconfig-askql.yaml")
    with custom_mp_config(test_config1):
        ti_settings = get_provider_settings()

        check.is_instance(ti_settings, dict)
        check.greater_equal(1, len(ti_settings))

        # Try to load TIProviders - should throw a warning on
        # missing provider class
        as_byoti_prov = AzSTI(query_provider=query_provider)
        ti_lookup = TILookup(primary_providers=[as_byoti_prov])

        # should have 2 successfully loaded providers
        check.greater_equal(1, len(ti_lookup.loaded_providers))
        check.greater_equal(1, len(ti_lookup.provider_status))


_IOC_URL = "http://ajaraheritage.ge/g7cberv"
_IOC_URLS = [
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
_IOC_IP = "91.219.31.18"
_IOC_IPS = [
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


def test_sentinel_ti_provider(ti_lookup):
    """Test TI provider queries."""
    end = datetime(2019, 8, 5, 22, 59, 59, 809000)
    start = datetime(2019, 8, 5, 22, 16, 16, 574000)

    result = ti_lookup.lookup_ioc(ioc=_IOC_URL, start=start, end=end)
    check.is_not_none(result)
    ioc_lookups = result.to_dict(orient="records")

    check.greater_equal(1, len(ioc_lookups))
    check.equal(ioc_lookups[0]["Provider"], "AzSTI")
    azs_result = ioc_lookups[0]
    check.equal(azs_result["Ioc"].lower(), _IOC_URL.lower())
    check.equal(azs_result["IocType"], "url")
    check.is_in("alert", azs_result["Details"]["Action"])
    check.equal(True, azs_result["Details"]["Active"])
    check.equal(100, azs_result["Details"]["ConfidenceScore"])
    check.is_in("Malware", azs_result["Details"]["ThreatType"])

    res_df = azs_result["RawResult"]
    check.is_instance(res_df, Dict)
    check.is_instance(azs_result["Reference"], str)
    check.is_true("ThreatIntelligenceIndicator  | where" in azs_result["Reference"])

    # IP Lookups
    result = ti_lookup.lookup_ioc(ioc=_IOC_IP, start=start, end=end)
    check.is_not_none(result)
    ioc_lookups = result.to_dict(orient="records")

    check.greater_equal(1, len(ioc_lookups))
    check.equal(ioc_lookups[0]["Provider"], "AzSTI")
    azs_result = ioc_lookups[0]
    check.equal(azs_result["Ioc"], _IOC_IP)
    check.equal(azs_result["IocType"], "ipv4")
    check.is_in("alert", azs_result["Details"]["Action"])
    check.equal(True, azs_result["Details"]["Active"])
    check.equal(70, azs_result["Details"]["ConfidenceScore"])
    check.is_in("Malware", azs_result["Details"]["ThreatType"])


_TEST_MULTI = [
    (_IOC_URLS, 10, 7, LookupStatus.OK.value, None),
    (_IOC_IPS, 20, 15, LookupStatus.OK.value, None),
    (
        {"c:\\empty_result.txt": "windows_path"},
        1,
        0,
        LookupStatus.NO_DATA.value,
        "Not found",
    ),
    (
        {"c:\\failed_query.txt": "windows_path"},
        1,
        0,
        LookupStatus.QUERY_FAILED.value,
        "Query failure",
    ),
    (
        {"c:\\no_dataframe.txt": "windows_path"},
        1,
        0,
        LookupStatus.QUERY_FAILED.value,
        "Query failure",
    ),
]


@pytest.mark.parametrize("src, num_results, positives, status, details", _TEST_MULTI)
def test_sentinel_ti_multi_lookups(
    ti_lookup, src, num_results, positives, status, details
):
    """Test multiple IOCs and failures."""
    end = datetime(2019, 8, 5, 22, 59, 59, 809000)
    start = datetime(2019, 8, 5, 22, 16, 16, 574000)
    # Bulk IP Lookups
    results = ti_lookup.lookup_iocs(data=src, start=start, end=end)
    check.is_not_none(results)
    check.equal(num_results, len(src))
    check.equal(len(results[results["Result"]]), positives)
    if details:
        check.equal(results.iloc[0]["Details"], details)
    check.equal(results.iloc[0]["Status"], status)
