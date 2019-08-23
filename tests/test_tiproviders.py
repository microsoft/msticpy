# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""TIProviders test class."""
import unittest
from unittest import mock
import os
from pathlib import Path
from typing import Union, Any, Tuple

from ..msticpy.nbtools import pkg_config
from ..msticpy.sectools.iocextract import IoCExtract
from ..msticpy.sectools.tilookup import TILookup
from ..msticpy.sectools.tiproviders import (
    TIProviderSettings,
    get_provider_settings,
    preprocess_observable,
    LookupResult,
    HttpProvider,
)

_test_data_folders = [
    d for d, _, _ in os.walk(os.getcwd()) if d.endswith("/tests/testdata")
]
if len(_test_data_folders) == 1:
    _TEST_DATA = _test_data_folders[0]
else:
    _TEST_DATA = "./tests/testdata"


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
]
ioc_benign_iocs = ["40.76.4.15", "40.112.72.205", "40.113.200.201"]  # benign


def is_benign_ioc(request_item):
    if isinstance(request_item, str):
        return any([item for item in ioc_benign_iocs if item in request_item])
    if isinstance(request_item, dict):
        return any([item for item in ioc_benign_iocs if item in request_item.values()])
    return False


def mocked_session(*args, **kwargs):
    return mock_req_session()


# This class will requests.Session()
class mock_req_session:
    def get(self, *args, **kwargs):
        class MockResponse:
            def __init__(self, json_data, status_code):
                self.json_data = json_data
                self.status_code = status_code

            def json(self):
                return self.json_data

        if "url" not in kwargs:
            return MockResponse(None, 404)
        if kwargs["url"].startswith("https://otx.alienvault.com"):
            if is_benign_ioc(kwargs["url"]):
                return MockResponse(None, 404)
            mocked_result = {
                "response": "Found stuff",
                "pulse_info": {
                    "pulses": [
                        {
                            "name": ["somename"],
                            "tags": ["bad", "good", "ugly"],
                            "references": ["url1", "url2"],
                        }
                    ]
                },
            }
            return MockResponse(mocked_result, 200)
        elif kwargs["url"].startswith("https://api.xforce.ibmcloud.com"):
            if is_benign_ioc(kwargs["url"]):
                return MockResponse(None, 404)
            mocked_result = {
                "score": 1,
                "cats": ["one", "two"],
                "reason": "no reason really",
                "reasonDescription": "what does it mean?",
                "tags": ["bad", "good", "ugly"],
                "malware": {
                    "risk": "high",
                    "family": {"type": "trojan", "name": "mybadfile"},
                },
                "total_rows": 4,
                "categoryDescriptions": ["desc1", "desc2"],
                "contact": [
                    {
                        "type": "registrant",
                        "name": "Domain Administrator",
                        "organization": "Microsoft Corporation",
                        "country": "United States",
                    }
                ],
            }
            return MockResponse(mocked_result, 200)
        elif kwargs["url"].startswith("https://www.virustotal.com/"):
            if is_benign_ioc(kwargs["params"]):
                return MockResponse(None, 404)
            mocked_result = {
                "resource": "ioc",
                "permalink": "https://virustotal.com/report.html",
                "positives": 1,
                "detected_urls": [{"url": "http://bad.com/foo", "positives": 1}],
                "verbose_msg": "A long description....",
                "response_code": 1,
            }
            return MockResponse(mocked_result, 200)
        return MockResponse(None, 404)


class TestTIProviders(unittest.TestCase):
    """Unit test class."""

    ti_lookup = None

    def setUp(self):
        self.ti_lookup = self.load_ti_lookup()

    @staticmethod
    def load_ti_lookup():
        test_config1 = Path(_TEST_DATA).joinpath(pkg_config._CONFIG_FILE)
        os.environ[pkg_config._CONFIG_ENV_VAR] = str(test_config1)

        pkg_config.refresh_config()
        return TILookup()

    def test_ti_config_and_load(self):
        self.load_ti_lookup()

        ti_settings = get_provider_settings()

        self.assertIsInstance(ti_settings, dict)
        self.assertGreaterEqual(4, len(ti_settings))

        # Try to load TIProviders - should throw a warning on
        # missing provider class
        with self.assertWarns(UserWarning):
            ti_lookup = TILookup()

        # should have 2 succesfully loaded providers
        self.assertGreaterEqual(3, len(ti_lookup.loaded_providers))
        self.assertGreaterEqual(3, len(ti_lookup.provider_status))

    def test_xforce(self):
        self.exercise_provider("XForce")

    def test_otx(self):
        self.exercise_provider("OTX")

    def test_virus_total(self):
        self.exercise_provider("VirusTotal")

    def exercise_provider(self, provider_name):
        ti_lookup = self.ti_lookup

        ti_provider = ti_lookup.loaded_providers[provider_name]
        saved_session = ti_provider._requests_session
        ti_provider._requests_session = mock_req_session()

        iocs = {
            "124.5.6.7": ("ipv4", None),
            "124.5.6.8": ("ipv4", "rep"),
            "124.5.6.9": ("ipv4", "malware"),
            "124.5.6.10": ("ipv4", "whois"),
            "office.microsoft.com": ("dns", "info"),
            "https://badplace.net/path1/path2?x=1": ("url", None),
            "7657fcb7d772448a6d8504e4b20168b7": ("file_hash", None),
            "7657fcb7d772448a6d8504e4b20168b8": ("md5_hash", None),
            "www.microsoft.com": ("hostname", "whois"),
        }

        # Lookup multiple IoCs
        for ioc, ioc_params in iocs.items():
            result = ti_lookup.lookup_ioc(
                observable=ioc,
                ioc_type=ioc_params[0],
                ioc_query_type=ioc_params[1],
                providers=[provider_name],
            )
            self.verify_result(result)

        results_df = ti_lookup.lookup_iocs(
            data=(ioc_ips + ioc_benign_iocs), providers=[provider_name]
        )
        self.assertEqual(20, len(results_df))
        self.assertEqual(17, len(results_df[results_df["Result"] == True]))

        ti_provider._requests_session = saved_session

    def verify_result(self, result):
        self.assertIsNotNone(result)
        for prov, lu_result in result[1]:
            self.assertIn(prov, ["OTX", "XForce", "VirusTotal"])
            self.assertIsNotNone(lu_result.ioc)
            self.assertIsNotNone(lu_result.ioc_type)
            if lu_result.result:
                self.assertIsNotNone(lu_result.details)
                self.assertIsNotNone(lu_result.raw_result)
                self.assertIsNotNone(lu_result.reference)
