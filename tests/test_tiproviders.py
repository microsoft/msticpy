# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""TIProviders test class."""
import io
import os
import random
import string
import unittest
import warnings
from contextlib import redirect_stdout
from pathlib import Path
from typing import Any, Tuple, Union
from unittest import mock

from ..msticpy.nbtools import pkg_config
from ..msticpy.sectools.iocextract import IoCExtract
from ..msticpy.sectools.tilookup import TILookup
from ..msticpy.sectools.tiproviders import (
    HttpProvider,
    LookupResult,
    ProviderSettings,
    get_provider_settings,
    preprocess_observable,
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


# This class will mock requests.Session()
class mock_req_session:
    def get(self, *args, **kwargs):
        class MockResponse:
            def __init__(self, json_data, status_code):
                self.json_data = json_data
                self.status_code = status_code

            def json(self):
                return self.json_data

        if "url" not in kwargs:
            if args:
                kwargs.update({"url", args[0]})
            else:
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
        elif kwargs["url"].startswith("https://openpagerank.com"):
            dom_responses = {
                "google.com": {
                    "status_code": 200,
                    "error": "",
                    "page_rank_integer": 10,
                    "page_rank_decimal": 10,
                    "rank": "6",
                    "domain": "google.com",
                },
                "microsoft.com": {
                    "status_code": 200,
                    "error": "",
                    "page_rank_integer": 8,
                    "page_rank_decimal": 7.63,
                    "rank": "40",
                    "domain": "microsoft.com",
                },
                "unknown.dom": {
                    "status_code": 404,
                    "error": "Domain not found",
                    "page_rank_integer": 0,
                    "page_rank_decimal": 0,
                    "rank": None,
                    "domain": "unknowndomain.com",
                },
            }

            if "params" in kwargs:
                mocked_result = {
                    "status_code": 200,
                    "response": [dom_responses["unknown.dom"]],
                }
                for domain in dom_responses.keys():
                    if domain in kwargs["params"].values():
                        mocked_result = {
                            "status_code": 200,
                            "response": [dom_responses[domain]],
                        }
                        break
                return MockResponse(mocked_result, 200)
            else:
                url_param_str = kwargs["url"].split("?", 1)[1]
                url_params = url_param_str.split("&")
                if len(url_params) > 100:
                    raise ValueError("Maximum of 100 items in bulk request")
                rand_responses = []
                for param in url_params:
                    dom = param.split("=")[1]
                    rank = random.randint(1, 1000)
                    if bool(rank % 2):
                        dom_resp = {
                            "status_code": 200,
                            "error": "",
                            "page_rank_integer": rank,
                            "page_rank_decimal": float(rank),
                            "rank": str(rank),
                            "domain": dom,
                        }
                    else:
                        dom_resp = {
                            "status_code": 404,
                            "error": "Domain not found",
                            "page_rank_integer": 0,
                            "page_rank_decimal": 0,
                            "rank": None,
                            "domain": dom,
                        }
                    rand_responses.append(dom_resp)
                mocked_result = {"status_code": 200, "response": rand_responses}
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

        with warnings.catch_warnings():
            # We want to ignore warnings from missing config
            warnings.simplefilter("ignore", category=UserWarning)

            pkg_config.refresh_config()
            return TILookup()

    def test_ti_config_and_load(self):
        self.load_ti_lookup()

        with self.assertWarns(UserWarning):
            ti_settings = get_provider_settings()

        self.assertIsInstance(ti_settings, dict)
        self.assertGreaterEqual(len(ti_settings), 4)

        # Try to load TIProviders - should throw a warning on
        # missing provider class
        with self.assertWarns(UserWarning):
            ti_lookup = TILookup()

        # should have 2 succesfully loaded providers
        self.assertGreaterEqual(len(ti_lookup.loaded_providers), 3)
        self.assertGreaterEqual(len(ti_lookup.provider_status), 3)

    def test_tilookup_utils(self):
        av_provs = self.ti_lookup.available_providers
        self.assertGreaterEqual(len(av_provs), 1)
        self.ti_lookup.provider_usage()
        self.ti_lookup.list_available_providers(show_query_types=True)
        self.ti_lookup.reload_providers()

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
                # exercise summary functions of Lookup class
                output = io.StringIO()
                with redirect_stdout(output):
                    lu_result.summary
                self.assertIsNotNone(output.getvalue())
                output = io.StringIO()
                with redirect_stdout(output):
                    lu_result.raw_result_fmtd
                self.assertIsNotNone(output.getvalue())

    def test_opr_single_lookup(self):
        ti_lookup = self.ti_lookup

        ti_provider = ti_lookup.loaded_providers["OPR"]
        saved_session = ti_provider._requests_session
        ti_provider._requests_session = mock_req_session()
        iocs = {
            "google.com": ("dns", None),
            "microsoft.com": ("dns", None),
            "badplace.net": ("dns", None),
        }

        # Lookup multiple IoCs
        for ioc, ioc_params in iocs.items():
            result = ti_lookup.lookup_ioc(
                observable=ioc,
                ioc_type=ioc_params[0],
                ioc_query_type=ioc_params[1],
                providers=["OPR"],
            )
            self.assertIsNotNone(result)
            for prov, lu_result in result[1]:
                self.assertIsNotNone(lu_result.ioc)
                self.assertIsNotNone(lu_result.ioc_type)
                if lu_result.severity > 0:
                    self.assertTrue(
                        "rank" in lu_result.details
                        and lu_result.details["rank"] is None
                    )
                    self.assertTrue(
                        "error" in lu_result.details
                        and lu_result.details["error"] == "Domain not found"
                    )
                else:
                    self.assertTrue(
                        "rank" in lu_result.details
                        and lu_result.details["rank"].isdigit()
                        and int(lu_result.details["rank"]) > 0
                    )
                    self.assertTrue(
                        "response" in lu_result.raw_result
                        and lu_result.raw_result["response"][0]
                        and lu_result.raw_result["response"][0]["domain"] == ioc
                    )

    def test_opr_bulk_lookup(self):
        ti_lookup = self.ti_lookup

        ti_provider = ti_lookup.loaded_providers["OPR"]
        saved_session = ti_provider._requests_session
        ti_provider._requests_session = mock_req_session()

        n_requests = 250
        gen_doms = {self._generate_rand_domain(): "dns" for i in range(n_requests)}
        results_df = ti_lookup.lookup_iocs(data=gen_doms, providers=["OPR"])

        self.assertEqual(n_requests, len(results_df))
        self.assertGreater(len(results_df[results_df["Severity"] > 0]), n_requests / 3)
        self.assertEqual(n_requests, len(results_df[results_df["Result"] == True]))

    def _generate_rand_domain(self):
        dom_suffixes = ["com", "org", "net", "biz"]
        letters = string.ascii_letters
        str_length = random.randint(4, 20)
        dom = ""
        for i in range(0, 2):
            dom_part = "".join(random.choice(letters) for i in range(str_length))
            dom = dom + "." + dom_part if dom else dom_part
        suffix = random.choice(dom_suffixes)

        return dom + "." + suffix

    def test_tor_exit_nodes(self):
        ti_lookup = self.ti_lookup

        # we can't use a fixed list since this changes all the time
        # so take a sample from the current list
        tor_prov = ti_lookup.loaded_providers["Tor"]
        tor_nodes = random.sample(tor_prov._nodelist.keys(), 4)

        other_ips = [
            "104.117.0.237",
            "13.107.4.50",
            "172.217.10.144",
            "172.217.11.16",
            "172.217.15.112",
        ]

        pos_results = []
        neg_results = []
        for ioc in tor_nodes + other_ips:
            result = ti_lookup.lookup_ioc(
                observable=ioc, ioc_type="ipv4", providers=["Tor"]
            )
            lu_result = result[1][0][1]
            self.assertTrue(lu_result.result)
            self.assertTrue(bool(lu_result.reference))
            if lu_result.severity > 0:
                self.assertTrue(bool(lu_result.details))
                self.assertTrue(bool(lu_result.raw_result))
                pos_results.append(lu_result)
            else:
                neg_results.append(lu_result)

        self.assertEqual(len(pos_results), 4)
        self.assertEqual(len(neg_results), 5)

        all_ips = tor_nodes + other_ips
        tor_results_df = ti_lookup.lookup_iocs(data=all_ips, providers=["Tor"])
        self.assertEqual(len(all_ips), len(tor_results_df))
        self.assertEqual(len(tor_results_df[tor_results_df["Severity"] > 0]), 4)
        self.assertEqual(len(tor_results_df[tor_results_df["Severity"] == 0]), 5)

    def test_check_ioc_type(self):
        provider = self.ti_lookup.loaded_providers["OTX"]
        lu_result = provider._check_ioc_type(ioc="a.b.c.d", ioc_type="ipv4")
        self.assertEqual(lu_result.status, 2)
        lu_result = provider._check_ioc_type(ioc="a.b.c.d", ioc_type="ipv6")
        self.assertEqual(lu_result.status, 2)
        lu_result = provider._check_ioc_type(ioc="url", ioc_type="ipv4")
        self.assertEqual(lu_result.status, 2)
        lu_result = provider._check_ioc_type(ioc="123", ioc_type="dns")
        self.assertEqual(lu_result.status, 2)
        lu_result = provider._check_ioc_type(ioc="123456", ioc_type="file_hash")
        self.assertEqual(lu_result.status, 2)
