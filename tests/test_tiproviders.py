# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""TIProviders test class."""
import unittest
import os
from pathlib import Path
from typing import Union, Any, Tuple

from ..msticpy.nbtools import pkg_config
from ..msticpy.sectools.tilookup import TILookup
from ..msticpy.sectools.tiproviders import (
    TIProviderSettings,
    get_provider_settings,
    preprocess_observable,
    LookupResult,
)

_test_data_folders = [
    d for d, _, _ in os.walk(os.getcwd()) if d.endswith("/tests/testdata")
]
if len(_test_data_folders) == 1:
    _TEST_DATA = _test_data_folders[0]
else:
    _TEST_DATA = "./tests/testdata"


class TestTIProviders(unittest.TestCase):
    """Unit test class."""

    def test_ti_config_and_load(self):
        test_config1 = Path(_TEST_DATA).joinpath(pkg_config._CONFIG_FILE)
        os.environ[pkg_config._CONFIG_ENV_VAR] = str(test_config1)

        pkg_config.refresh_config()

        ti_settings = get_provider_settings()

        self.assertIsInstance(ti_settings, dict)
        self.assertEqual(4, len(ti_settings))

        # Try to load TIProviders - should throw a warning on
        # missing provider class
        with self.assertWarns(UserWarning):
            ti_lookup = TILookup()

        # should have 2 succesfully loaded providers
        self.assertGreaterEqual(3, len(ti_lookup.loaded_providers))
        self.assertGreaterEqual(3, len(ti_lookup.provider_status))

    @staticmethod
    def load_ti_lookup():
        test_config1 = Path(_TEST_DATA).joinpath(pkg_config._CONFIG_FILE)
        os.environ[pkg_config._CONFIG_ENV_VAR] = str(test_config1)

        pkg_config.refresh_config()
        return TILookup()

    def test_OTX_provider(self):
        ti_lookup = self.load_ti_lookup()

        otx_provider = ti_lookup.loaded_providers["OTX"]
        iocs = {
            "124.5.6.7": ("ipv4", None),
            "office.microsoft.com": ("dns", None),
            "https://badplace.net/path1/path2?x=1": ("url", None),
            "7657fcb7d772448a6d8504e4b20168b7": ("file_hash", None),
            "7657fcb7d772448a6d8504e4b20168b8": ("md5_hash", None),
            "www.microsoft.com": ("hostname", None),
        }
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
        expected_params = ["headers"]

        ioc_unsupported = {"124.5.6.7": "ipv7"}
        self._run_http_provider_tests(
            otx_provider, iocs, mocked_result, expected_params, ioc_unsupported
        )

    def test_XForce_provider(self):
        ti_lookup = self.load_ti_lookup()

        provider = ti_lookup.loaded_providers["XForce"]
        iocs = {
            "124.5.6.7": ("ipv4", None),
            "124.5.6.7": ("ipv4", "rep"),
            "124.5.6.7": ("ipv4", "malware"),
            "124.5.6.7": ("ipv4", "whois"),
            "office.microsoft.com": ("dns", "info"),
            "https://badplace.net/path1/path2?x=1": ("url", None),
            "7657fcb7d772448a6d8504e4b20168b7": ("file_hash", None),
            "7657fcb7d772448a6d8504e4b20168b8": ("md5_hash", None),
            "www.microsoft.com": ("hostname", "whois"),
        }
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
        expected_params = ["auth"]

        ioc_unsupported = {"124.5.6.7": "ipv7"}
        self._run_http_provider_tests(
            provider, iocs, mocked_result, expected_params, ioc_unsupported
        )

    def test_VirusTotal_provider(self):
        ti_lookup = self.load_ti_lookup()

        provider = ti_lookup.loaded_providers["VirusTotal"]
        iocs = {
            "124.5.6.7": ("ipv4", None),
            "office.microsoft.com": ("dns", None),
            "https://badplace.net/path1/path2?x=1": ("url", None),
            "7657fcb7d772448a6d8504e4b20168b7": ("file_hash", None),
            "7657fcb7d772448a6d8504e4b20168b8": ("md5_hash", None),
        }
        mocked_result = {
            "resource": "ioc",
            "permalink": "https://virustotal.com/report.html",
            "positives": 1,
            "detected_urls": [{"url": "http://bad.com/foo", "positives": 1}],
            "verbose_msg": "A long description....",
            "response_code": 1,
        }
        expected_params = ["params", "headers"]

        ioc_unsupported = {"124.5.6.7": "ipv7"}
        self._run_http_provider_tests(
            provider, iocs, mocked_result, expected_params, ioc_unsupported
        )

    def _run_http_provider_tests(
        self, provider, test_iocs, test_results, expected_req_param, unsupported_iocs
    ):

        for ioc, (ioc_type, sub_type) in test_iocs.items():
            resolved_type = provider.resolve_ioc_type(ioc)
            if ioc_type == "file_hash":
                self.assertIn(resolved_type, ["md5_hash", "sha1_hash", "sha256_hash"])
            elif ioc_type == "hostname":
                self.assertEqual(resolved_type, "dns")
            else:
                self.assertEqual(resolved_type, ioc_type)

            self.assertTrue(provider.is_supported_type(resolved_type))
            self.assertTrue(provider.is_supported_type(ioc_type))
            clean_ioc = preprocess_observable(ioc, ioc_type)
            self.assertEqual(clean_ioc.status, "ok")

            verb, req_params = provider._substitute_parms(
                ioc=clean_ioc.observable, ioc_type=ioc_type, query_type=sub_type
            )

            self.assertEqual("GET", verb)
            self.assertIn("url", req_params)
            self.assertTrue(provider._ioc_extract.validate(req_params["url"], "url"))
            for param in expected_req_param:
                self.assertIn(param, req_params)

            # Create LookupResult
            result = LookupResult(ioc=ioc, ioc_type=ioc_type, query_subtype=sub_type)
            result.raw_result = test_results
            result.status = 200
            result.result, result.details = provider.parse_results(result)
            result.reference = req_params["url"]

            self.assertTrue(result.result)

        for ioc, ioc_type in unsupported_iocs.items():
            resolved_type = provider.resolve_ioc_type(ioc)
            self.assertNotEqual(resolved_type, ioc_type)

            self.assertFalse(provider.is_supported_type(ioc_type))
            clean_ioc = preprocess_observable(ioc, ioc_type)
            self.assertNotEqual(clean_ioc.status, "ok")
