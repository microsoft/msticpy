# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""vtlookup test class."""
import unittest
from os import path

from msticpy.context.preprocess_observable import preprocess_observable
from msticpy.context.vtlookupv3.vtlookup import VTLookup

from ..unit_test_lib import TEST_DATA_PATH

# pylint: disable=protected-access


class TestVTLookup(unittest.TestCase):
    _TEST_COLS = [
        "Observable",
        "IoCType",
        "Status",
        "ResponseCode",
        "Resource",
        "SourceIndex",
        "VerboseMsg",
        "Resource",
        "ScanId",
        "Permalink",
        "Positives",
        "MD5",
        "SHA1",
        "SHA256",
        "ResolvedDomains",
        "ResolvedIPs",
        "DetectedUrls",
    ]

    def test_ipvalidation(self):
        test_ips = [
            ("valid", "90.156.201.27", "90.156.201.27"),
            ("local_ip", "10.0.0.1", None),
            ("mask", "255.255.20.27", None),
            ("loopback", "127.0.0.1", None),
            ("bad_format", "122.1.3", None),
        ]

        for test_case in test_ips:
            result, status = preprocess_observable(test_case[1], "ipv4")
            self.assertEqual(result, test_case[2])
            print(test_case[0], status)

    def test_urlvalidation(self):
        v1_url = "http://club-fox.ru/img/www.loginalibaba.com/alibaba/alibaba/login.alibaba.com.php?email=biuro"
        v2_url = "https://www.virustotal.com/en/ip-address/90.156.201.27/information/"
        test_urls = [
            ("valid1", v1_url, v1_url),
            ("valid2", "https://microsoft.com", "https://microsoft.com"),
            ("valid3", "https://python.org", "https://python.org"),
            ("valid3", v2_url, v2_url),
            ("local_ip", "http://10.0.0.1/foo", None),
            ("local_hostname", "https://myhost/path", None),
            ("invalid1", "http//club-fox.ru/foo.html", None),
            ("invalid2", "//club-fox.ru/foo.html", None),
            ("invalid3", "https://123:microsoft.com@user/foo.txt", None),
            ("invalid4", "http//10.0.0.1/foo.txt", None),
        ]

        for test_case in test_urls:
            result, status = preprocess_observable(test_case[1], "url")
            print(test_case[0], status)
            self.assertEqual(
                result,
                test_case[2],
                f"Failed on test case {test_case} ({test_case[1]})",
            )

    def test_parse_file_results(self):
        vtlookup = VTLookup(vtkey="fake", verbosity=2)

        FILE_NAME = path.join(TEST_DATA_PATH, "fileresponse.json")
        with open(FILE_NAME, "r", encoding="utf8") as file_handle:
            txt = file_handle.read()

        vt_params = vtlookup._VT_API_TYPES["file"]
        vtlookup._parse_vt_results(
            vt_results=txt,
            observable="7657fcb7d772448a6d8504e4b20168b8",
            ioc_type="md5_hash",
            vt_param=vt_params,
        )

        test_df = vtlookup.results[self._TEST_COLS]
        self.assertEqual(len(test_df), 1)
        print(test_df.T)

        vtlookup = VTLookup(vtkey="fake", verbosity=2)
        FILE_NAME2 = path.join(TEST_DATA_PATH, "file-multi_pos.json")
        with open(FILE_NAME2, "r", encoding="utf8") as file_handle:
            txt = file_handle.read()

        vt_params = vtlookup._VT_API_TYPES["file"]
        vtlookup._parse_vt_results(
            vt_results=txt,
            observable="7657fcb7d772448a6d8504e4b20168b8",
            ioc_type="md5_hash",
            vt_param=vt_params,
        )

        test_df = vtlookup.results[self._TEST_COLS]
        self.assertEqual(len(test_df), 3)
        print(test_df.T)

    def test_parse_url_results(self):
        vtlookup = VTLookup(vtkey="fake", verbosity=2)

        FILE_NAME = path.join(TEST_DATA_PATH, "url_pos.json")
        with open(FILE_NAME, "r", encoding="utf8") as file_handle:
            txt = file_handle.read()

        vt_params = vtlookup._VT_API_TYPES["url"]
        vtlookup._parse_vt_results(
            vt_results=txt,
            observable="7657fcb7d772448a6d8504e4b20168b8",
            ioc_type="url",
            vt_param=vt_params,
        )
        test_df = vtlookup.results[self._TEST_COLS]
        self.assertEqual(len(test_df), 1)
        print(test_df.T)

        vtlookup = VTLookup(vtkey="fake", verbosity=2)
        FILE_NAME2 = path.join(TEST_DATA_PATH, "url_neg.json")
        with open(FILE_NAME2, "r", encoding="utf8") as file_handle:
            txt = file_handle.read()

        vt_params = vtlookup._VT_API_TYPES["url"]
        vtlookup._parse_vt_results(
            vt_results=txt,
            observable="7657fcb7d772448a6d8504e4b20168b8",
            ioc_type="url",
            vt_param=vt_params,
        )
        test_df = vtlookup.results[self._TEST_COLS]
        self.assertEqual(len(test_df), 1)
        print(test_df.T)

    def test_parse_domain_results(self):
        vtlookup = VTLookup(vtkey="fake", verbosity=2)

        FILE_NAME = path.join(TEST_DATA_PATH, "domain_pos.json")
        with open(FILE_NAME, "r", encoding="utf8") as file_handle:
            txt = file_handle.read()

        vt_params = vtlookup._VT_API_TYPES["domain"]
        vtlookup._parse_vt_results(
            vt_results=txt,
            observable="7657fcb7d772448a6d8504e4b20168b8",
            ioc_type="dns",
            vt_param=vt_params,
        )

        test_df = vtlookup.results[self._TEST_COLS]
        self.assertEqual(len(test_df), 1)
        self.assertGreater(len(test_df[["ResolvedIPs"]]), 0)
        self.assertGreater(len(test_df[["DetectedUrls"]].values), 0)
        self.assertGreater(test_df[["Positives"]].values, 0)

        print(test_df.T)

        vtlookup = VTLookup(vtkey="fake", verbosity=2)
        FILE_NAME2 = path.join(TEST_DATA_PATH, "domain_neg.json")
        with open(FILE_NAME2, "r", encoding="utf8") as file_handle:
            txt = file_handle.read()

        vt_params = vtlookup._VT_API_TYPES["domain"]
        vtlookup._parse_vt_results(
            vt_results=txt,
            observable="7657fcb7d772448a6d8504e4b20168b8",
            ioc_type="dns",
            vt_param=vt_params,
        )
        test_df = vtlookup.results[self._TEST_COLS]
        self.assertEqual(len(test_df), 1)
        self.assertGreater(len(test_df[["ResolvedIPs"]].values), 0)
        self.assertGreater(len(test_df[["DetectedUrls"]].values), 0)
        print(test_df.T)

    def test_parse_ip_results(self):
        vtlookup = VTLookup(vtkey="fake", verbosity=2)

        FILE_NAME = path.join(TEST_DATA_PATH, "ip-address_pos.json")
        with open(FILE_NAME, "r", encoding="utf8") as file_handle:
            txt = file_handle.read()

        vt_params = vtlookup._VT_API_TYPES["ip-address"]
        vtlookup._parse_vt_results(
            vt_results=txt,
            observable="7657fcb7d772448a6d8504e4b20168b8",
            ioc_type="ipv4",
            vt_param=vt_params,
        )
        test_df = vtlookup.results[self._TEST_COLS]
        self.assertEqual(len(test_df), 1)
        self.assertGreater(len(test_df[["ResolvedDomains"]].values), 0)
        self.assertGreater(len(test_df[["DetectedUrls"]].values), 0)
        self.assertGreater(test_df[["Positives"]].values, 0)
        print(test_df.T)

        vtlookup = VTLookup(vtkey="fake", verbosity=2)
        FILE_NAME2 = path.join(TEST_DATA_PATH, "ip-address_neg.json")
        with open(FILE_NAME2, "r", encoding="utf8") as file_handle:
            txt = file_handle.read()

        vt_params = vtlookup._VT_API_TYPES["ip-address"]
        vtlookup._parse_vt_results(
            vt_results=txt,
            observable="7657fcb7d772448a6d8504e4b20168b8",
            ioc_type="ipv4",
            vt_param=vt_params,
        )
        test_df = vtlookup.results[self._TEST_COLS]
        self.assertEqual(len(test_df), 1)
        self.assertGreater(len(test_df[["ResolvedDomains"]].values), 0)
        self.assertEqual(test_df[["Positives"]].values, 0)
        print(test_df.T)


if __name__ == "__main__":
    unittest.main()
    print("bye")
