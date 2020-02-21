# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""IP Utils test class."""
import unittest
import json
import os

import pandas as pd

from ..msticpy.sectools.ip_utils import get_whois_info, get_whois_df, get_ip_type


_test_data_folders = [
    d for d, _, _ in os.walk(os.getcwd()) if d.endswith("/tests/testdata")
]
if len(_test_data_folders) == 1:
    _TEST_DATA = _test_data_folders[0]
else:
    _TEST_DATA = "./tests/testdata"


class TestIpUtils(unittest.TestCase):
    """Unit test class."""

    IPV4 = {
        "Private": ("10.0.0.1", ["Private", "Reserved"]),
        "Multicast": ("224.0.0.1", None),
        "Unspecified": ("0.0.0.0", None),
        "Reserved": ("198.51.100.1", ["Private", "Reserved"]),
        "Loopback": ("127.0.0.1", None),
        "Public": ("153.2.3.4", None),
        "Link Local": ("169.254.0.1", None),
    }
    IPV6 = {
        "Private": ("FC00::C001:1DFF:FEE0:0", None),
        "Multicast": ("FF00::", None),
        "Unspecified": ("::", None),
        "Reserved": ("2001:db8::", ["Private", "Reserved"]),
        "Loopback": ("::1", None),
        "Public": ("2340:0023:AABA:0A01:0055:5054:9ABC:ABB0", None),
        "Link Local": ("FE80::C001:1DFF:FEE0:0", None),
    }

    def setUp(self):
        input_file = os.path.join(_TEST_DATA, "az_net_flows.csv")
        self.input_df = pd.read_csv(input_file).sample(10)

    def test_get_ip_type(self):

        for ip_type, (addr, alts) in self.IPV4.items():
            print(addr, ip_type)
            if alts:
                self.assertIn(get_ip_type(addr), alts)
            else:
                self.assertEqual(get_ip_type(addr), ip_type)
        for ip_type, (addr, alts) in self.IPV6.items():
            print(addr, ip_type)
            if alts:
                self.assertIn(get_ip_type(addr), alts)
            else:
                self.assertEqual(get_ip_type(addr), ip_type)

    def test_get_whois(self):
        ms_ip = "13.107.4.50"
        ms_asn = "MICROSOFT-CORP-MSN-AS-BLOCK, US"
        asn, whois = get_whois_info(ms_ip)
        self.assertEqual(asn, ms_asn)

        asn, whois = get_whois_info(self.IPV4["Private"][0])
        invalid_type = "No ASN Information for IP type: Private"
        self.assertEqual(asn, invalid_type)

    def test_get_whois_df(self):
        results = get_whois_df(data=self.input_df, ip_column="AllExtIPs")
        self.assertEqual(len(results), len(self.input_df))
        self.assertIn("AsnDescription", results.columns)

        results2 = get_whois_df(
            data=self.input_df, ip_column="AllExtIPs", asn_col="asn", whois_col="whois"
        )
        self.assertEqual(len(results2), len(self.input_df))
        self.assertIn("asn", results2.columns)
        self.assertIn("whois", results2.columns)
        self.assertEqual(len(results2[~results2["asn"].isna()]), len(self.input_df))
        self.assertEqual(len(results2[~results2["whois"].isna()]), len(self.input_df))

    def test_whois_pdext(self):
        results = self.input_df.mp_whois.lookup(ip_column="AllExtIPs")
        self.assertEqual(len(results), len(self.input_df))
        self.assertIn("AsnDescription", results.columns)

        results2 = self.input_df.mp_whois.lookup(
            ip_column="AllExtIPs", asn_col="asn", whois_col="whois"
        )
        self.assertEqual(len(results2), len(self.input_df))
        self.assertIn("asn", results2.columns)
        self.assertIn("whois", results2.columns)
        self.assertEqual(len(results2[~results2["asn"].isna()]), len(self.input_df))
        self.assertEqual(len(results2[~results2["whois"].isna()]), len(self.input_df))
