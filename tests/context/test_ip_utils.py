# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""IP Utils test class."""
import os

import pandas as pd
import pytest
import pytest_check as check

from msticpy.context.ip_utils import get_ip_type, get_whois_df, get_whois_info

from ..unit_test_lib import TEST_DATA_PATH


# pylint: disable=redefined-outer-name
@pytest.fixture(scope="module")
def net_df():
    """Return network dataframe."""
    input_file = os.path.join(TEST_DATA_PATH, "az_net_flows.csv")
    return pd.read_csv(input_file).sample(10)


IPV4 = {
    "Private": ("10.0.0.1", ["Private", "Reserved"]),
    "Multicast": ("224.0.0.1", None),
    "Unspecified": ("0.0.0.0", None),  # nosec
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


def test_get_ip_type():
    """Test IP Type."""
    for ip_type, (addr, alts) in IPV4.items():
        print(addr, ip_type)
        if alts:
            check.is_in(get_ip_type(addr), alts)
        else:
            check.equal(get_ip_type(addr), ip_type)
    for ip_type, (addr, alts) in IPV6.items():
        print(addr, ip_type)
        if alts:
            check.is_in(get_ip_type(addr), alts)
        else:
            check.equal(get_ip_type(addr), ip_type)


def test_get_whois():
    """Test IP Whois."""
    ms_ip = "13.107.4.50"
    ms_asn = "MICROSOFT-CORP"
    asn, _ = get_whois_info(ms_ip)
    check.is_in(ms_asn, asn)

    asn, _ = get_whois_info(IPV4["Private"][0])
    invalid_type = "No ASN Information for IP type: Private"
    check.equal(asn, invalid_type)


def test_get_whois_df(net_df):
    """Test IP Whois."""
    net_df = net_df.head(25)
    results = get_whois_df(data=net_df, ip_column="AllExtIPs")
    check.equal(len(results), len(net_df))
    check.is_in("AsnDescription", results.columns)

    results2 = get_whois_df(
        data=net_df, ip_column="AllExtIPs", asn_col="asn", whois_col="whois"
    )
    check.equal(len(results2), len(net_df))
    check.is_in("asn", results2.columns)
    check.is_in("whois", results2.columns)
    check.less_equal(len(results2[~results2["asn"].isna()]), len(net_df))
    check.equal(len(results2[~results2["whois"].isna()]), len(net_df))


@pytest.mark.filterwarnings("ignore::DeprecationWarning")
def test_whois_pdext(net_df):
    """Test IP Whois."""
    net_df = net_df.head(25)
    results = net_df.mp_whois.lookup(ip_column="AllExtIPs")
    check.equal(len(results), len(net_df))
    check.is_in("AsnDescription", results.columns)

    results2 = net_df.mp_whois.lookup(
        ip_column="AllExtIPs", asn_col="asn", whois_col="whois"
    )
    check.equal(len(results2), len(net_df))
    check.is_in("asn", results2.columns)
    check.is_in("whois", results2.columns)
    check.less_equal(len(results2[~results2["asn"].isna()]), len(net_df))
    check.equal(len(results2[~results2["whois"].isna()]), len(net_df))
