# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""IP Utils test class."""
import os
import re
from unittest.mock import patch

import pandas as pd
import pytest
import pytest_check as check
import respx

from msticpy.context.ip_utils import (
    get_asn_details,
    get_asn_from_ip,
    get_asn_from_name,
    get_ip_type,
    get_whois_df,
    ip_whois,
)

from ..unit_test_lib import TEST_DATA_PATH, get_test_data_path


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

ASN_RESPONSE = (
    "AS      | IP               | BGP Prefix          | CC | Registry | Allocated  "
    "| AS Name\n8068    | 13.107.4.50      | 13.107.4.0/24       | US | arin     "
    "| 2015-03-26 | MICROSOFT-CORP-MSN-AS-BLOCK, US\n"
)

ASN_RESPONSE_2 = (
    "AS      | IP               | BGP Prefix          | CC | Registry | Allocated  "
    "| AS Name\n8075    | 65.55.44.109     | 65.52.0.0/14        | US | arin     "
    "| 2001-02-14 | MICROSOFT-CORP-MSN-AS-BLOCK, US\n"
)

RDAP_RESPONSE = {
    "rdapConformance": [
        "nro_rdap_profile_0",
        "rdap_level_0",
        "cidr0",
        "arin_originas0",
    ],
    "notices": [
        {
            "title": "Terms of Service",
            "description": [
                "By using the ARIN RDAP/Whois service, you are agreeing to the RDAP/Whois Terms of Use"
            ],
            "links": [
                {
                    "value": "https://rdap.arin.net/registry/ip/13.107.4.50",
                    "rel": "terms-of-service",
                    "type": "text/html",
                    "href": "https://www.arin.net/resources/registry/whois/tou/",
                }
            ],
        },
        {
            "title": "Whois Inaccuracy Reporting",
            "description": ["If you see inaccuracies in the results, please visit: "],
            "links": [
                {
                    "value": "https://rdap.arin.net/registry/ip/13.107.4.50",
                    "rel": "inaccuracy-report",
                    "type": "text/html",
                    "href": "https://www.arin.net/resources/registry/whois/inaccuracy_reporting/",
                }
            ],
        },
        {
            "title": "Copyright Notice",
            "description": [
                "Copyright 1997-2022, American Registry for Internet Numbers, Ltd."
            ],
        },
    ],
    "handle": "NET-13-64-0-0-1",
    "startAddress": "13.64.0.0",
    "endAddress": "13.107.255.255",
    "ipVersion": "v4",
    "name": "MSFT",
    "type": "DIRECT ALLOCATION",
    "parentHandle": "NET-13-0-0-0-0",
    "events": [
        {"eventAction": "last changed", "eventDate": "2021-12-14T20:28:53-05:00"},
        {"eventAction": "registration", "eventDate": "2015-03-26T13:58:18-04:00"},
    ],
    "links": [
        {
            "value": "https://rdap.arin.net/registry/ip/13.107.4.50",
            "rel": "self",
            "type": "application/rdap+json",
            "href": "https://rdap.arin.net/registry/ip/13.64.0.0",
        },
        {
            "value": "https://rdap.arin.net/registry/ip/13.107.4.50",
            "rel": "alternate",
            "type": "application/xml",
            "href": "https://whois.arin.net/rest/net/NET-13-64-0-0-1",
        },
    ],
    "entities": [
        {
            "handle": "MSFT",
            "vcardArray": [
                "vcard",
                [
                    ["version", {}, "text", "4.0"],
                    ["fn", {}, "text", "Microsoft Corporation"],
                    [
                        "adr",
                        {
                            "label": (
                                "One Microsoft Way\nRedmond\nWA\n98052\nUnited States"
                            )
                        },
                        "text",
                        ["", "", "", "", "", "", ""],
                    ],
                    ["kind", {}, "text", "org"],
                ],
            ],
            "roles": ["registrant"],
            "remarks": [
                {
                    "title": "Registration Comments",
                    "description": [
                        "To report suspected security issues specific to traffic emanating from Microsoft online services, including the distribution of malicious content or other illicit or illegal material through a Microsoft online service, please submit reports to:\r",
                        "* https://cert.microsoft.com.  \r",
                        "\r",
                        "For SPAM and other abuse issues, such as Microsoft Accounts, please contact:\r",
                        "* abuse@microsoft.com.  \r",
                        "\r",
                        "To report security vulnerabilities in Microsoft products and services, please contact:\r",
                        "* secure@microsoft.com.  \r",
                        "\r",
                        "For legal and law enforcement-related requests, please contact:\r",
                        "* msndcc@microsoft.com\r",
                        "\r",
                        "For routing, peering or DNS issues, please \r",
                        "contact:\r",
                        "* IOC@microsoft.com",
                    ],
                }
            ],
            "links": [
                {
                    "value": "https://rdap.arin.net/registry/ip/13.107.4.50",
                    "rel": "self",
                    "type": "application/rdap+json",
                    "href": "https://rdap.arin.net/registry/entity/MSFT",
                },
                {
                    "value": "https://rdap.arin.net/registry/ip/13.107.4.50",
                    "rel": "alternate",
                    "type": "application/xml",
                    "href": "https://whois.arin.net/rest/org/MSFT",
                },
            ],
            "events": [
                {
                    "eventAction": "last changed",
                    "eventDate": "2022-03-28T13:51:35-04:00",
                },
                {
                    "eventAction": "registration",
                    "eventDate": "1998-07-10T00:00:00-04:00",
                },
            ],
            "entities": [
                {
                    "handle": "MRPD-ARIN",
                    "vcardArray": [
                        "vcard",
                        [
                            ["version", {}, "text", "4.0"],
                            [
                                "adr",
                                {
                                    "label": "One Microsoft Way\nRedmond\nWA\n98052\nUnited States"
                                },
                                "text",
                                ["", "", "", "", "", "", ""],
                            ],
                            ["fn", {}, "text", "Microsoft Routing, Peering, and DNS"],
                            ["org", {}, "text", "Microsoft Routing, Peering, and DNS"],
                            ["kind", {}, "text", "group"],
                            ["email", {}, "text", "IOC@microsoft.com"],
                            [
                                "tel",
                                {"type": ["work", "voice"]},
                                "text",
                                "+1-425-882-8080",
                            ],
                        ],
                    ],
                    "roles": ["technical"],
                    "remarks": [
                        {
                            "title": "Registration Comments",
                            "description": [
                                "To report suspected security issues specific to \r",
                                "traffic emanating from Microsoft online services, \r",
                                "including the distribution of malicious content \r",
                                "or other illicit or illegal material through a \r",
                                "Microsoft online service, please submit reports \r",
                                "to:\r",
                                "* https://cert.microsoft.com.  \r",
                                "\r",
                                "For SPAM and other abuse issues, such as Microsoft \r",
                                "Accounts, please contact:\r",
                                "* abuse@microsoft.com.  \r",
                                "\r",
                                "To report security vulnerabilities in Microsoft \r",
                                "products and services, please contact:\r",
                                "* secure@microsoft.com.  \r",
                                "\r",
                                "For legal and law enforcement-related requests, \r",
                                "please contact:\r",
                                "* msndcc@microsoft.com\r",
                                "\r",
                                "For routing, peering or DNS issues, please \r",
                                "contact:\r",
                                "* IOC@microsoft.com",
                            ],
                        }
                    ],
                    "links": [
                        {
                            "value": "https://rdap.arin.net/registry/ip/13.107.4.50",
                            "rel": "self",
                            "type": "application/rdap+json",
                            "href": "https://rdap.arin.net/registry/entity/MRPD-ARIN",
                        },
                        {
                            "value": "https://rdap.arin.net/registry/ip/13.107.4.50",
                            "rel": "alternate",
                            "type": "application/xml",
                            "href": "https://whois.arin.net/rest/poc/MRPD-ARIN",
                        },
                    ],
                    "events": [
                        {
                            "eventAction": "last changed",
                            "eventDate": "2021-10-16T10:56:11-04:00",
                        },
                        {
                            "eventAction": "registration",
                            "eventDate": "2013-08-20T19:26:59-04:00",
                        },
                    ],
                    "status": ["validated"],
                    "port43": "whois.arin.net",
                    "objectClassName": "entity",
                },
                {
                    "handle": "MAC74-ARIN",
                    "vcardArray": [
                        "vcard",
                        [
                            ["version", {}, "text", "4.0"],
                            [
                                "adr",
                                {
                                    "label": "One Microsoft Way\nRedmond\nWA\n98052\nUnited States"
                                },
                                "text",
                                ["", "", "", "", "", "", ""],
                            ],
                            ["fn", {}, "text", "Microsoft Abuse Contact"],
                            ["org", {}, "text", "Microsoft Abuse Contact"],
                            ["kind", {}, "text", "group"],
                            ["email", {}, "text", "abuse@microsoft.com"],
                            [
                                "tel",
                                {"type": ["work", "voice"]},
                                "text",
                                "+1-425-882-8080",
                            ],
                        ],
                    ],
                    "roles": ["abuse"],
                    "remarks": [
                        {
                            "title": "Registration Comments",
                            "description": [
                                "To report suspected security issues specific to traffic emanating from Microsoft Online Services, including the distribution of malicious content or other illicit or illegal material through a Microsoft Online Service, please submit reports to https://cert.microsoft.com.  \r",
                                "For SPAM and other abuse issues, such as Microsoft Accounts, please contact abuse@microsoft.com.  \r",
                                "To report security vulnerabilities in Microsoft products and services, please contact secure@microsoft.com.  \r",
                                "For legal and law enforcement-related requests, please contact msndcc@microsoft.com\r",
                                "For routing, peering or DNS issues, please contact IOC@microsoft.com",
                            ],
                        }
                    ],
                    "links": [
                        {
                            "value": "https://rdap.arin.net/registry/ip/13.107.4.50",
                            "rel": "self",
                            "type": "application/rdap+json",
                            "href": "https://rdap.arin.net/registry/entity/MAC74-ARIN",
                        },
                        {
                            "value": "https://rdap.arin.net/registry/ip/13.107.4.50",
                            "rel": "alternate",
                            "type": "application/xml",
                            "href": "https://whois.arin.net/rest/poc/MAC74-ARIN",
                        },
                    ],
                    "events": [
                        {
                            "eventAction": "last changed",
                            "eventDate": "2021-11-10T16:57:37-05:00",
                        },
                        {
                            "eventAction": "registration",
                            "eventDate": "2013-08-20T19:24:57-04:00",
                        },
                    ],
                    "status": ["validated"],
                    "port43": "whois.arin.net",
                    "objectClassName": "entity",
                },
                {
                    "handle": "IPHOS5-ARIN",
                    "vcardArray": [
                        "vcard",
                        [
                            ["version", {}, "text", "4.0"],
                            [
                                "adr",
                                {
                                    "label": "One Microsoft Way\nRedmond\nWA\n20147\nUnited States"
                                },
                                "text",
                                ["", "", "", "", "", "", ""],
                            ],
                            ["fn", {}, "text", "IPHostmaster IPHostmaster"],
                            [
                                "n",
                                {},
                                "text",
                                ["IPHostmaster", "IPHostmaster", "", "", ""],
                            ],
                            ["kind", {}, "text", "individual"],
                            ["email", {}, "text", "iphostmaster@microsoft.com"],
                            [
                                "tel",
                                {"type": ["work", "voice"]},
                                "text",
                                "+1-425-538-6637",
                            ],
                        ],
                    ],
                    "roles": ["administrative", "technical"],
                    "links": [
                        {
                            "value": "https://rdap.arin.net/registry/ip/13.107.4.50",
                            "rel": "self",
                            "type": "application/rdap+json",
                            "href": "https://rdap.arin.net/registry/entity/IPHOS5-ARIN",
                        },
                        {
                            "value": "https://rdap.arin.net/registry/ip/13.107.4.50",
                            "rel": "alternate",
                            "type": "application/xml",
                            "href": "https://whois.arin.net/rest/poc/IPHOS5-ARIN",
                        },
                    ],
                    "events": [
                        {
                            "eventAction": "last changed",
                            "eventDate": "2021-10-13T17:30:13-04:00",
                        },
                        {
                            "eventAction": "registration",
                            "eventDate": "2021-10-13T17:30:13-04:00",
                        },
                    ],
                    "status": ["validated"],
                    "port43": "whois.arin.net",
                    "objectClassName": "entity",
                },
            ],
            "port43": "whois.arin.net",
            "objectClassName": "entity",
        }
    ],
    "port43": "whois.arin.net",
    "status": ["active"],
    "objectClassName": "ip network",
    "cidr0_cidrs": [
        {"v4prefix": "13.64.0.0", "length": 11},
        {"v4prefix": "13.96.0.0", "length": 13},
        {"v4prefix": "13.104.0.0", "length": 14},
    ],
    "arin_originas0_originautnums": [],
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


@respx.mock
@patch("msticpy.context.ip_utils._asn_whois_query")
def test_get_whois(mock_asn_whois_query):
    """Test IP Whois."""
    mock_asn_whois_query.return_value = ASN_RESPONSE
    respx.get(re.compile(r"http://rdap\.arin\.net/.*")).respond(200, json=RDAP_RESPONSE)
    ms_ip = "13.107.4.50"
    ms_asn = "MICROSOFT-CORP"
    asn, _ = ip_whois(ms_ip)
    check.is_in(ms_asn, asn)


@respx.mock
@patch("msticpy.context.ip_utils._asn_whois_query")
@pytest.fixture(scope="module")
def test_get_whois_df(mock_asn_whois_query, net_df):
    """Test IP Whois."""
    net_df = net_df.head(25)
    mock_asn_whois_query.return_value = ASN_RESPONSE
    respx.get(re.compile(r"http://rdap\.arin\.net/.*")).respond(200, json=RDAP_RESPONSE)
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


@respx.mock
@pytest.fixture(scope="module")
@patch("msticpy.context.ip_utils._asn_whois_query")
def test_whois_pdext(net_df, mock_asn_whois_query):
    """Test IP Whois."""
    net_df = net_df.head(25)
    mock_asn_whois_query.return_value = ASN_RESPONSE
    respx.get(re.compile(r"http://rdap\.arin\.net/.*")).respond(200, json=RDAP_RESPONSE)
    results = net_df.mp_whois.lookup(ip_column="AllExtIPs")
    check.equal(len(results), len(net_df))
    check.is_in("AsnDescription", results.columns)

    results2 = net_df.mp.whois(ip_column="AllExtIPs", asn_col="asn", whois_col="whois")
    check.equal(len(results2), len(net_df))
    check.is_in("asn", results2.columns)
    check.is_in("whois", results2.columns)
    check.less_equal(len(results2[~results2["asn"].isna()]), len(net_df))
    check.equal(len(results2[~results2["whois"].isna()]), len(net_df))


@respx.mock
@patch("msticpy.context.ip_utils._asn_whois_query")
def test_asn_query_features(mock_asn_whois_query):
    """Test ASN query features"""
    # mock the potaroo request
    html_resp = get_test_data_path().joinpath("potaroo.html").read_bytes()
    respx.get("https://bgp.potaroo.net/cidr/autnums.html").respond(
        200, content=html_resp
    )
    # mock the whois response
    mock_asn_whois_query.return_value = ASN_RESPONSE_2
    # run tests
    asn_ip_details = get_asn_from_ip("65.55.44.109")
    check.is_in("AS", asn_ip_details.keys())
    check.equal(asn_ip_details["AS"], "8075")
    check.is_instance(asn_ip_details, dict)
    asn_details = get_asn_details("AS8075")
    check.is_in("ranges", asn_details.keys())
    check.is_instance(asn_details["ranges"], list)
    check.is_instance(asn_details, dict)
    asn_name_details = get_asn_from_name("Microsoft")
    check.is_in("AS3598", asn_name_details.keys())
    check.is_instance(asn_name_details, dict)
    check.equal(asn_name_details["AS3598"], "MICROSOFT-CORP-AS, US")
