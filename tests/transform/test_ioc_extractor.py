# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""IoC extract tests."""

import pandas as pd
import pytest
import pytest_check as check

from msticpy.transform.iocextract import IoCExtract

from ..unit_test_lib import TEST_DATA_PATH

__author__ = "Ian Hellen"

# pylint: disable=redefined-outer-name


@pytest.fixture(scope="module")
def ioc_extract():
    """Return IoCExtract instance."""
    return IoCExtract()


@pytest.fixture(scope="module")
def ioc_extract_no_df():
    """Return IoCExtract instance."""
    return IoCExtract(defanged=False)


TEST_CASES = {
    "ipv4_test": r"c:\one\path\or\another\myprocess -ip4:206.123.1.123",
    "ipv6_test": r"""c:\one\path\or\another\myprocess -ip6:(2001:0db8:85a3:0000:0000:8a2e:0370:7334,
    2001:db8:85a3:0:0:8a2e:370:7334,2001:db8:85a3::8a2e:370:7334,::ffff:192.0.2.128)""",
    "url_test": r"""c:\one\path\or\another\myprocess /url:https://some.domain.it/thepath?qry1=abc&qry2=xyz
        /url:https://myuser@some.domain.es:88/thepath?qry1=abc&qry2=xyz"<some other trailing stuff""",
    "windows_path_test": r'c:\one\path\or\another\myprocess -file:"..\another\file" -file:"\\uncpath\file"',
    "linux_path_test": r"/bin/bash --file:./bish --file:/bin/bash --file:../../bosh",
    "md5_hash_test": "00236a2ae558018ed13b5222ef1bd987hash -something-hash=00236a2ae558018ed13b5222ef1bd988hash -something -hash=00236a2ae558018ed13b5222ef1bd989",
    "sha1_hash_test": "00236a2ae558018ed13b5222ef1bd98700000001hash -something -hash=00236a2ae558018ed13b5222ef1bd98700000002hash -something -hash=00236a2ae558018ed13b5222ef1bd98700000003",
    "sha256_hash_test": """00236a2ae558018ed13b5222ef1bd98700000001424246789042424678901234hash -something -hash=00236a2ae558018ed13b5222ef1bd98700000001424246789042424678901235hash -something
-hash=00236a2ae558018ed13b5222ef1bd98700000001424246789042424678901236""",
    "url2_test": "curl 'https://www.virustotal.com/en/ip-address/90.156.201.27/information/'",
    "domain1_test": "some text with a domain.like.uk in it",
    "domain_neg_test": "some text with a bad domain.like.iandom in it",
    "domain_short_test": "some text with a microsoft.com in it",
    "domain_ycombinator_test": "some text with a news.ycombinator.com in it",
    "email1_test": "some text with user@microsoft.com in it",
    "email2_test": "some text with User <user@microsoft.com> in it",
    "email3_test": "some text with user@microsoft.com (User XYZ)> in it",
    "email4_test": "some text with user@abc.def.net> in it",
}


def _run_extract(ioc_extract, testcase, expected_items):
    """Run individual tests."""
    test_input = TEST_CASES[testcase + "_test"]
    results = ioc_extract.extract(test_input, include_paths=True)
    for k, v in expected_items.items():
        check.equal(len(results[k]), v, "Unexpected value for " + k)


def test_ipv4(ioc_extract):
    _run_extract(ioc_extract, "ipv4", {"ipv4": 1})


def test_ipv6(ioc_extract):
    _run_extract(ioc_extract, "ipv6", {"ipv6": 3})


def test_url(ioc_extract):
    _run_extract(ioc_extract, "url", {"url": 2, "dns": 2, "ipv4": 0})
    _run_extract(ioc_extract, "url2", {"url": 1, "dns": 1, "ipv4": 1})


def test_windows_path(ioc_extract):
    _run_extract(ioc_extract, "windows_path", {"windows_path": 3})


def test_linux_path(ioc_extract):
    _run_extract(ioc_extract, "linux_path", {"linux_path": 3})


def test_hashes(ioc_extract):
    _run_extract(ioc_extract, "md5_hash", {"md5_hash": 3})
    _run_extract(ioc_extract, "sha1_hash", {"sha1_hash": 3})
    _run_extract(ioc_extract, "sha256_hash", {"sha256_hash": 3})


def test_dns(ioc_extract):
    _run_extract(ioc_extract, "domain1", {"dns": 1})
    _run_extract(ioc_extract, "domain_neg", {"dns": 0})
    _run_extract(ioc_extract, "domain_short", {"dns": 1})
    _run_extract(ioc_extract, "domain_ycombinator", {"dns": 1})


def test_email(ioc_extract):
    _run_extract(ioc_extract, "email1", {"email": 1})
    _run_extract(ioc_extract, "email2", {"email": 1})
    _run_extract(ioc_extract, "email3", {"email": 1})
    _run_extract(ioc_extract, "email4", {"email": 1})


def test_dataframe(ioc_extract):
    input_df = pd.DataFrame.from_dict(
        data=TEST_CASES, orient="index", columns=["input"]
    )
    output_df = ioc_extract.extract(
        data=input_df, columns=["input"], include_paths=True
    )

    check.greater(output_df.shape[0], 0)
    check.equal(output_df[output_df["IoCType"] == "ipv4"].shape[0], 3)
    check.equal(output_df[output_df["IoCType"] == "ipv6"].shape[0], 3)
    check.equal(output_df[output_df["IoCType"] == "url"].shape[0], 3)
    check.equal(output_df[output_df["IoCType"] == "windows_path"].shape[0], 6)
    check.equal(output_df[output_df["IoCType"] == "linux_path"].shape[0], 0)
    check.equal(output_df[output_df["IoCType"] == "md5_hash"].shape[0], 3)
    check.equal(output_df[output_df["IoCType"] == "sha1_hash"].shape[0], 3)
    check.equal(output_df[output_df["IoCType"] == "sha256_hash"].shape[0], 3)

    input_df = pd.DataFrame.from_dict(
        data=TEST_CASES, orient="index", columns=["input"]
    )
    ioc_types = [
        "ipv4",
        "ipv6",
        "url",
        "linux_path",
        "md5_hash",
        "sha1_hash",
        "sha256_hash",
    ]
    output_df = ioc_extract.extract(
        data=input_df, columns=["input"], include_paths=True, ioc_types=ioc_types
    )
    # for _, row in output_df[output_df['IoCType'] == 'url'].iterrows():
    #     print(row.Observable)
    check.greater(output_df.shape[0], 0)
    check.equal(output_df[output_df["IoCType"] == "ipv4"].shape[0], 3)
    check.equal(output_df[output_df["IoCType"] == "ipv6"].shape[0], 3)
    check.equal(output_df[output_df["IoCType"] == "url"].shape[0], 3)
    check.equal(output_df[output_df["IoCType"] == "windows_path"].shape[0], 0)
    check.equal(output_df[output_df["IoCType"] == "linux_path"].shape[0], 8)
    check.equal(output_df[output_df["IoCType"] == "md5_hash"].shape[0], 3)
    check.equal(output_df[output_df["IoCType"] == "sha1_hash"].shape[0], 3)
    check.equal(output_df[output_df["IoCType"] == "sha256_hash"].shape[0], 3)


def test_dataframe_ioc_types(ioc_extract):
    input_df = pd.DataFrame.from_dict(
        data=TEST_CASES, orient="index", columns=["input"]
    )
    output_df = ioc_extract.extract(
        data=input_df, columns=["input"], ioc_types=["ipv4", "url", "md5_hash"]
    )

    check.greater(output_df.shape[0], 0)
    check.equal(output_df[output_df["IoCType"] == "ipv4"].shape[0], 3)
    check.equal(output_df[output_df["IoCType"] == "ipv6"].shape[0], 0)
    check.equal(output_df[output_df["IoCType"] == "url"].shape[0], 3)
    check.equal(output_df[output_df["IoCType"] == "windows_path"].shape[0], 0)
    check.equal(output_df[output_df["IoCType"] == "linux_path"].shape[0], 0)
    check.equal(output_df[output_df["IoCType"] == "md5_hash"].shape[0], 3)
    check.equal(output_df[output_df["IoCType"] == "sha1_hash"].shape[0], 0)
    check.equal(output_df[output_df["IoCType"] == "sha256_hash"].shape[0], 0)


def test_dataframe_new(ioc_extract):
    input_df = pd.DataFrame.from_dict(
        data=TEST_CASES, orient="index", columns=["input"]
    )
    output_df = ioc_extract.extract_df(
        data=input_df, columns=["input"], include_paths=True
    )

    check.greater(output_df.shape[0], 0)
    check.equal(output_df[output_df["IoCType"] == "ipv4"].shape[0], 3)
    check.equal(output_df[output_df["IoCType"] == "ipv6"].shape[0], 3)
    check.equal(output_df[output_df["IoCType"] == "url"].shape[0], 3)
    check.equal(output_df[output_df["IoCType"] == "windows_path"].shape[0], 6)
    check.equal(output_df[output_df["IoCType"] == "linux_path"].shape[0], 0)
    check.equal(output_df[output_df["IoCType"] == "md5_hash"].shape[0], 3)
    check.equal(output_df[output_df["IoCType"] == "sha1_hash"].shape[0], 3)
    check.equal(output_df[output_df["IoCType"] == "sha256_hash"].shape[0], 3)

    input_df = pd.DataFrame.from_dict(
        data=TEST_CASES, orient="index", columns=["input"]
    )
    ioc_types = [
        "ipv4",
        "ipv6",
        "url",
        "linux_path",
        "md5_hash",
        "sha1_hash",
        "sha256_hash",
    ]
    output_df = ioc_extract.extract_df(
        data=input_df, columns=["input"], include_paths=True, ioc_types=ioc_types
    )
    # for _, row in output_df[output_df['IoCType'] == 'url'].iterrows():
    #     print(row.Observable)
    check.greater(output_df.shape[0], 0)
    check.equal(output_df[output_df["IoCType"] == "ipv4"].shape[0], 3)
    check.equal(output_df[output_df["IoCType"] == "ipv6"].shape[0], 3)
    check.equal(output_df[output_df["IoCType"] == "url"].shape[0], 3)
    check.equal(output_df[output_df["IoCType"] == "windows_path"].shape[0], 0)
    check.equal(output_df[output_df["IoCType"] == "linux_path"].shape[0], 8)
    check.equal(output_df[output_df["IoCType"] == "md5_hash"].shape[0], 3)
    check.equal(output_df[output_df["IoCType"] == "sha1_hash"].shape[0], 3)
    check.equal(output_df[output_df["IoCType"] == "sha256_hash"].shape[0], 3)


def test_dataframe_ioc_types_new(ioc_extract):
    input_df = pd.DataFrame.from_dict(
        data=TEST_CASES, orient="index", columns=["input"]
    )
    output_df = ioc_extract.extract_df(
        data=input_df, columns=["input"], ioc_types=["ipv4", "url", "md5_hash"]
    )

    check.greater(output_df.shape[0], 0)
    check.equal(output_df[output_df["IoCType"] == "ipv4"].shape[0], 3)
    check.equal(output_df[output_df["IoCType"] == "ipv6"].shape[0], 0)
    check.equal(output_df[output_df["IoCType"] == "url"].shape[0], 3)
    check.equal(output_df[output_df["IoCType"] == "windows_path"].shape[0], 0)
    check.equal(output_df[output_df["IoCType"] == "linux_path"].shape[0], 0)
    check.equal(output_df[output_df["IoCType"] == "md5_hash"].shape[0], 3)
    check.equal(output_df[output_df["IoCType"] == "sha1_hash"].shape[0], 0)
    check.equal(output_df[output_df["IoCType"] == "sha256_hash"].shape[0], 0)


TEST_DF_CASES = {
    "ipv4": [
        (r"c:\one\path\or\another\myprocess -ip4:206[.]123.1.123", True),
        (r"c:\one\path\or\another\myprocess -ip4:206[.]123[.]1[.]123", True),
        (r"c:\one\path\or\another\myprocess -ip4:206.123[.]1[.]123", True),
        (r"c:\one\path\or\another\myprocess -ip4:206.123[.]1[.]com", False),
    ],
    "url": [
        (
            r"curl 'hXXps://www.virustotal.com/en/ip-address/90.156.201.27/information/'",
            True,
        ),
        (
            r"curl 'ftp://www[.]virustotal.com/en/ip-address/90.156.201.27/information/'",
            True,
        ),
        (
            r"curl 'ftp://www[.]virustotal[.]com/en/ip-address/90.156.201.27/information/'",
            True,
        ),
        (
            r"curl 'sftp://www[.]virustotal.com/en/ip-address/90.156.201.27/information/'",
            True,
        ),
        (
            r"curl 'ftps://www[.]virustotal.com/en/ip-address/90.156.201.27/information/'",
            True,
        ),
        (
            r"curl 'fXp://www[.]virustotal.com/en/ip-address/90.156.201.27/information/'",
            True,
        ),
        (
            r"curl 'sfXp://www.virustotal.com/en/ip-address/90.156.201.27/information/'",
            True,
        ),
        (
            r"curl 'fXps://www[.]virustotal.com/en/ip-address/90.156.201.27/information/'",
            True,
        ),
        (
            r"curl 'file://www[.]virustotal.com/en/ip-address/90.156.201.27/information/'",
            True,
        ),
        (
            r"curl 'telnet://www[.]virustotal.com/en/ip-address/90.156.201.27/information/'",
            True,
        ),
        (
            r"curl 'telnet://www[.]virustotal.com/en/ip-address/90.156.201.27/information/'",
            True,
        ),
    ],
    "dns": [
        ("some text with a domain[.]like.uk in it", True),
        ("some text with a domain.like[.]uk in it", True),
        ("some text with a domain[.]like[.]uk in it", True),
    ],
    "email": [
        ("some text with a user@domain[.]like.uk in it", True),
        ("some text with a user@domain[.]like.uk in it", True),
        ("some text with a user[AT]domain[.]like.uk in it", True),
        ("some text with a user@domain.like.uk in it", True),
    ],
}


def _test_ids():
    return [
        f"{grp}_{idx}"
        for grp, cases in TEST_DF_CASES.items()
        for idx, _ in enumerate(cases)
    ]


def _tests():
    return [
        (ioc_type, case[0], case[1])
        for ioc_type, cases in TEST_DF_CASES.items()
        for case in cases
    ]


@pytest.mark.parametrize("ioc, test, expected", _tests(), ids=_test_ids())
def test_defanged_iocs(ioc, test, expected, ioc_extract):
    """Test defanged IoC Types."""
    results = ioc_extract.extract(
        test,
    )
    if expected:
        check.greater(len(results[ioc]), 0)
    else:
        check.equal(len(results[ioc]), 0)


_DF_IP_CASES = TEST_DF_CASES["ipv4"]
_IDS = [f"ipv4_{idx}" for idx in range(len(_DF_IP_CASES))]


@pytest.mark.parametrize("test, expected", _DF_IP_CASES, ids=_IDS)
def test_defanged_iocs_no_defang(test, expected, ioc_extract_no_df):
    """Test defanged IoC Types without de-fanging."""
    del expected
    results = ioc_extract_no_df.extract(test)
    # all IP cases should fail
    check.equal(len(results["ipv4"]), 0)
