# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""domain_utilities extract test class."""
import ipaddress

import pandas as pd
import pytest_check as check

from msticpy.context import domain_utils


def test_validate_domain():
    test_dom_val = domain_utils.DomainValidator()
    valid_tld = test_dom_val.validate_tld("www.microsoft.com")
    resolvable = test_dom_val.is_resolvable("www.microsoft.com")
    blacklisted = test_dom_val.in_abuse_list("www.microsoft.com")
    assert valid_tld
    assert resolvable
    assert not blacklisted[0]


def test_validate_domain_fail():
    test_dom_val = domain_utils.DomainValidator()
    valid_tld = test_dom_val.validate_tld("www.contoso.garbage")
    resolvable = test_dom_val.is_resolvable("www.contoso.garbage")
    blacklisted = test_dom_val.in_abuse_list("www.contoso.garbage")
    assert not valid_tld
    assert not resolvable
    assert not blacklisted[0]
    assert blacklisted[1] is None


def test_resolver_funcs():
    """Test domain utils functions."""
    result = domain_utils.dns_resolve("www.microsoft.com")
    check.is_not_none(result["qname"])
    check.is_true(result["rrset"])
    ip = result["rrset"][0]
    result = domain_utils.dns_resolve("www.contoso.garbage")
    check.is_not_none(result)
    check.is_false(result.get("rrset"))

    result = domain_utils.ip_rev_resolve(ip)
    check.is_not_none(result)

    result_df = domain_utils.dns_resolve_df("www.microsoft.com")
    check.is_instance(result_df, pd.DataFrame)
    check.greater_equal(len(result_df), 1)
    ip = result_df.iloc[0].rrset
    ip_addr = ipaddress.ip_address(ip)
    check.is_instance(ip_addr, (ipaddress.IPv4Address, ipaddress.IPv6Address))

    result_df = domain_utils.ip_rev_resolve_df(ip)
    check.is_instance(result_df, pd.DataFrame)
    check.greater_equal(len(result_df), 1)

    result = domain_utils.dns_components("www.microsoft.com")
    check.equal(result["subdomain"], "www")
    check.equal(result["domain"], "microsoft")
    check.equal(result["suffix"], "com")
    result = domain_utils.url_components("http://www.microsoft.com")
    check.equal(result["scheme"], "http")
    check.equal(result["host"], "www.microsoft.com")
