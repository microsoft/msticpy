# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Test Pivot registered functions."""
import re
import warnings
from collections import namedtuple
from contextlib import redirect_stdout
from io import StringIO
from unittest.mock import patch

import pandas as pd
import pytest
import pytest_check as check
import respx

from msticpy.context.geoip import GeoLiteLookup
from msticpy.context.tilookup import TILookup
from msticpy.datamodel import entities

from ...context.test_ip_utils import ASN_RESPONSE, RDAP_RESPONSE

# pylint: disable=redefined-outer-name, unused-import, unused-argument
from .pivot_fixtures import create_pivot

__author__ = "Ian Hellen"

# pylint: disable=redefined-outer-name


@pytest.fixture(scope="session")
def data_providers():
    """Return dict of providers."""
    with warnings.catch_warnings():
        warnings.simplefilter("ignore", category=UserWarning)
        return {
            "ti_lookup": TILookup(),
            "geolite": GeoLiteLookup(),
            #  "ip_stack": IPStackLookup(),
        }


EntityQuery = namedtuple(
    "EntityQuery",
    "entity, args, attrib, provider, pivot_func, func_param, src_col, exp_col, exp_val",
)

_ENTITY_QUERIES = [
    pytest.param(
        EntityQuery(
            entity="IpAddress",
            args=dict(Address="13.107.4.50"),
            attrib="Address",
            provider="util",
            pivot_func="whois",
            func_param="ip_address",
            src_col="ip",
            exp_col="asn_description",
            exp_val="MICROSOFT-CORP-MSN-AS-BLOCK",
        ),
        id="IpAddress-who_is",
    ),
    pytest.param(
        EntityQuery(
            entity="IpAddress",
            args=dict(Address="13.107.4.50"),
            attrib="Address",
            provider="util",
            pivot_func="whois_asn",
            func_param="ip_address",
            src_col="ip",
            exp_col="AS Name",
            exp_val="MICROSOFT-CORP-MSN-AS-BLOCK",
        ),
        id="IpAddress-who_is_asn",
    ),
    pytest.param(
        EntityQuery(
            entity="IpAddress",
            args=dict(Address="13.107.4.50"),
            attrib="Address",
            provider="util",
            pivot_func="ip_type",
            func_param="ip_str",
            src_col="ip",
            exp_col="result",
            exp_val="Public",
        ),
        id="IpAddress-ip_type",
    ),
    pytest.param(
        EntityQuery(
            entity="Process",
            args=dict(
                CommandLine="VGhpcyBpcyBhIHRlc3Qgb2YgYmFzZTY0IGVuY29kZWQgc3RyaW5n"
            ),
            attrib="CommandLine",
            provider="util",
            pivot_func="b64decode",
            func_param="value",
            src_col="cmdline",
            exp_col="decoded_string",
            exp_val="This is a test of base64 encoded string",
        ),
        id="Process-b64decode",
    ),
    pytest.param(
        EntityQuery(
            entity="Url",
            args=dict(Url="https://www.contoso.com/path?p1=test"),
            attrib="Url",
            provider="util",
            pivot_func="extract_iocs",
            func_param="value",
            src_col="url",
            exp_col="Observable",
            exp_val="www.contoso.com",
        ),
        id="Url-extract_iocs",
    ),
    pytest.param(
        EntityQuery(
            entity="Url",
            args=dict(Url="https://www.contoso.com/path?p1=test"),
            attrib="host",
            provider="util",
            pivot_func="dns_validate_tld",
            func_param="value",
            src_col="host",
            exp_col="result",
            exp_val="True",
        ),
        id="Url-dns_validate_tld",
    ),
    pytest.param(
        EntityQuery(
            entity="Url",
            args=dict(Url="https://www.contoso.com/path?p1=test"),
            attrib="host",
            provider="util",
            pivot_func="dns_is_resolvable",
            func_param="value",
            src_col="host",
            exp_col="result",
            exp_val="True",
        ),
        id="Url-dns_is_resolvable",
    ),
    pytest.param(
        EntityQuery(
            entity="Url",
            args=dict(Url="https://www.contoso.com/path?p1=test"),
            attrib="host",
            provider="util",
            pivot_func="dns_in_abuse_list",
            func_param="value",
            src_col="host",
            exp_col="result",
            exp_val="False",
        ),
        id="Url-dns_in_abuse_list",
    ),
    pytest.param(
        EntityQuery(
            entity="Url",
            args=dict(Url="https://www.contoso.com/path?p1=test"),
            attrib="host",
            provider="util",
            pivot_func="dns_components",
            func_param="value",
            src_col="host",
            exp_col="subdomain",
            exp_val="www",
        ),
        id="Url-dns_components",
    ),
    pytest.param(
        EntityQuery(
            entity="Url",
            args=dict(Url="https://www.contoso.com/path?p1=test"),
            attrib="host",
            provider="util",
            pivot_func="url_components",
            func_param="value",
            src_col="host",
            exp_col="host",
            exp_val="www.contoso.com",
        ),
        id="Url-url_components",
    ),
    pytest.param(
        EntityQuery(
            entity="Url",
            args=dict(Url="https://www.contoso.com/path?p1=test"),
            attrib="Url",
            provider="util",
            pivot_func="dns_resolve",
            func_param="value",
            src_col="host",
            exp_col="qname",
            exp_val="www.contoso.com",
        ),
        id="Url-dns_resolve",
    ),
    pytest.param(
        EntityQuery(
            entity="IpAddress",
            args=dict(Address="104.211.30.1"),
            attrib="Address",
            provider="util",
            pivot_func="ip_rev_resolve",
            func_param="value",
            src_col="host",
            exp_col="qname",
            exp_val="104.211.30.1",
        ),
        id="IpAddress-ip_rev_resolve",
    ),
    pytest.param(
        EntityQuery(
            entity="IpAddress",
            args=dict(Address="104.211.30.1"),
            attrib="Address",
            provider="util",
            pivot_func="geoloc",
            func_param="value",
            src_col="ip",
            exp_col="CountryName",
            exp_val="United States",
        ),
        id="IpAddress-geoip_maxmind",
    ),
    # This test sometimes files because of non-responsiveness
    # from ipstack service
    # pytest.param(
    #     EntityQuery(
    #         entity="IpAddress",
    #         args=dict(Address="104.211.30.1"),
    #         attrib="Address",
    #         provider="util",
    #         pivot_func="geoloc_ips",
    #         func_param="value",
    #         src_col="ip",
    #         exp_col="CountryName",
    #         exp_val="United States",
    #     ),
    #     id="IpAddress-geoip_ipstack",
    # ),
]


@respx.mock
@pytest.mark.parametrize("test_case", _ENTITY_QUERIES)
@patch("msticpy.context.ip_utils._asn_whois_query")
def test_entity_attr_funcs_entity(mock_asn_whois_query, create_pivot, test_case):
    """Test calling function with entity attributes."""
    mock_asn_whois_query.return_value = ASN_RESPONSE
    respx.get(re.compile(r"http://rdap\.arin\.net/.*")).respond(200, json=RDAP_RESPONSE)
    # Test entity
    ent_cls = getattr(entities, test_case.entity)
    entity = ent_cls(**(test_case.args))
    func = getattr(getattr(entity, test_case.provider), test_case.pivot_func)
    # Test entity input
    result_df = func(entity)
    print(result_df)
    print(result_df.iloc[0], test_case.exp_col)
    check.is_in(test_case.exp_val, result_df.iloc[0][test_case.exp_col])


@respx.mock
@pytest.mark.parametrize("test_case", _ENTITY_QUERIES)
@patch("msticpy.context.ip_utils._asn_whois_query")
def test_entity_attr_funcs_value(mock_asn_whois_query, create_pivot, test_case):
    """Test calling function with value."""
    mock_asn_whois_query.return_value = ASN_RESPONSE
    respx.get(re.compile(r"http://rdap\.arin\.net/.*")).respond(200, json=RDAP_RESPONSE)
    ent_cls = getattr(entities, test_case.entity)
    entity = ent_cls(**(test_case.args))
    func = getattr(getattr(entity, test_case.provider), test_case.pivot_func)
    # Test value input
    val = getattr(entity, test_case.attrib)
    params = {test_case.func_param: val}
    result_df = func(**params)
    check.is_in(test_case.exp_val, result_df.iloc[0][test_case.exp_col])


@respx.mock
@pytest.mark.parametrize("test_case", _ENTITY_QUERIES)
@patch("msticpy.context.ip_utils._asn_whois_query")
def test_entity_attr_funcs_itbl(mock_asn_whois_query, create_pivot, test_case):
    """Test calling function with iterable input."""
    mock_asn_whois_query.return_value = ASN_RESPONSE
    respx.get(re.compile(r"http://rdap\.arin\.net/.*")).respond(200, json=RDAP_RESPONSE)
    ent_cls = getattr(entities, test_case.entity)
    entity = ent_cls(**(test_case.args))
    func = getattr(getattr(entity, test_case.provider), test_case.pivot_func)
    # Test iterable input
    val = [getattr(entity, test_case.attrib)]
    params = {test_case.func_param: val}
    result_df = func(**params)
    check.is_in(test_case.exp_val, result_df.iloc[0][test_case.exp_col])


@respx.mock
@pytest.mark.parametrize("test_case", _ENTITY_QUERIES)
@patch("msticpy.context.ip_utils._asn_whois_query")
def test_entity_attr_funcs_df(mock_asn_whois_query, create_pivot, test_case):
    """Test calling function with DF input attributes."""
    mock_asn_whois_query.return_value = ASN_RESPONSE
    respx.get(re.compile(r"http://rdap\.arin\.net/.*")).respond(200, json=RDAP_RESPONSE)
    ent_cls = getattr(entities, test_case.entity)
    entity = ent_cls(**(test_case.args))
    func = getattr(getattr(entity, test_case.provider), test_case.pivot_func)
    # Test DF input
    val = getattr(entity, test_case.attrib)
    in_df = pd.DataFrame([val], columns=[test_case.src_col])
    result_df = func(data=in_df, src_column=test_case.src_col)
    check.is_in(test_case.exp_val, result_df.iloc[0][test_case.exp_col])


def test_pivot_container(create_pivot):
    """Test operations of PivotContainer class."""
    pivot_funcs = entities.Host.pivots()
    check.greater_equal(len(pivot_funcs), 7)

    with redirect_stdout(StringIO()) as print_capture:
        entities.Host.util()

    pr_content = print_capture.getvalue()
    check.is_in("(pivot function)", pr_content)
