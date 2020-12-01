# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Pivot main library test."""
from collections import namedtuple
from datetime import datetime, timedelta

import pytest
import pytest_check as check
from msticpy.common.timespan import TimeSpan
from msticpy.data import QueryProvider
from msticpy.data.query_container import QueryContainer
from msticpy.datamodel import entities
from msticpy.datamodel.pivot import Pivot
from msticpy.sectools import GeoLiteLookup, IPStackLookup, TILookup

__author__ = "Ian Hellen"

# pylint: disable=redefined-outer-name


@pytest.fixture(scope="session")
def data_providers():
    """Return dict of providers."""
    return {
        "az_sent_prov": QueryProvider("AzureSentinel"),
        "mdatp_prov": QueryProvider("MDATP"),
        "splunk_prov": QueryProvider("Splunk"),
        "ti_lookup": TILookup(),
        "geolite": GeoLiteLookup(),
        "ip_stack": IPStackLookup(),
    }


def _reset_entities():
    """Clear any query containers in entities."""
    for entity_name in ("Host", "IpAddress", "Account", "Url"):
        entity = getattr(entities, entity_name)
        for attr in dir(entity):
            if isinstance(getattr(entity, attr), QueryContainer):
                delattr(entity, attr)


PivotTestCase = namedtuple("PivotTestCase", "entity, container, funcs")
_ENTITY_FUNCS = [
    pytest.param(PivotTestCase("Host", "AzureSentinel", 25), id="Host-AzureSentinel"),
    pytest.param(PivotTestCase("Host", "MDATP", 2), id="Host-MDATP"),
    pytest.param(PivotTestCase("Host", "other", 3), id="Host-other"),
    pytest.param(
        PivotTestCase("IpAddress", "AzureSentinel", 16), id="IpAddress-AzureSentinel"
    ),
    pytest.param(PivotTestCase("IpAddress", "MDATP", 2), id="IpAddress-MDATP"),
    pytest.param(PivotTestCase("IpAddress", "ti", 8), id="IpAddress-ti"),
    pytest.param(PivotTestCase("IpAddress", "other", 4), id="IpAddress-other"),
    pytest.param(
        PivotTestCase("Account", "AzureSentinel", 19), id="Account-AzureSentinel"
    ),
    pytest.param(PivotTestCase("Account", "MDATP", 4), id="Account-MDATP"),
    pytest.param(PivotTestCase("Url", "AzureSentinel", 7), id="Url-AzureSentinel"),
    pytest.param(PivotTestCase("Url", "MDATP", 2), id="Url-MDATP"),
    pytest.param(PivotTestCase("Url", "ti", 4), id="Url-ti"),
    pytest.param(PivotTestCase("Url", "other", 5), id="Url-other"),
]


@pytest.fixture(scope="session")
def _create_pivot_list(data_providers):
    _reset_entities()
    providers = data_providers.values()
    return Pivot(providers=providers)


@pytest.mark.parametrize("test_case", _ENTITY_FUNCS)
def test_pivot_providers(_create_pivot_list, test_case):
    """
    Test pivot intialized from provider list.

    Notes
    -----
    Test that the expected number of functions have been added to entities.

    """
    entity = getattr(entities, test_case.entity)
    query_contr = getattr(entity, test_case.container)
    check.is_not_none(query_contr)
    query_attrs = repr(query_contr).split("\n")
    check.greater_equal(len(query_attrs), test_case.funcs)


# # Generate test cases for pivot functions
# def test_gen_tests(_create_pivot):
#     """Function_docstring."""
#     for entity_name in ("Host", "IpAddress", "Account", "Url"):
#         entity = getattr(entities, entity_name)
#         for container in ("AzureSentinel", "Splunk", "MDATP", "ti", "other"):
#             query_contr = getattr(entity, container, None)
#             if not query_contr:
#                 continue
#             query_attrs = repr(query_contr).split("\n")
#             piv_case = f'PivotTestCase("{entity_name}", "{container}", {len(query_attrs)})'
#             print(f'    pytest.param({piv_case}, id=f"{entity_name}-{container}"),')
#     assert False


@pytest.fixture(scope="session")
def _create_pivot_ns(data_providers):
    _reset_entities()
    locals().update(data_providers)
    return Pivot(namespace=locals())


@pytest.mark.parametrize("test_case", _ENTITY_FUNCS)
def test_pivot_providers_namespace(_create_pivot_ns, test_case):
    """
    Test pivot initialized from globals/namespace.

    Notes
    -----
    Test that the expected number of functions have been added to entities.

    """
    entity = getattr(entities, test_case.entity)
    query_contr = getattr(entity, test_case.container)
    check.is_not_none(query_contr)
    query_attrs = repr(query_contr).split("\n")
    check.greater_equal(len(query_attrs), test_case.funcs)


def _fake_provider_connected(provider):
    # Lie to the query provider so that it will allow the call
    # pylint: disable=protected-access
    provider._query_provider._loaded = True
    provider._query_provider._connected = True
    # pylint: enable=protected-access


def test_pivot_time(data_providers):
    """Function_docstring."""
    providers = data_providers.values()
    end = datetime.utcnow()
    start = end - timedelta(1)
    timespan = TimeSpan(start=start, end=end)
    pivot = Pivot(providers=providers, timespan=timespan)
    check.equal(pivot.start, start)
    check.equal(pivot.end, end)

    end = end - timedelta(1)
    start = start - timedelta(1)
    timespan = TimeSpan(start=start, end=end)
    pivot.timespan = timespan
    check.equal(pivot.start, start)
    check.equal(pivot.end, end)

    _fake_provider_connected(data_providers["az_sent_prov"])

    query = entities.Host.AzureSentinel.list_host_processes(
        host_name="test", print_query=True
    )
    check.is_in(start.isoformat(), query)
    check.is_in(end.isoformat(), query)


EntityQuery = namedtuple("EntityQuery", "entity, args, provider, pivot_func, expected")
_ENTITY_QUERIES = [
    pytest.param(
        EntityQuery(
            "Host",
            dict(HostName="testhost", DnsDomain="contoso.com"),
            "AzureSentinel",
            "list_host_processes",
            'Computer has "testhost.contoso.com"',
        ),
        id="Host",
    ),
    pytest.param(
        EntityQuery(
            "Account",
            dict(Name="testacct"),
            "AzureSentinel",
            "list_logons_by_account",
            'where Account has "testacct"',
        ),
        id="Account",
    ),
    pytest.param(
        EntityQuery(
            "IpAddress",
            dict(Address="192.168.1.2"),
            "AzureSentinel",
            "get_info_by_ipaddress",
            '| where ComputerIP == "192.168.1.2"',
        ),
        id="IpAddress",
    ),
]


@pytest.mark.parametrize("test_case", _ENTITY_QUERIES)
def test_entity_attr_funcs(_create_pivot_ns, test_case):
    """Test calling function with entity attributes."""
    # Test entity
    ent_cls = getattr(entities, test_case.entity)
    entity = ent_cls(test_case.args)
    _fake_provider_connected(_create_pivot_ns.get_provider("AzureSentinel"))
    func = getattr(getattr(entity, test_case.provider), test_case.pivot_func)
    query = func(entity, print_query=True)
    check.is_in(test_case.expected, query)
