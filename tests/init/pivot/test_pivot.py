# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Pivot main library test."""

import contextlib
import warnings
from collections import namedtuple
from datetime import datetime, timedelta
from typing import Optional

import pytest
import pytest_check as check

from msticpy.common.timespan import TimeSpan
from msticpy.data import QueryProvider
from msticpy.datamodel import entities
from msticpy.init.pivot import Pivot

# pylint: disable=unused-import, unused-argument
from .pivot_fixtures import (
    create_data_providers,
    create_pivot,
    data_providers,
    exec_connect,
)

__author__ = "Ian Hellen"

pytestmark = pytest.mark.filterwarnings("ignore::UserWarning")
# pylint: disable=redefined-outer-name, protected-access

_KQL_IMP_OK = False
with contextlib.suppress(ImportError):
    # pylint: disable=unused-import
    from msticpy.data.drivers import kql_driver

    del kql_driver
    _KQL_IMP_OK = True
_SPLUNK_IMP_OK = False
with contextlib.suppress(ImportError):
    from msticpy.data.drivers import splunk_driver

    del splunk_driver
    _SPLUNK_IMP_OK = True
_IPSTACK_IMP_OK = False
ip_stack_cls: Optional[type]
try:
    from msticpy.context.geoip import IPStackLookup as ip_stack_cls

    _IPSTACK_IMP_OK = True
except ImportError:
    ip_stack_cls = None

pytestmark = pytest.mark.skipif(not _KQL_IMP_OK, reason="Partial msticpy install")


def _test_create_pivot_namespace(data_providers):
    """Test instantiating Pivot with namespace arg."""
    with warnings.catch_warnings():
        warnings.simplefilter("ignore", category=UserWarning)
        pivot = Pivot(namespace=data_providers)
        pivot.reload_pivots(namespace=data_providers, clear_existing=True)

    check.greater(len(pivot.providers), 1)
    for provider in data_providers.values():
        if isinstance(provider, QueryProvider):
            exec_connect(provider)
    check.greater(len(pivot.providers), 1)


@pytest.fixture
def _create_pivot(data_providers):
    with warnings.catch_warnings():
        warnings.simplefilter("ignore", category=UserWarning)
        pivot = Pivot(namespace=data_providers)
        pivot.reload_pivots(namespace=data_providers, clear_existing=True)
    for provider in data_providers.values():
        if isinstance(provider, QueryProvider):
            exec_connect(provider)
    return pivot


PivotTestCase = namedtuple("PivotTestCase", "entity, container, funcs")
_ENTITY_FUNCS = [
    pytest.param(PivotTestCase("Host", "MSSentinel", 25), id="Host-MSSentinel"),
    pytest.param(PivotTestCase("Host", "MDE", 2), id="Host-MDE"),
    pytest.param(PivotTestCase("Host", "util", 3), id="Host-util"),
    pytest.param(
        PivotTestCase("IpAddress", "MSSentinel", 15), id="IpAddress-MSSentinel"
    ),
    pytest.param(PivotTestCase("IpAddress", "MDE", 2), id="IpAddress-MDE"),
    pytest.param(PivotTestCase("IpAddress", "ti", 1), id="IpAddress-ti"),
    pytest.param(PivotTestCase("IpAddress", "util", 4), id="IpAddress-util"),
    pytest.param(PivotTestCase("Account", "MSSentinel", 16), id="Account-MSSentinel"),
    pytest.param(PivotTestCase("Account", "MDE", 4), id="Account-MDE"),
    pytest.param(PivotTestCase("Url", "MSSentinel", 2), id="Url-MSSentinel"),
    pytest.param(PivotTestCase("Url", "MDE", 2), id="Url-MDE"),
    pytest.param(PivotTestCase("Url", "ti", 1), id="Url-ti"),
    pytest.param(PivotTestCase("Url", "util", 5), id="Url-util"),
]


@pytest.mark.parametrize("test_case", _ENTITY_FUNCS)
def test_pivot_providers(create_pivot, test_case):
    """
    Test pivot initialized from provider list.

    Notes
    -----
    Test that the expected number of functions have been added to entities.

    """
    entity = getattr(entities, test_case.entity)
    query_contr = getattr(entity, test_case.container)
    check.is_not_none(query_contr)
    query_attrs = repr(query_contr).split("\n")
    check.greater_equal(len(query_attrs), test_case.funcs)


class _TimeObj:
    def __init__(self, start, end):
        self.start = start
        self.end = end


def test_pivot_time(create_pivot):
    """Function_docstring."""
    end = datetime.utcnow()
    start = end - timedelta(1)
    timespan = TimeSpan(start=start, end=end)

    pivot = create_pivot
    pivot.timespan = timespan
    check.equal(pivot.start, start)
    check.equal(pivot.end, end)

    end = end - timedelta(1)
    start = start - timedelta(1)
    # Test different ways of setting the time
    timespan = TimeSpan(start=start, end=end)
    pivot.timespan = timespan
    check.equal(pivot.start, start)
    check.equal(pivot.end, end)

    pivot.timespan = _TimeObj(start=timespan.start, end=timespan.end)
    check.equal(pivot.start, start)
    check.equal(pivot.end, end)

    pivot.set_timespan(timespan)
    check.equal(pivot.start, start)
    check.equal(pivot.end, end)

    pivot.set_timespan(start=timespan.start, end=timespan.end)
    check.equal(pivot.start, start)
    check.equal(pivot.end, end)

    # Make sure the values provided to queries match.
    # _fake_provider_connected(data_providers["az_sent_prov"])

    query = entities.Host.MSSentinel.wevt_processes(host_name="test", print=True)
    check.is_in(start.isoformat(), query)
    check.is_in(end.isoformat(), query)


EntityQuery = namedtuple("EntityQuery", "entity, args, provider, pivot_func, expected")
_ENTITY_QUERIES = [
    pytest.param(
        EntityQuery(
            "Host",
            dict(HostName="testhost", DnsDomain="contoso.com"),
            "MSSentinel",
            "wevt_processes",
            'Computer has "testhost.contoso.com"',
        ),
        id="Host",
    ),
    pytest.param(
        EntityQuery(
            "Account",
            dict(Name="testacct"),
            "MSSentinel",
            "wevt_logons",
            'where Account has "testacct"',
        ),
        id="Account",
    ),
    pytest.param(
        EntityQuery(
            "IpAddress",
            dict(Address="192.168.1.2"),
            "MSSentinel",
            "heartbeat",
            '| where ComputerIP == "192.168.1.2"',
        ),
        id="IpAddress",
    ),
]


@pytest.mark.parametrize("test_case", _ENTITY_QUERIES)
def test_entity_attr_funcs(create_pivot, test_case):
    """Test calling function with entity attributes."""
    # Test entity
    ent_cls = getattr(entities, test_case.entity)
    entity = ent_cls(test_case.args)
    func = getattr(getattr(entity, test_case.provider), test_case.pivot_func)
    query = func(entity, print_query=True)
    check.is_in(test_case.expected, query)


def test_misc_functions(create_pivot):
    """Test some additional methods of pivot.py."""
    check.greater(len(create_pivot.providers), 2)
    t_span = TimeSpan(end=datetime.utcnow(), period="1D")
    create_pivot.edit_query_time(timespan=t_span)
    check.equal(create_pivot.start, t_span.start)
    check.equal(create_pivot.end, t_span.end)
    check.equal(create_pivot.timespan, t_span)


_ENTITY_PIVOTS = [
    pytest.param(entities.Host, 25, id="Host"),
    pytest.param(entities.IpAddress, 25, id="IpAddress"),
    pytest.param(entities.Account, 20, id="Account"),
]


@pytest.mark.parametrize("entity, expected_funcs", _ENTITY_PIVOTS)
def test_entity_list_piv_functions(create_pivot, entity, expected_funcs):
    """Test the pivot_funcs property."""
    check.greater(len(entity.get_pivot_list()), expected_funcs)


def _get_piv_attrs(entity):
    return [
        attr
        for attr in dir(entity)
        if hasattr(getattr(entity, attr), "pivot_properties")
        or type(getattr(entity, attr)).__name__ == "PivotContainer"
    ]


def test_remove_pivots(create_pivot):
    """Test remove pivots function."""
    piv_attrs = _get_piv_attrs(entities.Host)
    check.is_true(piv_attrs)

    with pytest.raises(ValueError):
        Pivot.remove_pivot_funcs(entity="TestEntity")

    piv_attrs = _get_piv_attrs(entities.Host)
    check.is_true(piv_attrs)

    Pivot.remove_pivot_funcs(entity="Host")
    piv_attrs = _get_piv_attrs(entities.Host)
    check.is_false(piv_attrs)

    piv_attrs = _get_piv_attrs(entities.IpAddress)
    check.is_true(piv_attrs)
    Pivot.remove_pivot_funcs(entity="all")
    piv_attrs = _get_piv_attrs(entities.IpAddress)
    check.is_false(piv_attrs)
