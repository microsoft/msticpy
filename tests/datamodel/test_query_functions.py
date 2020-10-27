# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Test query_functions module."""
from functools import partial

import pytest
import pytest_check as check
from msticpy.data import QueryProvider
from msticpy.datamodel.query_functions import PivotQueryFunctions

__author__ = "Ian Hellen"


# pylint: disable=redefined-outer-name


@pytest.fixture(scope="module")
def azure_sentinel():
    """Fixture to get loaded Azure Sentinel Provider."""
    return QueryProvider("AzureSentinel")


def test_create_query_functions(azure_sentinel):
    """Test basic creation of query functions class."""
    az_qry_funcs = PivotQueryFunctions(azure_sentinel)

    check.greater_equal(len(az_qry_funcs.param_usage), 30)
    check.greater_equal(len(az_qry_funcs.query_reqd_params), 70)


def test_query_functions_methods(azure_sentinel):
    az_qry_funcs = PivotQueryFunctions(azure_sentinel)

    ip_addr_q_params = list(az_qry_funcs.get_queries_and_types_for_param("ip_address"))
    host_queries = list(az_qry_funcs.get_queries_for_param("host_name"))

    check.greater_equal(len(ip_addr_q_params), 4)
    check.greater_equal(len(host_queries), 20)

    func_name, func_family, func = [
        q_tup for q_tup in host_queries if q_tup[0] == "get_info_by_hostname"
    ][0]
    check.is_instance(func, partial)
    check.is_true(callable(func))

    q_params = az_qry_funcs.query_reqd_params.get(f"{func_family}.{func_name}")
    # expected results
    # all=['table', 'query_project', 'start', 'end', 'subscription_filter',
    #  'add_query_items', 'host_name', 'host_op'],
    # required=['host_name'],
    # full_required=['start', 'end', 'host_name']
    check.is_in("start", q_params.all)
    check.is_in("host_name", q_params.required)
    check.is_in("host_name", q_params.full_required)
    check.is_in("start", q_params.full_required)
    check.is_in("end", q_params.full_required)

    param_attrs = az_qry_funcs.get_param_attrs("ip_address")
    # Expected return
    # [ParamAttrs(type='str', query='get_info_by_ipaddress', family='Heartbeat',
    #  required=True),
    # ParamAttrs(type='str', query='list_logons_for_source_ip', family='LinuxSyslog',
    #  required=True),
    # ParamAttrs(type='str', query='get_host_for_ip', family='Network',
    #  required=True),
    # ParamAttrs(type='str', query='get_heartbeat_for_ip', family='Network',
    #  required=True)]
    check.is_in(param_attrs[0].type, ("str", "list", "datetime"))
    check.is_true(param_attrs[0].required)
    check.is_not_none(param_attrs[0].query)
    check.is_not_none(param_attrs[0].family)
