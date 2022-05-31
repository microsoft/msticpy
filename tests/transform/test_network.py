# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""transform.network module docstring."""
from collections import namedtuple

import pandas as pd
import pytest
import pytest_check as check

from msticpy.transform.network import df_to_networkx

from ..unit_test_lib import get_test_data_path

__author__ = "Ian Hellen"

# pylint: disable=redefined-outer-name


@pytest.fixture(scope="module")
def network_data():
    """Read DF of network data."""
    return pd.read_csv(get_test_data_path().joinpath("az_net_flows.csv"), index_col=0)


NetExp = namedtuple("NetExp", "nodes, edges, error, err_mssg")


_TEST_NETWORK = [
    pytest.param("SrcIP", "AllExtIPs", None, None, None, NetExp(3, 4, None, None)),
    pytest.param(
        "SrcIP",
        "AllExtIPs",
        ["TenantId", "VMName"],
        None,
        ["FlowType", "TotalAllowedFlows"],
        NetExp(3, 4, None, None),
        id="3 nodes - Src and Edge attrs",
    ),
    pytest.param(
        "SrcIP",
        "AllExtIPs",
        ["TenantId", "VMName"],
        ["DestPort", "ResourceGroup"],
        ["FlowType", "TotalAllowedFlows"],
        NetExp(3, 4, None, None),
        id="3 nodes - Src, Tgt and Edge attrs",
    ),
    pytest.param(
        "SrcIP",
        "AllExtIPs",
        None,
        ["DestPort", "ResourceGroup", "VMName"],
        None,
        NetExp(3, 4, None, None),
        id="3 nodes - Tgt attrs",
    ),
    pytest.param(
        "ResourceGroup",
        "AllExtIPs",
        None,
        ["DestPort", "ResourceGroup", "VMName"],
        None,
        NetExp(85, 84, None, None),
        id="85 nodes - Tgt attrs",
    ),
    pytest.param(
        "BadCol",
        "AllExtIPs",
        ["TenantId", "VMName"],
        None,
        ["FlowType", "TotalAllowedFlows"],
        NetExp(0, 0, ValueError, "source_col"),
        id="Error - bad src_column",
    ),
    pytest.param(
        "SrcIP",
        "AllExtIPs",
        ["BadCol", "VMName"],
        None,
        ["FlowType", "TotalAllowedFlows"],
        NetExp(0, 0, ValueError, "source_attrs"),
        id="Error - bad src_attr",
    ),
    pytest.param(
        "SrcIP",
        "AllExtIPs",
        ["TenantId", "VMName"],
        None,
        ["BadCol", "TotalAllowedFlows"],
        NetExp(0, 0, ValueError, "edge_attrs"),
        id="Error - bad edge attr",
    ),
]


@pytest.mark.parametrize(
    "src, tgt, src_attr, tgt_attr, edge_attr, expected", _TEST_NETWORK
)
def test_network(network_data, src, tgt, src_attr, tgt_attr, edge_attr, expected):
    """Test df_to_network function."""
    if expected.error == ValueError:
        with pytest.raises(ValueError) as err:
            nxg = df_to_networkx(
                data=network_data,
                source_col=src,
                target_col=tgt,
                source_attrs=src_attr,
                target_attrs=tgt_attr,
                edge_attrs=edge_attr,
            )
        check.is_in(expected.err_mssg, str(err))
    else:
        nxg = df_to_networkx(
            data=network_data,
            source_col=src,
            target_col=tgt,
            source_attrs=src_attr,
            target_attrs=tgt_attr,
            edge_attrs=edge_attr,
        )

        check.equal(len(nxg.nodes()), expected.nodes)
        check.equal(len(nxg.edges()), expected.edges)

        # Check against data
        src_nodes = network_data[[tgt, src]].dropna()[src].unique()
        tgt_nodes = network_data[[tgt, src]].dropna()[tgt].unique()
        missing_srcs = len([n for n in src_nodes if n not in nxg.nodes()])
        check.equal(missing_srcs, 0)
        missing_tgts = len([n for n in tgt_nodes if n not in nxg.nodes()])
        check.equal(missing_tgts, 0)

        # check attributes
        expected_src_attrs = 2 + len(src_attr or [])
        expected_tgt_attrs = 2 + len(tgt_attr or [])
        first_node = nxg.nodes[next(iter(nxg.nodes()))]

        expected_attrs = []
        if first_node["node_type"] == "source" and src_attr:
            check.greater_equal(len(first_node), expected_src_attrs)
            expected_attrs.extend(src_attr)
        elif first_node["node_type"] == "target" and tgt_attr:
            check.greater_equal(len(first_node), expected_tgt_attrs)
            expected_attrs.extend(tgt_attr)
        expected_attrs.extend(["node_role", "node_type"])
        for attr in expected_attrs:
            check.is_in(attr, first_node)
        check.is_in(first_node["node_type"], (src, tgt))

        expected_edge_attrs = len(edge_attr or [])
        first_edge = nxg.edges[next(iter(nxg.edges()))]
        check.equal(len(first_edge), expected_edge_attrs)
        for attr in edge_attr or []:
            check.is_in(attr, first_edge)
