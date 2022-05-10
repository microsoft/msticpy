# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""transform.network module docstring."""

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


_TEST_NETWORK = [
    ("SrcIP", "AllExtIPs", None, None, None, (3, 4, 2, 0)),
    (
        "SrcIP",
        "AllExtIPs",
        ("TenantId", "VMName"),
        None,
        ("FlowType", "TotalAllowedFlows"),
        (3, 4, 4, 2),
    ),
    (
        "SrcIP",
        "AllExtIPs",
        ("TenantId", "VMName"),
        ("DestPort", "ResourceGroup"),
        ("FlowType", "TotalAllowedFlows"),
        (3, 4, 4, 2),
    ),
    (
        "SrcIP",
        "AllExtIPs",
        None,
        ("DestPort", "ResourceGroup", "VMName"),
        None,
        (3, 4, 5, 0),
    ),
    (
        "ResourceGroup",
        "AllExtIPs",
        None,
        ("DestPort", "ResourceGroup", "VMName"),
        None,
        (85, 84, 5, 0),
    ),
    (
        "BadCol",
        "AllExtIPs",
        ("TenantId", "VMName"),
        None,
        ("FlowType", "TotalAllowedFlows"),
        (ValueError, "source_col"),
    ),
    (
        "SrcIP",
        "AllExtIPs",
        ("BadCol", "VMName"),
        None,
        ("FlowType", "TotalAllowedFlows"),
        (ValueError, "source_attrs"),
    ),
    (
        "SrcIP",
        "AllExtIPs",
        ("TenantId", "VMName"),
        None,
        ("BadCol", "TotalAllowedFlows"),
        (ValueError, "edge_attrs"),
    ),
]


@pytest.mark.parametrize(
    "src, tgt, src_attr, tgt_attr, edge_attr, expected", _TEST_NETWORK
)
def test_network(network_data, src, tgt, src_attr, tgt_attr, edge_attr, expected):
    """Test df_to_network function."""
    if expected[0] == ValueError:
        with pytest.raises(ValueError) as err:
            nxg = df_to_networkx(
                data=network_data,
                source_col=src,
                target_col=tgt,
                source_attrs=src_attr,
                target_attrs=tgt_attr,
                edge_attrs=edge_attr,
            )
        check.is_in(expected[1], str(err))
    else:
        nxg = df_to_networkx(
            data=network_data,
            source_col=src,
            target_col=tgt,
            source_attrs=src_attr,
            target_attrs=tgt_attr,
            edge_attrs=edge_attr,
        )

        check.equal(len(nxg.nodes()), expected[0])
        check.equal(len(nxg.edges()), expected[1])

        # Check against data
        # missing_srcs = len([n for n in network_data[src].dropna().unique() if n not in nxg.nodes()])
        # check.equal(missing_srcs, 0)
        # missing_tgts = len([n for n in network_data[tgt].dropna().unique() if n not in nxg.nodes()])
        # check.equal(missing_tgts, 0)

        # check attributes
        first_node = nxg.nodes[next(iter(nxg.nodes()))]
        check.greater_equal(len(first_node), expected[2])
        attr_cols = []
        if first_node["node_type"] == "source" and src_attr:
            attr_cols.extend(src_attr)
        elif first_node["node_type"] == "target" and tgt_attr:
            attr_cols.extend(tgt_attr)
        attr_cols.extend(["node_role", "node_type"])
        for attr in attr_cols:
            check.is_in(attr, first_node)
        check.is_in(first_node["node_type"], (src, tgt))

        first_edge = nxg.edges[next(iter(nxg.edges()))]
        check.equal(len(first_edge), expected[3])
        for attr in edge_attr or []:
            check.is_in(attr, first_edge)
