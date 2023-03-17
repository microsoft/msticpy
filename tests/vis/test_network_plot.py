# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Test network_plot functions."""
from collections import namedtuple

import networkx as nx
import pytest
import pytest_check as check

from msticpy.transform.network import df_to_networkx
from msticpy.vis import network_plot

from ..transform.test_network import _TEST_NETWORK, network_data
from ..unit_test_lib import get_test_data_path

__author__ = "Ian Hellen"

# pylint: disable=redefined-outer-name


@pytest.mark.parametrize(
    "src, tgt, src_attr, tgt_attr, edge_attr, expected", _TEST_NETWORK
)
def test_network_plot(network_data, src, tgt, src_attr, tgt_attr, edge_attr, expected):
    """Test network plot."""
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

        plot = network_plot.plot_nx_graph(
            nxg,
            source_attrs=src_attr,
            target_attrs=tgt_attr,
            edge_attrs=edge_attr,
        )

        check.is_not_none(plot)
        check.is_true(plot.plot.tools)


@pytest.fixture(scope="module")
def create_graph(network_data):
    """Fixture for NX Graph."""
    return df_to_networkx(
        data=network_data,
        source_col="SrcIP",
        target_col="AllExtIPs",
        source_attrs=["TenantId", "VMName"],
        target_attrs=["DestPort", "ResourceGroup"],
        edge_attrs=["FlowType", "TotalAllowedFlows"],
    )


PlotParams = namedtuple(
    "PlotParams",
    [
        "title",
        "node_size",
        "font_size",
        "width",
        "height",
        "scale",
        "hide",
        "source_color",
        "target_color",
        "edge_color",
    ],
)

_PLOT_PARAMS = [
    PlotParams("Title", 20, 15, 800, 800, 2, True, "red", "blue", "green"),
    PlotParams(
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
    ),
]


@pytest.mark.parametrize("plot_params", _PLOT_PARAMS)
def test_plot_nx_graph_params(create_graph, plot_params):
    """Test different parameters."""
    plot_args = {
        param: val for param, val in plot_params._asdict().items() if val is not None
    }

    plot = network_plot.plot_nx_graph(create_graph, **plot_args)

    check.is_not_none(plot)


_TEST_LAYOUTS = [
    "spring",
    "spectral",
    nx.spring_layout,
]


@pytest.mark.parametrize("layout", _TEST_LAYOUTS)
def test_plot_nx_graph_layout(create_graph, layout):
    """Test different parameters."""
    plot_args = {
        param: val
        for param, val in _PLOT_PARAMS[0]._asdict().items()
        if val is not None
    }

    plot = network_plot.plot_nx_graph(create_graph, layout=layout, **plot_args)
    check.is_not_none(plot)

    if callable(layout):
        g_layout = layout(create_graph)
        plot = network_plot.plot_nx_graph(create_graph, layout=g_layout, **plot_args)
        check.is_not_none(plot)
