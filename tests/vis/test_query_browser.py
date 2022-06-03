# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""test Query browser."""
from functools import partial

import pytest
import pytest_check as check

from msticpy.data import QueryProvider
from msticpy.data.core.query_container import QueryContainer
from msticpy.vis.query_browser import _query_display_func

# pylint: disable=redefined-outer-name


@pytest.fixture(scope="module")
def query_prov():
    """Test fixture to create query provider."""
    return QueryProvider("LocalData")


def test_display_function(query_prov):
    """Test getting and executing the display function."""
    disp_func = _query_display_func(query_prov)
    containers = {
        name: cont
        for name, cont in query_prov.__dict__.items()
        if isinstance(cont, QueryContainer)
    }
    for name, container in containers.items():
        if name == "all_queries":
            continue
        for qry_name, qry_obj in container:
            if isinstance(qry_obj, partial):
                html = disp_func(f"{name}.{qry_name}")
                check.is_in(qry_name, html.data)
                check.is_in("Query", html.data)
                check.is_in("Parameters", html.data)
                check.is_in("start=start, end=end, hostname=host", html.data)
                check.is_in(f"{name}.{qry_name}(", html.data)
