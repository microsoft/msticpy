# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Pivot VT import test."""
import pytest
import pytest_check as check

from msticpy.datamodel import entities

# pylint: disable=unused-import, unused-argument, redefined-outer-name
from .pivot_fixtures import create_data_providers, create_pivot, data_providers


@pytest.mark.filterwarnings("ignore::UserWarning")
def test_import_vt_funcs(create_pivot):
    """Test VT Pivot functions loaded correctly."""
    print([x for x in dir(entities.Url) if not x.startswith("_")])
    if not hasattr(entities.Url, "VT"):
        import warnings

        warnings.warn(
            "TEST SKIPPED - test_import_vt_funcs. Entities have no VT attribute."
        )
        return

    check.is_in("VT", dir(entities.Url))
    check.is_in("VT", dir(entities.File))
    check.is_in("VT", dir(entities.IpAddress))
    check.is_in("VT", dir(entities.Dns))

    check.greater_equal(
        len([attr for attr in dir(entities.Url.VT) if not attr.startswith("_")]), 5
    )
    check.greater_equal(
        len([attr for attr in dir(entities.Url.VT) if not attr.startswith("_")]), 5
    )
    check.greater_equal(
        len([attr for attr in dir(entities.Url.VT) if not attr.startswith("_")]), 5
    )
    check.greater_equal(
        len([attr for attr in dir(entities.Url.VT) if not attr.startswith("_")]), 5
    )
