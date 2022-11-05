# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""test TI Browser."""
from pathlib import Path

import pandas as pd
import pytest
import pytest_check as check

from msticpy.vis.ti_browser import get_ti_select_options, ti_details_display

from ..unit_test_lib import TEST_DATA_PATH

# pylint: disable=redefined-outer-name


@pytest.fixture(scope="module")
def ti_results():
    """Test fixture to read Dataframe."""
    df_path = Path(TEST_DATA_PATH).joinpath("ti_results.df.pkl")
    return pd.read_pickle(df_path)


def test_extract_options(ti_results):
    """Test extracting select list options from data."""
    output = get_ti_select_options(ti_results)
    check.equal(len(output), 9)
    output = get_ti_select_options(ti_results, severities=["warning"])
    check.equal(len(output), 4)


def test_display_function(ti_results):
    """Test getting and executing the display function."""
    disp_func = ti_details_display(ti_results)
    for _, row in ti_results.iterrows():
        html = disp_func((row.Ioc, [row.Provider]))
        check.is_in("Reference:", html.data)
        check.is_in(f"Provider: {row.Provider}", html.data)
