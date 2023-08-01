# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Module docstring."""
from pathlib import Path

import pandas as pd
import pytest
import pytest_check as check

# pylint: disable=redefined-outer-name, protected-access, unused-import
try:
    import panel as pn

    _PANEL_AVAILABLE = True
    from msticpy.vis.data_viewer_panel import DataTableColumnChooser, DataViewer
except ImportError:
    _PANEL_AVAILABLE = False


from ..unit_test_lib import TEST_DATA_PATH

__author__ = "Ian Hellen"


@pytest.fixture(scope="module")
def dataframe():
    """Get dataframe and columns."""
    df_path = Path(TEST_DATA_PATH).joinpath("host_logons.csv")
    return pd.read_csv(
        str(df_path),
        index_col=0,
        parse_dates=["TimeGenerated"],
    )


_TEST_PARAMS = [
    ({"selectable": (False, "checkbox")}, True),
    ({"show_index": (False, True)}, True),
    ({"show_tenant_id": (False, True)}, True),
    ({"max_col_width": (500, 20)}, True),
    ({"detail_cols": (None, ["Account", "Computer"])}, True),
    ({"header_filters": (False, True)}, True),
    ({"height": (200, 1000)}, True),
    ({"hidden_cols": (None, [], ["Account"])}, True),
    ({"selected_cols": (None, ["Account", "Computer", "TimeGenerated"])}, True),
]

_TEST_IDS = [next(iter(item[0])) for item in _TEST_PARAMS]


@pytest.mark.skipif(not _PANEL_AVAILABLE, reason="panel not installed")
@pytest.mark.parametrize("kwargs, expected", _TEST_PARAMS, ids=_TEST_IDS)
def test_data_viewer(kwargs, expected, dataframe):
    """Test DataViewer parameters."""
    for k_name, cases in kwargs.items():
        for case in cases:
            k_params = {k_name: case}
            DataViewer(data=dataframe, **k_params)


def test_data_viewer_props(dataframe):
    """Test DataView properties."""
    test_df = dataframe.dropna(how="all", axis="columns")
    dv = DataViewer(data=dataframe, show_tenant_id=True)
    check.equal(dv.value.shape, test_df.shape)
    check.is_false(dv.selection)
    check.is_true(dv.selected_dataframe.empty)
    check.equal(dv.current_view.shape, test_df.shape)


def test_datatable_column_chooser(dataframe):
    """Test basic data column chooser functionality."""
    dataframe = dataframe.dropna(how="all", axis="columns")
    dt_col = DataTableColumnChooser(data=dataframe)
    check.equal(len(dataframe.columns), len(dt_col.selected_columns))
    for col in dataframe.columns:
        check.is_in(col, dt_col.selected_columns)

    col_subset = list(dataframe.columns)[:5]
    dt_col = DataTableColumnChooser(data=dataframe, selected_cols=col_subset)
    check.equal(len(col_subset), len(dt_col.selected_columns))
    for col in col_subset:
        check.is_in(col, dt_col.selected_columns)
