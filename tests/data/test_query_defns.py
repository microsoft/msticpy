# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Test Query definitions."""

import pandas as pd
import pytest_check as check

from msticpy.common.data_utils import ensure_df_datetimes

from ..unit_test_lib import get_test_data_path

__author__ = "Ian Hellen"

# pylint: disable=redefined-outer-name


def test_ensure_df_datetimes():
    """Test timezone auto-conversion."""
    source_df = pd.read_pickle(
        str(get_test_data_path().joinpath("localdata/alerts_list.pkl"))
    )
    source_df["TimeString"] = source_df.TimeGenerated.astype("str")
    source_df["DTString"] = source_df.TimeGenerated.astype("str")

    check.equal(source_df.select_dtypes("datetime").shape[1], 4)
    check.equal(source_df.select_dtypes("datetimetz").shape[1], 0)

    # This should convert all 5 cols to TZ-aware
    result_df = ensure_df_datetimes(source_df)
    check.equal(result_df.select_dtypes("datetime").shape[1], 0)
    check.equal(result_df.select_dtypes("datetimetz").shape[1], 5)

    # Only process 1 column
    result_df = ensure_df_datetimes(source_df, columns="TimeString")
    check.equal(result_df.select_dtypes("datetime").shape[1], 4)
    check.equal(result_df.select_dtypes("datetimetz").shape[1], 1)

    # Process list of columns
    result_df = ensure_df_datetimes(
        source_df, columns=["TimeGenerated", "StartTimeUtc", "TimeString", "DTString"]
    )
    check.equal(result_df.select_dtypes("datetime").shape[1], 2)
    check.equal(result_df.select_dtypes("datetimetz").shape[1], 4)

    # Process list of columns but do not change TZ-naive columns
    result_df = ensure_df_datetimes(
        source_df,
        columns=["TimeGenerated", "StartTimeUtc", "TimeString", "DTString"],
        add_utc_tz=False,
    )
    check.equal(result_df.select_dtypes("datetime").shape[1], 4)
    check.equal(result_df.select_dtypes("datetimetz").shape[1], 2)
