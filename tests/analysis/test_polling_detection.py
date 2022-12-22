# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Test polling detection module"""
import numpy as np
import pandas as pd
import pytest

from msticpy.analysis import polling_detection as poll

__author__ = "Daniel Yates"


## ###### ##
## g_test ##
## ###### ##

@pytest.mark.parametrize(
    "exclude_pi, expected", [(True, 0.5857339), (False, 0.5357916)]
)
def test_g_test(null_df, exclude_pi, expected):
    test_power_spectral_density = [10, 60, 20, 30, 40, 10, 10, 10]

    per = poll.PeriodogramPollingDetector(null_df)

    test_stat, pval = per._g_test(test_power_spectral_density, exclude_pi)

    assert round(pval, 7) == expected


## ##################### ##
## _check_equally_spaced ##
## ##################### ##

@pytest.mark.parametrize(
    "df_name, expected", [("transformed_data_ip1_5T", True), ("edge_data", False)]
)
def test_check_equally_spaced(null_df, request, df_name, expected):
    per = poll.PeriodogramPollingDetector(null_df)

    df = request.getfixturevalue(df_name)
    df = df[df["edges"] == "ip1:ip2"]

    assert per._check_equally_spaced(df) == expected


## ######################### ##
## _check_data_frame_columns ##
## ######################### ##

@pytest.mark.parametrize(
    "df_name, expected", [("null_df", True), ("null_df_wrong_cols", False)]
)
def test_check_data_frame_columns(null_df, request, df_name, expected):
    per = poll.PeriodogramPollingDetector(null_df)

    df = request.getfixturevalue(df_name)

    assert per._check_data_frame_columns(df) == expected


## ####################################### ##
## _check_data_frame_columns at class init ##
## ####################################### ##

def test_check_data_frame_columns_on_init(null_df):
    per = poll.PeriodogramPollingDetector(null_df)

    pd.testing.assert_frame_equal(null_df, per.edges_df)


def test_check_data_frame_columns_on_init_raises_exception(null_df_wrong_cols):
    with pytest.raises(ValueError):
        poll.PeriodogramPollingDetector(null_df_wrong_cols)


## ############## ##
## transform_data ##
## ############## ##

def test_transform_data(edge_data, transformed_data_ip1_5T):
    per = poll.PeriodogramPollingDetector(edge_data)

    edge_df = edge_data[edge_data["edges"] == "ip1:ip2"]

    transformed = per.transform_data(edge_df, "5T", "2022-01-01 00:00:00", "2022-01-01 01:00:00")

    pd.testing.assert_frame_equal(transformed_data_ip1_5T, transformed, check_like=True)

def test_transform_data_multiple_edges_fails(edge_data):
    per = poll.PeriodogramPollingDetector(edge_data)

    with pytest.raises(ValueError):
        per.transform_data(edge_data, "5T", "2022-01-01 00:00:00", "2022-01-01 01:00:00")

def test_transform_data_different_freq(edge_data, transformed_data_ip1_10T):
    per = poll.PeriodogramPollingDetector(edge_data)

    edge_df = edge_data[edge_data["edges"] == "ip1:ip2"]

    transformed = per.transform_data(edge_df, "10T", "2022-01-01 00:00:00", "2022-01-01 01:00:00")

    pd.testing.assert_frame_equal(transformed_data_ip1_10T, transformed, check_like=True)

def test_transform_data_custom_time_frame(edge_data, transformed_data_ip1_5T_diff_time_frame):
    per = poll.PeriodogramPollingDetector(edge_data)

    edge_df = edge_data[edge_data["edges"] == "ip1:ip2"]

    transformed = per.transform_data(edge_df, "5T", "2022-01-01 00:00:00", "2022-01-01 02:00:00")


## ############## ##
## detect_polling ##
## ############## ##

def test_detect_polling_detects_freq(periodic_data, null_df):
    per = poll.PeriodogramPollingDetector(null_df)

    edges = per.detect_polling(periodic_data)

    assert edges["edges"].tolist() == ["ip1:ip2", "ip2:ip3"]
    assert edges["p_val"].dtype == np.dtype("float64")

