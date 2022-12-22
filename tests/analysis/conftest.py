# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Polling detection module test fixtures"""
import numpy as np
import pandas as pd
import pytest

np.random.seed(10)

@pytest.fixture(scope="module")
def periodic_data():
    N = 1440
    homo_pois = np.random.poisson(1.5, N)
    freq = 0.01666666666666
    periodic = (10 * np.sin(2 * np.pi * freq * np.arange(0, N))).astype("int")
    periodic[periodic < 0] = 0
    x = (periodic + homo_pois).astype("bool")

    date_range = pd.date_range("2022-01-01 00:00:00", "2022-01-02 00:00:00", freq="T", inclusive="left")
    periodic_dates = date_range[x]
    non_periodic_dates = date_range[homo_pois.astype("bool")]

    data = pd.DataFrame(
        {
            "edges": ["ip1:ip2" for _ in range(len(periodic_dates))] + ["ip2:ip3" for _ in range(len(non_periodic_dates))],
            "datetime": periodic_dates.to_list() +  non_periodic_dates.to_list()
        }
    )

    return data

@pytest.fixture(scope="module")
def edge_data():
    data = pd.DataFrame(
        {
            "edges": [
                "ip1:ip2", "ip1:ip2", "ip1:ip2", "ip1:ip2", "ip1:ip2",
                "ip2:ip1", "ip2:ip1", "ip2:ip1", "ip2:ip1", "ip2:ip1",
                "ip3:ip1", "ip3:ip1", "ip3:ip1", "ip3:ip1", "ip3:ip1"
            ],
            "datetime": [
                "2022-01-01 00:01:00", "2022-01-01 00:02:00", "2022-01-01 00:03:00", "2022-01-01 00:09:00", "2022-01-01 00:11:00",
                "2022-01-01 00:11:00", "2022-01-01 00:22:00", "2022-01-01 00:33:00", "2022-01-01 00:44:00", "2022-01-01 00:55:00",
                "2022-01-01 00:01:00", "2022-01-01 00:01:10", "2022-01-01 00:01:20", "2022-01-01 00:01:30", "2022-01-01 00:01:40"
            ]
        },
    )

    data["datetime"] = pd.to_datetime(data["datetime"])

    return data

@pytest.fixture(scope="module")
def transformed_data_ip1_5T(edge_data):
    return pd.DataFrame(
        {
            "edges": [
                "ip1:ip2", "ip1:ip2", "ip1:ip2", "ip1:ip2", "ip1:ip2", "ip1:ip2",
                "ip1:ip2", "ip1:ip2", "ip1:ip2", "ip1:ip2", "ip1:ip2", "ip1:ip2"
            ],
            "datetime": pd.date_range("2022-01-01 00:00:00", "2022-01-01 01:00:00", freq="5T", inclusive="left"),
            "connection": [1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0]
        }
    )

@pytest.fixture(scope="module")
def transformed_data_ip1_5T_diff_time_frame(edge_data):
    return pd.DataFrame(
        {
            "edges": [
                "ip1:ip2", "ip1:ip2", "ip1:ip2", "ip1:ip2", "ip1:ip2", "ip1:ip2",
                "ip1:ip2", "ip1:ip2", "ip1:ip2", "ip1:ip2", "ip1:ip2", "ip1:ip2",
                "ip1:ip2", "ip1:ip2", "ip1:ip2", "ip1:ip2", "ip1:ip2", "ip1:ip2",
                "ip1:ip2", "ip1:ip2", "ip1:ip2", "ip1:ip2", "ip1:ip2", "ip1:ip2"
            ],
            "datetime": pd.date_range("2022-01-01 00:00:00", "2022-01-01 02:00:00", freq="5T", inclusive="left"),
            "connection": [1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
        }
    )

@pytest.fixture(scope="module")
def transformed_data_ip1_10T(edge_data):
    return pd.DataFrame(
        {
            "edges": ["ip1:ip2", "ip1:ip2", "ip1:ip2", "ip1:ip2", "ip1:ip2", "ip1:ip2"],
            "datetime": pd.date_range("2022-01-01 00:00:00", "2022-01-01 01:00:00", freq="10T", inclusive="left"),
            "connection": [1, 1, 0, 0, 0, 0]
        }
    )

@pytest.fixture(scope="module")
def null_df():
    """Empty data frame for initializing the polling detector class"""

    return pd.DataFrame(
        {
            "edges": "ip1:ip2",
            "datetime": ["1900-01-01 00:00:00"],
            "number_of_bytes": [np.nan],
        }
    )


@pytest.fixture(scope="module")
def null_df_wrong_cols():
    """Empty data frame for initializing the polling detector class"""

    return pd.DataFrame(
        {
            "b": "ip1:ip2",
            "c": ["1900-01-01 00:00:00"],
            "d": [np.nan],
        }
    )
