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

np.random.seed(10)

## ######## ##
## Fixtures ##
## ######## ##

@pytest.fixture(scope="module")
def transformed_data():
    """Simulated periodic data"""
    n = 1440
    periodic = 3 * np.sin(2 * np.pi * 0.05 * np.arange(n))
    noise = np.random.randn(n)

    return pd.DataFrame(
        {
            "src": ["ip1" for _ in range(n)],
            "dst": ["ip2" for _ in range(n)],
            "datetime": pd.date_range(
                "2022-01-01 09:00:00",
                "2022-01-02 09:00:00",
                freq="min",
                inclusive="left"
            ),
            "number_of_connections": periodic + noise
        }
    )

@pytest.fixture(scope="module")
def null_df():
    """Simulated periodic data"""

    return pd.DataFrame(
        {
            "src": "ip1",
            "dst": "ip2",
            "number_of_connections": [np.nan]
        }
    )

## ##### ##
## Tests ##
## ##### ##

@pytest.mark.parametrize("exclude_pi, expected", [(True, 0.5857339), (False, 0.5357916)])
def test_g_test(null_df, exclude_pi, expected):
    test_power_spectral_density = [10, 60, 20, 30, 40, 10, 10, 10]

    per = poll.PeriodogramPollingDetector(null_df)

    test_stat, pval = per._g_test(test_power_spectral_density, exclude_pi) 
    
    assert round(pval, 7) == expected

def test_time_series_equally_spaced(transformed_data):
    per = poll.PeriodogramPollingDetector(transformed_data)

    assert poll._check_equally_spaced() == True
