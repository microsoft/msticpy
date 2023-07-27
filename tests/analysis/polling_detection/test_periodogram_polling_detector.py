# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
Test polling detection module

The g test outputs are tested against the fisher.g.test.single function in the GeneCycle R package.

The code for this is located at https://github.com/cran/GeneCycle/blob/master/R/fisher.g.test.R
"""
import unittest.mock as mock
from datetime import datetime

import numpy as np
import pandas as pd
import pytest

from msticpy.analysis import polling_detection as poll

__author__ = "Daniel Yates"


## #### ##
## init ##
## #### ##
@pytest.mark.parametrize(
    "copy, equality_assertion",
    [(True, lambda x, y: x != y), (False, lambda x, y: x == y)],
)
def test_df_copied(copy, equality_assertion):
    null_df = pd.DataFrame()

    per = poll.PeriodogramPollingDetector(null_df, copy=copy)

    assert equality_assertion(id(null_df), id(per.data))


## ###### ##
## g_test ##
## ###### ##


@pytest.mark.parametrize(
    "exclude_pi, expected", [(True, 0.5857339), (False, 0.5357916)]
)
def test_g_test_small_sample(exclude_pi, expected):
    test_power_spectral_density = [10, 60, 20, 30, 40, 10, 10, 10]

    null_df = pd.DataFrame()

    per = poll.PeriodogramPollingDetector(null_df)
    _, pval = per._g_test(test_power_spectral_density, exclude_pi)

    assert round(pval, 7) == expected


@pytest.mark.parametrize("exclude_pi, expected", [(True, 1), (False, 1)])
def test_g_test_large_sample(exclude_pi, expected):
    test_power_spectral_density = [10, 60, 20, 30, 40, 10, 10, 10] * 200

    null_df = pd.DataFrame()

    per = poll.PeriodogramPollingDetector(null_df)
    _, pval = per._g_test(test_power_spectral_density, exclude_pi)

    assert round(pval, 7) == expected


## ################### ##
## _detect_polling_arr ##
## ################### ##


def test__detect_polling_arr_significant(periodic_data):
    per = poll.PeriodogramPollingDetector(periodic_data)

    ts = per.data["timestamps"]

    p_value, _, _ = per._detect_polling_arr(ts, min(ts), max(ts))

    assert p_value < 0.01


def test__detect_polling_arr_non_significant(non_periodic_data):
    per = poll.PeriodogramPollingDetector(non_periodic_data)

    ts = per.data["timestamps"]

    p_value, _, _ = per._detect_polling_arr(ts, min(ts), max(ts))

    assert p_value > 0.5


def test__detect_polling_arr_multiple_observations_per_second(periodic_data):
    periodic_data_add_obs = pd.concat(
        [periodic_data, periodic_data.sample(n=10000, replace=True)]
    )
    per = poll.PeriodogramPollingDetector(periodic_data)

    ts = per.data["timestamps"]

    p_value, _, _ = per._detect_polling_arr(ts, min(ts), max(ts))

    per_add_obs = poll.PeriodogramPollingDetector(periodic_data_add_obs)

    ts_add_obs = per_add_obs.data["timestamps"]

    p_value_add_obs, _, _ = per_add_obs._detect_polling_arr(
        ts_add_obs, min(ts_add_obs), max(ts_add_obs)
    )

    assert p_value == p_value_add_obs


def test_detect_polling_arr_returns_frequency(periodic_data):
    per = poll.PeriodogramPollingDetector(periodic_data)

    ts = per.data["timestamps"]

    _, freq, freq_reciprocal = per._detect_polling_arr(ts, min(ts), max(ts))

    assert round(freq, 6) == 0.016667
    assert round(freq_reciprocal, 6) == 59.999306


## ############## ##
## detect_polling ##
## ############## ##


def test_detect_polling_periodic_df(periodic_data):
    per = poll.PeriodogramPollingDetector(periodic_data)

    per.detect_polling("timestamps")

    assert all(per.data["p_value"] < 0.01)

    assert all(per.data["dominant_frequency"].round(6) == 0.016667)

    assert all(per.data["dominant_interval"].round(0) == 60)


def test_detect_polling_non_periodic_df(non_periodic_data):
    per = poll.PeriodogramPollingDetector(non_periodic_data)
    per.detect_polling("timestamps")

    assert all(per.data["p_value"].unique() > 0.5)


def test_detect_polling_on_grouped_df(periodic_data, non_periodic_data):
    df = pd.concat([periodic_data, non_periodic_data])

    per = poll.PeriodogramPollingDetector(df)
    per.detect_polling("timestamps", groupby="edges")

    assert all(per.data[per.data["edges"] == "periodic_edge"]["p_value"].values < 0.01)
    assert all(
        per.data[per.data["edges"] == "non_periodic_edge"]["p_value"].values > 0.5
    )


## ########### ##
## Integration ##
## ########### ##


@pytest.mark.parametrize("offset, pi_excluded", [(0, False), (1, True)])
@mock.patch.object(poll.PeriodogramPollingDetector, "_g_test", return_value=(0.5, 0.5))
def test_pi_freq_exclusion(g_test, periodic_data, offset, pi_excluded):
    max_ts = periodic_data["timestamps"].max()
    periodic_data = pd.concat(
        [
            periodic_data,
            pd.DataFrame({"edges": ["periodic_edge"], "timestamps": [max_ts + offset]}),
        ]
    )

    per = poll.PeriodogramPollingDetector(periodic_data)
    per.detect_polling("timestamps")

    args = g_test.call_args.args

    assert args[1] == pi_excluded
