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
def test_g_test_small_sample(exclude_pi, expected):
    test_power_spectral_density = [10, 60, 20, 30, 40, 10, 10, 10]

    per = poll.PeriodogramPollingDetector()

    _, pval = per._g_test(test_power_spectral_density, exclude_pi)

    assert round(pval, 7) == expected


@pytest.mark.parametrize("exclude_pi, expected", [(True, 1), (False, 1)])
def test_g_test_large_sample(exclude_pi, expected):
    test_power_spectral_density = [10, 60, 20, 30, 40, 10, 10, 10] * 200

    per = poll.PeriodogramPollingDetector()

    _, pval = per._g_test(test_power_spectral_density, exclude_pi)

    assert round(pval, 7) == expected


## ############## ##
## detect_polling ##
## ############## ##


def test_detect_polling_significant(periodic_data):
    per = poll.PeriodogramPollingDetector()

    p_val = per.detect_polling(periodic_data, min(periodic_data), max(periodic_data))

    assert p_val < 0.01


@pytest.mark.parametrize("transform", [list, pd.Series])
def test_detect_polling_different_input_types(transform, periodic_data):
    data = transform(periodic_data)

    per = poll.PeriodogramPollingDetector()

    p_val = per.detect_polling(data, min(periodic_data), max(periodic_data))

    assert p_val < 0.01


def test_detect_polling_non_significant(non_periodic_data):
    per = poll.PeriodogramPollingDetector()

    p_val = per.detect_polling(
        non_periodic_data, min(non_periodic_data), max(non_periodic_data)
    )

    assert p_val > 0.5


def test_detect_polling_multiple_observations_per_second(periodic_data):
    additional_observations = np.random.choice(
        periodic_data[:10000], size=10000, replace=True
    )
    periodic_data_add_obs = np.append(periodic_data, additional_observations)

    per = poll.PeriodogramPollingDetector()

    p_val_add_obs = per.detect_polling(
        periodic_data_add_obs, min(periodic_data_add_obs), max(periodic_data_add_obs)
    )
    p_val = per.detect_polling(periodic_data, min(periodic_data), max(periodic_data))

    assert p_val == p_val_add_obs


## ########### ##
## Integration ##
## ########### ##


def test_detect_polling_works_on_grouped_df(periodic_data, non_periodic_data):
    df = pd.concat(
        [
            pd.DataFrame({"edge": "edge1", "timestamps": periodic_data}),
            pd.DataFrame({"edge": "edge2", "timestamps": non_periodic_data}),
        ]
    )
    per = poll.PeriodogramPollingDetector()

    output = df.groupby("edge").apply(
        lambda x: per.detect_polling(
            x["timestamps"], min(x["timestamps"]), max(x["timestamps"])
        )
    )

    assert output.loc["edge1"] < 0.01
    assert output.loc["edge2"] > 0.5
