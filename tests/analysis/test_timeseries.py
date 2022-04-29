# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Test time series anomalies module."""
from collections import Counter

import pandas as pd
import pytest
import pytest_check as check

from msticpy.analysis import timeseries
from msticpy.common.timespan import TimeSpan
from msticpy.vis.timeseries import display_timeseries_anomalies

from ..unit_test_lib import get_test_data_path

__author__ = "Ian Hellen"

# pylint: disable=redefined-outer-name


@pytest.fixture(scope="module")
def df_data():
    """Input dataframe."""
    df_path = get_test_data_path().joinpath("time_series.csv")
    return pd.read_csv(str(df_path), parse_dates=["TimeGenerated"])


def test_ts_anomalies_stl(df_data):
    """Test ts_anomalies_stl function."""
    results = timeseries.ts_anomalies_stl(
        df_data, time_column="TimeGenerated", data_column="TotalBytesSent"
    )
    check.equal(len(results.query("anomalies != 0")), 3)

    check.is_in("residual", results.columns)
    check.is_in("trend", results.columns)
    check.is_in("seasonal", results.columns)
    check.is_in("weights", results.columns)
    check.is_in("baseline", results.columns)
    check.is_in("score", results.columns)
    check.is_in("anomalies", results.columns)

    # check pd accessor equivalent
    results = df_data.mp_timeseries.analyze(
        time_column="TimeGenerated", data_column="TotalBytesSent"
    )
    check.equal(len(results.query("anomalies != 0")), 3)


def test_anomaly_periods(df_data):
    """Test extracting periods."""

    time_spans = df_data.mp_timeseries.analyze(
        time_column="TimeGenerated", data_column="TotalBytesSent"
    ).mp_timeseries.anomaly_periods()
    check.equal(len(time_spans), 3)
    check.is_instance(time_spans[0], TimeSpan)


def test_kql_criteria(df_data):
    """Test kql criteria generation."""
    expected = "| where TimeGenerated between (datetime(2019-05-13 16:00:00+00:00) .. datetime(2019-05-13 18:00:00+00:00)) or"
    kql = df_data.mp_timeseries.kql_periods()
    check.is_true(kql.startswith(expected))
    count_words = Counter(kql.split(" "))
    check.equal(count_words["between"], 3)


def test_new_threshold(df_data):
    """Test applying new threshold."""
    old_anoms = len(df_data[df_data["anomalies"] == 1])
    check.equal(old_anoms, 3)

    new_anoms = df_data.mp_timeseries.apply_threshold(threshold=2)
    check.equal(len(new_anoms.query("anomalies != 0")), 7)
    check.equal(len(new_anoms.query("anomalies == 1")), 3)
    check.equal(len(new_anoms.query("anomalies == -1")), 4)

    new_anoms = df_data.mp_timeseries.apply_threshold(threshold=2, threshold_low=1)
    check.equal(new_anoms.query("anomalies != 0").shape, (12, 5))


def test_pd_accessors(df_data):
    """Test accessors have imported docs."""
    for func_name in ("analyze", "anomaly_periods", "apply_threshold", "kql_periods"):
        func = getattr(df_data.mp_timeseries, func_name)
        check.greater(len(func.__doc__.split("\n")), 2)


def test_display_timeseries(df_data):
    """Basic test of plot without error."""
    display_timeseries_anomalies(df_data)
    display_timeseries_anomalies(
        df_data.rename(columns={"TimeGenerated": "timestamp"}), time_column="timestamp"
    )

    df_data.mp_timeseries.plot()
