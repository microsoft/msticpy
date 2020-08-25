# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
from collections import Counter
import os
import unittest
from pathlib import Path

import nbformat
import pandas as pd
import pytest
from nbconvert.preprocessors import CellExecutionError, ExecutePreprocessor

from msticpy.analysis.timeseries import (
    timeseries_anomalies_stl,
    extract_anomaly_periods,
    create_time_period_kqlfilter,
    set_new_anomaly_threshold,
)

_NB_FOLDER = "docs/notebooks"
_NB_NAME = "TimeSeriesAnomaliesVisualization.ipynb"

_test_data_folders = [
    d for d, _, _ in os.walk(os.getcwd()) if d.endswith("/docs/notebooks/data")
]
if len(_test_data_folders) == 1:
    _TEST_DATA = _test_data_folders[0]
else:
    _TEST_DATA = "./docs/notebooks/data"


class TestTimeSeries(unittest.TestCase):
    """Unit test class."""

    def setUp(self):
        input_file = os.path.join(_TEST_DATA, "TimeSeriesDemo.csv")
        self.input_df = pd.read_csv(
            input_file,
            index_col=["TimeGenerated"],
            parse_dates=["TimeGenerated"],
            infer_datetime_format=True,
            usecols=["TimeGenerated", "TotalBytesSent"],
        )
        self.output_df = timeseries_anomalies_stl(data=self.input_df)

    def test_timeseries_anomalies_stl(self):
        out_df = self.output_df

        self.assertIn("residual", out_df.columns)
        self.assertIn("trend", out_df.columns)
        self.assertIn("seasonal", out_df.columns)
        self.assertIn("weights", out_df.columns)
        self.assertIn("baseline", out_df.columns)
        self.assertIn("score", out_df.columns)
        self.assertIn("anomalies", out_df.columns)
        self.assertGreater(len(out_df[out_df["anomalies"] == 1]), 0)

        self.output_df = out_df

    @pytest.mark.skipif(
        not os.environ.get("MSTICPY_TEST_NOSKIP"), reason="Skipped for local tests."
    )
    def test_timeseries_controls(self):
        nb_path = Path(_NB_FOLDER).joinpath(_NB_NAME)
        abs_path = Path(_NB_FOLDER).absolute()
        with open(nb_path) as f:
            nb = nbformat.read(f, as_version=4)
        ep = ExecutePreprocessor(timeout=600, kernel_name="python3")

        try:
            ep.preprocess(nb, {"metadata": {"path": abs_path}})
        except CellExecutionError:
            nb_err = str(nb_path).replace(".ipynb", "-err.ipynb")
            msg = f"Error executing the notebook '{nb_path}'.\n"
            msg += f"See notebook '{nb_err}' for the traceback."
            print(msg)
            with open(nb_err, mode="w", encoding="utf-8") as f:
                nbformat.write(nb, f)
            raise

    def test_extract_anomaly_periods(self):
        """Test extracting anomaly periods."""
        if self.output_df is None:
            raise ValueError("No dataframe loaded")

        periods = extract_anomaly_periods(self.output_df)
        self.assertEqual(len(periods), 3)

        neg_periods = extract_anomaly_periods(self.output_df, pos_only=False)
        self.assertEqual(len(neg_periods), 5)

        k_filter = create_time_period_kqlfilter(periods)
        self.assertIsNotNone(k_filter)
        self.assertIn("where TimeGenerated", k_filter)
        count_words = Counter(k_filter.split(" "))
        self.assertEqual(count_words["between"], len(periods))

    def test_set_threshold(self):
        """Test setting new thresholds on data."""
        old_anoms = len(self.output_df[self.output_df["anomalies"] == 1])
        self.assertEqual(old_anoms, 3)
        new_df = set_new_anomaly_threshold(self.output_df, 2.5)
        new_anoms = len(new_df[new_df["anomalies"] == 1])
        self.assertEqual(new_anoms, 16)
        new_anoms = len(new_df[new_df["anomalies"] == -1])
        self.assertEqual(new_anoms, 16)

        # Use different thresholds for pos and neg
        new_df = set_new_anomaly_threshold(self.output_df, 2.5, 3.5)
        new_anoms = len(new_df[new_df["anomalies"] == 1])
        self.assertEqual(new_anoms, 16)
        new_anoms = len(new_df[new_df["anomalies"] == -1])
        self.assertEqual(new_anoms, 5)
