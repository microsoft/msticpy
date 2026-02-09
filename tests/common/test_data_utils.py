# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Tests for data_utils module."""

import numpy as np
import pandas as pd

from msticpy.common.data_utils import (
    ensure_df_timedeltas,
    parse_timespan,
)


class TestParseTimespan:
    """Tests for parse_timespan function (wrapper for azure.kusto.data.helpers)."""

    def test_parse_none(self):
        """Test parsing None returns None."""
        assert parse_timespan(None) is None

    def test_parse_small_timespan_no_frac(self):
        """Test parsing small timespan without fractional seconds."""
        result = parse_timespan("00:00:01")
        expected = pd.Timedelta(seconds=1)
        assert result == expected

    def test_parse_small_timespan_with_frac(self):
        """Test parsing small timespan with fractional seconds."""
        result = parse_timespan("00:00:00.001")
        expected = pd.Timedelta(milliseconds=1)
        assert result == expected

    def test_parse_medium_timespan(self):
        """Test parsing medium timespan (hours, minutes, seconds)."""
        result = parse_timespan("12:34:56")
        expected = pd.Timedelta(hours=12, minutes=34, seconds=56)
        assert result == expected

    def test_parse_large_timespan_one_day(self):
        """Test parsing timespan of exactly 1 day."""
        result = parse_timespan("1.00:00:00")
        expected = pd.Timedelta(days=1)
        assert result == expected

    def test_parse_large_timespan_with_time(self):
        """Test parsing timespan with days and time components."""
        result = parse_timespan("3.12:34:56")
        expected = pd.Timedelta(days=3, hours=12, minutes=34, seconds=56)
        assert result == expected

    def test_parse_large_timespan_with_frac(self):
        """Test parsing large timespan with fractional seconds."""
        result = parse_timespan("1.00:00:00.001")
        expected = pd.Timedelta(days=1, milliseconds=1)
        assert result == expected

    def test_parse_zero_timespan(self):
        """Test parsing zero timespan."""
        result = parse_timespan("00:00:00")
        expected = pd.Timedelta(0)
        assert result == expected

    def test_parse_examples_from_issue(self):
        """Test examples from the GitHub issue."""
        # Small timespan
        result = parse_timespan("00:00:00.0010000")
        assert result == pd.Timedelta(milliseconds=1)

        # Large timespan
        result = parse_timespan("1.00:00:00")
        assert result == pd.Timedelta(days=1)


class TestEnsureDfTimedeltas:
    """Tests for ensure_df_timedeltas function."""

    def test_convert_small_timespan_column(self):
        """Test converting a column with small timespans."""
        df = pd.DataFrame({"duration": ["00:00:00.001", "00:00:01", "00:10:30"]})
        result = ensure_df_timedeltas(df, columns="duration")

        assert pd.api.types.is_timedelta64_dtype(result["duration"])
        assert result["duration"].iloc[0] == pd.Timedelta(milliseconds=1)
        assert result["duration"].iloc[1] == pd.Timedelta(seconds=1)
        assert result["duration"].iloc[2] == pd.Timedelta(minutes=10, seconds=30)

    def test_convert_large_timespan_column(self):
        """Test converting a column with large timespans (>= 1 day)."""
        df = pd.DataFrame({"duration": ["1.00:00:00", "3.12:34:56", "10.00:00:00.001"]})
        result = ensure_df_timedeltas(df, columns="duration")

        assert pd.api.types.is_timedelta64_dtype(result["duration"])
        assert result["duration"].iloc[0] == pd.Timedelta(days=1)
        assert result["duration"].iloc[1] == pd.Timedelta(
            days=3, hours=12, minutes=34, seconds=56
        )
        assert result["duration"].iloc[2] == pd.Timedelta(days=10, milliseconds=1)

    def test_convert_mixed_timespan_column(self):
        """Test converting a column with mixed small and large timespans."""
        df = pd.DataFrame({"duration": ["00:00:01", "1.00:00:00", "00:10:30", "2.12:00:00"]})
        result = ensure_df_timedeltas(df, columns="duration")

        assert pd.api.types.is_timedelta64_dtype(result["duration"])
        assert result["duration"].iloc[0] == pd.Timedelta(seconds=1)
        assert result["duration"].iloc[1] == pd.Timedelta(days=1)
        assert result["duration"].iloc[2] == pd.Timedelta(minutes=10, seconds=30)
        assert result["duration"].iloc[3] == pd.Timedelta(days=2, hours=12)

    def test_convert_multiple_columns(self):
        """Test converting multiple timespan columns."""
        df = pd.DataFrame(
            {
                "duration1": ["00:00:01", "1.00:00:00"],
                "duration2": ["00:10:30", "2.12:00:00"],
                "other": ["a", "b"],
            }
        )
        result = ensure_df_timedeltas(df, columns=["duration1", "duration2"])

        assert pd.api.types.is_timedelta64_dtype(result["duration1"])
        assert pd.api.types.is_timedelta64_dtype(result["duration2"])
        assert result["other"].dtype == "object"

    def test_skip_already_timedelta_column(self):
        """Test that columns already in timedelta format are skipped."""
        df = pd.DataFrame({"duration": pd.to_timedelta(["00:00:01", "00:10:30"])})
        result = ensure_df_timedeltas(df, columns="duration")

        assert pd.api.types.is_timedelta64_dtype(result["duration"])
        # Should be unchanged
        pd.testing.assert_frame_equal(result, df)

    def test_skip_nonexistent_column(self):
        """Test that nonexistent columns are skipped without error."""
        df = pd.DataFrame({"duration": ["00:00:01", "1.00:00:00"]})
        result = ensure_df_timedeltas(df, columns=["duration", "nonexistent"])

        assert pd.api.types.is_timedelta64_dtype(result["duration"])
        assert "nonexistent" not in result.columns

    def test_dataframe_with_none_values(self):
        """Test handling of None/NaN values in timespan column."""
        df = pd.DataFrame({"duration": ["00:00:01", None, "1.00:00:00"]})
        result = ensure_df_timedeltas(df, columns="duration")

        assert pd.api.types.is_timedelta64_dtype(result["duration"])
        assert pd.isna(result["duration"].iloc[1])
        assert result["duration"].iloc[0] == pd.Timedelta(seconds=1)
        assert result["duration"].iloc[2] == pd.Timedelta(days=1)

    def test_original_dataframe_unchanged(self):
        """Test that original dataframe is not modified."""
        df = pd.DataFrame({"duration": ["00:00:01", "1.00:00:00"]})
        df_copy = df.copy()
        ensure_df_timedeltas(df, columns="duration")

        # Original should be unchanged
        pd.testing.assert_frame_equal(df, df_copy)

    def test_issue_example_small_timespan(self):
        """Test example from GitHub issue - small timespan."""
        df = pd.DataFrame({"print_0": ["00:00:00.0010000"]})
        result = ensure_df_timedeltas(df, columns="print_0")

        assert pd.api.types.is_timedelta64_dtype(result["print_0"])
        # Check that values are numpy.timedelta64
        assert isinstance(result["print_0"].values[0], np.timedelta64)

    def test_issue_example_large_timespan(self):
        """Test example from GitHub issue - large timespan that fails with pandas."""
        df = pd.DataFrame({"print_0": ["1.00:00:00"]})
        result = ensure_df_timedeltas(df, columns="print_0")

        assert pd.api.types.is_timedelta64_dtype(result["print_0"])
        # Check that values are numpy.timedelta64
        assert isinstance(result["print_0"].values[0], np.timedelta64)
        assert result["print_0"].iloc[0] == pd.Timedelta(days=1)
