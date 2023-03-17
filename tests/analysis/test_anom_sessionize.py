import unittest

import numpy as np
import pandas as pd
from pandas.testing import assert_frame_equal

from msticpy.analysis.anomalous_sequence import sessionize


class TestSessionize(unittest.TestCase):
    def setUp(self):
        self.df1 = pd.DataFrame({"UserId": [], "time": [], "operation": []})
        self.df1_with_ses_col = pd.DataFrame(
            {"UserId": [], "time": [], "operation": [], "session_ind": []}
        )
        self.df1_sessionized = pd.DataFrame(
            {
                "UserId": [],
                "time_min": [],
                "time_max": [],
                "operation_list": [],
                "duration": [],
                "number_events": [],
            }
        )
        self.df2 = pd.DataFrame(
            {
                "UserId": [1, 1, 2, 3, 1, 2, 2],
                "time": [
                    pd.to_datetime("2020-01-03 00:00:00", utc=True),
                    pd.to_datetime("2020-01-03 00:01:00", utc=True),
                    pd.to_datetime("2020-01-05 00:00:00", utc=True),
                    pd.to_datetime("2020-01-06 11:06:00", utc=True),
                    pd.to_datetime("2020-01-03 01:00:00", utc=True),
                    pd.to_datetime("2020-01-05 00:21:00", utc=True),
                    pd.to_datetime("2020-01-05 00:25:00", utc=True),
                ],
                "operation": ["A", "B", "C", "A", "A", "B", "C"],
            }
        )
        self.df2_with_ses_col_1 = pd.DataFrame(
            {
                "UserId": [1, 1, 1, 2, 2, 2, 3],
                "time": [
                    pd.to_datetime("2020-01-03 00:00:00", utc=True),
                    pd.to_datetime("2020-01-03 00:01:00", utc=True),
                    pd.to_datetime("2020-01-03 01:00:00", utc=True),
                    pd.to_datetime("2020-01-05 00:00:00", utc=True),
                    pd.to_datetime("2020-01-05 00:21:00", utc=True),
                    pd.to_datetime("2020-01-05 00:25:00", utc=True),
                    pd.to_datetime("2020-01-06 11:06:00", utc=True),
                ],
                "operation": ["A", "B", "A", "C", "B", "C", "A"],
                "session_ind": [0, 0, 1, 2, 3, 4, 5],
            }
        )
        self.df2_sessionized_1 = pd.DataFrame(
            {
                "UserId": [1, 1, 2, 2, 2, 3],
                "time_min": [
                    pd.to_datetime("2020-01-03 00:00:00", utc=True),
                    pd.to_datetime("2020-01-03 01:00:00", utc=True),
                    pd.to_datetime("2020-01-05 00:00:00", utc=True),
                    pd.to_datetime("2020-01-05 00:21:00", utc=True),
                    pd.to_datetime("2020-01-05 00:25:00", utc=True),
                    pd.to_datetime("2020-01-06 11:06:00", utc=True),
                ],
                "time_max": [
                    pd.to_datetime("2020-01-03 00:01:00", utc=True),
                    pd.to_datetime("2020-01-03 01:00:00", utc=True),
                    pd.to_datetime("2020-01-05 00:00:00", utc=True),
                    pd.to_datetime("2020-01-05 00:21:00", utc=True),
                    pd.to_datetime("2020-01-05 00:25:00", utc=True),
                    pd.to_datetime("2020-01-06 11:06:00", utc=True),
                ],
                "operation_list": [["A", "B"], ["A"], ["C"], ["B"], ["C"], ["A"]],
                "duration": [
                    pd.to_timedelta(1, "min"),
                    pd.to_timedelta(0, "min"),
                    pd.to_timedelta(0, "min"),
                    pd.to_timedelta(0, "min"),
                    pd.to_timedelta(0, "min"),
                    pd.to_timedelta(0, "min"),
                ],
                "number_events": [2, 1, 1, 1, 1, 1],
            }
        )
        self.df2_with_ses_col_2 = pd.DataFrame(
            {
                "UserId": [1, 1, 1, 2, 2, 2, 3],
                "time": [
                    pd.to_datetime("2020-01-03 00:00:00", utc=True),
                    pd.to_datetime("2020-01-03 00:01:00", utc=True),
                    pd.to_datetime("2020-01-03 01:00:00", utc=True),
                    pd.to_datetime("2020-01-05 00:00:00", utc=True),
                    pd.to_datetime("2020-01-05 00:21:00", utc=True),
                    pd.to_datetime("2020-01-05 00:25:00", utc=True),
                    pd.to_datetime("2020-01-06 11:06:00", utc=True),
                ],
                "operation": ["A", "B", "A", "C", "B", "C", "A"],
                "session_ind": [0, 0, 1, 2, 3, 3, 4],
            }
        )
        self.df2_sessionized_2 = pd.DataFrame(
            {
                "UserId": [1, 1, 2, 2, 3],
                "time_min": [
                    pd.to_datetime("2020-01-03 00:00:00", utc=True),
                    pd.to_datetime("2020-01-03 01:00:00", utc=True),
                    pd.to_datetime("2020-01-05 00:00:00", utc=True),
                    pd.to_datetime("2020-01-05 00:21:00", utc=True),
                    pd.to_datetime("2020-01-06 11:06:00", utc=True),
                ],
                "time_max": [
                    pd.to_datetime("2020-01-03 00:01:00", utc=True),
                    pd.to_datetime("2020-01-03 01:00:00", utc=True),
                    pd.to_datetime("2020-01-05 00:00:00", utc=True),
                    pd.to_datetime("2020-01-05 00:25:00", utc=True),
                    pd.to_datetime("2020-01-06 11:06:00", utc=True),
                ],
                "operation_list": [["A", "B"], ["A"], ["C"], ["B", "C"], ["A"]],
                "duration": [
                    pd.to_timedelta(1, "min"),
                    pd.to_timedelta(0, "min"),
                    pd.to_timedelta(0, "min"),
                    pd.to_timedelta(4, "min"),
                    pd.to_timedelta(0, "min"),
                ],
                "number_events": [2, 1, 1, 2, 1],
            }
        )
        self.df3 = pd.DataFrame(
            {
                "UserId": [np.nan, np.nan],
                "time": [
                    pd.to_datetime("2020-01-03 00:00:00", utc=True),
                    pd.to_datetime("2020-01-03 00:01:00", utc=True),
                ],
                "operation": ["A", "B"],
            }
        )
        self.df3_with_ses_col = pd.DataFrame(
            {
                "UserId": [np.nan, np.nan],
                "time": [
                    pd.to_datetime("2020-01-03 00:00:00", utc=True),
                    pd.to_datetime("2020-01-03 00:01:00", utc=True),
                ],
                "operation": ["A", "B"],
                "session_ind": [0, 0],
            }
        )
        self.df3_sessionized = pd.DataFrame(
            {
                "UserId": [np.nan],
                "time_min": [pd.to_datetime("2020-01-03 00:00:00", utc=True)],
                "time_max": [pd.to_datetime("2020-01-03 00:01:00", utc=True)],
                "operation_list": [["A", "B"]],
                "duration": [pd.to_timedelta(1, "min")],
                "number_events": [2],
            }
        )

    def tearDown(self):
        self.df1 = None
        self.df1_with_ses_col = None
        self.df1_sessionized = None
        self.df2 = None
        self.df2_with_ses_col_1 = None
        self.df2_sessionized_1 = None
        self.df2_with_ses_col_2 = None
        self.df2_sessionized_2 = None
        self.df3 = None
        self.df3_with_ses_col = None
        self.df3_sessionized = None

    def test_create_session_col(self):
        actual = sessionize.create_session_col(
            data=self.df1,
            user_identifier_cols=["UserId"],
            time_col="time",
            max_session_time_mins=20,
            max_event_separation_mins=2,
        )
        assert actual.shape == self.df1_with_ses_col.shape

        actual = sessionize.create_session_col(
            data=self.df2,
            user_identifier_cols=["UserId"],
            time_col="time",
            max_session_time_mins=20,
            max_event_separation_mins=2,
        )

        assert_frame_equal(actual, self.df2_with_ses_col_1, check_dtype=False)

        actual = sessionize.create_session_col(
            data=self.df2,
            user_identifier_cols=["UserId"],
            time_col="time",
            max_session_time_mins=20,
            max_event_separation_mins=5,
        )

        assert_frame_equal(actual, self.df2_with_ses_col_2, check_dtype=False)

        actual = sessionize.create_session_col(
            data=self.df3,
            user_identifier_cols=["UserId"],
            time_col="time",
            max_session_time_mins=20,
            max_event_separation_mins=2,
        )

        assert_frame_equal(actual, self.df3_with_ses_col, check_dtype=False)

    def test_sessionize_data(self):
        actual = sessionize.sessionize_data(
            data=self.df1,
            user_identifier_cols=["UserId"],
            time_col="time",
            max_session_time_mins=20,
            max_event_separation_mins=2,
            event_col="operation",
        )
        assert actual.shape == self.df1_sessionized.shape

        actual = sessionize.sessionize_data(
            data=self.df2,
            user_identifier_cols=["UserId"],
            time_col="time",
            max_session_time_mins=20,
            max_event_separation_mins=2,
            event_col="operation",
        )

        assert_frame_equal(actual, self.df2_sessionized_1, check_dtype=False)

        actual = sessionize.sessionize_data(
            data=self.df2,
            user_identifier_cols=["UserId"],
            time_col="time",
            max_session_time_mins=20,
            max_event_separation_mins=5,
            event_col="operation",
        )

        assert_frame_equal(actual, self.df2_sessionized_2, check_dtype=False)

        actual = sessionize.sessionize_data(
            data=self.df3,
            user_identifier_cols=["UserId"],
            time_col="time",
            max_session_time_mins=20,
            max_event_separation_mins=2,
            event_col="operation",
        )

        assert_frame_equal(actual, self.df3_sessionized, check_dtype=False)


if __name__ == "__main__":
    unittest.main()
