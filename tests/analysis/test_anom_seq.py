import unittest
from datetime import datetime

import numpy as np
import pandas as pd

from msticpy.analysis.anomalous_sequence import anomalous
from msticpy.analysis.anomalous_sequence.utils.data_structures import Cmd


class TestAnomalous(unittest.TestCase):
    def setUp(self) -> None:
        self.sessions1 = [
            ["Set-User", "Set-User"],
            ["Set-Mailbox", "Set-User", "Set-User"],
        ]
        self.sessions2 = [
            [
                Cmd("Set-User", {"Identity"}),
                Cmd("Set-User", {"Identity", "City", "Name"}),
            ],
            [
                Cmd("Set-Mailbox", {"Identity"}),
                Cmd("Set-User", {"Identity", "City"}),
                Cmd("Set-User", {"Identity"}),
            ],
        ]
        self.sessions3 = [
            [
                Cmd("Set-User", {"Identity": "blah"}),
                Cmd("Set-User", {"Identity": "haha", "City": "york", "Name": "bob"}),
            ],
            [
                Cmd("Set-Mailbox", {"Identity": "blah"}),
                Cmd("Set-User", {"Identity": "blah", "City": "london"}),
                Cmd("Set-User", {"Identity": "haha"}),
            ],
        ]
        self.times = [datetime(2019, 3, 1), datetime(2019, 5, 6)]
        self.data1 = pd.DataFrame({"session": self.sessions1, "time": self.times})
        self.data2 = pd.DataFrame({"session": self.sessions2, "time": self.times})
        self.data3 = pd.DataFrame({"session": self.sessions3, "time": self.times})

    def tearDown(self) -> None:
        self.sessions1 = None
        self.sessions2 = None
        self.sessions3 = None
        self.times = None
        self.data1 = None
        self.data2 = None
        self.data3 = None

    def test_score_sessions(self):
        actual = anomalous.score_sessions(
            data=self.data1, session_column="session", window_length=3
        )
        self.assertTrue(isinstance(actual, pd.DataFrame))
        for col in self.data1.columns:
            self.assertTrue(col in actual.columns)
        self.assertEqual(len(actual.columns), len(self.data1.columns) + 2)
        self.assertEqual(len(actual), len(self.data1))
        window = actual["rarest_window3"].iloc[0]
        self.assertTrue(isinstance(window, list))
        self.assertTrue(isinstance(window[0], str))

        actual = anomalous.score_sessions(
            data=self.data2, session_column="session", window_length=3
        )
        window = actual["rarest_window3"].iloc[0]
        cmd = window[0]
        self.assertTrue(isinstance(window, list))
        self.assertTrue("name" in dir(cmd))
        self.assertTrue("params" in dir(cmd))
        self.assertTrue(isinstance(cmd.params, set))

        actual = anomalous.score_sessions(
            data=self.data3, session_column="session", window_length=3
        )
        window = actual["rarest_window3"].iloc[0]
        cmd = window[0]
        self.assertTrue(isinstance(window, list))
        self.assertTrue("name" in dir(cmd))
        self.assertTrue("params" in dir(cmd))
        self.assertTrue(isinstance(cmd.params, dict))

        actual = anomalous.score_sessions(
            data=self.data3, session_column="session", window_length=5
        )
        window = actual["rarest_window5"].iloc[0]
        lik = actual["rarest_window5_likelihood"].iloc[0]
        self.assertTrue(isinstance(window, list))
        self.assertEqual(len(window), 0)
        self.assertTrue(np.isnan(lik))


if __name__ == "__main__":
    unittest.main()
