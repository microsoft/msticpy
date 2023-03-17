import unittest
from collections import defaultdict

import numpy as np

from msticpy.analysis.anomalous_sequence.utils import cmds_only
from msticpy.analysis.anomalous_sequence.utils.data_structures import StateMatrix

START_TOKEN = "##START##"  # nosec B105
END_TOKEN = "##END##"  # nosec B105
UNK_TOKEN = "##UNK##"  # nosec B105


class TestCmdsOnly(unittest.TestCase):
    def setUp(self):
        prior_probs = {
            "##START##": 0.19047619047619047,
            "Set-User": 0.3333333333333333,
            "##END##": 0.19047619047619047,
            "##UNK##": 0.2857142857142857,
        }
        self.prior_probs = StateMatrix(states=prior_probs, unk_token=UNK_TOKEN)

        trans_probs = {
            "##START##": {"Set-User": 0.5, "##END##": 0.25, "##UNK##": 0.25},
            "Set-User": {"##END##": 0.5, "Set-User": 0.25, "##UNK##": 0.25},
            "##UNK##": {
                "Set-User": 0.3333333333333333,
                "##END##": 0.3333333333333333,
                "##UNK##": 0.3333333333333333,
            },
        }
        self.trans_probs = StateMatrix(states=trans_probs, unk_token=UNK_TOKEN)

    def tearDown(self):
        self.prior_probs = None
        self.trans_probs = None

    def test_compute_counts(self):
        sessions = [[]]
        seq1_expected = {START_TOKEN: 1, END_TOKEN: 1}
        seq2_expected = {START_TOKEN: {END_TOKEN: 1}}
        seq1_actual, seq2_actual = cmds_only.compute_counts(
            sessions=sessions,
            start_token=START_TOKEN,
            end_token=END_TOKEN,
            unk_token=UNK_TOKEN,
        )
        self.assertDictEqual(seq1_actual, seq1_expected)
        self.assertDictEqual(seq2_actual, seq2_expected)

        cmd = "Set-User"
        sessions = [[cmd]]
        seq1_expected = {START_TOKEN: 1, cmd: 1, END_TOKEN: 1}
        seq2_expected = {START_TOKEN: {cmd: 1}, cmd: {END_TOKEN: 1}}

        seq1_actual, seq2_actual = cmds_only.compute_counts(
            sessions=sessions,
            start_token=START_TOKEN,
            end_token=END_TOKEN,
            unk_token=UNK_TOKEN,
        )
        self.assertDictEqual(seq1_actual, seq1_expected)
        self.assertDictEqual(seq2_actual, seq2_expected)

    def test_laplace_smooth_counts(self):
        # sessions = [[]]
        _seq1 = defaultdict(lambda: 0, {START_TOKEN: 1, END_TOKEN: 1})
        _seq2 = defaultdict(lambda: defaultdict(lambda: 0))
        _seq2[START_TOKEN][END_TOKEN] = 1
        seq1_expected = {START_TOKEN: 3, END_TOKEN: 3, UNK_TOKEN: 4}
        seq2_expected = {
            START_TOKEN: {END_TOKEN: 2, UNK_TOKEN: 1},
            UNK_TOKEN: {END_TOKEN: 1, UNK_TOKEN: 1},
        }
        seq1_actual, seq2_actual = cmds_only.laplace_smooth_counts(
            seq1_counts=_seq1,
            seq2_counts=_seq2,
            start_token=START_TOKEN,
            end_token=END_TOKEN,
            unk_token=UNK_TOKEN,
        )
        self.assertDictEqual(seq1_actual, seq1_expected)
        self.assertDictEqual(seq2_actual, seq2_expected)

        cmd = "Set-User"
        # sessions = [[cmd]]
        _seq1 = defaultdict(lambda: 0, {START_TOKEN: 1, cmd: 1, END_TOKEN: 1})
        _seq2 = defaultdict(lambda: defaultdict(lambda: 0))
        _seq2[START_TOKEN][cmd] = 1
        _seq2[cmd][END_TOKEN] = 1
        seq1_expected = {START_TOKEN: 4, cmd: 7, END_TOKEN: 4, UNK_TOKEN: 6}
        seq2_expected = {
            START_TOKEN: {cmd: 2, END_TOKEN: 1, UNK_TOKEN: 1},
            cmd: {END_TOKEN: 2, cmd: 1, UNK_TOKEN: 1},
            UNK_TOKEN: {cmd: 1, END_TOKEN: 1, UNK_TOKEN: 1},
        }

        seq1_actual, seq2_actual = cmds_only.laplace_smooth_counts(
            seq1_counts=_seq1,
            seq2_counts=_seq2,
            start_token=START_TOKEN,
            end_token=END_TOKEN,
            unk_token=UNK_TOKEN,
        )
        self.assertDictEqual(seq1_actual, seq1_expected)
        self.assertDictEqual(seq2_actual, seq2_expected)

    def test_compute_likelihood_window(self):
        actual = cmds_only.compute_likelihood_window(
            window=[],
            prior_probs=self.prior_probs,
            trans_probs=self.trans_probs,
            use_start_token=False,
            use_end_token=False,
            start_token=START_TOKEN,
            end_token=END_TOKEN,
        )
        self.assertTrue(np.isnan(actual))

        actual = cmds_only.compute_likelihood_window(
            window=["Set-User"],
            prior_probs=self.prior_probs,
            trans_probs=self.trans_probs,
            use_start_token=False,
            use_end_token=False,
            start_token=START_TOKEN,
            end_token=END_TOKEN,
        )
        expected = 1 / 3
        self.assertEqual(actual, expected)

        actual = cmds_only.compute_likelihood_window(
            window=["Set-User"],
            prior_probs=self.prior_probs,
            trans_probs=self.trans_probs,
            use_start_token=True,
            use_end_token=False,
            start_token=START_TOKEN,
            end_token=END_TOKEN,
        )
        expected = 0.5
        self.assertEqual(actual, expected)

        actual = cmds_only.compute_likelihood_window(
            window=["Set-User"],
            prior_probs=self.prior_probs,
            trans_probs=self.trans_probs,
            use_start_token=False,
            use_end_token=True,
            start_token=START_TOKEN,
            end_token=END_TOKEN,
        )
        expected = (1 / 3) * 0.5
        self.assertEqual(actual, expected)

        actual = cmds_only.compute_likelihood_window(
            window=["Set-User"],
            prior_probs=self.prior_probs,
            trans_probs=self.trans_probs,
            use_start_token=True,
            use_end_token=True,
            start_token=START_TOKEN,
            end_token=END_TOKEN,
        )
        expected = 0.5 * 0.5
        self.assertEqual(actual, expected)

        actual = cmds_only.compute_likelihood_window(
            window=["dfre"],
            prior_probs=self.prior_probs,
            trans_probs=self.trans_probs,
            use_start_token=False,
            use_end_token=False,
            start_token=START_TOKEN,
            end_token=END_TOKEN,
        )
        expected = self.prior_probs[UNK_TOKEN]
        self.assertEqual(actual, expected)

    def test_compute_likelihood_windows_in_session(self):
        actual = cmds_only.compute_likelihood_windows_in_session(
            session=[],
            prior_probs=self.prior_probs,
            trans_probs=self.trans_probs,
            window_len=1,
            use_start_end_tokens=False,
            start_token=START_TOKEN,
            end_token=END_TOKEN,
            use_geo_mean=False,
        )
        self.assertListEqual(actual, [])

        actual = cmds_only.compute_likelihood_windows_in_session(
            session=[],
            prior_probs=self.prior_probs,
            trans_probs=self.trans_probs,
            window_len=1,
            use_start_end_tokens=True,
            start_token=START_TOKEN,
            end_token=END_TOKEN,
            use_geo_mean=False,
        )
        self.assertListEqual(actual, [0.25])

        session = ["Set-User", "dfgg", "Set-User", "Set-User"]
        actual = cmds_only.compute_likelihood_windows_in_session(
            session=session,
            prior_probs=self.prior_probs,
            trans_probs=self.trans_probs,
            window_len=len(session),
            use_start_end_tokens=False,
            start_token=START_TOKEN,
            end_token=END_TOKEN,
            use_geo_mean=False,
        )
        self.assertEqual(len(actual), 1)

        non_geo = actual[0]
        geo_actual = cmds_only.compute_likelihood_windows_in_session(
            session=session,
            prior_probs=self.prior_probs,
            trans_probs=self.trans_probs,
            window_len=len(session),
            use_start_end_tokens=False,
            start_token=START_TOKEN,
            end_token=END_TOKEN,
            use_geo_mean=True,
        )[0]
        self.assertEqual(geo_actual, non_geo ** (1 / len(session)))

        actual = cmds_only.compute_likelihood_windows_in_session(
            session=session,
            prior_probs=self.prior_probs,
            trans_probs=self.trans_probs,
            window_len=len(session),
            use_start_end_tokens=True,
            start_token=START_TOKEN,
            end_token=END_TOKEN,
            use_geo_mean=False,
        )
        self.assertEqual(len(actual), 2)

        actual = cmds_only.compute_likelihood_windows_in_session(
            session=session,
            prior_probs=self.prior_probs,
            trans_probs=self.trans_probs,
            window_len=len(session) + 1,
            use_start_end_tokens=False,
            start_token=START_TOKEN,
            end_token=END_TOKEN,
            use_geo_mean=False,
        )
        self.assertEqual(len(actual), 0)

        actual = cmds_only.compute_likelihood_windows_in_session(
            session=session,
            prior_probs=self.prior_probs,
            trans_probs=self.trans_probs,
            window_len=len(session) + 1,
            use_start_end_tokens=True,
            start_token=START_TOKEN,
            end_token=END_TOKEN,
            use_geo_mean=False,
        )
        self.assertEqual(len(actual), 1)

    def test_rarest_window_session(self):
        actual = cmds_only.rarest_window_session(
            session=[],
            prior_probs=self.prior_probs,
            trans_probs=self.trans_probs,
            window_len=1,
            use_start_end_tokens=False,
            start_token=START_TOKEN,
            end_token=END_TOKEN,
            use_geo_mean=False,
        )
        self.assertTrue(np.isnan(actual[1]))
        self.assertListEqual(actual[0], [])

        actual = cmds_only.rarest_window_session(
            session=[],
            prior_probs=self.prior_probs,
            trans_probs=self.trans_probs,
            window_len=1,
            use_start_end_tokens=True,
            start_token=START_TOKEN,
            end_token=END_TOKEN,
            use_geo_mean=False,
        )
        self.assertEqual(actual[1], 0.25)
        self.assertListEqual(actual[0], [])

        actual = cmds_only.rarest_window_session(
            session=["Set-User", "Set-User"],
            prior_probs=self.prior_probs,
            trans_probs=self.trans_probs,
            window_len=3,
            use_start_end_tokens=False,
            start_token=START_TOKEN,
            end_token=END_TOKEN,
            use_geo_mean=False,
        )
        self.assertListEqual(actual[0], [])
        self.assertTrue(np.isnan(actual[1]))

        actual = cmds_only.rarest_window_session(
            session=["Set-User", "Set-User"],
            prior_probs=self.prior_probs,
            trans_probs=self.trans_probs,
            window_len=3,
            use_start_end_tokens=True,
            start_token=START_TOKEN,
            end_token=END_TOKEN,
            use_geo_mean=False,
        )
        self.assertListEqual(actual[0], ["Set-User", "Set-User"])
        self.assertEqual(actual[1], 0.0625)


if __name__ == "__main__":
    unittest.main()
