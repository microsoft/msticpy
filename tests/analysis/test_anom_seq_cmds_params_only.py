import unittest
from collections import defaultdict

import numpy as np

from msticpy.analysis.anomalous_sequence.utils import cmds_params_only
from msticpy.analysis.anomalous_sequence.utils.data_structures import Cmd, StateMatrix

START_TOKEN = "##START##"  # nosec B105
END_TOKEN = "##END##"  # nosec B105
UNK_TOKEN = "##UNK##"  # nosec B105


class TestCmdsParamsOnly(unittest.TestCase):
    """
    Test cmds_params_only module.

    Note that when modelling the params:

    We make the modelling assumption that the parameters are independent
    Bernoulii random variables conditional on the command.

    Note also that because multiple parameters can appear at a time for
    a command, and because we are computing the probability that each
    parameter is present or not, we do NOT expect the probabilities to
    sum to 1.

    Note also that we use laplace smoothing in the counting
    stage of the calculations. Therefore if you have parameter p which
    appeared for every occurrence of command c, the resulting
    probability for param p appearing conditional on command c would
    NOT equal 1. It would be slightly less due to the laplace smoothing.

    """

    def setUp(self):
        self.data1 = {}
        self.data2 = {}
        self.data3 = {}

        # populate data1
        self.data1["sessions"] = []
        self.data1["_seq1_counts"] = defaultdict(lambda: 0)
        self.data1["seq1_counts"] = StateMatrix({UNK_TOKEN: 2}, UNK_TOKEN)
        self.data1["_seq2_counts"] = defaultdict(lambda: defaultdict(lambda: 0))
        self.data1["seq2_counts"] = StateMatrix({UNK_TOKEN: {UNK_TOKEN: 1}}, UNK_TOKEN)
        self.data1["_param_counts"] = defaultdict(lambda: 0)
        self.data1["param_counts"] = StateMatrix({UNK_TOKEN: 1}, UNK_TOKEN)
        self.data1["_cmd_param_counts"] = defaultdict(lambda: defaultdict(lambda: 0))
        self.data1["cmd_param_counts"] = StateMatrix(
            {UNK_TOKEN: {UNK_TOKEN: 1}}, UNK_TOKEN
        )
        self.data1["prior_probs"] = StateMatrix({UNK_TOKEN: 1}, UNK_TOKEN)
        self.data1["trans_probs"] = StateMatrix({UNK_TOKEN: {UNK_TOKEN: 1}}, UNK_TOKEN)
        self.data1["param_probs"] = StateMatrix({UNK_TOKEN: 0.5}, UNK_TOKEN)
        self.data1["param_cond_cmd_probs"] = StateMatrix(
            {UNK_TOKEN: {UNK_TOKEN: 0.5}}, UNK_TOKEN
        )

        # populate data2
        self.data2["sessions"] = [[]]
        self.data2["_seq1_counts"] = defaultdict(
            lambda: 0, {START_TOKEN: 1, END_TOKEN: 1}
        )
        self.data2["seq1_counts"] = StateMatrix(
            {UNK_TOKEN: 4, START_TOKEN: 3, END_TOKEN: 3}, UNK_TOKEN
        )
        self.data2["_seq2_counts"] = defaultdict(lambda: defaultdict(lambda: 0))
        self.data2["_seq2_counts"][START_TOKEN][END_TOKEN] = 1
        self.data2["seq2_counts"] = StateMatrix(
            {
                START_TOKEN: {END_TOKEN: 2, UNK_TOKEN: 1},
                UNK_TOKEN: {END_TOKEN: 1, UNK_TOKEN: 1},
            },
            UNK_TOKEN,
        )
        self.data2["_param_counts"] = defaultdict(lambda: 0)
        self.data2["param_counts"] = StateMatrix({UNK_TOKEN: 3}, UNK_TOKEN)
        self.data2["_cmd_param_counts"] = defaultdict(lambda: defaultdict(lambda: 0))
        self.data2["cmd_param_counts"] = StateMatrix(
            {
                START_TOKEN: {UNK_TOKEN: 1},
                END_TOKEN: {UNK_TOKEN: 1},
                UNK_TOKEN: {UNK_TOKEN: 1},
            },
            UNK_TOKEN,
        )
        self.data2["prior_probs"] = StateMatrix(
            {START_TOKEN: 0.3, END_TOKEN: 0.3, UNK_TOKEN: 0.4}, UNK_TOKEN
        )
        self.data2["trans_probs"] = StateMatrix(
            {
                START_TOKEN: {
                    END_TOKEN: 0.6666666666666666,
                    UNK_TOKEN: 0.3333333333333333,
                },
                UNK_TOKEN: {END_TOKEN: 0.5, UNK_TOKEN: 0.5},
            },
            UNK_TOKEN,
        )
        self.data2["param_probs"] = StateMatrix({UNK_TOKEN: 0.3}, UNK_TOKEN)
        self.data2["param_cond_cmd_probs"] = StateMatrix(
            {
                START_TOKEN: {UNK_TOKEN: 0.3333333333333333},
                END_TOKEN: {UNK_TOKEN: 0.3333333333333333},
                UNK_TOKEN: {UNK_TOKEN: 0.25},
            },
            UNK_TOKEN,
        )

        # populate data3
        cmd = "Set-User"
        self.data3["sessions"] = [
            [
                Cmd(name="Set-User", params={"City", "Identity"}),
                Cmd(name="Set-User", params={"Identity"}),
            ]
        ]
        self.data3["_seq1_counts"] = defaultdict(
            lambda: 0, {START_TOKEN: 1, END_TOKEN: 1, cmd: 2}
        )
        self.data3["seq1_counts"] = StateMatrix(
            {UNK_TOKEN: 6, START_TOKEN: 4, END_TOKEN: 4, cmd: 8}, UNK_TOKEN
        )
        self.data3["_seq2_counts"] = defaultdict(lambda: defaultdict(lambda: 0))
        self.data3["_seq2_counts"][START_TOKEN][cmd] = 1
        self.data3["_seq2_counts"][cmd][END_TOKEN] = 1
        self.data3["_seq2_counts"][cmd][cmd] = 1
        self.data3["seq2_counts"] = StateMatrix(
            {
                START_TOKEN: {END_TOKEN: 1, UNK_TOKEN: 1, cmd: 2},
                UNK_TOKEN: {END_TOKEN: 1, UNK_TOKEN: 1, cmd: 1},
                cmd: {cmd: 2, END_TOKEN: 2, UNK_TOKEN: 1},
            },
            UNK_TOKEN,
        )
        self.data3["_param_counts"] = defaultdict(lambda: 0, {"Identity": 2, "City": 1})
        self.data3["param_counts"] = StateMatrix(
            {UNK_TOKEN: 4, "City": 2, "Identity": 3}, UNK_TOKEN
        )
        self.data3["_cmd_param_counts"] = defaultdict(lambda: defaultdict(lambda: 0))
        self.data3["_cmd_param_counts"][cmd]["Identity"] = 2
        self.data3["_cmd_param_counts"][cmd]["City"] = 1
        self.data3["cmd_param_counts"] = StateMatrix(
            {
                START_TOKEN: {UNK_TOKEN: 1},
                END_TOKEN: {UNK_TOKEN: 1},
                UNK_TOKEN: {UNK_TOKEN: 1},
                cmd: {"City": 2, "Identity": 3, UNK_TOKEN: 1},
            },
            UNK_TOKEN,
        )
        self.data3["prior_probs"] = StateMatrix(
            {
                START_TOKEN: 0.18181818181818182,
                END_TOKEN: 0.18181818181818182,
                UNK_TOKEN: 0.2727272727272727,
                cmd: 0.36363636363636365,
            },
            UNK_TOKEN,
        )
        self.data3["trans_probs"] = StateMatrix(
            {
                START_TOKEN: {END_TOKEN: 0.25, UNK_TOKEN: 0.25, cmd: 0.5},
                UNK_TOKEN: {END_TOKEN: 1 / 3, UNK_TOKEN: 1 / 3, cmd: 1 / 3},
                cmd: {END_TOKEN: 0.4, UNK_TOKEN: 0.2, cmd: 0.4},
            },
            UNK_TOKEN,
        )
        self.data3["param_probs"] = StateMatrix(
            {
                UNK_TOKEN: 0.18181818181818182,
                "Identity": 0.13636363636363635,
                "City": 0.09090909090909091,
            },
            UNK_TOKEN,
        )
        self.data3["param_cond_cmd_probs"] = StateMatrix(
            {
                START_TOKEN: {UNK_TOKEN: 0.25},
                END_TOKEN: {UNK_TOKEN: 0.25},
                UNK_TOKEN: {UNK_TOKEN: 0.16666666666666666},
                cmd: {"City": 0.25, "Identity": 0.375, UNK_TOKEN: 0.125},
            },
            UNK_TOKEN,
        )

    def tearDown(self):
        self.data1 = None
        self.data2 = None
        self.data3 = None

    def test_compute_counts(self):
        (
            seq1_actual,
            seq2_actual,
            param_actual,
            cmd_param_actual,
        ) = cmds_params_only.compute_counts(
            sessions=self.data1["sessions"],
            start_token=START_TOKEN,
            end_token=END_TOKEN,
        )

        self.assertDictEqual(seq1_actual, self.data1["_seq1_counts"])
        self.assertDictEqual(seq2_actual, self.data1["_seq2_counts"])
        self.assertDictEqual(param_actual, self.data1["_param_counts"])
        self.assertDictEqual(cmd_param_actual, self.data1["_cmd_param_counts"])

        (
            seq1_actual,
            seq2_actual,
            param_actual,
            cmd_param_actual,
        ) = cmds_params_only.compute_counts(
            sessions=self.data2["sessions"],
            start_token=START_TOKEN,
            end_token=END_TOKEN,
        )

        self.assertDictEqual(seq1_actual, self.data2["_seq1_counts"])
        self.assertDictEqual(seq2_actual, self.data2["_seq2_counts"])
        self.assertDictEqual(param_actual, self.data2["_param_counts"])
        self.assertDictEqual(cmd_param_actual, self.data2["_cmd_param_counts"])

        (
            seq1_actual,
            seq2_actual,
            param_actual,
            cmd_param_actual,
        ) = cmds_params_only.compute_counts(
            sessions=self.data3["sessions"],
            start_token=START_TOKEN,
            end_token=END_TOKEN,
        )

        self.assertDictEqual(seq1_actual, self.data3["_seq1_counts"])
        self.assertDictEqual(seq2_actual, self.data3["_seq2_counts"])
        self.assertDictEqual(param_actual, self.data3["_param_counts"])
        self.assertDictEqual(cmd_param_actual, self.data3["_cmd_param_counts"])

    def test_laplace_smooth_counts(self):
        (
            seq1_actual,
            seq2_actual,
            param_actual,
            cmd_param_actual,
        ) = cmds_params_only.laplace_smooth_counts(
            seq1_counts=self.data1["_seq1_counts"],
            seq2_counts=self.data1["_seq2_counts"],
            param_counts=self.data1["_param_counts"],
            cmd_param_counts=self.data1["_cmd_param_counts"],
            start_token=START_TOKEN,
            end_token=END_TOKEN,
            unk_token=UNK_TOKEN,
        )

        self.assertDictEqual(seq1_actual, self.data1["seq1_counts"])
        self.assertDictEqual(seq2_actual, self.data1["seq2_counts"])
        self.assertDictEqual(param_actual, self.data1["param_counts"])
        self.assertDictEqual(cmd_param_actual, self.data1["cmd_param_counts"])

        (
            seq1_actual,
            seq2_actual,
            param_actual,
            cmd_param_actual,
        ) = cmds_params_only.laplace_smooth_counts(
            seq1_counts=self.data2["_seq1_counts"],
            seq2_counts=self.data2["_seq2_counts"],
            param_counts=self.data2["_param_counts"],
            cmd_param_counts=self.data2["_cmd_param_counts"],
            start_token=START_TOKEN,
            end_token=END_TOKEN,
            unk_token=UNK_TOKEN,
        )

        self.assertDictEqual(seq1_actual, self.data2["seq1_counts"])
        self.assertDictEqual(seq2_actual, self.data2["seq2_counts"])
        self.assertDictEqual(param_actual, self.data2["param_counts"])
        self.assertDictEqual(cmd_param_actual, self.data2["cmd_param_counts"])

        (
            seq1_actual,
            seq2_actual,
            param_actual,
            cmd_param_actual,
        ) = cmds_params_only.laplace_smooth_counts(
            seq1_counts=self.data3["_seq1_counts"],
            seq2_counts=self.data3["_seq2_counts"],
            param_counts=self.data3["_param_counts"],
            cmd_param_counts=self.data3["_cmd_param_counts"],
            start_token=START_TOKEN,
            end_token=END_TOKEN,
            unk_token=UNK_TOKEN,
        )

        self.assertDictEqual(seq1_actual, self.data3["seq1_counts"])
        self.assertDictEqual(seq2_actual, self.data3["seq2_counts"])
        self.assertDictEqual(param_actual, self.data3["param_counts"])
        self.assertDictEqual(cmd_param_actual, self.data3["cmd_param_counts"])

    def test_compute_prob_setofparams_given_cmd(self):
        actual = cmds_params_only.compute_prob_setofparams_given_cmd(
            cmd="Set-User",
            params=set(),
            param_cond_cmd_probs=self.data3["param_cond_cmd_probs"],
            use_geo_mean=False,
        )
        self.assertEqual(actual, 1)

        actual = cmds_params_only.compute_prob_setofparams_given_cmd(
            cmd="Set-User",
            params={"Identity"},
            param_cond_cmd_probs=self.data3["param_cond_cmd_probs"],
            use_geo_mean=False,
        )
        self.assertEqual(actual, 0.24609375)

        actual = cmds_params_only.compute_prob_setofparams_given_cmd(
            cmd="Set-User",
            params={"Identity"},
            param_cond_cmd_probs=self.data3["param_cond_cmd_probs"],
            use_geo_mean=True,
        )
        self.assertEqual(actual, 0.6266622418705731)

    def test_compute_likelihood_window(self):
        actual = cmds_params_only.compute_likelihood_window(
            window=[],
            prior_probs=self.data3["prior_probs"],
            trans_probs=self.data3["trans_probs"],
            param_cond_cmd_probs=self.data3["param_cond_cmd_probs"],
            use_start_token=False,
            use_end_token=False,
            start_token=START_TOKEN,
            end_token=END_TOKEN,
        )
        self.assertTrue(np.isnan(actual))

        actual = cmds_params_only.compute_likelihood_window(
            window=[Cmd("Set-User", {"Identity"})],
            prior_probs=self.data3["prior_probs"],
            trans_probs=self.data3["trans_probs"],
            param_cond_cmd_probs=self.data3["param_cond_cmd_probs"],
            use_start_token=False,
            use_end_token=False,
            start_token=START_TOKEN,
            end_token=END_TOKEN,
        )
        self.assertEqual(actual, 0.22787717886202657)

        actual = cmds_params_only.compute_likelihood_window(
            window=[Cmd("Set-User", {"Identity"})],
            prior_probs=self.data3["prior_probs"],
            trans_probs=self.data3["trans_probs"],
            param_cond_cmd_probs=self.data3["param_cond_cmd_probs"],
            use_start_token=True,
            use_end_token=False,
            start_token=START_TOKEN,
            end_token=END_TOKEN,
        )
        self.assertEqual(actual, 0.31333112093528653)

        actual = cmds_params_only.compute_likelihood_window(
            window=[Cmd("Set-User", {"Identity"})],
            prior_probs=self.data3["prior_probs"],
            trans_probs=self.data3["trans_probs"],
            param_cond_cmd_probs=self.data3["param_cond_cmd_probs"],
            use_start_token=False,
            use_end_token=True,
            start_token=START_TOKEN,
            end_token=END_TOKEN,
        )
        self.assertEqual(actual, 0.09115087154481064)

    def test_compute_likelihood_windows_in_session(self):
        actual = cmds_params_only.compute_likelihood_windows_in_session(
            session=[],
            prior_probs=self.data3["prior_probs"],
            trans_probs=self.data3["trans_probs"],
            param_cond_cmd_probs=self.data3["param_cond_cmd_probs"],
            window_len=1,
            use_start_end_tokens=False,
            start_token=START_TOKEN,
            end_token=END_TOKEN,
            use_geo_mean=False,
        )
        self.assertListEqual(actual, [])

        actual = cmds_params_only.compute_likelihood_windows_in_session(
            session=[],
            prior_probs=self.data3["prior_probs"],
            trans_probs=self.data3["trans_probs"],
            param_cond_cmd_probs=self.data3["param_cond_cmd_probs"],
            window_len=1,
            use_start_end_tokens=True,
            start_token=START_TOKEN,
            end_token=END_TOKEN,
            use_geo_mean=False,
        )
        self.assertListEqual(actual, [0.25])

        actual = cmds_params_only.compute_likelihood_windows_in_session(
            session=[Cmd("Set-User", {"Identity"})],
            prior_probs=self.data3["prior_probs"],
            trans_probs=self.data3["trans_probs"],
            param_cond_cmd_probs=self.data3["param_cond_cmd_probs"],
            window_len=1,
            use_start_end_tokens=False,
            start_token=START_TOKEN,
            end_token=END_TOKEN,
            use_geo_mean=False,
        )
        self.assertListEqual(actual, [0.22787717886202657])

    def test_rarest_window_session(self):
        actual = cmds_params_only.rarest_window_session(
            session=[],
            prior_probs=self.data3["prior_probs"],
            trans_probs=self.data3["trans_probs"],
            param_cond_cmd_probs=self.data3["param_cond_cmd_probs"],
            window_len=1,
            use_start_end_tokens=False,
            start_token=START_TOKEN,
            end_token=END_TOKEN,
            use_geo_mean=False,
        )
        self.assertListEqual(actual[0], [])
        self.assertTrue(np.isnan(actual[1]))

        actual = cmds_params_only.rarest_window_session(
            session=[],
            prior_probs=self.data3["prior_probs"],
            trans_probs=self.data3["trans_probs"],
            param_cond_cmd_probs=self.data3["param_cond_cmd_probs"],
            window_len=1,
            use_start_end_tokens=True,
            start_token=START_TOKEN,
            end_token=END_TOKEN,
            use_geo_mean=False,
        )
        self.assertListEqual(actual[0], [])
        self.assertEqual(actual[1], 0.25)

        actual = cmds_params_only.rarest_window_session(
            session=[Cmd("Set-User", {"City"}), Cmd("drfjh", {})],
            prior_probs=self.data3["prior_probs"],
            trans_probs=self.data3["trans_probs"],
            param_cond_cmd_probs=self.data3["param_cond_cmd_probs"],
            window_len=3,
            use_start_end_tokens=False,
            start_token=START_TOKEN,
            end_token=END_TOKEN,
            use_geo_mean=False,
        )
        self.assertListEqual(actual[0], [])
        self.assertTrue(np.isnan(actual[1]))

        actual = cmds_params_only.rarest_window_session(
            session=[Cmd("Set-User", {"City"}), Cmd("drfjh", {})],
            prior_probs=self.data3["prior_probs"],
            trans_probs=self.data3["trans_probs"],
            param_cond_cmd_probs=self.data3["param_cond_cmd_probs"],
            window_len=3,
            use_start_end_tokens=True,
            start_token=START_TOKEN,
            end_token=END_TOKEN,
            use_geo_mean=False,
        )
        self.assertEqual(len(actual[0]), 2)
        self.assertTrue(~np.isnan(actual[1]))


if __name__ == "__main__":
    unittest.main()
