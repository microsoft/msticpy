import unittest
import numpy as np

from ..msticpy.analysis.anomalous_sequence.utils import cmds_params_values
from ..msticpy.analysis.anomalous_sequence.utils.data_structures import StateMatrix, Cmd

START_TOKEN = "##START##"
END_TOKEN = "##END##"
UNK_TOKEN = "##UNK##"


class TestCmdsParamsValues(unittest.TestCase):
    def setUp(self):
        self.data1 = dict()
        self.data2 = dict()
        self.data3 = dict()

        # populate data1
        self.data1["sessions"] = []
        self.data1["seq1_counts"] = StateMatrix({UNK_TOKEN: 2}, UNK_TOKEN)
        self.data1["seq2_counts"] = StateMatrix({UNK_TOKEN: {UNK_TOKEN: 1}}, UNK_TOKEN)
        self.data1["param_counts"] = StateMatrix({UNK_TOKEN: 1}, UNK_TOKEN)
        self.data1["cmd_param_counts"] = StateMatrix(
            {UNK_TOKEN: {UNK_TOKEN: 1}}, UNK_TOKEN
        )
        self.data1["value_counts"] = StateMatrix({UNK_TOKEN: 1}, UNK_TOKEN)
        self.data1["param_value_counts"] = StateMatrix(
            {UNK_TOKEN: {UNK_TOKEN: 1}}, UNK_TOKEN
        )
        self.data1["prior_probs"] = StateMatrix({UNK_TOKEN: 1}, UNK_TOKEN)
        self.data1["trans_probs"] = StateMatrix({UNK_TOKEN: {UNK_TOKEN: 1}}, UNK_TOKEN)
        self.data1["param_probs"] = StateMatrix({UNK_TOKEN: 1}, UNK_TOKEN)
        self.data1["param_cond_cmd_probs"] = StateMatrix(
            {UNK_TOKEN: {UNK_TOKEN: 1}}, UNK_TOKEN
        )
        self.data1["value_probs"] = StateMatrix({UNK_TOKEN: 1}, UNK_TOKEN)
        self.data1["value_cond_param_probs"] = StateMatrix(
            {UNK_TOKEN: {UNK_TOKEN: 1}}, UNK_TOKEN
        )

        # populate data2
        self.data2["sessions"] = [[]]
        self.data2["seq1_counts"] = StateMatrix(
            {UNK_TOKEN: 4, START_TOKEN: 3, END_TOKEN: 3}, UNK_TOKEN
        )
        self.data2["seq2_counts"] = StateMatrix(
            {
                START_TOKEN: {END_TOKEN: 2, UNK_TOKEN: 1},
                UNK_TOKEN: {END_TOKEN: 1, UNK_TOKEN: 1},
            },
            UNK_TOKEN,
        )
        self.data2["param_counts"] = StateMatrix({UNK_TOKEN: 3}, UNK_TOKEN)
        self.data2["cmd_param_counts"] = StateMatrix(
            {
                START_TOKEN: {UNK_TOKEN: 1},
                END_TOKEN: {UNK_TOKEN: 1},
                UNK_TOKEN: {UNK_TOKEN: 1},
            },
            UNK_TOKEN,
        )
        self.data2["value_counts"] = StateMatrix({UNK_TOKEN: 1}, UNK_TOKEN)
        self.data2["param_value_counts"] = StateMatrix(
            {UNK_TOKEN: {UNK_TOKEN: 1}}, UNK_TOKEN
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
        self.data2["param_probs"] = StateMatrix({UNK_TOKEN: 1}, UNK_TOKEN)
        self.data2["param_cond_cmd_probs"] = StateMatrix(
            {
                START_TOKEN: {UNK_TOKEN: 1.0},
                END_TOKEN: {UNK_TOKEN: 1.0},
                UNK_TOKEN: {UNK_TOKEN: 1.0},
            },
            UNK_TOKEN,
        )
        self.data2["value_probs"] = StateMatrix({UNK_TOKEN: 1}, UNK_TOKEN)
        self.data2["value_cond_param_probs"] = StateMatrix(
            {UNK_TOKEN: {UNK_TOKEN: 1}}, UNK_TOKEN
        )

        # populate data3
        cmd = "Set-User"
        self.data3["sessions"] = [
            [
                Cmd(name="Set-User", params={"City": "york", "Identity": "blah"}),
                Cmd(name="Set-User", params={"Identity": "blah"}),
            ]
        ]
        self.data3["seq1_counts"] = StateMatrix(
            {UNK_TOKEN: 6, START_TOKEN: 4, END_TOKEN: 4, cmd: 8}, UNK_TOKEN
        )
        self.data3["seq2_counts"] = StateMatrix(
            {
                START_TOKEN: {END_TOKEN: 1, UNK_TOKEN: 1, cmd: 2},
                UNK_TOKEN: {END_TOKEN: 1, UNK_TOKEN: 1, cmd: 1},
                cmd: {cmd: 2, END_TOKEN: 2, UNK_TOKEN: 1},
            },
            UNK_TOKEN,
        )
        self.data3["param_counts"] = StateMatrix(
            {UNK_TOKEN: 4, "City": 2, "Identity": 3}, UNK_TOKEN
        )
        self.data3["cmd_param_counts"] = StateMatrix(
            {
                START_TOKEN: {UNK_TOKEN: 1},
                END_TOKEN: {UNK_TOKEN: 1},
                UNK_TOKEN: {UNK_TOKEN: 1},
                cmd: {"City": 2, "Identity": 3, UNK_TOKEN: 1},
            },
            UNK_TOKEN,
        )
        self.data3["value_counts"] = StateMatrix(
            {"york": 2, "blah": 3, UNK_TOKEN: 3}, UNK_TOKEN
        )
        self.data3["param_value_counts"] = StateMatrix(
            {
                "City": {"york": 2, UNK_TOKEN: 1},
                "Identity": {"blah": 3, UNK_TOKEN: 1},
                UNK_TOKEN: {UNK_TOKEN: 1},
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
                UNK_TOKEN: 0.4444444444444444,
                "Identity": 0.3333333333333333,
                "City": 0.2222222222222222,
            },
            UNK_TOKEN,
        )
        self.data3["param_cond_cmd_probs"] = StateMatrix(
            {
                START_TOKEN: {UNK_TOKEN: 1.0},
                END_TOKEN: {UNK_TOKEN: 1.0},
                UNK_TOKEN: {UNK_TOKEN: 1.0},
                cmd: {
                    "City": 0.3333333333333333,
                    "Identity": 0.5,
                    UNK_TOKEN: 0.16666666666666666,
                },
            },
            UNK_TOKEN,
        )
        self.data3["value_probs"] = StateMatrix(
            {"york": 0.25, "blah": 0.375, UNK_TOKEN: 0.375}, UNK_TOKEN
        )
        self.data3["value_cond_param_probs"] = StateMatrix(
            {
                "City": {"york": 0.6666666666666666, UNK_TOKEN: 0.3333333333333333},
                "Identity": {"blah": 0.75, UNK_TOKEN: 0.25},
                UNK_TOKEN: {UNK_TOKEN: 1.0},
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
            val_actual,
            param_val_actual,
        ) = cmds_params_values.compute_counts(
            sessions=self.data1["sessions"],
            start_token=START_TOKEN,
            end_token=END_TOKEN,
            unk_token=UNK_TOKEN,
        )

        self.assertDictEqual(seq1_actual, self.data1["seq1_counts"])
        self.assertDictEqual(seq2_actual, self.data1["seq2_counts"])
        self.assertDictEqual(param_actual, self.data1["param_counts"])
        self.assertDictEqual(cmd_param_actual, self.data1["cmd_param_counts"])
        self.assertDictEqual(val_actual, self.data1["value_counts"])
        self.assertDictEqual(param_val_actual, self.data1["param_value_counts"])

        (
            seq1_actual,
            seq2_actual,
            param_actual,
            cmd_param_actual,
            val_actual,
            param_val_actual,
        ) = cmds_params_values.compute_counts(
            sessions=self.data2["sessions"],
            start_token=START_TOKEN,
            end_token=END_TOKEN,
            unk_token=UNK_TOKEN,
        )

        self.assertDictEqual(seq1_actual, self.data2["seq1_counts"])
        self.assertDictEqual(seq2_actual, self.data2["seq2_counts"])
        self.assertDictEqual(param_actual, self.data2["param_counts"])
        self.assertDictEqual(cmd_param_actual, self.data2["cmd_param_counts"])
        self.assertDictEqual(val_actual, self.data2["value_counts"])
        self.assertDictEqual(param_val_actual, self.data2["param_value_counts"])

        (
            seq1_actual,
            seq2_actual,
            param_actual,
            cmd_param_actual,
            val_actual,
            param_val_actual,
        ) = cmds_params_values.compute_counts(
            sessions=self.data3["sessions"],
            start_token=START_TOKEN,
            end_token=END_TOKEN,
            unk_token=UNK_TOKEN,
        )

        self.assertDictEqual(seq1_actual, self.data3["seq1_counts"])
        self.assertDictEqual(seq2_actual, self.data3["seq2_counts"])
        self.assertDictEqual(param_actual, self.data3["param_counts"])
        self.assertDictEqual(cmd_param_actual, self.data3["cmd_param_counts"])
        self.assertDictEqual(val_actual, self.data3["value_counts"])
        self.assertDictEqual(param_val_actual, self.data3["param_value_counts"])

    def test_get_params_to_model_values(self):
        actual = cmds_params_values.get_params_to_model_values(
            param_counts=self.data1["param_counts"],
            param_value_counts=self.data1["param_value_counts"],
        )
        self.assertSetEqual(actual, set())

        actual = cmds_params_values.get_params_to_model_values(
            param_counts={"Identity": 50, "city": 2, UNK_TOKEN: 1},
            param_value_counts={
                "Identity": {"haha": 25, "hehe": 24, UNK_TOKEN: 1},
                "city": {"york": 1, UNK_TOKEN: 1},
                UNK_TOKEN: {UNK_TOKEN: 1},
            },
        )
        self.assertSetEqual(actual, {"Identity"})

    def test_compute_prob_setofparams_given_cmd(self):
        actual = cmds_params_values.compute_prob_setofparams_given_cmd(
            cmd="Set-User",
            params_with_vals=dict(),
            param_cond_cmd_probs=self.data3["param_cond_cmd_probs"],
            value_cond_param_probs=self.data3["value_cond_param_probs"],
            modellable_params=set(),
            use_geo_mean=False,
        )
        self.assertEqual(actual, 1)
        actual = cmds_params_values.compute_prob_setofparams_given_cmd(
            cmd="Set-User",
            params_with_vals={"Identity": "blah"},
            param_cond_cmd_probs=self.data3["param_cond_cmd_probs"],
            value_cond_param_probs=self.data3["value_cond_param_probs"],
            modellable_params=set(),
            use_geo_mean=False,
        )
        self.assertEqual(actual, 0.27777777777777785)

        actual = cmds_params_values.compute_prob_setofparams_given_cmd(
            cmd="Set-User",
            params_with_vals={"Identity": "blah"},
            param_cond_cmd_probs=self.data3["param_cond_cmd_probs"],
            value_cond_param_probs=self.data3["value_cond_param_probs"],
            modellable_params={"Identity"},
            use_geo_mean=False,
        )
        self.assertEqual(actual, 0.20833333333333334)

    def test_compute_likelihood_window(self):
        actual = cmds_params_values.compute_likelihood_window(
            window=[],
            prior_probs=self.data3["prior_probs"],
            trans_probs=self.data3["trans_probs"],
            param_cond_cmd_probs=self.data3["param_cond_cmd_probs"],
            value_cond_param_probs=self.data3["value_cond_param_probs"],
            modellable_params=set(),
            use_start_token=False,
            use_end_token=False,
            start_token=START_TOKEN,
            end_token=END_TOKEN,
        )
        self.assertTrue(np.isnan(actual))

        actual = cmds_params_values.compute_likelihood_window(
            window=[Cmd("Set-User", {"Identity": "blah"})],
            prior_probs=self.data3["prior_probs"],
            trans_probs=self.data3["trans_probs"],
            param_cond_cmd_probs=self.data3["param_cond_cmd_probs"],
            value_cond_param_probs=self.data3["value_cond_param_probs"],
            modellable_params=set(),
            use_start_token=False,
            use_end_token=False,
            start_token=START_TOKEN,
            end_token=END_TOKEN,
        )
        self.assertEqual(actual, 0.2372647055253857)

        actual = cmds_params_values.compute_likelihood_window(
            window=[Cmd("Set-User", {"Identity": "blah"})],
            prior_probs=self.data3["prior_probs"],
            trans_probs=self.data3["trans_probs"],
            param_cond_cmd_probs=self.data3["param_cond_cmd_probs"],
            value_cond_param_probs=self.data3["value_cond_param_probs"],
            modellable_params={"Identity"},
            use_start_token=False,
            use_end_token=False,
            start_token=START_TOKEN,
            end_token=END_TOKEN,
        )
        self.assertEqual(actual, 0.24567275541946082)

        actual = cmds_params_values.compute_likelihood_window(
            window=[Cmd("Set-User", {"Identity": "blah"})],
            prior_probs=self.data3["prior_probs"],
            trans_probs=self.data3["trans_probs"],
            param_cond_cmd_probs=self.data3["param_cond_cmd_probs"],
            value_cond_param_probs=self.data3["value_cond_param_probs"],
            modellable_params=set(),
            use_start_token=True,
            use_end_token=False,
            start_token=START_TOKEN,
            end_token=END_TOKEN,
        )
        self.assertEqual(actual, 0.3262389700974053)

        actual = cmds_params_values.compute_likelihood_window(
            window=[Cmd("Set-User", {"Identity": "blah"})],
            prior_probs=self.data3["prior_probs"],
            trans_probs=self.data3["trans_probs"],
            param_cond_cmd_probs=self.data3["param_cond_cmd_probs"],
            value_cond_param_probs=self.data3["value_cond_param_probs"],
            modellable_params=set(),
            use_start_token=False,
            use_end_token=True,
            start_token=START_TOKEN,
            end_token=END_TOKEN,
        )
        self.assertEqual(actual, 0.09490588221015428)

    def test_compute_likelihood_windows_in_session(self):
        actual = cmds_params_values.compute_likelihood_windows_in_session(
            session=[],
            prior_probs=self.data3["prior_probs"],
            trans_probs=self.data3["trans_probs"],
            param_cond_cmd_probs=self.data3["param_cond_cmd_probs"],
            value_cond_param_probs=self.data3["value_cond_param_probs"],
            modellable_params=set(),
            window_len=1,
            use_start_end_tokens=False,
            start_token=START_TOKEN,
            end_token=END_TOKEN,
            use_geo_mean=False,
        )
        self.assertListEqual(actual, [])

        actual = cmds_params_values.compute_likelihood_windows_in_session(
            session=[],
            prior_probs=self.data3["prior_probs"],
            trans_probs=self.data3["trans_probs"],
            param_cond_cmd_probs=self.data3["param_cond_cmd_probs"],
            value_cond_param_probs=self.data3["value_cond_param_probs"],
            modellable_params=set(),
            window_len=1,
            use_start_end_tokens=True,
            start_token=START_TOKEN,
            end_token=END_TOKEN,
            use_geo_mean=False,
        )
        self.assertListEqual(actual, [0.25])

        actual = cmds_params_values.compute_likelihood_windows_in_session(
            session=[Cmd("Set-User", {"Identity": "blah"})],
            prior_probs=self.data3["prior_probs"],
            trans_probs=self.data3["trans_probs"],
            param_cond_cmd_probs=self.data3["param_cond_cmd_probs"],
            value_cond_param_probs=self.data3["value_cond_param_probs"],
            modellable_params=set(),
            window_len=1,
            use_start_end_tokens=False,
            start_token=START_TOKEN,
            end_token=END_TOKEN,
            use_geo_mean=False,
        )
        self.assertListEqual(actual, [0.2372647055253857])

    def test_rarest_window_session(self):
        actual = cmds_params_values.rarest_window_session(
            session=[],
            prior_probs=self.data3["prior_probs"],
            trans_probs=self.data3["trans_probs"],
            param_cond_cmd_probs=self.data3["param_cond_cmd_probs"],
            value_cond_param_probs=self.data3["value_cond_param_probs"],
            modellable_params=set(),
            window_len=1,
            use_start_end_tokens=False,
            start_token=START_TOKEN,
            end_token=END_TOKEN,
            use_geo_mean=False,
        )
        self.assertListEqual(actual[0], [])
        self.assertTrue(np.isnan(actual[1]))

        actual = cmds_params_values.rarest_window_session(
            session=[],
            prior_probs=self.data3["prior_probs"],
            trans_probs=self.data3["trans_probs"],
            param_cond_cmd_probs=self.data3["param_cond_cmd_probs"],
            value_cond_param_probs=self.data3["value_cond_param_probs"],
            modellable_params=set(),
            window_len=1,
            use_start_end_tokens=True,
            start_token=START_TOKEN,
            end_token=END_TOKEN,
            use_geo_mean=False,
        )
        self.assertListEqual(actual[0], [])
        self.assertEqual(actual[1], 0.25)

        actual = cmds_params_values.rarest_window_session(
            session=[Cmd("Set-User", {"City": "york"}), Cmd("drfjh", {})],
            prior_probs=self.data3["prior_probs"],
            trans_probs=self.data3["trans_probs"],
            param_cond_cmd_probs=self.data3["param_cond_cmd_probs"],
            value_cond_param_probs=self.data3["value_cond_param_probs"],
            modellable_params=set(),
            window_len=3,
            use_start_end_tokens=False,
            start_token=START_TOKEN,
            end_token=END_TOKEN,
            use_geo_mean=False,
        )

        self.assertListEqual(actual[0], [])
        self.assertTrue(np.isnan(actual[1]))

        actual = cmds_params_values.rarest_window_session(
            session=[Cmd("Set-User", {"City": "york"}), Cmd("drfjh", {})],
            prior_probs=self.data3["prior_probs"],
            trans_probs=self.data3["trans_probs"],
            param_cond_cmd_probs=self.data3["param_cond_cmd_probs"],
            value_cond_param_probs=self.data3["value_cond_param_probs"],
            modellable_params=set(),
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
