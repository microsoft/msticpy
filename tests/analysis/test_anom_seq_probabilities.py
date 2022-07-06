import unittest

from msticpy.analysis.anomalous_sequence.utils import probabilities
from msticpy.analysis.anomalous_sequence.utils.data_structures import Cmd, StateMatrix

START_TOKEN = "##START##"  # nosec B105
END_TOKEN = "##END##"  # nosec B105
UNK_TOKEN = "##UNK##"  # nosec B105


class TestProbabilities(unittest.TestCase):
    """
    Test probabilities module.

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
        self.data1["param_probs"] = StateMatrix({UNK_TOKEN: 0.5}, UNK_TOKEN)
        self.data1["param_cond_cmd_probs"] = StateMatrix(
            {UNK_TOKEN: {UNK_TOKEN: 0.5}}, UNK_TOKEN
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
        self.data2["param_probs"] = StateMatrix({UNK_TOKEN: 0.3}, UNK_TOKEN)
        self.data2["param_cond_cmd_probs"] = StateMatrix(
            {
                START_TOKEN: {UNK_TOKEN: 0.3333333333333333},
                END_TOKEN: {UNK_TOKEN: 0.3333333333333333},
                UNK_TOKEN: {UNK_TOKEN: 0.25},
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

    def test_compute_cmds_probs(self):
        seq1_actual, seq2_actual = probabilities.compute_cmds_probs(
            seq1_counts=self.data1["seq1_counts"],
            seq2_counts=self.data1["seq2_counts"],
            unk_token=UNK_TOKEN,
        )
        self.assertDictEqual(seq1_actual, self.data1["prior_probs"])
        self.assertDictEqual(seq2_actual, self.data1["trans_probs"])

        seq1_actual, seq2_actual = probabilities.compute_cmds_probs(
            seq1_counts=self.data2["seq1_counts"],
            seq2_counts=self.data2["seq2_counts"],
            unk_token=UNK_TOKEN,
        )
        self.assertDictEqual(seq1_actual, self.data2["prior_probs"])
        self.assertDictEqual(seq2_actual, self.data2["trans_probs"])

        seq1_actual, seq2_actual = probabilities.compute_cmds_probs(
            seq1_counts=self.data3["seq1_counts"],
            seq2_counts=self.data3["seq2_counts"],
            unk_token=UNK_TOKEN,
        )
        self.assertDictEqual(seq1_actual, self.data3["prior_probs"])
        self.assertDictEqual(seq2_actual, self.data3["trans_probs"])

    def test_compute_params_probs(self):
        param_actual, param_cond_cmd_actual = probabilities.compute_params_probs(
            param_counts=self.data1["param_counts"],
            cmd_param_counts=self.data1["cmd_param_counts"],
            seq1_counts=self.data1["seq1_counts"],
            unk_token=UNK_TOKEN,
        )
        self.assertDictEqual(param_actual, self.data1["param_probs"])
        self.assertDictEqual(param_cond_cmd_actual, self.data1["param_cond_cmd_probs"])

        param_actual, param_cond_cmd_actual = probabilities.compute_params_probs(
            param_counts=self.data2["param_counts"],
            cmd_param_counts=self.data2["cmd_param_counts"],
            seq1_counts=self.data2["seq1_counts"],
            unk_token=UNK_TOKEN,
        )
        self.assertDictEqual(param_actual, self.data2["param_probs"])
        self.assertDictEqual(param_cond_cmd_actual, self.data2["param_cond_cmd_probs"])

        param_actual, param_cond_cmd_actual = probabilities.compute_params_probs(
            param_counts=self.data3["param_counts"],
            cmd_param_counts=self.data3["cmd_param_counts"],
            seq1_counts=self.data3["seq1_counts"],
            unk_token=UNK_TOKEN,
        )
        self.assertDictEqual(param_actual, self.data3["param_probs"])
        self.assertDictEqual(param_cond_cmd_actual, self.data3["param_cond_cmd_probs"])

    def test_compute_values_probs(self):
        value_actual, value_cond_param_actual = probabilities.compute_values_probs(
            value_counts=self.data1["value_counts"],
            param_value_counts=self.data1["param_value_counts"],
            unk_token=UNK_TOKEN,
        )
        self.assertDictEqual(value_actual, self.data1["value_probs"])
        self.assertDictEqual(
            value_cond_param_actual, self.data1["value_cond_param_probs"]
        )

        value_actual, value_cond_param_actual = probabilities.compute_values_probs(
            value_counts=self.data2["value_counts"],
            param_value_counts=self.data2["param_value_counts"],
            unk_token=UNK_TOKEN,
        )
        self.assertDictEqual(value_actual, self.data2["value_probs"])
        self.assertDictEqual(
            value_cond_param_actual, self.data2["value_cond_param_probs"]
        )

        value_actual, value_cond_param_actual = probabilities.compute_values_probs(
            value_counts=self.data3["value_counts"],
            param_value_counts=self.data3["param_value_counts"],
            unk_token=UNK_TOKEN,
        )
        self.assertDictEqual(value_actual, self.data3["value_probs"])
        self.assertDictEqual(
            value_cond_param_actual, self.data3["value_cond_param_probs"]
        )


if __name__ == "__main__":
    unittest.main()
