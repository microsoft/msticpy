import unittest
from collections import defaultdict

from msticpy.analysis.anomalous_sequence.utils import laplace_smooth
from msticpy.analysis.anomalous_sequence.utils.data_structures import Cmd

START_TOKEN = "##START##"  # nosec B105
END_TOKEN = "##END##"  # nosec B105
UNK_TOKEN = "##UNK##"  # nosec B105


class TestLaplaceSmooth(unittest.TestCase):
    def setUp(self):
        self.data1 = {}
        self.data2 = {}
        self.data3 = {}

        # populate data1
        self.data1["sessions"] = []
        self.data1["seq1_counts"] = defaultdict(lambda: 0)
        self.data1["seq1_counts_ls"] = {UNK_TOKEN: 2}
        self.data1["seq2_counts"] = defaultdict(lambda: defaultdict(lambda: 0))
        self.data1["seq2_counts_ls"] = {UNK_TOKEN: {UNK_TOKEN: 1}}
        self.data1["param_counts"] = defaultdict(lambda: 0)
        self.data1["param_counts_ls"] = {UNK_TOKEN: 1}
        self.data1["cmd_param_counts"] = defaultdict(lambda: defaultdict(lambda: 0))
        self.data1["cmd_param_counts_ls"] = {UNK_TOKEN: {UNK_TOKEN: 1}}
        self.data1["value_counts"] = defaultdict(lambda: 0)
        self.data1["value_counts_ls"] = {UNK_TOKEN: 1}
        self.data1["param_value_counts"] = defaultdict(lambda: defaultdict(lambda: 0))
        self.data1["param_value_counts_ls"] = {UNK_TOKEN: {UNK_TOKEN: 1}}
        self.data1["cmds"] = [UNK_TOKEN]
        self.data1["params"] = [UNK_TOKEN]

        # populate data2
        self.data2["sessions"] = [[]]
        self.data2["seq1_counts"] = defaultdict(
            lambda: 0, {START_TOKEN: 1, END_TOKEN: 1}
        )
        self.data2["seq1_counts_ls"] = {UNK_TOKEN: 4, START_TOKEN: 3, END_TOKEN: 3}
        self.data2["seq2_counts"] = defaultdict(lambda: defaultdict(lambda: 0))
        self.data2["seq2_counts"][START_TOKEN][END_TOKEN] = 1
        self.data2["seq2_counts_ls"] = {
            START_TOKEN: {END_TOKEN: 2, UNK_TOKEN: 1},
            UNK_TOKEN: {END_TOKEN: 1, UNK_TOKEN: 1},
        }
        self.data2["param_counts"] = defaultdict(lambda: 0)
        self.data2["param_counts_ls"] = {UNK_TOKEN: 3}
        self.data2["cmd_param_counts"] = defaultdict(lambda: defaultdict(lambda: 0))
        self.data2["cmd_param_counts_ls"] = {
            START_TOKEN: {UNK_TOKEN: 1},
            END_TOKEN: {UNK_TOKEN: 1},
            UNK_TOKEN: {UNK_TOKEN: 1},
        }
        self.data2["value_counts"] = defaultdict(lambda: 0)
        self.data2["value_counts_ls"] = {UNK_TOKEN: 1}
        self.data2["param_value_counts"] = defaultdict(lambda: defaultdict(lambda: 0))
        self.data2["param_value_counts_ls"] = {UNK_TOKEN: {UNK_TOKEN: 1}}
        self.data2["cmds"] = [START_TOKEN, END_TOKEN, UNK_TOKEN]
        self.data2["params"] = [UNK_TOKEN]

        # populate data3
        cmd = "Set-User"
        self.data3["sessions"] = [
            [
                Cmd(name="Set-User", params={"City": "york", "Identity": "blah"}),
                Cmd(name="Set-User", params={"Identity": "blah"}),
            ]
        ]
        self.data3["seq1_counts"] = defaultdict(
            lambda: 0, {START_TOKEN: 1, cmd: 2, END_TOKEN: 1}
        )
        self.data3["seq1_counts_ls"] = {
            UNK_TOKEN: 6,
            START_TOKEN: 4,
            END_TOKEN: 4,
            cmd: 8,
        }
        self.data3["seq2_counts"] = defaultdict(lambda: defaultdict(lambda: 0))
        self.data3["seq2_counts"][START_TOKEN][cmd] = 1
        self.data3["seq2_counts"][cmd][cmd] = 1
        self.data3["seq2_counts"][cmd][END_TOKEN] = 1
        self.data3["seq2_counts_ls"] = {
            START_TOKEN: {END_TOKEN: 1, UNK_TOKEN: 1, cmd: 2},
            UNK_TOKEN: {END_TOKEN: 1, UNK_TOKEN: 1, cmd: 1},
            cmd: {cmd: 2, END_TOKEN: 2, UNK_TOKEN: 1},
        }
        self.data3["param_counts"] = defaultdict(lambda: 0, {"City": 1, "Identity": 2})
        self.data3["param_counts_ls"] = {UNK_TOKEN: 4, "City": 2, "Identity": 3}
        self.data3["cmd_param_counts"] = defaultdict(lambda: defaultdict(lambda: 0))
        self.data3["cmd_param_counts"][cmd]["City"] = 1
        self.data3["cmd_param_counts"][cmd]["Identity"] = 2
        self.data3["cmd_param_counts_ls"] = {
            START_TOKEN: {UNK_TOKEN: 1},
            END_TOKEN: {UNK_TOKEN: 1},
            UNK_TOKEN: {UNK_TOKEN: 1},
            cmd: {"City": 2, "Identity": 3, UNK_TOKEN: 1},
        }
        self.data3["value_counts"] = defaultdict(lambda: 0, {"york": 1, "blah": 2})
        self.data3["value_counts_ls"] = {"york": 2, "blah": 3, UNK_TOKEN: 3}
        self.data3["param_value_counts"] = defaultdict(lambda: defaultdict(lambda: 0))
        self.data3["param_value_counts"]["City"]["york"] = 1
        self.data3["param_value_counts"]["Identity"]["blah"] = 2
        self.data3["param_value_counts_ls"] = {
            "City": {"york": 2, UNK_TOKEN: 1},
            "Identity": {"blah": 3, UNK_TOKEN: 1},
            UNK_TOKEN: {UNK_TOKEN: 1},
        }
        self.data3["cmds"] = [START_TOKEN, END_TOKEN, UNK_TOKEN, cmd]
        self.data3["params"] = [UNK_TOKEN, "City", "Identity"]

    def tearDown(self):
        self.data1 = None
        self.data2 = None
        self.data3 = None

    def test_laplace_smooth_cmd_counts(self):
        seq1_ls_actual, seq2_ls_actual = laplace_smooth.laplace_smooth_cmd_counts(
            seq1_counts=self.data1["seq1_counts"],
            seq2_counts=self.data1["seq2_counts"],
            start_token=START_TOKEN,
            end_token=END_TOKEN,
            unk_token=UNK_TOKEN,
        )
        self.assertDictEqual(seq1_ls_actual, self.data1["seq1_counts_ls"])
        self.assertDictEqual(seq2_ls_actual, self.data1["seq2_counts_ls"])

        seq1_ls_actual, seq2_ls_actual = laplace_smooth.laplace_smooth_cmd_counts(
            seq1_counts=self.data2["seq1_counts"],
            seq2_counts=self.data2["seq2_counts"],
            start_token=START_TOKEN,
            end_token=END_TOKEN,
            unk_token=UNK_TOKEN,
        )
        self.assertDictEqual(seq1_ls_actual, self.data2["seq1_counts_ls"])
        self.assertDictEqual(seq2_ls_actual, self.data2["seq2_counts_ls"])

        seq1_ls_actual, seq2_ls_actual = laplace_smooth.laplace_smooth_cmd_counts(
            seq1_counts=self.data3["seq1_counts"],
            seq2_counts=self.data3["seq2_counts"],
            start_token=START_TOKEN,
            end_token=END_TOKEN,
            unk_token=UNK_TOKEN,
        )
        self.assertDictEqual(seq1_ls_actual, self.data3["seq1_counts_ls"])
        self.assertDictEqual(seq2_ls_actual, self.data3["seq2_counts_ls"])

    def test_laplace_smooth_param_counts(self):
        (
            param_ls_actual,
            cmd_param_ls_actual,
        ) = laplace_smooth.laplace_smooth_param_counts(
            cmds=self.data1["cmds"],
            param_counts=self.data1["param_counts"],
            cmd_param_counts=self.data1["cmd_param_counts"],
            unk_token=UNK_TOKEN,
        )
        self.assertDictEqual(param_ls_actual, self.data1["param_counts_ls"])
        self.assertDictEqual(cmd_param_ls_actual, self.data1["cmd_param_counts_ls"])

        (
            param_ls_actual,
            cmd_param_ls_actual,
        ) = laplace_smooth.laplace_smooth_param_counts(
            cmds=self.data2["cmds"],
            param_counts=self.data2["param_counts"],
            cmd_param_counts=self.data2["cmd_param_counts"],
            unk_token=UNK_TOKEN,
        )
        self.assertDictEqual(param_ls_actual, self.data2["param_counts_ls"])
        self.assertDictEqual(cmd_param_ls_actual, self.data2["cmd_param_counts_ls"])

        (
            param_ls_actual,
            cmd_param_ls_actual,
        ) = laplace_smooth.laplace_smooth_param_counts(
            cmds=self.data3["cmds"],
            param_counts=self.data3["param_counts"],
            cmd_param_counts=self.data3["cmd_param_counts"],
            unk_token=UNK_TOKEN,
        )
        self.assertDictEqual(param_ls_actual, self.data3["param_counts_ls"])
        self.assertDictEqual(cmd_param_ls_actual, self.data3["cmd_param_counts_ls"])

    def test_laplace_smooth_value_counts(self):
        (
            values_ls_actual,
            param_value_ls_actual,
        ) = laplace_smooth.laplace_smooth_value_counts(
            params=self.data1["params"],
            value_counts=self.data1["value_counts"],
            param_value_counts=self.data1["param_value_counts"],
            unk_token=UNK_TOKEN,
        )
        self.assertDictEqual(values_ls_actual, self.data1["value_counts_ls"])
        self.assertDictEqual(param_value_ls_actual, self.data1["param_value_counts_ls"])

        (
            values_ls_actual,
            param_value_ls_actual,
        ) = laplace_smooth.laplace_smooth_value_counts(
            params=self.data2["params"],
            value_counts=self.data2["value_counts"],
            param_value_counts=self.data2["param_value_counts"],
            unk_token=UNK_TOKEN,
        )
        self.assertDictEqual(values_ls_actual, self.data2["value_counts_ls"])
        self.assertDictEqual(param_value_ls_actual, self.data2["param_value_counts_ls"])

        (
            values_ls_actual,
            param_value_ls_actual,
        ) = laplace_smooth.laplace_smooth_value_counts(
            params=self.data3["params"],
            value_counts=self.data3["value_counts"],
            param_value_counts=self.data3["param_value_counts"],
            unk_token=UNK_TOKEN,
        )
        self.assertDictEqual(values_ls_actual, self.data3["value_counts_ls"])
        self.assertDictEqual(param_value_ls_actual, self.data3["param_value_counts_ls"])


if __name__ == "__main__":
    unittest.main()
