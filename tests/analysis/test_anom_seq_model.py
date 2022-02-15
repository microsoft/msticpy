import unittest

from msticpy.analysis.anomalous_sequence.model import Model
from msticpy.analysis.anomalous_sequence.utils.data_structures import Cmd
from msticpy.common.exceptions import MsticpyException


class TestModel(unittest.TestCase):
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

    def tearDown(self) -> None:
        self.sessions1 = None
        self.sessions2 = None
        self.sessions3 = None

    def test__init__(self):
        self.assertRaises(MsticpyException, lambda: Model(sessions=[]))
        self.assertRaises(MsticpyException, lambda: Model(sessions=[[]]))
        self.assertRaises(MsticpyException, lambda: Model(sessions=["Set-User"]))
        self.assertRaises(MsticpyException, lambda: Model(sessions=[["Set-User"], []]))
        self.assertRaises(
            Exception, lambda: Model(sessions=[[{"Set-User": {"Identity"}}]])
        )

    def test_train(self):
        model = Model(sessions=self.sessions1)
        model.train()
        self.assertTrue(model.seq1_counts is not None)
        self.assertTrue(model.seq2_counts is not None)
        self.assertTrue(model.prior_probs is not None)
        self.assertTrue(model.trans_probs is not None)
        self.assertTrue(model.param_counts is None)
        self.assertTrue(model.cmd_param_counts is None)
        self.assertTrue(model.param_probs is None)
        self.assertTrue(model.param_cond_cmd_probs is None)
        self.assertTrue(model.value_counts is None)
        self.assertTrue(model.param_value_counts is None)
        self.assertTrue(model.value_probs is None)
        self.assertTrue(model.value_cond_param_probs is None)
        self.assertTrue(model.modellable_params is None)

        model = Model(sessions=self.sessions2)
        model.train()
        self.assertTrue(model.seq1_counts is not None)
        self.assertTrue(model.seq2_counts is not None)
        self.assertTrue(model.prior_probs is not None)
        self.assertTrue(model.trans_probs is not None)
        self.assertTrue(model.param_counts is not None)
        self.assertTrue(model.cmd_param_counts is not None)
        self.assertTrue(model.param_probs is not None)
        self.assertTrue(model.param_cond_cmd_probs is not None)
        self.assertTrue(model.value_counts is None)
        self.assertTrue(model.param_value_counts is None)
        self.assertTrue(model.value_probs is None)
        self.assertTrue(model.value_cond_param_probs is None)
        self.assertTrue(model.modellable_params is None)

        model = Model(sessions=self.sessions3)
        model.train()
        self.assertTrue(model.seq1_counts is not None)
        self.assertTrue(model.seq2_counts is not None)
        self.assertTrue(model.prior_probs is not None)
        self.assertTrue(model.trans_probs is not None)
        self.assertTrue(model.param_counts is not None)
        self.assertTrue(model.cmd_param_counts is not None)
        self.assertTrue(model.param_probs is not None)
        self.assertTrue(model.param_cond_cmd_probs is not None)
        self.assertTrue(model.value_counts is not None)
        self.assertTrue(model.param_value_counts is not None)
        self.assertTrue(model.value_probs is not None)
        self.assertTrue(model.value_cond_param_probs is not None)
        self.assertTrue(model.modellable_params is not None)

    def test_compute_setof_params_cond_cmd(self):
        model = Model(sessions=self.sessions1)
        model.train()
        self.assertRaises(
            Exception, lambda: model.compute_setof_params_cond_cmd(use_geo_mean=False)
        )

        model = Model(sessions=self.sessions2)
        model.train()
        model.compute_setof_params_cond_cmd(use_geo_mean=False)
        self.assertTrue(len(model.set_params_cond_cmd_probs) > 0)

        model = Model(sessions=self.sessions3)
        model.train()
        model.compute_setof_params_cond_cmd(use_geo_mean=False)
        self.assertTrue(len(model.set_params_cond_cmd_probs) > 0)

        model = Model(sessions=self.sessions3)
        self.assertRaises(
            Exception, lambda: model.compute_setof_params_cond_cmd(use_geo_mean=False)
        )

    def test_compute_scores(self):
        model = Model(sessions=self.sessions3)
        self.assertRaises(
            MsticpyException, lambda: model.compute_scores(use_start_end_tokens=True)
        )

        model.train()
        model.compute_scores(use_start_end_tokens=True)
        self.assertTrue(model.session_likelihoods is not None)
        self.assertTrue(model.session_geomean_likelihoods is not None)
        self.assertTrue(2 in model.rare_window_likelihoods)
        self.assertTrue(3 in model.rare_window_likelihoods)
        self.assertTrue(2 in model.rare_windows)
        self.assertTrue(3 in model.rare_windows)

    def test_compute_likelihoods_of_sessions(self):
        model = Model(sessions=self.sessions3)
        self.assertRaises(
            MsticpyException,
            lambda: model.compute_likelihoods_of_sessions(use_start_end_tokens=True),
        )

        model.train()
        model.compute_likelihoods_of_sessions(use_start_end_tokens=True)
        self.assertTrue(model.session_likelihoods is not None)

    def test_compute_rarest_windows(self):
        model = Model(sessions=self.sessions2)
        self.assertRaises(
            MsticpyException,
            lambda: model.compute_rarest_windows(
                window_len=3, use_start_end_tokens=True, use_geo_mean=False
            ),
        )

        model.train()
        model.compute_rarest_windows(
            window_len=3, use_start_end_tokens=True, use_geo_mean=False
        )
        self.assertTrue(3 in model.rare_window_likelihoods)
        self.assertTrue(3 in model.rare_windows)

        model = Model(sessions=self.sessions2)
        model.train()
        model.compute_rarest_windows(
            window_len=3, use_start_end_tokens=True, use_geo_mean=True
        )
        self.assertTrue(3 in model.rare_window_likelihoods_geo)
        self.assertTrue(3 in model.rare_windows_geo)


if __name__ == "__main__":
    unittest.main()
