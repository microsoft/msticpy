# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Module for Model class for modelling sessions data."""

from collections import defaultdict
from typing import Dict, List, Union

from ...common.exceptions import MsticpyException
from .utils import cmds_only, cmds_params_only, cmds_params_values, probabilities
from .utils.data_structures import Cmd


# pylint: disable=too-many-instance-attributes
# pylint: disable=too-few-public-methods
class Model:
    """Class for modelling sessions data."""

    def __init__(
        self, sessions: List[List[Union[str, Cmd]]], modellable_params: set = None
    ):
        """
        Instantiate the Model class.

        This Model class can be used to model sessions, where each
        session is a sequence of commands. We use a sliding window
        approach to calculate the rarest part of each session. We
        can view the sessions in ascending order of this metric to
        see if the top sessions are anomalous/malicious.

        Parameters
        ----------
        sessions: List[List[Union[str, Cmd]]]
            list of sessions, where each session is a list of either
            strings or a list of the Cmd datatype.

            The Cmd datatype should have "name" and "params" as attributes
            where "name" is the name of the command (string) and "params"
            is either a set of accompanying params or a dict of
            accompanying params and values.

            examples formats of a session:
            1) ['Set-User', 'Set-Mailbox']
            2) [Cmd(name='Set-User', params={'Identity', 'Force'}),
            Cmd(name='Set-Mailbox', params={'Identity', 'AuditEnabled'})]
            3) [Cmd(name='Set-User',
            params={'Identity': 'blahblah', 'Force': 'true'}),
            Cmd(name='Set-Mailbox',
            params={'Identity': 'blahblah', 'AuditEnabled': 'false'})]

        modellable_params: set, optional
            set of params which you deem to have categorical values which are suitable
            for modelling.
            Note this argument will only have an effect if your sessions include commands,
            params and values. If your sessions include commands, params and values and
            this argument is not set, then some rough heuristics will be used to determine
            which params have values which are suitable for modelling.

        """
        if not isinstance(sessions, list):
            raise MsticpyException("`sessions` should be a list")
        if not sessions:
            raise MsticpyException("`sessions` should not be an empty list")
        for i, ses in enumerate(sessions):
            if not isinstance(ses, list):
                raise MsticpyException("each session in `sessions` should be a list")
            if len(ses) == 0:
                raise MsticpyException(
                    f"session at index {i} of `sessions` is empty. Each session "
                    "should contain at least one command"
                )

        self.start_token = "##START##"  # nosec B105
        self.end_token = "##END##"  # nosec B105
        self.unk_token = "##UNK##"  # nosec B105

        self.sessions = sessions
        self.session_type = None
        self._asses_input()

        # non laplace smoothed counts
        self._seq1_counts = None
        self._seq2_counts = None
        self._param_counts = None
        self._cmd_param_counts = None
        self._value_counts = None
        self._param_value_counts = None

        # laplace smoothed counts
        self.seq1_counts = None
        self.seq2_counts = None
        self.param_counts = None
        self.cmd_param_counts = None
        self.value_counts = None
        self.param_value_counts = None

        self.modellable_params = modellable_params

        self.prior_probs = None
        self.trans_probs = None
        self.param_probs = None
        self.param_cond_cmd_probs = None
        self.value_probs = None
        self.value_cond_param_probs = None

        self.set_params_cond_cmd_probs: Dict[str, Dict[str, float]] = {}

        self.session_likelihoods = None
        self.session_geomean_likelihoods = None

        self.rare_windows: Dict[int, list] = {}
        self.rare_window_likelihoods: Dict[int, list] = {}

        self.rare_windows_geo: Dict[int, list] = {}
        self.rare_window_likelihoods_geo: Dict[int, list] = {}

    def train(self):
        """
        Train the model by computing counts and probabilities.

        In particular, computes the counts and probabilities of the commands
        (and possibly the params if provided, and possibly the values if provided)

        """
        self._compute_counts()
        self._laplace_smooth_counts()
        self._compute_probs()

    def compute_scores(self, use_start_end_tokens: bool):
        """
        Compute some likelihood based scores/metrics for each of the sessions.

        In particular, computes the likelihoods and geometric mean of
        the likelihoods for each of the sessions. Also, uses the sliding
        window approach to compute the rarest window likelihoods for each
        of the sessions. It does this for windows of length 2 and 3.

        Note that if we have a session of length k, and we use a sliding
        window of length k+1, then we will end up with np.nan for the
        rarest window likelihood metric for that session.
        However, if `use_start_end_tokens` is set to True, then
        because we will be appending self.end_token to the session,
        the session will be treated as a session of length k+1,
        therefore, we will end up with a non np.nan value for that session.

        Parameters
        ----------
        use_start_end_tokens: bool
            if True, then self.start_token and self.end_token will be
            prepended and appended to each
            of the sessions respectively before the calculations are done.

        """
        if self.prior_probs is None:
            raise MsticpyException(
                "please train the model first before using this method"
            )
        self.compute_likelihoods_of_sessions(use_start_end_tokens=use_start_end_tokens)
        self.compute_geomean_lik_of_sessions()
        self.compute_rarest_windows(
            window_len=2, use_geo_mean=False, use_start_end_tokens=use_start_end_tokens
        )
        self.compute_rarest_windows(
            window_len=3, use_geo_mean=False, use_start_end_tokens=use_start_end_tokens
        )

    def _compute_counts(self):
        """
        Compute all the counts for the model.

        The items we will count depend on the the `session_type` attribute.
        We will compute the individual command and transition command counts.

        If params are provided with the commands, then, in addition,
        we will compute the individual param counts and param conditional
        on the command counts.

        If values are provided with the params, then in addition, we
        will compute the individual value counts and value conditional
        on the param counts. Also, we will use rough heuristics
        to determine which params take categorical values, and hence
        have modellable values.

        """
        if self.session_type is None:
            raise MsticpyException("session_type attribute should not be None")

        if self.session_type == SessionType.cmds_only:
            seq1_counts, seq2_counts = cmds_only.compute_counts(
                sessions=self.sessions,
                start_token=self.start_token,
                end_token=self.end_token,
                unk_token=self.unk_token,
            )
            self._seq1_counts = seq1_counts
            self._seq2_counts = seq2_counts

        elif self.session_type == SessionType.cmds_params_only:
            (
                seq1_counts,
                seq2_counts,
                param_counts,
                cmd_param_counts,
            ) = cmds_params_only.compute_counts(
                sessions=self.sessions,
                start_token=self.start_token,
                end_token=self.end_token,
            )

            self._seq1_counts = seq1_counts
            self._seq2_counts = seq2_counts
            self._param_counts = param_counts
            self._cmd_param_counts = cmd_param_counts

        elif self.session_type == SessionType.cmds_params_values:
            (
                seq1_counts,
                seq2_counts,
                param_counts,
                cmd_param_counts,
                value_counts,
                param_value_counts,
            ) = cmds_params_values.compute_counts(
                sessions=self.sessions,
                start_token=self.start_token,
                end_token=self.end_token,
            )

            if self.modellable_params is None:
                modellable_params = cmds_params_values.get_params_to_model_values(
                    param_counts=param_counts, param_value_counts=param_value_counts
                )
                self.modellable_params = modellable_params

            self._seq1_counts = seq1_counts
            self._seq2_counts = seq2_counts
            self._param_counts = param_counts
            self._cmd_param_counts = cmd_param_counts
            self._value_counts = value_counts
            self._param_value_counts = param_value_counts

    def _laplace_smooth_counts(self):
        """
        Laplace smooth all the counts for the model.

        We do this by adding 1 to all the counts. This is so we shift
        some of the probability mass from the very probable
        commands/params/values to the unseen and very unlikely
        commands/params/values. The `unk_token` means we can handle
        unseen commands, params, values, sequences of commands.

        """
        if self._seq1_counts is None:
            raise MsticpyException("Please run the _compute_counts method first.")

        if self.session_type == SessionType.cmds_only:
            seq1_counts_ls, seq2_counts_ls = cmds_only.laplace_smooth_counts(
                seq1_counts=self._seq1_counts,
                seq2_counts=self._seq2_counts,
                start_token=self.start_token,
                end_token=self.end_token,
                unk_token=self.unk_token,
            )
            self.seq1_counts = seq1_counts_ls
            self.seq2_counts = seq2_counts_ls

        elif self.session_type == SessionType.cmds_params_only:
            (
                seq1_counts_ls,
                seq2_counts_ls,
                param_counts_ls,
                cmd_param_counts_ls,
            ) = cmds_params_only.laplace_smooth_counts(
                seq1_counts=self._seq1_counts,
                seq2_counts=self._seq2_counts,
                param_counts=self._param_counts,
                cmd_param_counts=self._cmd_param_counts,
                start_token=self.start_token,
                end_token=self.end_token,
                unk_token=self.unk_token,
            )

            self.seq1_counts = seq1_counts_ls
            self.seq2_counts = seq2_counts_ls
            self.param_counts = param_counts_ls
            self.cmd_param_counts = cmd_param_counts_ls

        elif self.session_type == SessionType.cmds_params_values:
            (
                seq1_counts_ls,
                seq2_counts_ls,
                param_counts_ls,
                cmd_param_counts_ls,
                value_counts_ls,
                param_value_counts_ls,
            ) = cmds_params_values.laplace_smooth_counts(
                seq1_counts=self._seq1_counts,
                seq2_counts=self._seq2_counts,
                param_counts=self._param_counts,
                cmd_param_counts=self._cmd_param_counts,
                value_counts=self._value_counts,
                param_value_counts=self._param_value_counts,
                start_token=self.start_token,
                end_token=self.end_token,
                unk_token=self.unk_token,
            )
            self.seq1_counts = seq1_counts_ls
            self.seq2_counts = seq2_counts_ls
            self.param_counts = param_counts_ls
            self.cmd_param_counts = cmd_param_counts_ls
            self.value_counts = value_counts_ls
            self.param_value_counts = param_value_counts_ls

    def _compute_probs(self):
        """
        Compute all the probabilities for the model.

        The probabilities we compute depends on the `session_type` attribute.
        We will compute the individual command and transition
        command probabilities.

        If params are provided with the commands, then, in addition,
        we will compute the individual param probabilities and param
        conditional on the command probabilities.

        If values are provided with the params, then in addition,
        we will compute the individual value probabilities and
        value conditional on the param probabilities.

        """
        self._compute_probs_cmds()
        if self.session_type in [
            SessionType.cmds_params_only,
            SessionType.cmds_params_values,
        ]:
            self._compute_probs_params()
        if self.session_type == SessionType.cmds_params_values:
            self._compute_probs_values()

    def compute_setof_params_cond_cmd(self, use_geo_mean: bool):  # noqa: MC0001
        """
        Compute likelihood of combinations of params conditional on the cmd.

        In particular, go through each command from each session and
        compute the probability of that set of params (and values if provided)
        appearing conditional on the command.

        This can help us to identify unlikely combinations of params
        (and values if provided) for each distinct command.

        Note, this method is only available if each session is a list
        of the Cmd datatype. It will result in an Exception if you
        try and use it when each session is a list of strings.

        Parameters
        ----------
        use_geo_mean: bool
            if True, then the probabilities will be raised to
            the power of (1/K)

            case1: we have only params:
                Then K is the number of distinct params which appeared
                for the given cmd across all the sessions.
            case2: we have params and values:
                Then K is the number of distinct params which appeared
                for the given cmd across all the sessions + the number
                of values which we included in the modelling for this cmd.

        """
        if self.param_probs is None:
            raise MsticpyException(
                "please train the model first before using this method"
            )

        if self.session_type is None:
            raise MsticpyException("session_type attribute should not be None")

        if self.session_type == SessionType.cmds_only:
            raise MsticpyException(
                'this method is not available for your type of input data "sessions"'
            )
        if self.session_type == SessionType.cmds_params_only:
            result = defaultdict(lambda: defaultdict(lambda: 0))
            for ses in self.sessions:
                for cmd in ses:
                    c_name = cmd.name
                    params = cmd.params
                    prob = cmds_params_only.compute_prob_setofparams_given_cmd(
                        cmd=c_name,
                        params=params,
                        param_cond_cmd_probs=self.param_cond_cmd_probs,
                        use_geo_mean=use_geo_mean,
                    )
                    result[c_name][tuple(params)] = prob
            self.set_params_cond_cmd_probs = result
        else:
            result = defaultdict(lambda: defaultdict(lambda: 0))
            for ses in self.sessions:
                for cmd in ses:
                    c_name = cmd.name
                    params = cmd.params
                    pars = set(cmd.params.keys())
                    intersection_pars = pars.intersection(self.modellable_params)
                    key = set()
                    for par in pars:
                        if par in intersection_pars:
                            key.add(f"{par} --- {params[par]}")
                        else:
                            key.add(par)
                    prob = cmds_params_values.compute_prob_setofparams_given_cmd(
                        cmd=c_name,
                        params_with_vals=params,
                        param_cond_cmd_probs=self.param_cond_cmd_probs,
                        value_cond_param_probs=self.value_cond_param_probs,
                        modellable_params=self.modellable_params,
                        use_geo_mean=use_geo_mean,
                    )
                    result[c_name][tuple(key)] = prob
            self.set_params_cond_cmd_probs = result

    def compute_likelihoods_of_sessions(self, use_start_end_tokens: bool = True):
        """
        Compute the likelihoods for each of the sessions.

        Note: If the lengths (number of commands) of the sessions vary a lot,
        then you may not be able to fairly compare the likelihoods between a
        long session and a short session. This is because longer sessions
        involve multiplying more numbers together which are between 0 and 1.
        Therefore the length of the session will be negatively correlated with
        the likelihoods. If you take the geometric mean of the likelihood, then
        you can compare the likelihoods more fairly across different session
        lengths

        Parameters
        ----------
        use_start_end_tokens: bool
            if True, then `start_token` and `end_token` will be prepended
            and appended to the session respectively before the calculations
            are done

        """
        if self.prior_probs is None:
            raise MsticpyException(
                "please train the model first before using this method"
            )

        result = []

        for sess in self.sessions:
            if self.session_type == SessionType.cmds_only:
                tmp = cmds_only.compute_likelihood_window(
                    window=sess,
                    prior_probs=self.prior_probs,
                    trans_probs=self.trans_probs,
                    use_start_token=use_start_end_tokens,
                    use_end_token=use_start_end_tokens,
                    start_token=self.start_token,
                    end_token=self.end_token,
                )
            elif self.session_type == SessionType.cmds_params_only:
                tmp = cmds_params_only.compute_likelihood_window(
                    window=sess,
                    prior_probs=self.prior_probs,
                    trans_probs=self.trans_probs,
                    param_cond_cmd_probs=self.param_cond_cmd_probs,
                    use_start_token=use_start_end_tokens,
                    use_end_token=use_start_end_tokens,
                    start_token=self.start_token,
                    end_token=self.end_token,
                )
            else:
                tmp = cmds_params_values.compute_likelihood_window(
                    window=sess,
                    prior_probs=self.prior_probs,
                    trans_probs=self.trans_probs,
                    param_cond_cmd_probs=self.param_cond_cmd_probs,
                    value_cond_param_probs=self.value_cond_param_probs,
                    modellable_params=self.modellable_params,
                    use_start_token=use_start_end_tokens,
                    use_end_token=use_start_end_tokens,
                    start_token=self.start_token,
                    end_token=self.end_token,
                )

            result.append(tmp)

        self.session_likelihoods = result

    def compute_geomean_lik_of_sessions(self):
        """
        Compute the geometric mean of the likelihood for each of the sessions.

        This is done by raising the likelihood of the session to the power of
        (1 / k) where k is the length of the session.

        Note: If the lengths (number of commands) of the sessions vary a lot,
        then you may not be able to fairly compare the likelihoods between a
        long session and a short session. This is because longer sessions
        involve multiplying more numbers together which are between 0 and 1.
        Therefore the length of the session will be negatively correlated with
        the likelihoods. If you take the geometric mean of the likelihood, then
        you can compare the likelihoods more fairly across different session
        lengths.

        """
        if self.session_likelihoods is None:
            self.compute_likelihoods_of_sessions()
        result = [
            self.session_likelihoods[idx] ** (1 / len(session))
            for idx, session in enumerate(self.sessions)
        ]

        self.session_geomean_likelihoods = result

    def compute_rarest_windows(
        self,
        window_len: int,
        use_start_end_tokens: bool = True,
        use_geo_mean: bool = False,
    ):
        """
        Find the rarest window and corresponding likelihood for each session.

        In particular, uses a sliding window approach to find the rarest window
        and corresponding likelihood for that window for each session.

        If we have a long session filled with benign activity except for a small
        window of suspicious behaviour, then this approach should be able to
        identity the session as anomalous. This approach should be more
        effective than simply taking the geometric mean of the full session
        likelihood. This is because the small window of suspicious behaviour
        might get averaged out by the majority benign behaviour in the session
        when using the geometric mean approach.

        Note that if we have a session of length k, and we use a sliding window
        of length k+1, then we will end up with np.nan for the rarest window
        likelihood metric for that session. However, if `use_start_end_tokens`
        is set to True, then because we will be appending self.end_token to the
        session, the session will be treated as a session of length k+1,
        therefore, we will end up with a non np.nan value.

        Parameters
        ----------
        window_len: int
            length of sliding window for likelihood calculations
        use_start_end_tokens: bool
            if True, then `start_token` and `end_token` will be prepended
            and appended to each
            session respectively before the calculations are done
        use_geo_mean: bool
            if True, then each of the likelihoods of the sliding windows
            will be raised to the power
            of (1/`window_len`)

        """
        if self.prior_probs is None:
            raise MsticpyException(
                "please train the model first before using this method"
            )

        if self.session_type == SessionType.cmds_only:
            rare_tuples = [
                cmds_only.rarest_window_session(
                    session=ses,
                    prior_probs=self.prior_probs,
                    trans_probs=self.trans_probs,
                    window_len=window_len,
                    use_start_end_tokens=use_start_end_tokens,
                    start_token=self.start_token,
                    end_token=self.end_token,
                    use_geo_mean=use_geo_mean,
                )
                for ses in self.sessions
            ]
        elif self.session_type == SessionType.cmds_params_only:
            rare_tuples = [
                cmds_params_only.rarest_window_session(
                    session=ses,
                    prior_probs=self.prior_probs,
                    trans_probs=self.trans_probs,
                    param_cond_cmd_probs=self.param_cond_cmd_probs,
                    window_len=window_len,
                    use_start_end_tokens=use_start_end_tokens,
                    start_token=self.start_token,
                    end_token=self.end_token,
                    use_geo_mean=use_geo_mean,
                )
                for ses in self.sessions
            ]
        else:
            rare_tuples = [
                cmds_params_values.rarest_window_session(
                    session=ses,
                    prior_probs=self.prior_probs,
                    trans_probs=self.trans_probs,
                    param_cond_cmd_probs=self.param_cond_cmd_probs,
                    value_cond_param_probs=self.value_cond_param_probs,
                    modellable_params=self.modellable_params,
                    window_len=window_len,
                    use_start_end_tokens=use_start_end_tokens,
                    start_token=self.start_token,
                    end_token=self.end_token,
                    use_geo_mean=use_geo_mean,
                )
                for ses in self.sessions
            ]

        if use_geo_mean:
            self.rare_windows_geo[window_len] = [rare[0] for rare in rare_tuples]
            self.rare_window_likelihoods_geo[window_len] = [
                rare[1] for rare in rare_tuples
            ]
        else:
            self.rare_windows[window_len] = [rare[0] for rare in rare_tuples]
            self.rare_window_likelihoods[window_len] = [rare[1] for rare in rare_tuples]

    def _compute_probs_cmds(self):
        """Compute the individual and transition command probabilties."""
        if self.seq1_counts is None:
            raise MsticpyException("seq1_counts attribute should not be None")
        if self.seq2_counts is None:
            raise MsticpyException("seq2_counts attribute should not be None")

        prior_probs, trans_probs = probabilities.compute_cmds_probs(
            seq1_counts=self.seq1_counts,
            seq2_counts=self.seq2_counts,
            unk_token=self.unk_token,
        )

        self.prior_probs = prior_probs
        self.trans_probs = trans_probs

    def _compute_probs_params(self):
        """Compute the individual param probs and param conditional on command probs."""
        if self.param_counts is None:
            raise MsticpyException("param_counts attribute should not be None")
        if self.cmd_param_counts is None:
            raise MsticpyException("cmd_param_counts attribute should not be None")

        param_probs, param_cond_cmd_probs = probabilities.compute_params_probs(
            param_counts=self.param_counts,
            cmd_param_counts=self.cmd_param_counts,
            seq1_counts=self.seq1_counts,
            unk_token=self.unk_token,
        )

        self.param_probs = param_probs
        self.param_cond_cmd_probs = param_cond_cmd_probs

    def _compute_probs_values(self):
        """Compute the individual value probs and value conditional on param probs."""
        if self.value_counts is None:
            raise MsticpyException("value_counts attribute should not be None")
        if self.param_value_counts is None:
            raise MsticpyException("param_value_counts attribute should not be None")

        value_probs, value_cond_param_probs = probabilities.compute_values_probs(
            value_counts=self.value_counts,
            param_value_counts=self.param_value_counts,
            unk_token=self.unk_token,
        )

        self.value_probs = value_probs
        self.value_cond_param_probs = value_cond_param_probs

    def _asses_input(self):
        """
        Determine what type of sessions we have.

        In particular, assess the input `self.sessions` to see whether each
        session is a list of strings, or list of the Cmd datatype. And if each
        session is a list of the Cmd datatype, it will assess whether the params
        attribute of the Cmd datatype is a set or a dict.

        """
        session = self.sessions[0]
        cmd = session[0]
        if isinstance(cmd, str):
            self.session_type = SessionType.cmds_only
        elif self._check_cmd_type():
            if isinstance(cmd.params, set):
                self.session_type = SessionType.cmds_params_only
            elif isinstance(cmd.params, dict):
                self.session_type = SessionType.cmds_params_values
            else:
                raise MsticpyException(
                    "Params attribute of Cmd data structure should "
                    + "be either a set or a dict"
                )
        else:
            raise MsticpyException(
                "Each element of 'sessions' should be a list of either "
                + "strings, or Cmd data types"
            )

    def _check_cmd_type(self):
        """Check whether the Cmd datatype has the expected attributes."""
        session = self.sessions[0]
        cmd = session[0]
        if "name" in dir(cmd) and "params" in dir(cmd):
            return True
        return False


class SessionType:
    """Class for storing the types of accepted sessions."""

    cmds_only = "cmds_only"
    cmds_params_only = "cmds_params_only"
    cmds_params_values = "cmds_params_values"
