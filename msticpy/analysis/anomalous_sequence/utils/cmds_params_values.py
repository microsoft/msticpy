# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
Helper module for computations when modelling sessions.

In particular, this module is for when each session is a list of the
Cmd datatype with the params attribute set to a dictionary of
accompanying params and values.
"""

import copy
from collections import defaultdict
from typing import DefaultDict, List, Tuple, Union

import numpy as np

from ....common.exceptions import MsticpyException
from ..utils.data_structures import Cmd, StateMatrix
from ..utils.laplace_smooth import (
    laplace_smooth_cmd_counts,
    laplace_smooth_param_counts,
    laplace_smooth_value_counts,
)


# pylint: disable=too-many-locals, too-many-branches
def compute_counts(  # noqa MC0001  # nosec
    sessions: List[List[Cmd]], start_token: str, end_token: str
) -> Tuple[
    DefaultDict[str, int],
    DefaultDict[str, DefaultDict[str, int]],
    DefaultDict[str, int],
    DefaultDict[str, DefaultDict[str, int]],
    DefaultDict[str, int],
    DefaultDict[str, DefaultDict[str, int]],
]:
    """
    Compute the training counts for the sessions.

    In particular, computes counts of individual commands and of sequences
    of two commands. It also computes the counts of individual params as
    well as counts of params conditional on the command. It also computes
    the counts of individual values as well as counts of values conditional
    on the param.

    Parameters
    ----------
    sessions: List[List[Cmd]]
        each session is a list of the Cmd datatype.
        Where the Cmd datatype has a name attribute (command name) and
        a params attribute (dict with the params and values associated
        with the command)
        an example session::

            [
                Cmd(
                    name='Set-User',
                    params={'Identity': 'blahblah', 'Force': 'true'}
                ),
                Cmd(
                    name='Set-Mailbox',
                    params={'Identity': 'blahblah', 'AuditEnabled': 'false'}
                )
            ]

    start_token: str
        dummy command to signify the start of a session (e.g. "##START##")
    end_token: str
        dummy command to signify the end of a session (e.g. "##END##")

    Returns
    -------
    tuple of counts:
        individual command counts,
        sequence command (length 2) counts,
        individual param counts,
        param conditional on command counts
        individual value counts,
        value conditional on param counts

    """
    seq1_counts: DefaultDict[str, int] = defaultdict(lambda: 0)
    seq2_counts: DefaultDict[str, DefaultDict[str, int]] = defaultdict(
        lambda: defaultdict(lambda: 0)
    )

    param_counts: DefaultDict[str, int] = defaultdict(lambda: 0)
    cmd_param_counts: DefaultDict[str, DefaultDict[str, int]] = defaultdict(
        lambda: defaultdict(lambda: 0)
    )

    value_counts: DefaultDict[str, int] = defaultdict(lambda: 0)
    param_value_counts: DefaultDict[str, DefaultDict[str, int]] = defaultdict(
        lambda: defaultdict(lambda: 0)
    )

    for session in sessions:
        prev = start_token
        seq1_counts[prev] += 1
        for cmd in session:
            seq1_counts[cmd.name] += 1
            seq2_counts[prev][cmd.name] += 1
            prev = cmd.name
            pars = cmd.params
            if isinstance(pars, set):
                pars = dict.fromkeys(pars)
            for par, val in pars.items():
                param_counts[par] += 1
                cmd_param_counts[cmd.name][par] += 1
                if val:
                    value_counts[val] += 1
                    param_value_counts[par][val] += 1
        seq2_counts[prev][end_token] += 1
        seq1_counts[end_token] += 1

    return (
        seq1_counts,
        seq2_counts,
        param_counts,
        cmd_param_counts,
        value_counts,
        param_value_counts,
    )


# pylint: disable=too-many-arguments
def laplace_smooth_counts(
    seq1_counts: DefaultDict[str, int],
    seq2_counts: DefaultDict[str, DefaultDict[str, int]],
    param_counts: DefaultDict[str, int],
    cmd_param_counts: DefaultDict[str, DefaultDict[str, int]],
    value_counts: DefaultDict[str, int],
    param_value_counts: DefaultDict[str, DefaultDict[str, int]],
    start_token: str,
    end_token: str,
    unk_token: str,
) -> Tuple[
    StateMatrix, StateMatrix, StateMatrix, StateMatrix, StateMatrix, StateMatrix
]:
    """
    Laplace smoothing is applied to the counts.

    We do this by adding 1 to each of the counts.
    This is so we shift some of the probability mass from the very probable
    commands/params/values to the unseen and very unlikely commands/params/values.
    The `unk_token` means we can handle unseen commands, params, values, sequences of
    commands.

    Parameters
    ----------
    seq1_counts: DefaultDict[str, int]
        individual command counts
    seq2_counts: DefaultDict[str, DefaultDict[str, int]]
        sequence command (length 2) counts
    param_counts: DefaultDict[str, int]
        individual param counts
    cmd_param_counts: DefaultDict[str, DefaultDict[str, int]]
        param conditional on command counts
    value_counts: DefaultDict[str, int]
        individual value counts
    param_value_counts: DefaultDict[str, DefaultDict[str, int]]
        value conditional on param counts
    start_token: str
        dummy command to signify the start of a session (e.g. "##START##")
    end_token: str
        dummy command to signify the end of a session (e.g. "##END##")
    unk_token: str
        dummy command to signify an unseen command (e.g. "##UNK##")

    Returns
    -------
    tuple of StateMatrix counts:
        individual command counts,
        sequence command (length 2) counts,
        individual param counts,
        param conditional on command counts
        individual value counts,
        value conditional on param counts

    """
    cmds: List[str] = list(seq1_counts.keys()) + [unk_token]

    # apply laplace smoothing to the cmds
    seq1_counts_ls, seq2_counts_ls = laplace_smooth_cmd_counts(
        seq1_counts=copy.deepcopy(seq1_counts),
        seq2_counts=copy.deepcopy(seq2_counts),
        start_token=start_token,
        end_token=end_token,
        unk_token=unk_token,
    )

    params: List[str] = list(param_counts.keys()) + [unk_token]

    # apply laplace smoothing to the params
    param_counts_ls, cmd_param_counts_ls = laplace_smooth_param_counts(
        cmds=cmds,
        param_counts=copy.deepcopy(param_counts),
        cmd_param_counts=copy.deepcopy(cmd_param_counts),
        unk_token=unk_token,
    )

    # apply laplace smoothing for the values
    value_counts_ls, param_value_counts_ls = laplace_smooth_value_counts(
        params=params,
        value_counts=copy.deepcopy(value_counts),
        param_value_counts=copy.deepcopy(param_value_counts),
        unk_token=unk_token,
    )

    seq1_counts_sm = StateMatrix(states=seq1_counts_ls, unk_token=unk_token)
    seq2_counts_sm = StateMatrix(states=seq2_counts_ls, unk_token=unk_token)
    param_counts_sm = StateMatrix(states=param_counts_ls, unk_token=unk_token)
    cmd_param_counts_sm = StateMatrix(states=cmd_param_counts_ls, unk_token=unk_token)
    value_counts_sm = StateMatrix(states=value_counts_ls, unk_token=unk_token)
    param_value_counts_sm = StateMatrix(
        states=param_value_counts_ls, unk_token=unk_token
    )

    return (
        seq1_counts_sm,
        seq2_counts_sm,
        param_counts_sm,
        cmd_param_counts_sm,
        value_counts_sm,
        param_value_counts_sm,
    )


def get_params_to_model_values(
    param_counts: Union[StateMatrix, dict], param_value_counts: Union[StateMatrix, dict]
) -> set:
    """
    Determine using heuristics which params take categoricals vs arbitrary strings.

    This function helps us decide which params we should model
    the values of later on.

    Parameters
    ----------
    param_counts: Union[StateMatrix, dict]
        counts of each of the individual params
    param_value_counts: Union[StateMatrix, dict]
        counts of each value conditional on the params

    Returns
    -------
    set of params which have been determined to be categorical

    """
    param_stats = [
        (param, len(vals), param_counts[param], 100 * len(vals) / param_counts[param])
        for param, vals in param_value_counts.items()
    ]

    modellable_params = [
        param[0]
        for param in param_stats
        if param[1] <= 20 <= param[2] and param[3] <= 10
    ]

    return set(modellable_params)


# pylint: disable=too-many-arguments, too-many-branches
def compute_prob_setofparams_given_cmd(
    cmd: str,
    params_with_vals: Union[dict, set],
    param_cond_cmd_probs: Union[StateMatrix, dict],
    value_cond_param_probs: Union[StateMatrix, dict],
    modellable_params: Union[set, list],
    use_geo_mean: bool = True,
) -> float:
    """
    Compute probability of a set of params + values given the cmd.

    Parameters
    ----------
    cmd: str
        name of command (e.g. for Exchange powershell commands: "Set-Mailbox")
    params_with_vals: Union[dict, set]
        dict of accompanying params and values for the cmd
        e.g for Exchange powershell commands::

            {'Identity': 'an_identity' , 'ForwardingEmailAddress': 'email@email.com'}

        If params is set to be a set, then an artificial dictionary will
        be created with the set as the keys and Nones for the values.
    param_cond_cmd_probs: Union[StateMatrix, dict]
        computed probabilities of params conditional on the command
    value_cond_param_probs: Union[StateMatrix, dict]
        computed probabilities of values conditional on the param
    modellable_params: set
        set of params for which we will also include the probabilties
        of their values in the calculation of the likelihood
    use_geo_mean: bool
        if True, then the likelihood will be raised to the power of (1/K)
        where K is the number of distinct params which appeared for the
        given `cmd` across our training set + the number of
        values which we included in the modelling for this cmd.
        Note: some commands may have more params set in general compared
        with other commands. It can be useful to use the geo mean
        so that you can compare this probability across different
        commands with differing number of params.

    Returns
    -------
    computed probability

    """
    pars = params_with_vals.copy()
    if isinstance(pars, set):
        pars = dict.fromkeys(pars)
    if len(pars) == 0:
        return 1.0
    ref_cmd = param_cond_cmd_probs[cmd]
    lik: float = 1
    num = 0
    for param, prob in ref_cmd.items():
        if param in pars:
            lik *= prob
            if param in modellable_params:
                num += 1
                val = pars[param]
                lik *= value_cond_param_probs[param][val]
        else:
            lik *= 1 - prob
    if use_geo_mean:
        k = len(ref_cmd) + num
        if k > 0:
            lik = lik ** (1 / k)

    return lik


# pylint: disable=too-many-locals, too-many-arguments, too-many-branches
def compute_likelihood_window(
    window: List[Cmd],
    prior_probs: Union[StateMatrix, dict],
    trans_probs: Union[StateMatrix, dict],
    param_cond_cmd_probs: Union[StateMatrix, dict],
    value_cond_param_probs: Union[StateMatrix, dict],
    modellable_params: set,
    use_start_token: bool,
    use_end_token: bool,
    start_token: str = None,
    end_token: str = None,
) -> float:
    """
    Compute the likelihood of the input `window`.

    Parameters
    ----------
    window: List[Cmd]
        part or all of a session, where a session is a list the Cmd datatype
        an example session::

            [
                Cmd(name='Set-User', params={'Identity': 'blahblah', 'Force': 'true'}),
                Cmd(name='Set-Mailbox',
                    params={'Identity': 'blahblah', 'AuditEnabled': 'false'})
            ]

    prior_probs: Union[StateMatrix, dict]
        computed probabilities of individual commands
    trans_probs: Union[StateMatrix, dict]
        computed probabilities of sequences of commands (length 2)
    param_cond_cmd_probs: Union[StateMatrix, dict]
        computed probabilities of the params conditional on the commands
    value_cond_param_probs: Union[StateMatrix, dict]
        computed probabilities of the values conditional on the params
    modellable_params: set
        set of params for which we will also include the probabilties
        of their values in the calculation of the likelihood
    use_start_token: bool
        if set to True, the start_token will be prepended to the
        window before the likelihood calculation is done
    use_end_token: bool
        if set to True, the end_token will be appended to the window
        before the likelihood calculation is done
    start_token: str
        dummy command to signify the start of the session (e.g. "##START##")
    end_token: str
        dummy command to signify the end of the session (e.g. "##END##")

    Returns
    -------
    likelihood of the window

    """
    if use_start_token:
        if start_token is None:
            raise MsticpyException(
                "start_token should not be None, when use_start_token is True"
            )
    if use_end_token:
        if end_token is None:
            raise MsticpyException(
                "end_token should not be None, when use_end_token is True"
            )

    w_len = len(window)
    if w_len == 0:
        return np.nan
    prob: float = 1

    cur_cmd = window[0].name
    params = window[0].params
    param_vals_prob = compute_prob_setofparams_given_cmd(
        cmd=cur_cmd,
        params_with_vals=params,
        param_cond_cmd_probs=param_cond_cmd_probs,
        value_cond_param_probs=value_cond_param_probs,
        modellable_params=modellable_params,
        use_geo_mean=True,
    )

    if use_start_token:
        prob *= trans_probs[start_token][cur_cmd] * param_vals_prob
    else:
        prob *= prior_probs[cur_cmd] * param_vals_prob

    for i in range(1, w_len):
        prev, cur = window[i - 1], window[i]
        prev_cmd, cur_cmd = prev.name, cur.name
        cur_par = cur.params
        prob *= trans_probs[prev_cmd][cur_cmd]
        param_vals_prob = compute_prob_setofparams_given_cmd(
            cmd=cur_cmd,
            params_with_vals=cur_par,
            param_cond_cmd_probs=param_cond_cmd_probs,
            value_cond_param_probs=value_cond_param_probs,
            modellable_params=modellable_params,
            use_geo_mean=True,
        )
        prob *= param_vals_prob

    if use_end_token:
        prob *= trans_probs[cur_cmd][end_token]

    return prob


# pylint: disable=too-many-locals, too-many-arguments
def compute_likelihood_windows_in_session(
    session: List[Cmd],
    prior_probs: Union[StateMatrix, dict],
    trans_probs: Union[StateMatrix, dict],
    param_cond_cmd_probs: Union[StateMatrix, dict],
    value_cond_param_probs: Union[StateMatrix, dict],
    modellable_params: set,
    window_len: int,
    use_start_end_tokens: bool,
    start_token: str = None,
    end_token: str = None,
    use_geo_mean: bool = False,
) -> List[float]:
    """
    Compute the likelihoods of a sliding window of `window_len` in the session.

    Parameters
    ----------
    session: List[Cmd]
        list of Cmd datatype
        an example session::

            [
                Cmd(
                    name='Set-User',
                    params={'Identity': 'blahblah', 'Force': 'true'}
                ),
                Cmd(
                    name='Set-Mailbox',
                    params={'Identity': 'blahblah', 'AuditEnabled': 'false'}
                )
            ]

    prior_probs: Union[StateMatrix, dict]
        computed probabilities of individual commands
    trans_probs: Union[StateMatrix, dict]
        computed probabilities of sequences of commands (length 2)
    param_cond_cmd_probs: Union[StateMatrix, dict]
        computed probabilities of the params conditional on the commands
    value_cond_param_probs: Union[StateMatrix, dict]
        computed probabilities of the values conditional on the params
    modellable_params: set
        set of params for which we will also include the probabilties
        of their values in the calculation of the likelihood
    window_len: int
        length of sliding window for likelihood calculations
    use_start_end_tokens: bool
        if True, then `start_token` and `end_token` will be prepended
        and appended to the session respectively before the calculations
        are done
    start_token: str
        dummy command to signify the start of the session (e.g. "##START##")
    end_token: str
        dummy command to signify the end of the session (e.g. "##END##")
    use_geo_mean: bool
        if True, then each of the likelihoods of the sliding windows
        will be raised to the power of (1/`window_len`)

    Returns
    -------
    list of likelihoods

    """
    if use_start_end_tokens and (start_token is None or end_token is None):
        raise MsticpyException(
            "start_token and end_token should not be set to None when "
            "use_start_end_tokens is set to True"
        )

    likelihoods = []
    sess = session.copy()
    if use_start_end_tokens and end_token:
        sess += [Cmd(name=str(end_token), params={})]
    end = len(sess) - window_len
    for i in range(end + 1):
        window = sess[i : i + window_len]  # noqa E203
        use_start = use_start_end_tokens if i == 0 else False
        lik = compute_likelihood_window(
            window=window,
            prior_probs=prior_probs,
            trans_probs=trans_probs,
            param_cond_cmd_probs=param_cond_cmd_probs,
            value_cond_param_probs=value_cond_param_probs,
            modellable_params=modellable_params,
            use_start_token=use_start,
            use_end_token=False,
            start_token=start_token,
            end_token=end_token,
        )
        if use_geo_mean:
            k = window_len
            lik = lik ** (1 / k)
        likelihoods.append(lik)

    return likelihoods


# pylint: disable=too-many-arguments
def rarest_window_session(
    session: List[Cmd],
    prior_probs: Union[StateMatrix, dict],
    trans_probs: Union[StateMatrix, dict],
    param_cond_cmd_probs: Union[StateMatrix, dict],
    value_cond_param_probs: Union[StateMatrix, dict],
    modellable_params: set,
    window_len: int,
    use_start_end_tokens: bool,
    start_token: str,
    end_token: str,
    use_geo_mean: bool = False,
) -> Tuple[List[Cmd], float]:
    """
    Find and compute likelihood of the rarest window of `window_len` in the session.

    Parameters
    ----------
    session: List[Cmd]
        list of Cmd datatype
        an example session::

            [
                Cmd(
                    name='Set-User',
                    params={'Identity': 'blahblah', 'Force': 'true'}
                ),
                Cmd(
                    name='Set-Mailbox',
                    params={'Identity': 'blahblah', 'AuditEnabled': 'false'}
                )
            ]

    prior_probs: Union[StateMatrix, dict]
        computed probabilities of individual commands
    trans_probs: Union[StateMatrix, dict]
        computed probabilities of sequences of commands (length 2)
    param_cond_cmd_probs: Union[StateMatrix, dict]
        computed probabilities of the params conditional on the commands
    value_cond_param_probs: Union[StateMatrix, dict]
        computed probabilities of the values conditional on the params
    modellable_params: set
        set of params for which we will also include the probabilties of
        their values in the calculation of the likelihood
    window_len: int
        length of sliding window for likelihood calculations
    use_start_end_tokens: bool
        if True, then `start_token` and `end_token` will be prepended
        and appended to the session respectively before the calculations are done
    start_token: str
        dummy command to signify the start of the session (e.g. "##START##")
    end_token: str
        dummy command to signify the end of the session (e.g. "##END##")
    use_geo_mean: bool
        if True, then each of the likelihoods of the sliding windows
        will be raised to the power of (1/`window_len`)

    Returns
    -------
    Tuple:
        rarest window part of the session,
        likelihood of the rarest window

    """
    likelihoods = compute_likelihood_windows_in_session(
        session=session,
        prior_probs=prior_probs,
        trans_probs=trans_probs,
        param_cond_cmd_probs=param_cond_cmd_probs,
        value_cond_param_probs=value_cond_param_probs,
        modellable_params=modellable_params,
        window_len=window_len,
        use_start_end_tokens=use_start_end_tokens,
        start_token=start_token,
        end_token=end_token,
        use_geo_mean=use_geo_mean,
    )
    if len(likelihoods) == 0:
        return [], np.nan
    min_lik = min(likelihoods)
    ind = likelihoods.index(min_lik)
    return session[ind : ind + window_len], min_lik  # noqa E203
