# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Helper module for computations when each session is a list of strings."""

import copy
from collections import defaultdict
from typing import DefaultDict, List, Tuple, Union

import numpy as np

from ....common.exceptions import MsticpyException
from ..utils.data_structures import StateMatrix
from ..utils.laplace_smooth import laplace_smooth_cmd_counts


def compute_counts(  # nosec
    sessions: List[List[str]], start_token: str, end_token: str, unk_token: str
) -> Tuple[DefaultDict[str, int], DefaultDict[str, DefaultDict[str, int]]]:
    """
    Compute counts of individual commands and of sequences of two commands.

    Parameters
    ----------
    sessions: List[List[str]]
        each session is a list of commands (strings)
        an example session::

            ['Set-User', 'Set-Mailbox']

    start_token: str
        dummy command to signify the start of a session (e.g. "##START##")
    end_token: str
        dummy command to signify the end of a session (e.g. "##END##")
    unk_token: str
        dummy command to signify an unseen command (e.g. "##UNK##")

    Returns
    -------
    tuple of counts:
        individual command counts,
        sequence command (length 2) counts

    """
    if not start_token != end_token != unk_token:
        raise MsticpyException(
            "start_token, end_token, unk_tokens should all be set to something "
            "different"
        )

    seq1_counts: DefaultDict[str, int] = defaultdict(lambda: 0)
    seq2_counts: DefaultDict[str, DefaultDict[str, int]] = defaultdict(
        lambda: defaultdict(lambda: 0)
    )

    for session in sessions:
        prev = start_token
        seq1_counts[prev] += 1
        for cmd in session:
            seq1_counts[cmd] += 1
            seq2_counts[prev][cmd] += 1
            prev = cmd
        seq2_counts[prev][end_token] += 1
        seq1_counts[end_token] += 1

    return seq1_counts, seq2_counts


def laplace_smooth_counts(
    seq1_counts: DefaultDict[str, int],
    seq2_counts: DefaultDict[str, DefaultDict[str, int]],
    start_token: str,
    end_token: str,
    unk_token: str,
) -> Tuple[StateMatrix, StateMatrix]:
    """
    Laplace smoothing is applied to the counts.

    We do this by adding 1 to each of the counts. This is so when we
    compute the probabilities from the counts, we shift some of the
    probability mass from the very probable commands and command sequences
    to the unseen and very unlikely commands and command sequences.
    The `unk_token` means we can handle unseen commands and sequences of commands.

    Parameters
    ----------
    seq1_counts: DefaultDict[str, int]
        individual command counts
    seq2_counts: DefaultDict[str, DefaultDict[str, int]]
        sequence command (length 2) counts
    start_token: str
        dummy command to signify the start of a session (e.g. "##START##")
    end_token: str
        dummy command to signify the end of a session (e.g. "##END##")
    unk_token: str
        dummy command to signify an unseen command (e.g. "##UNK##")

    Returns
    -------
    tuple of StateMatrix laplace smoothed counts:
        individual command counts,
        sequence command (length 2) counts

    """
    # apply laplace smoothing
    seq1_counts_ls, seq2_counts_ls = laplace_smooth_cmd_counts(
        seq1_counts=copy.deepcopy(seq1_counts),
        seq2_counts=copy.deepcopy(seq2_counts),
        start_token=start_token,
        end_token=end_token,
        unk_token=unk_token,
    )

    seq1_counts_st = StateMatrix(states=seq1_counts_ls, unk_token=unk_token)
    seq2_counts_st = StateMatrix(states=seq2_counts_ls, unk_token=unk_token)

    return seq1_counts_st, seq2_counts_st


# pylint: disable=too-many-arguments, too-many-branches
def compute_likelihood_window(
    window: List[str],
    prior_probs: Union[StateMatrix, dict],
    trans_probs: Union[StateMatrix, dict],
    use_start_token: bool,
    use_end_token: bool,
    start_token: str = None,
    end_token: str = None,
) -> float:
    """
    Compute the likelihood of the input `window`.

    Parameters
    ----------
    window: List[str]
        part or all of a session, where a session is a list of commands (strings)
        an example session::

            ['Set-User', 'Set-Mailbox']

    prior_probs: Union[StateMatrix, dict]
        computed probabilities of individual commands
    trans_probs: Union[StateMatrix, dict]
        computed probabilities of sequences of commands (length 2)
    use_start_token: bool
        if set to True, the start_token will be prepended to the window
        before the likelihood calculation is done
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
    prob = 1

    cur = window[0]
    if use_start_token:
        prob *= trans_probs[start_token][cur]
    else:
        prob *= prior_probs[cur]

    for i in range(1, w_len):
        prev, cur = window[i - 1], window[i]
        prob *= trans_probs[prev][cur]

    if use_end_token:
        prob *= trans_probs[cur][end_token]

    return prob


# pylint: disable=too-many-locals, too-many-arguments, too-many-branches
# pylint: disable=too-many-locals, too-many-branches
def compute_likelihood_windows_in_session(
    session: List[str],
    prior_probs: Union[StateMatrix, dict],
    trans_probs: Union[StateMatrix, dict],
    window_len: int,
    use_start_end_tokens: bool,
    start_token: str = None,
    end_token: str = None,
    use_geo_mean: bool = False,
) -> List[float]:
    """
    Compute the likelihoods of a sliding window of length `window_len` in the session.

    Parameters
    ----------
    session: List[str]
        list of commands (strings)
        an example session::

            ['Set-User', 'Set-Mailbox']

    prior_probs: Union[StateMatrix, dict]
        computed probabilities of individual commands
    trans_probs: Union[StateMatrix, dict]
        computed probabilities of sequences of commands (length 2)
    window_len: int
        length of sliding window for likelihood calculations
    use_start_end_tokens: bool
        if True, then `start_token` and `end_token` will be prepended and appended to the
        session respectively before the calculations are done
    start_token: str
        dummy command to signify the start of the session (e.g. "##START##")
    end_token: str
        dummy command to signify the end of the session (e.g. "##END##")
    use_geo_mean: bool
        if True, then each of the likelihoods of the sliding windows will be
        raised to the power of (1/`window_len`)

    Returns
    -------
    list of likelihoods

    """
    if use_start_end_tokens:
        if start_token is None or end_token is None:
            raise MsticpyException(
                "start_token and end_token should not be set to None when "
                "use_start_end_tokens is set to True"
            )

    likelihoods = []
    sess = session.copy()
    if use_start_end_tokens and end_token:
        sess += [str(end_token)]
    end = len(sess) - window_len
    for i in range(end + 1):
        window = sess[i : i + window_len]  # noqa: E203

        if i == 0:
            use_start = use_start_end_tokens
        else:
            use_start = False

        lik = compute_likelihood_window(
            window=window,
            prior_probs=prior_probs,
            trans_probs=trans_probs,
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
    session: List[str],
    prior_probs: Union[StateMatrix, dict],
    trans_probs: Union[StateMatrix, dict],
    window_len: int,
    use_start_end_tokens: bool,
    start_token: str,
    end_token: str,
    use_geo_mean: bool = False,
) -> Tuple[List[str], float]:
    """
    Find and compute likelihood of the rarest window in the session.

    Parameters
    ----------
    session: List[str]
        list of commands (strings)
        an example session::

            ['Set-User', 'Set-Mailbox']

    prior_probs: Union[StateMatrix, dict]
        computed probabilities of individual commands
    trans_probs: Union[StateMatrix, dict]
        computed probabilities of sequences of commands (length 2)
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
        if True, then each of the likelihoods of the sliding windows will be
        raised to the power of (1/`window_len`)

    Returns
    -------
    (rarest window part of the session, likelihood of the rarest window)

    """
    likelihoods = compute_likelihood_windows_in_session(
        session=session,
        prior_probs=prior_probs,
        trans_probs=trans_probs,
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
    return session[ind : ind + window_len], min_lik  # noqa: E203
