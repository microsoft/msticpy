from collections import defaultdict
import numpy as np
from typing import Tuple, List, Union

from msticpy.analysis.anomalous_sequence.utils.data_structures import StateMatrix


def compute_counts(
    sessions: List[List[str]],
    start_token: str = "##START##",
    end_token: str = "##END##",
    unk_token: str = "##UNK##",
) -> Tuple[StateMatrix, StateMatrix]:
    """
    computes counts of individual commands and of sequences of two commands.

    Laplace smoothing is applied to the counts.
    This is so we shift some of the probability mass from the very probable commands and command sequences to
    the unseen and very unlikely commands and command sequences.
    The `unk_token` means we can handle unseen commands and sequences of commands

    Parameters
    ----------
    sessions: List[List[str]]
        each session is a list of commands (strings)
        an example session:
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
    assert start_token != end_token != unk_token, (
        "start_token, end_token, unk_tokens should all be set to something " "different"
    )

    seq1_counts = defaultdict(lambda: 0)
    seq2_counts = defaultdict(lambda: defaultdict(lambda: 0))

    for session in sessions:
        prev = start_token
        seq1_counts[prev] += 1
        for cmd in session:
            seq1_counts[cmd] += 1
            seq2_counts[prev][cmd] += 1
            prev = cmd
        seq2_counts[prev][end_token] += 1
        seq1_counts[end_token] += 1

    # apply laplace smoothing
    cmds = list(seq1_counts.keys()) + [unk_token]
    for cmd1 in cmds:
        for cmd2 in cmds:
            if cmd1 != end_token and cmd2 != start_token:
                seq1_counts[cmd1] += 1
                seq2_counts[cmd1][cmd2] += 1
                seq1_counts[cmd2] += 1

    # remove start token from prior counts
    # seq1_counts.pop(start_token)

    seq1_counts = StateMatrix(states=seq1_counts, unk_token=unk_token)
    seq2_counts = StateMatrix(states=seq2_counts, unk_token=unk_token)

    return seq1_counts, seq2_counts


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
    computes the likelihood of the input `window`

    Parameters
    ----------
    window: List[str]
        part or all of a session, where a session is a list of commands (strings)
        an example session:
            ['Set-User', 'Set-Mailbox']
    prior_probs: Union[StateMatrix, dict]
        computed probabilities of individual commands
    trans_probs: Union[StateMatrix, dict]
        computed probabilities of sequences of commands (length 2)
    use_start_token: bool
        if set to True, the start_token will be prepended to the window before the likelihood
        calculation is done
    use_end_token: bool
        if set to True, the end_token will be appended to the window before the likelihood
        calculation is done
    start_token: str
        dummy command to signify the start of the session (e.g. "##START##")
    end_token: str
        dummy command to signify the end of the session (e.g. "##END##")

    Returns
    -------
    likelihood of the window
    """
    if use_start_token:
        assert start_token is not None
    if use_end_token:
        assert end_token is not None

    n = len(window)
    if n == 0:
        return np.nan
    prob = 1

    cur = window[0]
    if use_start_token:
        prob *= trans_probs[start_token][cur]
    else:
        prob *= prior_probs[cur]

    for i in range(1, n):
        prev, cur = window[i - 1], window[i]
        prob *= trans_probs[prev][cur]

    if use_end_token:
        prob *= trans_probs[cur][end_token]

    return prob


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
    computes the likelihoods of a sliding window of length `window_len` throughout the session

    Parameters
    ----------
    session: List[str]
        list of commands (strings)
        an example session:
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
        if True, then each of the likelihoods of the sliding windows will be raised to the power of
        (1/`window_len`)

    Returns
    -------
    list of likelihoods
    """
    if use_start_end_tokens:
        assert start_token is not None and end_token is not None

    likelihoods = []
    sess = session.copy()
    if use_start_end_tokens:
        sess += [end_token]
    end = len(sess) - window_len
    for i in range(end + 1):
        window = sess[i : i + window_len]
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
    finds and computes the likelihood of the rarest window of length `window_len` from the `session`

    Parameters
    ----------
    session: List[str]
        list of commands (strings)
        an example session:
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
        if True, then each of the likelihoods of the sliding windows will be raised to the power of
        (1/`window_len`)

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
    return session[ind : ind + window_len], min_lik
