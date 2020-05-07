# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Helper module for laplace smoothing counts."""

from typing import Tuple, List, DefaultDict


def laplace_smooth_cmd_counts(seq1_counts: DefaultDict[str, int],
                              seq2_counts: DefaultDict[str, DefaultDict[str, int]],
                              start_token: str, end_token: str,
                              unk_token: str) \
        -> Tuple[DefaultDict[str, int], DefaultDict[str, DefaultDict[str, int]]]:
    """
    Apply laplace smoothing to the input counts for the cmds.

    In particular, add 1 to each of the counts, including the unk_token. By including the
    unk_token, we can handle unseen commands.

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
    tuple of laplace smoothed counts:
        individual command counts,
        sequence command (length 2) counts

    """
    seq1_counts_ls = seq1_counts.copy()
    seq2_counts_ls = seq2_counts.copy()

    cmds: List[str] = list(seq1_counts_ls.keys()) + [unk_token]
    for cmd1 in cmds:
        for cmd2 in cmds:
            if cmd1 != end_token and cmd2 != start_token:
                seq1_counts_ls[cmd1] += 1
                seq2_counts_ls[cmd1][cmd2] += 1
                seq1_counts_ls[cmd2] += 1

    return seq1_counts_ls, seq2_counts_ls


def laplace_smooth_param_counts(cmds: List[str], param_counts: DefaultDict[str, int],
                                cmd_param_counts: DefaultDict[str, DefaultDict[str, int]],
                                unk_token: str) \
        -> Tuple[DefaultDict[str, int], DefaultDict[str, DefaultDict[str, int]]]:
    """
    Apply laplace smoothing to the input counts for the params.

    In particular, add 1 to each of the counts, including the unk_token. By including the
    unk_token, we can handle unseen params.

    Parameters
    ----------
    cmds: List[str]
        list of all the possible commands (including the unk_token)
    param_counts: DefaultDict[str, int]
        individual param counts
    cmd_param_counts: DefaultDict[str, DefaultDict[str, int]]
        param conditional on command counts
    unk_token: str
        dummy command to signify an unseen command (e.g. "##UNK##")

    Returns
    -------
    Tuple:
        individual param probabilities,
        param conditional on command probabilities

    """
    param_counts_ls = param_counts.copy()
    cmd_param_counts_ls = cmd_param_counts.copy()

    params: List[str] = list(param_counts.keys()) + [unk_token]
    for cmd in cmds:
        for param in params:
            if param in cmd_param_counts_ls[cmd] or param == unk_token:
                param_counts_ls[param] += 1
                cmd_param_counts_ls[cmd][param] += 1

    return param_counts_ls, cmd_param_counts_ls
