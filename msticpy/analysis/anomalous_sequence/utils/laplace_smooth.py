# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Helper module for laplace smoothing counts."""

from collections import defaultdict
from typing import Tuple, List


def laplace_smooth_cmd_counts(seq1_counts: defaultdict, seq2_counts: defaultdict, start_token: str,
                              end_token: str, unk_token: str) -> Tuple[defaultdict, defaultdict]:
    """
    Apply laplace smoothing to the input counts for the cmds.

    In particular, add 1 to each of the counts, including the unk_token. By including the
    unk_token, we can handle unseen commands.

    Parameters
    ----------
    seq1_counts: defaultdict
        individual command counts
    seq2_counts: defaultdict
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
