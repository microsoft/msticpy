# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
Wrapper module for Model class for modelling sessions.

In particular, this module is for both modelling and visualising your session data.
"""

import pandas as pd

from ...common.exceptions import MsticpyException
from ...vis.timeline_values import display_timeline_values
from .model import Model


def score_sessions(
    data: pd.DataFrame, session_column: str, window_length: int
) -> pd.DataFrame:
    """
    Model sessions using a sliding window approach within a markov model.

    Parameters
    ----------
    data: pd.DataFrame
        Dataframe which contains at least a column for sessions
    session_column: str
        name of the column which contains the sessions
        The values in the session column should take one of the following formats:

        1) ['Set-User', 'Set-Mailbox']
        2) [Cmd(name='Set-User', params={'Identity', 'Force'}),
           Cmd(name='Set-Mailbox', params={'Identity', 'AuditEnabled'})]
        3) [Cmd(name='Set-User',
           params={'Identity': 'blahblah', 'Force': 'true'}),
           Cmd(name='Set-Mailbox',
           params={'Identity': 'blahblah', 'AuditEnabled': 'false'})]

        The Cmd datatype can be accessed from
        anomalous_sequence.utils.data_structures.Cmd
    window_length: int
        length of the sliding window to use when computing the likelihood
        metrics for each session.
        This should be set to an integer >= 2. Note that sessions which have
        fewer commands than the chosen window_length + 1 will end up with a
        np.nan score. (The + 1 is because we append a dummy `end_token` to each
        session before starting the sliding window, so a session of length 2,
        would be treated as length 3)

    Returns
    -------
    input dataframe with two additional columns appended.

    """
    if not isinstance(data, pd.DataFrame):
        raise MsticpyException("`data` should be a pandas dataframe")
    if session_column not in data.columns:
        raise MsticpyException(f'"{session_column}" should be a column in the `data`')

    sessions_df = data.copy()
    sessions = sessions_df[session_column].values.tolist()

    model = Model(sessions=sessions)
    model.train()
    model.compute_rarest_windows(
        window_len=window_length, use_geo_mean=False, use_start_end_tokens=True
    )

    sessions_df[
        f"rarest_window{window_length}_likelihood"
    ] = model.rare_window_likelihoods[window_length]
    sessions_df[f"rarest_window{window_length}"] = model.rare_windows[window_length]

    return sessions_df


# pylint: disable=too-many-arguments
def visualise_scored_sessions(
    data_with_scores: pd.DataFrame,
    time_column: str,
    score_column: str,
    window_column: str,
    score_upper_bound: float = None,
    source_columns: list = None,
):
    """
    Visualise the scored sessions on an interactive timeline.

    Parameters
    ----------
    data_with_scores: pd.DataFrame
        Dataframe which contains at least columns for time,
        session score, window representing the session
    time_column: str
        name of the column which contains a timestamp
    score_column: str
        name of the column which contains a numerical score for each
        of the sessions
    window_column: str
        name of the column which contains a representation of each of the sessions.
        This representation will appear in the tooltips in the figure.
        For example, it could be the rarest window of the session,
        or the full session etc.
    score_upper_bound: float, optional
        an optional upper bound on the score for the visualisation figure.
        This can help to zoom in on the more anomalous sessions
    source_columns: list, optional
        an optional list of source columns to include in the tooltips
        in the visualisation.
        Note, the content of each of these columns should be json serializable
        in order to be compatible with the figure

    Returns
    -------
    figure

    """
    scored_sessions = data_with_scores.copy()  # so we don't affect input dataframe
    scored_sessions[window_column] = scored_sessions[window_column].apply(
        lambda x: [str(cmd) for cmd in x]
    )  # so it
    # becomes json serializable for the figure
    scored_sessions["time_col"] = scored_sessions[time_column].astype(
        str
    )  # so it appears nicely in the figure
    # tooltips

    if score_upper_bound is None:
        score_upper_bound = scored_sessions[score_column].max()

    if source_columns is None:
        source_columns = []
    source_columns += [score_column, window_column, "time_col"]
    source_columns = list(set(source_columns))

    display_timeline_values(
        data=scored_sessions.loc[scored_sessions[score_column] <= score_upper_bound],
        y=score_column,
        time_column=time_column,
        source_columns=source_columns,
        kind="circle",
    )


# pylint: disable=too-many-arguments
def score_and_visualise_sessions(
    data: pd.DataFrame,
    session_column: str,
    window_length: int,
    time_column: str,
    likelihood_upper_bound: float = None,
    source_columns: list = None,
):
    """
    Model sessions and then produces an interactive timeline visualisation plot.

    In particular, the sessions are modelled using a sliding window approach
    within a markov model. The visualisation plot has time on the x-axis and
    the modelled session likelihood metric on the y-axis.

    Parameters
    ----------
    data: pd.DataFrame
        Dataframe which contains at least columns for time and sessions
    session_column: str
        name of the column which contains the sessions
        The values in the session column should take one of the following formats:

        1) ['Set-User', 'Set-Mailbox']
        2) [Cmd(name='Set-User', params={'Identity', 'Force'}),
           Cmd(name='Set-Mailbox', params={'Identity', 'AuditEnabled'})]
        3) [Cmd(name='Set-User',
           params={'Identity': 'blahblah', 'Force': 'true'}),
           Cmd(name='Set-Mailbox',
           params={'Identity': 'blahblah', 'AuditEnabled': 'false'})]

        The Cmd datatype can be accessed from
        seqeunce.utils.data_structures.Cmd
    window_length: int
        length of the sliding window to use when computing the
        likelihood metrics for each session.

        This should be set to an integer >= 2.
        Note that sessions which have fewer commands than the chosen
        window_length + 1 will not appear in the visualisation. (The + 1 is
        because we append a dummy `end_token` to each session before starting
        the sliding window, so a session of length 2, would be treated as length
        3)
    time_column: str
        name of the column which contains a timestamp
    likelihood_upper_bound: float, optional
        an optional upper bound on the likelihood metrics for the visualisation
        plot. This can help to zoom in on the more anomalous sessions
    source_columns: list, optional
        An optional list of source columns to include in the tooltips
        in the visualisation.
        Note, the content of each of these columns should be json
        serializable in order to be compatible with the figure

    Returns
    -------
    figure

    """
    scored_sessions = score_sessions(
        data=data, session_column=session_column, window_length=window_length
    )
    score_column = f"rarest_window{window_length}_likelihood"
    window_column = f"rarest_window{window_length}"

    visualise_scored_sessions(
        data_with_scores=scored_sessions,
        time_column=time_column,
        score_column=score_column,
        window_column=window_column,
        score_upper_bound=likelihood_upper_bound,
        source_columns=source_columns,
    )
