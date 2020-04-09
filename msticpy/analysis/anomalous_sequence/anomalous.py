import pandas as pd
from msticpy.nbtools import timeline

from msticpy.analysis.anomalous_sequence.model import Model


def score_sessions(data: pd.DataFrame, session_column: str, window_length: int) -> pd.DataFrame:
    """
    Models sessions using a sliding window approach within a morkov model.

    Parameters
    ----------
    data: pd.DataFrame
        Dataframe which contains at least a column for sessions
    session_column: str
        name of the column which contains the sessions
        The values in the session column should take one of the following formats:
            examples formats of a session:
            1) ['Set-User', 'Set-Mailbox']
            2) [Cmd(name='Set-User', params={'Identity', 'Force'}), Cmd(name='Set-Mailbox', params={'Identity',
                'AuditEnabled'})]
            3) [Cmd(name='Set-User', params={'Identity': 'blahblah', 'Force': 'true'}), Cmd(name='Set-Mailbox',
                params={'Identity': 'blahblah', 'AuditEnabled': 'false'})]
        The Cmd datatype can be accessed from seqeunce.utils.data_structures.Cmd
    window_length: int
        length of the sliding window to use when computing the likelihood metrics for each session.
            This should be set to an integer >= 2.
            Note that sessions which have fewer commands than the chosen window_length + 1 will not appear in the
            visualisation. (The + 1 is because we append a dummy `end_token` to each session before starting the
            sliding window, so a session of length 2, would be treated as length 3)

    Returns
    -------
    input dataframe with two additional columns appended.

    """
    assert isinstance(data, pd.DataFrame), '`data` should be a pandas dataframe'
    assert session_column in data.columns, '"{}" should be a column in "data"'.format(session_column)

    df = data.copy()
    sessions = df[session_column].values.tolist()

    model = Model(sessions=sessions)
    model.train()
    model.compute_rarest_windows(window_len=window_length, use_geo_mean=False, use_start_end_tokens=True)

    df['rarest_window{}_likelihood'.format(window_length)] = model.rare_window_likelihoods[window_length]
    df['rarest_window{}'.format(window_length)] = model.rare_windows[window_length]

    return df


def visualise_scored_sessions(data_with_scores: pd.DataFrame, time_column: str, score_column: str, window_column: str,
                              score_upper_bound: float = None, source_columns: list = None):
    """
    takes scored sessions data as input and visualises them on an interactive timeline figure

    Parameters
    ----------
    data_with_scores: pd.DataFrame
        Dataframe which contains at least columns for time, session score, window representing the session
    time_column: str
        name of the column which contains a timestamp
    score_column: str
        name of the column which contains a numerical score for each of the sessions
    window_column: str
        name of the column which contains a representation of each of the sessions.
        This representation will appear in the tooltips in the figure.
        For example, it could be the rarest window of the session, or the full session etc.
    score_upper_bound: float, optional
        an optional upper bound on the score for the visualisation figure.
        This can help to zoom in on the more anomalous sessions
    source_columns: list, optional
        an optional list of source columns to include in the tooltips in the visualisation.
        Note, the content of each of these columns should be json serializable in order to be compatible with the figure

    Returns
    -------
    figure
    """
    scored_sessions = data_with_scores.copy()  # so we don't affect input dataframe
    scored_sessions[window_column] = scored_sessions[window_column].apply(lambda x: [str(cmd) for cmd in x])  # so it
    # becomes json serializable for the figure
    scored_sessions['time_col'] = scored_sessions[time_column].astype(str)  # so it appears nicely in the figure
    # tooltips

    if score_upper_bound is None:
        score_upper_bound = scored_sessions[score_column].max()

    if source_columns is None:
        source_columns = []
    source_columns += [score_column, window_column, 'time_col']
    source_columns = list(set(source_columns))

    timeline.display_timeline_values(
        data=scored_sessions.loc[scored_sessions[score_column] <= score_upper_bound],
        y=score_column,
        time_column=time_column,
        source_columns=source_columns,
        kind='circle'
    )


def score_and_visualise_sessions(data: pd.DataFrame, session_column: str, window_length: int, time_column: str,
                                 likelihood_upper_bound: float = None, source_columns: list = None):
    """
    models sessions using a sliding window approach within a markov model and then display a timeline of the sessions
    with the session likelihood metrics on the y axis

    Parameters
    ----------
    data: pd.DataFrame
        Dataframe which contains at least columns for time and sessions
    session_column: str
        name of the column which contains the sessions
        The values in the session column should take one of the following formats:
            examples formats of a session:
            1) ['Set-User', 'Set-Mailbox']
            2) [Cmd(name='Set-User', params={'Identity', 'Force'}), Cmd(name='Set-Mailbox', params={'Identity',
                'AuditEnabled'})]
            3) [Cmd(name='Set-User', params={'Identity': 'blahblah', 'Force': 'true'}), Cmd(name='Set-Mailbox',
                params={'Identity': 'blahblah', 'AuditEnabled': 'false'})]
        The Cmd datatype can be accessed from seqeunce.utils.data_structures.Cmd
    window_length: int
        length of the sliding window to use when computing the likelihood metrics for each session.
            This should be set to an integer >= 2.
            Note that sessions which have fewer commands than the chosen window_length + 1 will not appear in the
            visualisation. (The + 1 is because we append a dummy `end_token` to each session before starting the
            sliding window, so a session of length 2, would be treated as length 3)
    time_column: str
        name of the column which contains a timestamp
    likelihood_upper_bound: float, optional
        an optional upper bound on the likelihood metrics for the visualisation plot. This can help to zoom in
        on the more anomalous sessions
    source_columns: list, optional
        An optional list of source columns to include in the tooltips in the visualisation.
        Note, the content of each of these columns should be json serializable in order to be compatible with the figure

    Returns
    -------
    figure
    """
    scored_sessions = score_sessions(data=data, session_column=session_column, window_length=window_length)
    score_column = 'rarest_window{}_likelihood'.format(window_length)
    window_column = 'rarest_window{}'.format(window_length)

    visualise_scored_sessions(
        data_with_scores=scored_sessions,
        time_column=time_column,
        score_column=score_column,
        window_column=window_column,
        score_upper_bound=likelihood_upper_bound,
        source_columns=source_columns
    )
