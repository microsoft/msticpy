# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Module for creating sessions out of raw data."""

from typing import List

import numpy as np
import pandas as pd
from pandas.core.dtypes.dtypes import DatetimeTZDtype


def sessionize_data(
    data: pd.DataFrame,
    user_identifier_cols: List[str],
    time_col: str,
    max_session_time_mins: int,
    max_event_separation_mins: int,
    event_col: str,
) -> pd.DataFrame:
    """
    Sessionize the input data.

    In particular, the resulting dataframe will have 1 row per session. It will contain the
    following columns: the user_identifier_cols, <time_col>_min, <time_col>_max,
    <event_col>_list, duration (<time_col>_max - <time_col>_min), number_events (length of the
    <event_col>_list value)

    Parameters
    ----------
    data: pd.DataFrame
        This dataframe should contain at least the following columns:
        - time stamp column
        - columns related to user name and/or computer name and/or ip address etc
        - column containing an event
    user_identifier_cols: List[str]
        Name of the columns which contain username and/or computer name and/or ip address etc.
        Each time the value of one of these columns changes, a new session will be started.
    time_col: str
        Name of the column which contains a time stamp.
        If this column is not already in datetime64[ns, UTC] format, it will be casted to it.
    max_session_time_mins: int
        The maximum length of a session in minutes. If a sequence of events for the same
        user_identifier_cols values exceeds this length, then a new session will be started.
    max_event_separation_mins: int
        The maximum length in minutes between two events in a session. If we have 2 events for
        the same user_identifier_cols values, and if those two events are more than
        `max_event_separation_mins` apart, then a new session will be started.
    event_col: str
        Name of the column which contains the event of interest.
        For example, if we are interested in sessionizing exchange admin commands,
        the "event_col" could contain values like: "Set-Mailbox" or "Set-User" etc.

    Returns
    -------
    pd.DataFrame containing the sessionized data. 1 row per session.

    """
    df_with_sesind = create_session_col(
        data=data,
        user_identifier_cols=user_identifier_cols,
        time_col=time_col,
        max_session_time_mins=max_session_time_mins,
        max_event_separation_mins=max_event_separation_mins,
    )

    # aggregating will not work properly with nans. Temporarily replace nan values with dummy_str.
    for col in user_identifier_cols:
        df_with_sesind[col] = df_with_sesind[col].fillna("dummy_str")

    # aggregate by the session_ind column
    agg_df = (
        df_with_sesind.sort_values(["session_ind", time_col])
        .groupby(["session_ind"] + user_identifier_cols, as_index=False)
        .agg({time_col: ["min", "max"], event_col: list})
        .reset_index()
    )

    # rename some columns
    outer_cols = agg_df.columns.get_level_values(0)
    inner_cols = agg_df.columns.get_level_values(1)
    agg_df.columns = [
        outer_cols[i] + "_" + inner_cols[i] if inner_cols[i] != "" else outer_cols[i]
        for i in range(len(outer_cols))
    ]

    # calculate some additional columns
    agg_df["duration"] = agg_df[f"{time_col}_max"] - agg_df[f"{time_col}_min"]
    agg_df["number_events"] = agg_df[f"{event_col}_list"].apply(len)

    agg_df = agg_df.drop("session_ind", axis=1)
    if "index" in agg_df.columns:
        agg_df = agg_df.drop("index", axis=1)

    # replace dummy_str with nan values
    for col in user_identifier_cols:
        agg_df[col] = agg_df[col].replace("dummy_str", np.nan)

    return agg_df


# pylint: disable=too-many-locals, too-many-branches
def create_session_col(
    data: pd.DataFrame,
    user_identifier_cols: List[str],
    time_col: str,
    max_session_time_mins: int,
    max_event_separation_mins: int,
) -> pd.DataFrame:
    """
    Create a "session_ind" column in the dataframe.

    In particular, the session_ind column will be incremented each time a new session
    starts.

    Parameters
    ----------
    data: pd.DataFrame
        This dataframe should contain at least the following columns:
        - time stamp column
        - columns related to user name and/or computer name and/or ip address etc
    user_identifier_cols: List[str]
        Name of the columns which contain username and/or computer name and/or ip address etc.
        Each time the value of one of these columns changes, a new session will be started.
    time_col: str
        Name of the column which contains a time stamp.
        If this column is not already in datetime64[ns, UTC] format, it will be casted to it.
    max_session_time_mins: int
        The maximum length of a session in minutes. If a sequence of events for the same
        user_identifier_cols values exceeds this length, then a new session will be started.
    max_event_separation_mins: int
        The maximum length in minutes between two events in a session. If we have 2 events for
        the same user_identifier_cols values, and if those two events are more than
        `max_event_separation_mins` apart, then a new session will be started.

    Returns
    -------
    pd.DataFrame with an additional "session_ind" column

    """
    max_sep = pd.to_timedelta(max_event_separation_mins, unit="min")
    max_ses = pd.to_timedelta(max_session_time_mins, unit="min")

    df_with_sesind = data.copy()
    if not isinstance(df_with_sesind[time_col].dtype, DatetimeTZDtype):
        df_with_sesind[time_col] = pd.to_datetime(df_with_sesind[time_col])

    final_cols = list(df_with_sesind.columns) + ["session_ind"]

    if len(df_with_sesind) == 0:
        df_with_sesind["session_ind"] = None
        return df_with_sesind

    # Sessionising will not work properly with nans. Temporarily replace nan values with dummy_str.
    for col in user_identifier_cols:
        df_with_sesind[col] = df_with_sesind[col].fillna("dummy_str")

    df_with_sesind = df_with_sesind.sort_values(
        user_identifier_cols + [time_col]
    ).reset_index(drop=True)

    # initialise first row
    ses_ind = 0
    df_with_sesind.loc[0, "time_diff"] = pd.to_timedelta(0)
    df_with_sesind.loc[0, "cml_time"] = pd.to_timedelta(0)
    df_with_sesind.loc[0, "session_ind"] = ses_ind

    for i in range(1, len(df_with_sesind)):
        cur = df_with_sesind.iloc[i]
        prev = df_with_sesind.iloc[i - 1]

        # if any of the user_identifier_cols values change, a new session should start
        new_flag = False
        for col in user_identifier_cols:
            if cur[col] != prev[col]:
                new_flag = True
                break

        dif = cur[time_col] - prev[time_col]
        cml = prev["cml_time"] + dif
        # if the max session length is exceeded or the max separation between events is exceeded,
        # a new session should start
        if dif > max_sep or cml > max_ses:
            new_flag = True

        if new_flag:
            df_with_sesind.loc[i, "time_diff"] = pd.to_timedelta(0)
            df_with_sesind.loc[i, "cml_time"] = pd.to_timedelta(0)
            ses_ind += 1
            df_with_sesind.loc[i, "session_ind"] = ses_ind
        else:
            df_with_sesind.loc[i, "time_diff"] = dif
            df_with_sesind.loc[i, "cml_time"] = cml
            df_with_sesind.loc[i, "session_ind"] = ses_ind

    # replace dummy_str with nan values
    for col in user_identifier_cols:
        df_with_sesind[col] = df_with_sesind[col].replace("dummy_str", np.nan)

    return df_with_sesind[final_cols]
