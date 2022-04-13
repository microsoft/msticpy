# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
cmd_line - Syslog Command processing module.

Contains a series of functions required to correct collect, parse and visualize
linux syslog data.

Designed to support standard linux syslog for investigations where auditd
is not available.

"""
import datetime as dt
import json
import re
from pathlib import Path
from typing import Optional

import numpy as np
import pandas as pd

from .._version import VERSION
from ..common.exceptions import MsticpyException
from ..common.utility import export
from .base64unpack import unpack

__version__ = VERSION
__author__ = "Pete Bryan"

_DETECTIONS_DEF_DIR = "resources"


@export
def risky_cmd_line(
    events: pd.DataFrame,
    log_type: str,
    detection_rules: Optional[str] = None,
    cmd_field: str = "Command",
) -> dict:
    """
    Detect patterns of risky commands in syslog messages.

    Risky patterns are defined in a json format file.

    Parameters
    ----------
    events: pd.DataFrame
        A DataFrame of all syslog events potentially containing risky
        command line activity.
    log_type: str
        The log type of the data included in events.
        Must correspond to a detection type in detection_rules file.
    detection_rules: str, optional
        Path to json file containing patterns of risky activity to detect.
        (Defaults to msticpy/resources/cmd_line_rules.json)
    cmd_field: str, optional;
        The column in the events dataset that contains the command lines to
        be analysed.
        (Defaults to "Command")

    Returns
    -------
    risky actions: dict
        A dictionary of commands that match a risky pattern

    Raises
    ------
    MsticpyException
        The provided dataset does not contain the cmd_field field

    """
    if cmd_field not in events.columns:
        raise MsticpyException(
            f"The provided dataset does not contain the {cmd_field} field"
        )
    if detection_rules is None:
        detection_rules = str(
            Path(__file__)
            .parent.parent.joinpath(_DETECTIONS_DEF_DIR)
            .joinpath("cmd_line_rules.json")
        )

    events[cmd_field].replace("", np.nan, inplace=True)
    activity = (
        events[["TimeGenerated", cmd_field]]
        .dropna()
        .set_index("TimeGenerated")
        .to_dict()
    )
    with open(detection_rules, "r", encoding="utf-8") as json_file:
        rules = json.load(json_file)

    # Decode any Base64 encoded commands so we can match on them as well
    b64_regex = re.compile(
        "(?P<b64>(?:[A-Za-z0-9+/]{4})*(?:[A-Za-z0-9+/]{2}==|"
        + "[A-Za-z0-9+/]{3}=|[A-Za-z0-9+/]{4})$)"
    )
    risky_actions = {}
    detections = rules[log_type]
    for detection in detections:
        for date, message in activity[cmd_field].items():
            if b64_regex.match(message):
                b64match = b64_regex.search(message)
                b64string = unpack(input_string=b64match[1])  # type: ignore
                b64string = b64string[1]["decoded_string"].to_string()  # type: ignore
                if re.match(detection, message):
                    risky_actions.update({date: message})
                else:
                    pass
            else:
                if re.match(detection, message):
                    risky_actions.update({date: message})
                else:
                    pass
    return risky_actions


@export
def cmd_speed(
    cmd_events: pd.DataFrame, cmd_field: str, time: int = 5, events: int = 10
) -> list:
    """
    Detect patterns of cmd_line activity whose speed of execution may be suspicious.

    Parameters
    ----------
    cmd_events: pd.DataFrame
        A DataFrame of all sudo events to check.
    cmd_field: str
        The column of the event data that contains command line activity
    time: int, optional
        Time window in seconds in which to evaluate speed of execution against
        (Defaults to 5)
    events: int, optional
        Number of syslog command execution events in which to evaluate
        speed of execution against
        (Defaults to 10)

    Returns
    -------
    risky suspicious_actions: list
        A list of commands that match a risky pattern

    Raises
    ------
    AttributeError
        If cmd_field is not in supplied data set or TimeGenerated note datetime format

    """
    if cmd_field not in cmd_events.columns:
        raise MsticpyException(f"Dataframe does not contain {cmd_field} column")

    if isinstance(cmd_events["TimeGenerated"].iloc[0], dt.datetime) is False:
        raise MsticpyException("TimeGenerated is not a datetime format")

    suspicious_actions = []
    cmd_events[cmd_field].replace("", np.nan, inplace=True)
    # Only focus on logs that contain comand line activity
    actions = cmd_events.dropna(subset=[cmd_field]).reset_index()
    df_len = len(actions.index) - (events + 1)
    while df_len >= 0:
        delta = (
            actions["TimeGenerated"][(df_len + events)]
            - actions["TimeGenerated"][df_len]
        )
        if delta < dt.timedelta(seconds=time):
            suspicious_actions.append(
                {df_len: [actions[df_len : (df_len + events)], delta]}  # noqa: E203
            )
        else:
            pass
        df_len = df_len - 1
    return suspicious_actions
