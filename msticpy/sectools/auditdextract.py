# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
Auditd extractor.

Module to load and decode Linux audit logs. It collapses messages
sharing the same message ID into single events, decodes hex-encoded data
fields and performs some event-specific formatting and normalization
(e.g. for process start events it will re-assemble the process command
line arguments into a single string). This is still a work-in-progress.

"""
import codecs
import re
from datetime import datetime
from typing import Mapping, Any, Tuple, Dict, List, Optional, Set
import pandas as pd

from .proc_tree_builder import build_process_tree

try:
    # pylint: disable=unused-import
    from ..analysis import cluster_auditd  # type: ignore
except ImportError:

    def cluster_auditd(*args, **kwargs):  # type: ignore
        """Fake cluster_auditd function for partial install."""
        raise NotImplementedError("Cannot import auditd cluster module.")


from .._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"

# Constants
# Fields that we know are frequently encoded
_ENCODED_PARAMS: Dict[str, Set[str]] = {
    "EXECVE": {"a0", "a1", "a2", "a3", "arch"},
    "PROCTITLE": {"proctitle"},
    "USER_CMD": {"cmd"},
}

# USER_START message schema
_USER_START: Dict[str, Optional[str]] = {
    "pid": "int",
    "uid": "int",
    "auid": "int",
    "ses": "int",
    "msg": None,
    "acct": None,
    "exe": None,
    "hostname": None,
    "addr": None,
    "terminal": None,
    "res": None,
}

# Message types schema
_FIELD_DEFS: Dict[str, Dict[str, Optional[str]]] = {
    "SYSCALL": {
        "success": None,
        "ppid": "int",
        "pid": "int",
        "auid": "int",
        "uid": "int",
        "gid": "int",
        "euid": "int",
        "egid": "int",
        "ses": "int",
        "exe": None,
        "com": None,
    },
    "CWD": {"cwd": None},
    "PROCTITLE": {"proctitle": None},
    "LOGIN": {
        "pid": "int",
        "uid": "int",
        "tty": None,
        "old-ses": "int",
        "ses": "int",
        "res": None,
    },
    "EXECVE": {"argc": "int", "a0": None, "a1": None, "a2": None},
    "_USER_START": _USER_START,
    "USER_END": _USER_START,
    "CRED_DISP": _USER_START,
    "USER_ACCT": _USER_START,
    "CRED_ACQ": _USER_START,
    "USER_CMD": {
        "pid": "int",
        "uid": "int",
        "auid": "int",
        "ses": "int",
        "msg": None,
        "cmd": None,
        "terminal": None,
        "res": None,
    },
}


def unpack_auditd(audit_str: List[Dict[str, str]]) -> Mapping[str, Mapping[str, Any]]:
    """
    Unpack an Audit message and returns a dictionary of fields.

    Parameters
    ----------
    audit_str : str
        The auditd raw record

    Returns
    -------
    Mapping[str, Any]
        The extracted message fields and values

    """
    event_dict: Dict[str, Dict[str, Any]] = {}
    # The audit_str should be a list of dicts - '{EXECVE : {'p1': 'foo', p2: 'bar'...},
    #                                      PATH: {'a1': 'xyz',....}}

    for record in audit_str:
        # process a single message type, splitting into type name
        # and contents
        for rec_key, rec_val in record.items():
            rec_dict: Dict[str, Optional[str]] = {}
            # Get our field mapping for encoded params for this
            # mssg_type (rec_key)
            encoded_fields_map = _ENCODED_PARAMS.get(rec_key, None)
            for rec_item in rec_val:
                # for each mssg item, split into k/v pair
                rec_split = rec_item.split("=", maxsplit=1)
                if len(rec_split) == 1:
                    rec_dict[rec_split[0]] = None
                    continue
                if (
                    not encoded_fields_map
                    or rec_split[1].startswith('"')
                    or rec_split[0] not in encoded_fields_map
                ):
                    field_value = rec_split[1].strip('"')
                else:
                    try:
                        # Try to decode this from hex-string to text
                        # Mypy thinks codecs.decode returns a str so
                        # incorrectly issues a type warning - in this case it
                        # will return a bytes string.
                        field_value = codecs.decode(  # type: ignore
                            bytes(rec_split[1], "utf-8"), "hex"
                        ).decode("utf-8")
                    except ValueError:
                        field_value = rec_split[1]
                        print(rec_val)
                        print(
                            "ERR:",
                            rec_key,
                            rec_split[0],
                            rec_split[1],
                            type(rec_split[1]),
                        )
                rec_dict[rec_split[0]] = field_value
            event_dict[rec_key] = rec_dict

    return event_dict


def _extract_event(message_dict: Mapping[str, Any]) -> Tuple[str, Mapping[str, Any]]:
    """
    Assemble discrete messages sharing the same message Id into a single event.

    Parameters
    ----------
    message_dict : Mapping[str, Any]
        the input dictionary

    Returns
    -------
    Tuple[str, Mapping[str, Any]
        the assembled message type and contents

    """
    # Handle process executions specially
    if "SYSCALL" in message_dict and "EXECVE" in message_dict:
        proc_create_dict: Dict[str, Any] = {}
        for mssg_type in ["SYSCALL", "CWD", "EXECVE", "PROCTITLE"]:
            if mssg_type not in message_dict or mssg_type not in _FIELD_DEFS:
                continue
            _extract_mssg_value(mssg_type, message_dict, proc_create_dict)

            if mssg_type == "EXECVE":
                args = int(proc_create_dict.get("argc", 1))
                arg_strs = [
                    proc_create_dict.get(f"a{arg_idx}", "") for arg_idx in range(args)
                ]
                proc_create_dict["cmdline"] = " ".join(arg_strs)
        return "SYSCALL_EXECVE", proc_create_dict

    event_dict: Dict[str, Any] = {}
    for mssg_type, _ in message_dict.items():
        if mssg_type in _FIELD_DEFS:
            _extract_mssg_value(mssg_type, message_dict, event_dict)
        else:
            # We don't check for duplicated keys here - if
            # there are multiple messages with the same key, the
            # last one will overwrite the previous value
            event_dict.update(message_dict[mssg_type])
    return list(message_dict.keys())[0], event_dict


def _extract_mssg_value(
    mssg_type: str,
    message_dict: Mapping[str, Mapping[str, Any]],
    event_dict: Dict[str, Any],
):
    """
    Extract field/value from the message dictionary.

    Parameters
    ----------
    mssg_type : str
        The Audit message type
    message_dict : Mapping[str, str]
        The input dictionary
    event_dict : Dict[str, Any]
        The output dictionary

    """
    # if the field requires conversion conv will specify the
    # target type - only int currently
    for fieldname, conv in _FIELD_DEFS[mssg_type].items():
        value = message_dict[mssg_type].get(fieldname, None)
        if not value:
            return
        if conv and conv == "int":
            value = int(value)
            if value == 4294967295:
                value = -1
        if fieldname in event_dict:
            event_dict[f"{fieldname}_{mssg_type}"] = value
        else:
            event_dict[fieldname] = value


def _move_cols_to_front(data: pd.DataFrame, column_count: int = 1) -> pd.DataFrame:
    """
    Move N columns from end to front of DataFrame.

    Parameters
    ----------
    data : pd.DataFrame
        The input DataFrame
    column_count : int, optional
        The number of columns to move (the default is 1)

    Returns
    -------
    pd.DataFrame
        DataFrame with `column_count` columns shifted to front

    """
    return data[list(data.columns[-column_count:]) + list(data.columns[:-column_count])]


def extract_events_to_df(
    data: pd.DataFrame,
    input_column: str = "AuditdMessage",
    event_type: str = None,
    verbose: bool = False,
) -> pd.DataFrame:
    """
    Extract auditd raw messages into a dataframe.

    Parameters
    ----------
    data : pd.DataFrame
        The input dataframe with raw auditd data in
        a single string column
    input_column : str, optional
        the input column name (the default is 'AuditdMessage')
    event_type : str, optional
        the event type, if None, defaults to all (the default is None)
    verbose : bool, optional
        Give feedback on stages of processing (the default is False)

    Returns
    -------
    pd.DataFrame
        The resultant DataFrame

    """
    if verbose:
        start_time = datetime.utcnow()
        print(f"Unpacking auditd messages for {len(data)} events...")

    # If the provided table has auditd messages as a string format and
    # extract key elements.
    if isinstance(data[input_column].head(1)[0], str):
        data["mssg_id"] = data.apply(
            lambda x: _extract_timestamp(x[input_column]), axis=1
        )
        data[input_column] = data.apply(
            lambda x: _parse_audit_message(x[input_column]), axis=1
        )

    # Our first pandas expression does most of the work - unpacking the
    # column contents, then extracting these into a two columns
    # EventType (the main auditd mssg type) and a dict of k/v values
    # EventData
    tmp_df = data.apply(
        lambda x: _extract_event(unpack_auditd(x[input_column])),
        axis=1,
        result_type="expand",
    ).rename(columns={0: "EventType", 1: "EventData"})
    # if only one type of event is requested
    if event_type:
        tmp_df = tmp_df.loc[tmp_df["EventType"] == event_type]
        if verbose:
            print(f"Event subset = {event_type} (events: {len(tmp_df)})")

    if verbose:
        print("Building output dataframe...")

    # We convert the EventData dict into a series,
    # then merge with:
    # First - the intermediate input DF to add back the EventType column
    # Second - the original input DF to add back metadata columns like Computer
    # Finally get rid of any empty columns
    tmp_df = (
        tmp_df.apply(lambda x: pd.Series(x.EventData), axis=1)
        .merge(tmp_df[["EventType"]], left_index=True, right_index=True)
        .merge(
            data.drop([input_column], axis=1),
            how="inner",
            left_index=True,
            right_index=True,
        )
        .dropna(axis=1, how="all")
    )

    if verbose:
        print("Fixing timestamps...")

    # extract real timestamp from mssg_id
    tmp_df["TimeStamp"] = tmp_df.apply(
        lambda x: datetime.utcfromtimestamp(float(x["mssg_id"].split(":")[0])), axis=1
    )
    if "TimeGenerated" in tmp_df:
        tmp_df = tmp_df.drop(["TimeGenerated"], axis=1)
    tmp_df = tmp_df.rename(columns={"TimeStamp": "TimeGenerated"}).pipe(
        _move_cols_to_front, column_count=5
    )
    if verbose:
        print(f"Complete. {len(tmp_df)} output rows", end=" ")
        delta = datetime.utcnow() - start_time
        print(f"time: {delta.seconds + delta.microseconds/1_000_000} sec")

    return tmp_df


def get_event_subset(data: pd.DataFrame, event_type: str) -> pd.DataFrame:
    """
    Return a subset of the events matching type event_type.

    Parameters
    ----------
    data : pd.DataFrame
        The input data
    event_type : str
        The event type to select

    Returns
    -------
    pd.DataFrame
        The subset of the data where
        data['EventType'] == event_type

    """
    return (
        data[data["EventType"] == event_type].dropna(axis=1, how="all").infer_objects()
    )


def read_from_file(
    filepath: str, event_type: str = None, verbose: bool = False, dummy_sep: str = "\t"
) -> pd.DataFrame:
    r"""
    Extract Audit events from a log file.

    Parameters
    ----------
    filepath : str
        path to the input file
    event_type : str, optional
        The type of event to extract if only a subset required.
        (the default is None, which processes all types)
    verbose : bool, optional
        If true more progress messages are output
        (the default is False)
    dummy_sep : str, optional
        Separator to use for reading the 'csv' file
        (default is tab - '\t')

    Returns
    -------
    pd.DataFrame
        The output DataFrame

    Notes
    -----
    The dummy_sep parameter should be a character that does not
    occur in an input line. This function uses pandas read_csv
    to read the audit lines into a single column. Using a separator
    that does appear in the input (e.g. space or comma) will cause
    data to be parsed into muliple columns and anything after the
    first separator in a line will be lost.

    """
    # read in the file using pd.read_csv()
    df_raw = pd.read_csv(
        filepath, sep=dummy_sep, names=["raw_data"], skip_blank_lines=True
    )

    # extract message ID into seperate column
    df_raw["mssg_id"] = df_raw.apply(
        lambda x: _extract_timestamp(x["raw_data"]), axis=1
    )
    # Pack message type and content into a dictionary:
    # {'mssg_type: ['item1=x, item2=y....]}
    df_raw["AuditdMessage"] = df_raw.apply(
        lambda x: _parse_audit_message(x["raw_data"]), axis=1
    )

    # Group the data by message id string and concatenate the message content
    # dictionaries in a list.
    df_grouped_cols = (
        df_raw.groupby(["mssg_id"]).agg({"AuditdMessage": list}).reset_index()
    )
    # pass this DataFrame to the event extractor.
    return extract_events_to_df(
        data=df_grouped_cols,
        input_column="AuditdMessage",
        event_type=event_type,
        verbose=verbose,
    )


def _parse_audit_message(audit_str: str) -> Dict[str, List[str]]:
    """
    Parse an auditd message string into Dict format required by unpack_auditd.

    Parameters
    ----------
    audit_str : str
        The Audit message

    Returns
    -------
    Dict[str, str]
        The extracted message values

    """
    audit_message = audit_str.rstrip().split(": ")
    audit_headers = audit_message[0]
    audit_hdr_match = re.match(r"type=([^\s]+)", audit_headers)
    if audit_hdr_match:
        return {audit_hdr_match.group(1): audit_message[1].split(" ")}
    return {}  # type ignore


def _extract_timestamp(audit_str: str) -> str:
    """
    Parse an auditd message string and extract the message time.

    Parameters
    ----------
    audit_str : str
        The Audit message

    Returns
    -------
    str
        The extracted message time string

    """
    audit_message = audit_str.rstrip().split(": ")
    audit_headers = audit_message[0]
    audit_hdr_match = re.match(r".*msg=audit\(([^\)]+)\)", audit_headers)
    if audit_hdr_match:
        return audit_hdr_match.group(1).split(":")[0]
    return ""


# pylint: disable=too-many-branches
def generate_process_tree(  # noqa: MC0001
    audit_data: pd.DataFrame, branch_depth: int = 4, processes: pd.DataFrame = None
) -> pd.DataFrame:
    """
    Generate process tree data from auditd logs.

    Parameters
    ----------
    audit_data : pd.DataFrame
        The Audit data containing process creation events
    branch_depth: int, optional
        The maximum depth of parent or child processes to extract from the data
        (The default is 4)
    processes: pd.DataFrame, optional
        Dataframe of processes to generate tree for

    Returns
    -------
    pd.DataFrame
        The formatted process tree data

    """
    # Superceded by process_tree_utils module
    del branch_depth, processes
    return build_process_tree(audit_data)
