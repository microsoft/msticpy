# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Auditd extractor."""
import codecs
from datetime import datetime

import pandas as pd

from .. _version import VERSION

__version__ = VERSION
__author__ = 'Ian Hellen'

_ENCODED_PARAMS = {'EXECVE': {'a0', 'a1', 'a2', 'a3', 'arch'},
                   'PROCTITLE': {'proctitle'},
                   'USER_CMD': {'cmd'}}


def unpack_auditd(audit_str: str) -> dict:
    """
    Unpack an Auditd message and returns a dictionary of fields.

    Arguments:
        audit_str {str} -- The auditd raw record

    Returns:
        {dict} -- the extract message fields and values

    """
    event_dict = {}
    for record in audit_str:

        for rec_key, rec_val in record.items():
            rec_dict = {}
            encoded_fields_map = _ENCODED_PARAMS.get(rec_key, None)
            for rec_item in rec_val:
                rec_split = rec_item.split('=', maxsplit=1)
                if len(rec_split) == 1:
                    rec_dict[rec_split[0]] = None
                    continue
                if (not encoded_fields_map or rec_split[1].startswith('"') or
                        rec_split[0] not in encoded_fields_map):
                    field_value = rec_split[1].strip('\"')
                else:
                    try:
                        field_value = codecs.decode(
                            rec_split[1], 'hex').decode('utf-8')
                    except ValueError:
                        field_value = rec_split[1]
                        print(rec_val)
                        print('ERR:', rec_key,
                              rec_split[0], rec_split[1], type(rec_split[1]))
                rec_dict[rec_split[0]] = field_value
            event_dict[rec_key] = rec_dict

    return event_dict


_USER_START = {'pid': 'int', 'uid': 'int', 'auid': 'int',
               'ses': 'int', 'msg': None, 'acct': None, 'exe': None,
               'hostname': None, 'addr': None, 'terminal': None,
               'res': None}
_FIELD_DEFS = {'SYSCALL': {'success': None, 'ppid': 'int', 'pid': 'int',
                           'auid': 'int', 'uid': 'int', 'gid': 'int',
                           'euid': 'int', 'egid': 'int', 'ses': 'int',
                           'exe': None, 'com': None},
               'CWD': {'cwd': None},
               'PROCTITLE': {'proctitle': None},
               'LOGIN': {'pid': 'int', 'uid': 'int', 'tty': None, 'old-ses': 'int',
                         'ses': 'int', 'res': None},
               'EXECVE': {'argc': 'int', 'a0': None, 'a1': None, 'a2': None},
               '_USER_START': _USER_START,
               'USER_END': _USER_START,
               'CRED_DISP': _USER_START,
               'USER_ACCT': _USER_START,
               'CRED_ACQ': _USER_START,
               'USER_CMD': {'pid': 'int', 'uid': 'int', 'auid': 'int',
                            'ses': 'int', 'msg': None, 'cmd': None,
                            'terminal': None, 'res': None},
               }


def _extract_event(message_dict: dict) -> tuple(str, dict):
    """
    Assemble discrete messages sharing the same message Id into a single event.

    Arguments:
        message_dict {dict} -- the input dictionary

    Returns:
        {tuple({str}, {dict})} the assembled message type and contents

    """
    if 'SYSCALL' in message_dict:
        proc_create_dict = {}
        for mssg_type in ['SYSCALL', 'CWD', 'EXECVE', 'PROCTITLE']:
            if (mssg_type not in message_dict or
                    mssg_type not in _FIELD_DEFS):
                continue
            for fieldname, conv in _FIELD_DEFS[mssg_type].items():
                value = message_dict[mssg_type].get(fieldname, None)
                if not value:
                    continue
                if conv:
                    if conv == 'int':
                        value = int(value)
                        if value == 4294967295:
                            value = -1
                proc_create_dict[fieldname] = value
            if mssg_type == 'EXECVE':
                args = int(proc_create_dict.get('argc', 1))
                arg_strs = []
                for arg_idx in range(0, args):
                    arg_strs.append(proc_create_dict.get(f'a{arg_idx}', ''))

                proc_create_dict['cmdline'] = ' '.join(arg_strs)
        return 'SYSCALL', proc_create_dict
    else:
        event_dict = {}
        for mssg_type, _ in message_dict.items():
            if mssg_type in _FIELD_DEFS:
                for fieldname, conv in _FIELD_DEFS[mssg_type].items():
                    value = message_dict[mssg_type].get(fieldname, None)
                    if conv:
                        if conv == 'int':
                            value = int(value)
                            if value == 4294967295:
                                value = -1
                    event_dict[fieldname] = value
            else:

                event_dict.update(message_dict[mssg_type])
        return list(message_dict.keys())[0], event_dict


def _move_cols_to_front(data: pd.DataFrame, column_count: int = 1) -> pd.DataFrame:
    """
    Move N columns from end to front of DataFrame.

    Arguments:
        data {pd.DataFrame} -- The input DataFrame

    Keyword Arguments:
        column_count {int} -- The number of columns to move (default: {1})

    Returns:
        pd.DataFrame -- DataFrame with N columns shifted to front

    """
    return data[list(data.columns[-column_count:]) + list(data.columns[:-column_count])]


def extract_events_to_df(data: pd.DataFrame,
                         input_column: str = 'AuditdMessage',
                         event_type: str = None,
                         verbose: bool = False) -> pd.DataFrame:
    """
    Extract auditd raw messages into a dataframe.

    Arguments:
        data {pd.DataFrame} -- The input dataframe with raw auditd
            data in a single string column

    Keyword Arguments:
        input_column {str} -- the input column name (default: {'AuditdMessage'})
        event_type {str} -- the event type, if None, defaults to all
            (default: {None})
        verbose {bool} -- Give feedback on stages of processing (default: {False})

    Returns:
        pd.DataFrame -- The resultant DataFrame

    """
    if verbose:
        start_time = datetime.utcnow()
        print(f'Unpacking auditd messages for {len(data)} events...')

    tmp_df = (data.apply(lambda x: _extract_event(unpack_auditd(x[input_column])),
                         axis=1, result_type='expand')
              .rename(columns={0: 'EventType',
                               1: 'EventData'})
              )
    # if only one type of event is requested
    if event_type:
        tmp_df = tmp_df[tmp_df['EventType'] == event_type]
        if verbose:
            print(f'Event subset = ', event_type, ' (events: {len(tmp_df)})')

    if verbose:
        print('Building output dataframe...')

    tmp_df = (tmp_df.apply(lambda x: pd.Series(x.EventData), axis=1)
              .merge(tmp_df[['EventType']], left_index=True, right_index=True)
              .merge(data.drop([input_column], axis=1),
                     how='inner', left_index=True, right_index=True)
              .dropna(axis=1, how='all'))

    if verbose:
        print('Fixing timestamps...')

    # extract real timestamp from mssg_id
    tmp_df['TimeStamp'] = (tmp_df.apply(lambda x:
                                        datetime.utcfromtimestamp(
                                            float(x.mssg_id.split(':')[0])),
                                        axis=1))
    tmp_df = (tmp_df.drop(['TimeGenerated'], axis=1)
              .rename(columns={'TimeStamp': 'TimeGenerated'})
              .pipe(_move_cols_to_front, column_count=5))
    if verbose:
        print(f'Complete. {len(tmp_df)} output rows', end=' ')
        delta = datetime.utcnow() - start_time
        print(f'time: {delta.seconds + delta.microseconds/1_000_000} sec')

    return tmp_df


def get_event_subset(data: pd.DataFrame, event_type: str) -> pd.DataFrame:
    """
    Return a subset of the events matching type event_type.

    Arguments:
        data {pd.DataFrame} -- The input data
        event_type {str} -- The event type to select

    Returns:
        pd.DataFrame -- The subset of the data where
            data['EventType'] == event_type

    """
    return (data[data['EventType'] == event_type]
            .dropna(axis=1, how='all')
            .infer_objects())
