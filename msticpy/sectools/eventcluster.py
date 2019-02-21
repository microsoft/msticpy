# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
eventcluster module.

Contains:
dbcluster_events: generic clustering method using DBSCAN designed to summarize
process events and other similar data by grouping on common features.

add_process_features: derives numerical features from text features such as
commandline and process path.
"""
from math import log10, floor

import numpy as np
import pandas as pd
from sklearn.cluster import DBSCAN
from sklearn.preprocessing import Normalizer

from .. nbtools.utility import export, pd_version_23
from .. _version import VERSION

__version__ = VERSION
__author__ = 'Ian Hellen'


@export
def dbcluster_events(data, cluster_columns=None, verbose=False, normalize=True,
                     time_column='TimeCreatedUtc',
                     max_cluster_distance=0.01, min_cluster_samples=2,
                     **kwargs):
    """
    Cluster data set according to cluster_columns features.

    Uses sklearn DBSCAN
        :param data: Input data as a pandas DataFrame or numpy array
        :param cluster_columns=None: List of columns to use for features
            for DataFrame this is a list of column names
            for numpy array this is a list of column indexes
        :param verbose=False: Print additional information about clustering
            results
        :param normalize=True: Normalize the input data (should probably always be True)
        :param time_column='TimeCreatedUtc': If there is a time column the output
            data will be ordered by this
        :param max_cluster_distance=0.01: DBSCAN eps (max cluster member distance)
        :param min_cluster_samples=2: DBSCAN min_samples (the minimum cluster size)
        :param **kwargs: Other arguments are passed to DBSCAN constructor
    """
    allowed_types = [np.ndarray, pd.DataFrame]

    x_input = None
    if isinstance(data, pd.DataFrame):
        src_columns = data.columns
        if cluster_columns is None:
            x_input = data.values
        else:
            x_input = data[cluster_columns].values
    elif isinstance(data, np.ndarray):
        if cluster_columns is None:
            x_input = data
        else:
            x_input = data[:, cluster_columns].values

    if x_input is None:
        mssg = 'Input data not in expected format.\n{} is not one of allowed types {}'
        type_list = ', '.join([str(t) for t in allowed_types])
        mssg = mssg.format(str(type(data)), type_list)
        raise ValueError(mssg)

    # Create output frame
    clustered_events = pd.DataFrame(columns=src_columns)

    # Create DBSCAN cluster object
    db_cluster = DBSCAN(eps=max_cluster_distance,
                        min_samples=min_cluster_samples,
                        **kwargs)

    # Normalize the data (most clustering algorithms don't do well with
    # unnormalized data)
    if normalize:
        x_norm = Normalizer().fit_transform(x_input)
    else:
        x_norm = x_input

    # fit the data set
    db_cluster.fit(x_norm)
    labels = db_cluster.labels_
    cluster_set, counts = np.unique(labels, return_counts=True)
    if verbose:
        print('Clustering for set size ', len(x_norm),
              ' - ', len(cluster_set), ' clusters')
        print('Individual cluster sizes: ',
              ', '.join([str(c) for c in counts]))

    # Iterate through clusters, adding exemplar to output frame
    # pylint: disable=C0200
    # we need to know the index of the item within the loop
    for idx in range(len(cluster_set)):
        cluster_id = cluster_set[idx]
        class_members = labels == cluster_id
        if isinstance(data, pd.DataFrame):
            last_event_time = data[class_members][-1:][time_column].iat[0]
        else:
            last_event_time = None

        if cluster_id == -1:
            # 'Noise' events are individual items that could not be assigned
            # to a cluster and so are unique
            if pd_version_23():
                clustered_events = clustered_events.append(
                    data[class_members].assign(Clustered=False,
                                               ClusterId=cluster_id,
                                               ClusterSize=1,
                                               LastEventTime=last_event_time),
                    sort=False)
            else:
                clustered_events = clustered_events.append(
                    data[class_members].assign(Clustered=False,
                                               ClusterId=cluster_id,
                                               ClusterSize=1,
                                               LastEventTime=last_event_time))
        else:
            # Otherwise, just choose the first example of the cluster set
            if pd_version_23():
                clustered_events = clustered_events.append(
                    data[class_members].assign(Clustered=True,
                                               ClusterId=cluster_id,
                                               ClusterSize=counts[idx],
                                               LastEventTime=last_event_time)[0:1],
                    sort=False)
            else:
                clustered_events = clustered_events.append(
                    data[class_members].assign(Clustered=True,
                                               ClusterId=cluster_id,
                                               ClusterSize=counts[idx],
                                               LastEventTime=last_event_time)[0:1])

    # pylint: enable=C0200
    if verbose:
        print('Cluster output rows: ', len(clustered_events))

    return clustered_events, db_cluster, x_norm


@export
def add_process_features(input_frame, path_separator=None, force=False):
    r"""
    Add numerical features based on patterns of command line and process name.

        :param input_frame: The input dataframe
        :param path_separator=None: Path separator - if not supplied, try to determine
            from 'NewProcessName' column of first 10 rows
        :param force=False: Forces re-calculation of feature columns even if they
            already exist

    Features:
        processNameLen: length of process file name (inc path)
        processNameTokens: the number of elements in the path
        processName: the process file name (minus path)
        commandlineTokens: number of space-separated tokens in the command line
        commandlineLen: length of the command line
        commandlineLogLen: log10 length of commandline
        isSystemSession: 1 if session Id is 0x3e7 for Windows or -1 for Linux
        commandlineTokensFull: counts number of token separators in commandline
            [\s\-\\/\.,"\'|&:;%$()]
        pathScore: sum of ord() value of characters in path
        pathLogScore: log10 of pathScore
        commandlineScore: sum of ord() value of characters in commandline
        commandlineLogScore: log10 of commandlineScore
    """
    output_df = input_frame.copy()

    # Set any NaN values to empty string
    if 'NewProcessName' in output_df and 'CommandLine' in output_df:
        output_df[['NewProcessName', 'CommandLine']] = output_df[['NewProcessName',
                                                                  'CommandLine']].fillna(value='')

    # try to determine the path separator
    if path_separator is None:
        sample_df = output_df.head(10)
        lx_path = len(sample_df[sample_df['NewProcessName'].str.contains('/')])
        if lx_path:
            path_separator = '/'
        else:
            path_separator = '\\'

    # Create features from process name and command line
    if 'NewProcessName' in output_df:
        if 'processNameLen' not in output_df or force:
            output_df['processNameLen'] = output_df.apply(lambda x:
                                                          len(x.NewProcessName),
                                                          axis=1)
        if 'processNameTokens' not in output_df or force:
            output_df['processNameTokens'] = output_df.apply(lambda x:
                                                             len(x.NewProcessName.split(
                                                                 path_separator)),
                                                             axis=1)
        if 'processName' not in output_df or force:
            output_df['processName'] = output_df.apply(lambda x:
                                                       x.NewProcessName.split(
                                                           path_separator)[-1],
                                                       axis=1)
        if 'pathScore' not in output_df or force:
            output_df['pathScore'] = output_df.apply(lambda x:
                                                     _string_score(
                                                         x.NewProcessName),
                                                     axis=1)
        if 'pathLogScore' not in output_df or force:
            output_df['pathLogScore'] = output_df.apply(lambda x:
                                                        log10(x.pathScore)
                                                        if x.pathScore else 0,
                                                        axis=1)

    if 'CommandLine' in output_df:
        if 'commandlineTokens' not in output_df or force:
            output_df['commandlineTokens'] = output_df.apply(lambda x:
                                                             len(x.CommandLine.split(
                                                                 path_separator)),
                                                             axis=1)
        if 'commandlineLen' not in output_df or force:
            output_df['commandlineLen'] = output_df.apply(lambda x:
                                                          len(x.CommandLine),
                                                          axis=1)
        if 'commandlineLogLen' not in output_df or force:
            output_df['commandlineLogLen'] = output_df.apply(lambda x:
                                                             log10(
                                                                 x.commandlineLen)
                                                             if x.commandlineLen else 0, axis=1)
        if 'commandlineTokensFull' not in output_df or force:
            delim_rgx = r'[\s\-\\/\.,"\'|&:;%$()]'
            output_df['commandlineTokensFull'] = (output_df[['CommandLine']]
                                                  .apply(lambda x: x.str.count(delim_rgx),
                                                         axis=1))

        if 'commandlineScore' not in output_df or force:
            output_df['commandlineScore'] = output_df.apply(lambda x:
                                                            _string_score(
                                                                x.CommandLine),
                                                            axis=1)
        if 'commandlineLogScore' not in output_df or force:
            output_df['commandlineLogScore'] = output_df.apply(lambda x:
                                                               log10(
                                                                   x.commandlineScore)
                                                               if x.commandlineScore else 0,
                                                               axis=1)

    if 'SubjectLogonId' in output_df:
        if (('isSystemSession' not in output_df or force) and 'SubjectLogonId' in output_df):
            output_df['isSystemSession'] = output_df.apply(lambda x:
                                                           True if x.SubjectLogonId == '0x3e7' or
                                                           x.SubjectLogonId == '-1' else False,
                                                           axis=1)

    return output_df


@export
def delim_count(input_row: pd.Series, column: str,
                delim_list: str = r'[\s\-\\/\.,"\'|&:;%$()]') -> int:
    r"""
    Count the delimiters in input column.

        :param input_row:pd.Series: The series to process
        :param column:str: Column name
        :param delim_list:str=r'[\s\-\\/\."\'|&:;%$()]: delimiters to use.
    """
    return input_row[column].str.count(delim_list)


@export
def char_ord_score(input_row: pd.Series, column: str, scale: int = 1) -> int:
    """
    Return sum of ord values of characters in string.

    This function sums the ordinal value of each character in the
    input string. Two strings with minor differences will result in
    a similar score. However, for strings with highly variable content
    (e.g. command lines or http requests containing GUIDs) this may result
    in too much variance to be useful when you are trying to detect
    similar patterns. You can use the scale parameter to reduce the
    influence of features using this function on clustering and anomaly
    algorithms.

    Arguments:
        input_row {pd.Series} -- The series to process
        column {str} -- Column name
        scale {int} -- reduce the scale of the feature (reducing the
            influence of variations this feature on the )

    Keyword Arguments:
        delimiter {str} -- Delimiter used to split the column string (default: {' '})

    Returns:
        {int} -- count of tokens

    """
    return floor(sum([ord(x) for x in input_row[column]]) / scale)


@export
def token_count(input_row: pd.Series, column: str, delimiter: str = ' ') -> int:
    """
    Return delimiter-separated tokens pd.Series column.

    Arguments:
        input_row {pd.Series} -- The series to process
        column {str} -- Column name

    Keyword Arguments:
        delimiter {str} -- Delimiter used to split the column string (default: {' '})

    Returns:
        {int} -- count of tokens

    """
    return len(input_row[column].split(delimiter))


def _string_score(input_str):
    """Sum the ord(c) for characters in a string."""
    return sum([ord(x) for x in input_str])
