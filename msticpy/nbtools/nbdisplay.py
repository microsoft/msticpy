# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Module for common display functions."""

import matplotlib.pyplot as plt
import networkx as nx
import numpy as np
import pandas as pd
from bokeh.io import output_notebook, show
from bokeh.models import (ColumnDataSource, DatetimeTickFormatter, HoverTool,
                          Label)
from bokeh.plotting import figure, reset_output
from IPython.core.display import HTML, display
from IPython.display import Javascript

from .security_alert import SecurityAlert
from .utility import export
from .. _version import VERSION

__version__ = VERSION
__author__ = 'Ian Hellen'


@export
def display_alert(alert=None, show_entities=False):
    """
    Display the alert properties as HTML.

        :param alert: The alert to display
            pd.Series or SecurityAlert
    """
    if alert is None:
        return

    if isinstance(alert, SecurityAlert):
        display(HTML(alert.to_html(show_entities=False)))
        if show_entities:
            for entity in alert.entities:
                print(entity)
        return

    # Display subset of raw properties
    if isinstance(alert, pd.Series):
        entity = (alert['CompromisedEntity']
                  if 'CompromisedEntity' in alert
                  else '')
        title = '''
            <h3>Alert: '{name}'</h3><br>time=<b>{start}</b>, entity=<b>{entity}</b>, id=<b>{id}</b>
            '''.format(start=alert['StartTimeUtc'],
                       name=alert['AlertDisplayName'],
                       entity=entity,
                       id=alert['ProviderAlertId'])
        display(HTML(title))
        display(pd.DataFrame(alert))
    else:
        raise ValueError(
            'Unrecognized alert object type ' + str(type(alert)))


def _print_process(process_row: pd.Series):
    if process_row.NodeRole == 'parent':
        if process_row.Level > 1:
            level = 0
        else:
            level = 1
    elif process_row.NodeRole == 'source':
        level = 2
    elif process_row.NodeRole == 'child':
        level = 3 + process_row.Level
    else:
        level = 2

    spaces = 20 * level * 2
    if process_row.NodeRole == 'source':
        line1 = '''
        <span style="color:red;font-weight:bold">[alert:lev{}] {} {}
        [PID: {}, SubjSess:{}, TargSess:{}]</span>
        '''.format(process_row.Level,
                   process_row.TimeCreatedUtc,
                   process_row.NewProcessName,
                   process_row.NewProcessId,
                   process_row.SubjectLogonId,
                   process_row.TargetLogonId)
    else:
        line1 = '[{}:lev{}] {} <b>{}</b> [PID: {}, SubjSess:{}, TargSess:{}]'.format(
            process_row.NodeRole,
            process_row.Level,
            process_row.TimeCreatedUtc,
            process_row.NewProcessName,
            process_row.NewProcessId,
            process_row.SubjectLogonId,
            process_row.TargetLogonId)

    line2 = '(Cmdline: \'{}\') [Account: \'{}\']'.format(
        process_row.CommandLine, process_row.SubjectUserName)

    display(HTML('<div style="margin-left:{indent}px">{txt}<br>{txt2}</div>'.format(indent=spaces,
                                                                                    txt=line1,
                                                                                    txt2=line2)))


@export
def display_process_tree(process_tree: pd.DataFrame):
    """
    Display process tree data frame.

        :param process_tree
    """
    tree = process_tree[['TimeCreatedUtc', 'NodeRole', 'Level', 'NewProcessName',
                         'CommandLine', 'SubjectUserName', 'NewProcessId', 'ProcessId',
                         'SubjectLogonId', 'TargetLogonId']]
    tree = tree.sort_values(by=['TimeCreatedUtc'], ascending=False)

    display(HTML("<h3>Alert process tree:</h3>"))
    tree.sort_values(by=['TimeCreatedUtc']).apply(_print_process, 1)


@export
def exec_remaining_cells():
    """Execute all cells below currently selected cell."""
    Javascript("Jupyter.notebook.execute_cells_below()")


@export
def draw_alert_entity_graph(nx_graph: nx.Graph, font_size: int = 12,
                            height: int = 15, width: int = 15,
                            margin: float = 0.3, scale: int = 1):
    """
    "Draw networkX graph with matplotlib.

    Arguments:
        nx_graph {networkx.graph} -- [description]

    Keyword Arguments:
        font_size {int} -- base font size (default: {12})
        height {int} -- Image height (default: {15})
        width {int} -- Image width (default: {20})
        margin {float} -- Image margin (default: {0.3})
        scale {int} -- Position scale (default: {1})
    """
    alert_node = [n for (n, node_type) in
                  nx.get_node_attributes(nx_graph, 'node_type').items()
                  if node_type == 'alert']
    entity_nodes = [n for (n, node_type) in
                    nx.get_node_attributes(nx_graph, 'node_type').items()
                    if node_type == 'entity']

    # now draw them in subsets  using the `nodelist` arg
    plt.rcParams['figure.figsize'] = (width, height)

    plt.margins(x=margin, y=margin)

    pos = nx.kamada_kawai_layout(nx_graph, scale=scale, weight='weight')
    nx.draw_networkx_nodes(nx_graph, pos, nodelist=alert_node,
                           node_color='red', alpha=0.5, node_shape='o')
    nx.draw_networkx_nodes(nx_graph, pos, nodelist=entity_nodes,
                           node_color='green', alpha=0.5, node_shape='s',
                           s=200)
    nlabels = nx.get_node_attributes(nx_graph, 'description')
    nx.relabel_nodes(nx_graph, nlabels)
    nx.draw_networkx_labels(nx_graph, pos, nlabels, font_size=font_size)
    nx.draw_networkx_edges(nx_graph, pos)
    elabels = nx.get_edge_attributes(nx_graph, 'description')
    nx.draw_networkx_edge_labels(nx_graph, pos, edge_labels=elabels,
                                 font_size=font_size * 2 / 3, alpha=0.6)


@export
def display_timeline(data, alert=None, overlay_data=None, title: str = None,
                     time_column: str = 'TimeGenerated',
                     source_columns: list = None,
                     overlay_colums: list = None,
                     height: int = 300):
    """
    Display a timeline of events.

    Arguments:
        data {pd.DataFrame} -- Input DataFrame

    Keyword Arguments:
        alert {SecurityAlert} -- Input alert (optional) (default: {None})
        overlay_data {pd.DataFrame} -- Second event stream (DataFrame)
            to display as overlay (default: {None})
        title {str} -- [description] (default: {None})
        time_column {str} -- The name of the time
            property used in the Dataframe(s) (default: {'TimeGenerated'})
        source_columns {list} -- List of source columns to use in
            tooltips (default: {None})
        overlay_colums {list} -- List of source columns to use in
            overlay data tooltips (default: {None})
        heigh {int} -- the height of the plot figure (under 300 limits access
            to Bokeh tools)
    """
    reset_output()
    output_notebook()

# pylint: disable=C0103
    WRAP = 50
    WRAP_CMDL = 'WrapCmdl'
# pylint: enable=C0103
    y_max = 1

    if not source_columns:
        source_columns = ['NewProcessName', 'EventID', 'CommandLine']
    if time_column not in source_columns:
        source_columns.append(time_column)

    if 'CommandLine' in source_columns:
        graph_df = data[source_columns].copy()
        graph_df[WRAP_CMDL] = graph_df.apply(lambda x:
                                             _wrap_text(x.CommandLine, WRAP),
                                             axis=1)
    else:
        graph_df = data[source_columns].copy()

    # if we have an overlay - add this data and shift the y co-ordinates to
    # show on two separate lines
    if overlay_data is not None:
        overlay_colums = (overlay_colums if overlay_colums is not None
                          else source_columns)
        if time_column not in overlay_colums:
            overlay_colums.append(time_column)
        if 'CommandLine' in overlay_colums:
            overlay_df = overlay_data[overlay_colums].copy()
            overlay_df[WRAP_CMDL] = overlay_df.apply(lambda x:
                                                     _wrap_text(
                                                         x.CommandLine, WRAP),
                                                     axis=1)
        else:
            overlay_df = overlay_data[overlay_colums].copy()
        graph_df['y_index'] = 2
        overlay_df['y_index'] = 1
        y_max = 2
    else:
        graph_df['y_index'] = 1

    source = ColumnDataSource(graph_df)

    # build the tool tips from columns (excluding these)
    excl_cols = [time_column, 'CommandLine']
    tool_tip_items = [(f'{col}', f'@{col}')
                      for col in source_columns if col not in excl_cols]
    if WRAP_CMDL in graph_df:
        tool_tip_items.append(('CommandLine', f'@{WRAP_CMDL}'))
    hover = HoverTool(
        tooltips=tool_tip_items,
        formatters={'Tooltip': 'printf'}
        # display a tooltip whenever the cursor is vertically in line with a glyph
        # ,mode='vline'
    )

    if not title:
        title = 'Event Timeline'
    else:
        title = 'Timeline {}'.format(title)

    # tools = 'pan, box_zoom, wheel_zoom, reset, undo, redo, save, hover'
    plot = figure(min_border_left=50, plot_height=height, plot_width=900,
                  x_axis_label='Event Time', x_axis_type='datetime', x_minor_ticks=10,
                  tools=[hover, 'pan', 'xwheel_zoom', 'box_zoom', 'reset'],
                  title=title)
    plot.yaxis.visible = False

    # Tick formatting for different zoom levels
    # '%H:%M:%S.%3Nms
    tick_format = DatetimeTickFormatter()
    tick_format.days = ['%m-%d %H:%M']
    tick_format.hours = ['%H:%M:%S']
    tick_format.minutes = ['%H:%M:%S']
    tick_format.seconds = ['%H:%M:%S']
    tick_format.milliseconds = ['%H:%M:%S.%3N']

    plot.xaxis[0].formatter = tick_format
    plot.circle(x=time_column, y='y_index', color='navy',
                alpha=0.5, size=10, source=source)

    if overlay_data is not None:
        overlay_source = ColumnDataSource(overlay_df)
        plot.circle(x=time_column, y='y_index', color='green',
                    alpha=0.5, size=10, source=overlay_source)

    # Adding data labels stops everything working!
    # labels = LabelSet(x=time_column, y='y_index', y_offset=5,
    #                   text='NewProcessName', source=source,
    #                   angle='90deg', text_font_size='8pt')
    # p.add_layout(labels)

    # if we have an alert, plot the time as a line
    if alert is not None:
        x_alert_label = pd.Timestamp(alert['StartTimeUtc'])
        plot.line(x=[x_alert_label, x_alert_label], y=[0, y_max + 1])
        alert_label = Label(x=x_alert_label, y=0, y_offset=10, x_units='data', y_units='data',
                            text='< Alert time', render_mode='css',
                            border_line_color='red', border_line_alpha=1.0,
                            background_fill_color='white', background_fill_alpha=1.0)

        plot.add_layout(alert_label)

        print('Alert start time = ', alert['StartTimeUtc'])

    show(plot)


def _wrap_text(source_string, wrap_len):
    if len(source_string) <= wrap_len:
        return source_string
    out_string = ''
    input_parts = source_string.split()
    out_line = ''
    for part in input_parts:
        if len(part) > wrap_len:
            if out_line:
                out_string += out_line + '\n'
                out_line = ''
            out_line = part[0:wrap_len] + '...'
        else:
            if out_line:
                out_line += ' ' + part
            else:
                out_line = part
            if len(out_line) > wrap_len:
                out_string += out_line + '\n'
                out_line = ''

    return out_string


# Constants for Windows logon
_WIN_LOGON_TYPE_MAP = {0: 'Unknown',
                       2: 'Interactive', 3: 'Network', 4: 'Batch', 5: 'Service',
                       7: 'Unlock', 8: 'NetworkCleartext', 9: 'NewCredentials',
                       10: 'RemoteInteractive', 11: 'CachedInteractive'}
_WINDOWS_SID = {'S-1-0-0': 'Null SID', 'S-1-5-18': 'LOCAL_SYSTEM',
                'S-1-5-19': 'LOCAL_SERVICE', 'S-1-5-20': 'NETWORK_SERVICE'}
_ADMINISTRATOR_SID = '500'
_GUEST_SID = '501'
_DOM_OR_MACHINE_SID = 'S-1-5-21'


@export
def display_logon_data(logon_event: pd.DataFrame, alert: SecurityAlert = None,
                       os_family: str = None):
    """
    Display logon data for one or more events.

    Arguments:
        :logon_event: Dataframe containing one or more logon events
        :security alert: obtain os_family from the security alert
        :os_family: explicitly specify os_family (Linux or Windows)

    """
    if not os_family:
        os_family = alert.os_family if alert else 'Windows'

    for _, logon_row in logon_event.iterrows():
        print('### Account Logon')
        print('Account: ', logon_row['TargetUserName'])
        print('Account Domain: ', logon_row['TargetDomainName'])
        print('Logon Time: ', logon_row['TimeGenerated'])

        if os_family == 'Windows':
            logon_type = logon_row['LogonType']
            logon_desc_idx = logon_type
            if logon_type not in _WIN_LOGON_TYPE_MAP:
                logon_desc_idx = 0
            print(f'Logon type: {logon_type} ({_WIN_LOGON_TYPE_MAP[logon_desc_idx]})')

        account_id = logon_row.TargetUserSid
        print('User Id/SID: ', account_id)
        if os_family == 'Windows':
            _print_sid_info(account_id)
        else:
            print('Audit user: ', logon_row['audit_user'])

        session_id = logon_row['TargetLogonId']
        print(f'Session id \'{session_id}\'', end='  ')
        if session_id == '0x3e7' or session_id == '-1':
            print('System logon session')

        print()
        domain = logon_row['SubjectDomainName']
        if not domain:
            subj_account = logon_row.SubjectUserName
        else:
            subj_account = f'{domain}/{logon_row.SubjectUserName}'
        print('Subject (source) account: ', subj_account)

        print('Logon process: ', logon_row['LogonProcessName'])
        print('Authentication: ', logon_row['AuthenticationPackageName'])
        print('Source IpAddress: ', logon_row['IpAddress'])
        print('Source Host: ', logon_row['WorkstationName'])
        print('Logon status: ', logon_row['Status'])
        print()


def _print_sid_info(sid):
    if sid in _WINDOWS_SID:
        print('    SID {} is {}'.format(sid, _WINDOWS_SID[sid]))
    elif sid.endswith(_ADMINISTRATOR_SID):
        print('    SID {} is administrator'.format(sid))
    elif sid.endswith(_GUEST_SID):
        print('    SID {} is guest'.format(sid))
    if sid.startswith(_DOM_OR_MACHINE_SID):
        print('    SID {} is local machine or domain account'.format(sid))


@export
def plot_cluster(db_cluster, data, X, plot_label=None, plot_features=[0, 1], verbose=False, 
                 cut_off=3, xlabel=None, ylabel=None):
    """
    [summary]

    Arguments:
        db_cluster {[type]} -- DBScan Cluster (from SkLearn DBSCAN)
        data {[type]} -- Dataframe containing original data
        X {[type]} -- The DBSCAN predict numpy array

    Keyword Arguments:
        plot_label {str} -- If set the column to use to label data points
            (default: {None})
        plot_features {list} -- [description] Which two features in X to plot
        verbose {bool} -- Verbose execution with some extra info (default: {False})
        cut_off {int} -- The cluster size below which items are considered
            outliers (default: {3})
        xlabel {[type]} -- x-axis label (default: {None})
        ylabel {[type]} -- y-axis label (default: {None})
    """
    if plot_features[0] >= X.shape[1]:
        raise ValueError("plot_features[0] index must be a value from 0 to {}."
                         .format(X.shape[1] - 1))
    if plot_features[1] >= X.shape[1]:
        raise ValueError("plot_features[1] index must be a value from 0 to {}."
                         .format(X.shape[1] - 1))
    if plot_features[0] == plot_features[1]:
        raise ValueError("plot_features indexes must be 2 different values in range 0 to {}."
                         .format(X.shape[1] - 1))

    labels = db_cluster.labels_
    core_samples_mask = np.zeros_like(labels, dtype=bool)
    core_samples_mask[db_cluster.core_sample_indices_] = True
    unique_labels = set(labels)
    colors = [plt.cm.Spectral(each)
              for each in np.linspace(0, 1, len(unique_labels))]
    # Number of clusters in labels, ignoring noise if present.
    n_clusters_ = len(set(labels)) - (1 if -1 in labels else 0)
    n_noise_ = list(labels).count(-1)
    _, counts = np.unique(labels, return_counts=True)

    if verbose:
        print('Estimated number of clusters: %d' % n_clusters_)
        print('Estimated number of noise points: %d' % n_noise_)
        # print("Silhouette Coefficient: %0.3f"
        #       % metrics.silhouette_score(X, labels))

    if not isinstance(data, pd.DataFrame):
        plot_label = None
    elif plot_label is not None and plot_label not in data:
        plot_label = None

    p_label = None
    for cluster_id, color in zip(unique_labels, colors):
        if cluster_id == -1:
            # Black used for noise.
            color = [0, 0, 0, 1]
        class_member_mask = (labels == cluster_id)

        cluster_size = counts[cluster_id]
        marker_size = cluster_size
        marker = 'o'
        font_size = 'small'
        alpha = 0.4

        if cluster_size < cut_off:
            marker = '+'
            marker_size = 10
            font_size = 'large'
            alpha = 1.0
        first_row = data[class_member_mask].iloc[0]
        xy = X[class_member_mask & core_samples_mask]
        plt.plot(xy[:, plot_features[0]], xy[:, plot_features[1]], marker,
                 markerfacecolor=tuple(color),
                 markersize=marker_size)

        if plot_label:
            if len(first_row) > 0 and plot_label in first_row:
                p_label = first_row[plot_label]
                try:
                    plt.annotate(s=p_label, xy=(xy[0, plot_features[0]], xy[0, plot_features[1]]),
                                 fontsize=font_size, alpha=alpha)
                except IndexError:
                    pass

    plt.xlabel(xlabel)
    plt.ylabel(ylabel)
    plt.title('Estimated number of clusters: %d' % n_clusters_)
    plt.show()
    return plt
