# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""KQL Helper functions."""
import sys
from functools import partial

import pandas as pd
from IPython import get_ipython
from Kqlmagic import results

from . query_builtin_queries import query_definitions
# pylint: disable=locally-disabled, W0611
# (list_queries not used here but want to bring in into module namespace)
from . query_mgr import (replace_prov_query_params, list_queries,
                         clean_kql_query, query_help, print_kql)
# pylint: enable=locally-disabled, W0611
from . utility import export
from .. _version import VERSION

__version__ = VERSION
__author__ = 'Ian Hellen'

# pylint: disable=locally-disabled, C0103
_kql_magic_loaded = False
_ip = get_ipython()
# pylint: enable=locally-disabled, C0103


@export
def load_kql_magic():
    """Load KqlMagic if not loaded."""
    # KqlMagic
    if not _is_kqlmagic_loaded():
        print('Please wait. Loading Kqlmagic extension...')
        get_ipython().run_line_magic('reload_ext', 'Kqlmagic')
        # get_ipython().run_line_magic('config', 'Kqlmagic.auto_dataframe=True')
        if not _is_kqlmagic_loaded():
            raise EnvironmentError('Kqlmagic did not load correctly.')


def _is_kqlmagic_loaded() -> bool:
    """Return true if kql magic is loaded."""
    # pylint: disable=locally-disabled, C0103, W0603
    global _kql_magic_loaded
    # pylint: enable=locally-disabled, C0103, W0603

    if _ip is not None:
        _kql_magic_loaded = _ip.find_magic('kql') is not None
        return _kql_magic_loaded


@export
def exec_query(query_name: str, **kwargs) -> (pd.DataFrame, results.ResultSet):
    """
    Execute kql query with optional parameters and return a Dataframe.

    Use list_queries() to see the current set).
    Use query_help(query_name) to view the query and expected paramaters
    Arguments:
        query_name {string}: the name of query to run
        kwargs: additional replacable paramters for the query
            {string:bool} kql_result=True - return (DataFrame, KqlResultSet)
                    tuple.
                    kql_result=False - return DataFrame only (default)
            {string:[QueryParamProvider]} - for the key 'provs'
                (or alias 'providers')
                this should be a collection of objects that
                implement QueryParamProvider (from which query
                parameters can be extracted).
                OR
            {string:value pairs} -- custom parameter list
                (override default values and values extracted
                from QueryParamProviders).
    Returns:
        dataframe {pd.DataFrame}: if kql_result == False (default).
        (dataframe, ResultSet): tuple of dataframe and Kql ResultSet
            if kql_result==True (pass this as a kw argument).

    """
    if 'kql_result' in kwargs:
        kql_result = kwargs.pop('kql_result')
    else:
        kql_result = False

    replaced_query = replace_prov_query_params(query_name=query_name, **kwargs)
    replaced_query = clean_kql_query(replaced_query)
    if replaced_query:
        result = _ip.run_cell_magic('kql', line='', cell=replaced_query)
        if result is not None and result.completion_query_info['StatusCode'] == 0:
            data_frame = result.to_dataframe()
            if result.is_partial_table:
                print("Warning - query returned partial results.")
            # Did user want both dataframe and ResultSet
            if not kql_result:
                return data_frame
            else:
                return data_frame, result

        print("Warning - query did not complete successfully.")
        print("Kql ResultSet returned - check  \'completion_query_info\' property.")
        return result


@export
def show_filled_query(query_name: str, **kwargs) -> (pd.DataFrame, results.ResultSet):
    """
    Print the kql query with replaced parameter values.

    Use list_queries() to see the current set).
    Use query_help(query_name) to view the query and expected paramaters
    Arguments:
        query_name {string}: the name of query to run
        kwargs: additional replacable paramters for the query
            {string:bool} kql_result=True - return (DataFrame, KqlResultSet)
                    tuple.
                    kql_result=False - return DataFrame only (default)
            {string:[QueryParamProvider]} - for the key 'provs'
                (or alias 'providers')
                this should be a collection of objects that
                implement QueryParamProvider (from which query
                parameters can be extracted).
                OR
            {string:value pairs} -- custom parameter list
                (override default values and values extracted
                from QueryParamProviders).
    Returns:
        replaced_query {str}: the query with substituted parameters.

    """
    replaced_query = replace_prov_query_params(query_name=query_name, **kwargs)
    print_kql(replaced_query)
    return replaced_query


def _add_queries_to_module(module_name):
    """Add queries to the module as callable methods."""
    if module_name not in sys.modules:
        raise LookupError(f'Module {module_name} was not found sys.modules')
    for query_name in query_definitions:
        module = sys.modules[module_name]
        query_func = partial(exec_query, query_name=query_name)
        query_func.__doc__ = exec_query.__doc__
        setattr(module, query_name, query_func)


# Add all queries defined in builtin queries module as functions
if __name__ != '__main__':
    _add_queries_to_module(__name__)
