# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""KQL Helper functions."""
import sys
from functools import partial
from typing import Tuple, Union, Optional, Any

import pandas as pd
from IPython import get_ipython
from deprecated.sphinx import deprecated

from .query_builtin_queries import query_definitions

# pylint: disable=locally-disabled, unused-import
# (list_queries not used here but want to bring in into module namespace)
from .query_mgr import (  # noqa: F401
    replace_prov_query_params,
    list_queries,
    clean_kql_query,
    query_help,
    print_kql,
)

# pylint: enable=locally-disabled, unused-import
from .utility import export
from .._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"

# pylint: disable=locally-disabled, invalid-name
_kql_magic_loaded = False
_ip = get_ipython()
# pylint: enable=locally-disabled, invalid-name


def load_kql():
    """Closure to maintain state of Kql Magic load."""
    _is_loaded = False

    def load_if_not_loaded():
        nonlocal _is_loaded
        if _is_loaded:
            return True

        print("Loading kql")
        _load_kql_magic()
        _is_loaded = _is_kqlmagic_loaded()
        if _is_loaded:
            return True
        return False

    return load_if_not_loaded


_KQL_LOADER = load_kql()


@deprecated(reason="Superceded by msticpy.data.QueryProvider", version="0.2.0")
@export
def load_kql_magic():
    """Load KqlMagic if not loaded."""
    # KqlMagic
    if not _KQL_LOADER():
        raise EnvironmentError("Kqlmagic did not load correctly.")


def _load_kql_magic():
    """Load KqlMagic if not loaded."""
    # KqlMagic
    print("Please wait. Loading Kqlmagic extension...")
    get_ipython().run_line_magic("reload_ext", "Kqlmagic")


def _is_kqlmagic_loaded() -> bool:
    """Return true if kql magic is loaded."""
    if _ip is not None:
        return _ip.find_magic("kql") is not None

    return False


@deprecated(reason="Superceded by msticpy.data.QueryProvider", version="0.2.0")
@export
def exec_query(
    query_name: str, **kwargs
) -> Union[pd.DataFrame, Tuple[pd.DataFrame, Any]]:
    """
    Execute kql query with optional parameters and return a Dataframe.

    Parameters
    ----------
    query_name : str
        the name of query to run

    Other Parameters
    ----------------
    kwargs : Mapping[str, Any]
        additional replacable paramters for the query
        Parameters supplied here will override default
        for the query and values and values extracted
        from QueryParamProviders supplied in the provs
        kw parameter.
    kql_result : bool
        If True - return (DataFrame, KqlResultSet) tuple.
        If False or not present return DataFrame only (default)
    provs : Iterable[QueryParamProvider]
        this should be a collection of objects that
        implement QueryParamProvider (from which query
        parameters can be extracted).

    Returns
    -------
    Union[pd.DataFrame, Tuple[pd.DataFrame, results.ResultSet]
        if kql_result == False (default) returns a tuple of
        dataframe and Kql ResultSet. Otherwise returns
        just dataframe.

    """
    if "kql_result" in kwargs:
        kql_result = kwargs.pop("kql_result")
    else:
        kql_result = False

    replaced_query = replace_prov_query_params(query_name=query_name, **kwargs)
    replaced_query = clean_kql_query(replaced_query)
    if replaced_query:
        result = _ip.run_cell_magic("kql", line="", cell=replaced_query)
        if result is not None and result.completion_query_info["StatusCode"] == 0:
            data_frame = result.to_dataframe()
            if result.is_partial_table:
                print("Warning - query returned partial results.")
            # Did user want both dataframe and ResultSet
            if not kql_result:
                return data_frame

            return data_frame, result

        print("Warning - query did not complete successfully.")
        print("Kql ResultSet returned - check  'completion_query_info' property.")
        return result
    raise ValueError("Could not resolve query or query parameters.")


@deprecated(reason="Superceded by msticpy.data.QueryProvider", version="0.2.0")
@export
def show_filled_query(query_name: str, **kwargs) -> str:
    """
    Print the kql query with replaced parameter values.

    Parameters
    ----------
    query_name : str
        the name of query to run

    Other Parameters
    ----------------
    kwargs : Mapping[str, Any]
        additional replacable paramters for the query
        Parameters supplied here will override default
        for the query and values and values extracted
        from QueryParamProviders supplied in the provs
        kw parameter.
    kql_result : bool
        If True - return (DataFrame, KqlResultSet) tuple.
        If False or not present return DataFrame only (default)
    provs : Iterable[QueryParamProvider]
        this should be a collection of objects that
        implement QueryParamProvider (from which query
        parameters can be extracted).

    Returns
    -------
    str
        The query string with populated parameters.

    """
    replaced_query = replace_prov_query_params(query_name=query_name, **kwargs)
    print_kql(replaced_query)
    return replaced_query


@deprecated(reason="Superceded by msticpy.data.QueryProvider", version="0.2.0")
@export
def exec_query_string(query: str) -> Tuple[Optional[pd.DataFrame], Any]:
    """
    Execute query string and return DataFrame of results.

    Parameters
    ----------
    query : str
        The kql query to execute

    Returns
    -------
    Tuple[Optional[pd.DataFrame], results.ResultSet]
        Tuple of DataFrame (if successfull) and
        and Kql ResultSet.

    """
    result = _ip.run_cell_magic("kql", line="", cell=query)
    if result is not None and result.completion_query_info["StatusCode"] == 0:
        data_frame = result.to_dataframe()
        if result.is_partial_table:
            print("Warning - query returned partial results.")
        # Did user want both dataframe and ResultSet

        return data_frame, result

    print("Warning - query did not complete successfully.")
    print("Kql ResultSet returned - check  'completion_query_info' property.")
    return None, result


# pylint: disable=duplicate-code
def _add_queries_to_module(module_name):
    """Add queries to the module as callable methods."""
    if module_name not in sys.modules:
        raise LookupError(f"Module {module_name} was not found sys.modules")
    for query_name in query_definitions:
        module = sys.modules[module_name]
        query_func = partial(exec_query, query_name=query_name)
        query_func.__doc__ = exec_query.__doc__
        setattr(module, query_name, query_func)


# pylint: enable=duplicate-code


# Add all queries defined in builtin queries module as functions
if __name__ != "__main__":
    _add_queries_to_module(__name__)
