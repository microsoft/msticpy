# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Parameter extractor helper functions for use with IPython/Juptyer queries."""
from typing import List, Dict, Tuple, Any

from .query_store import QuerySource
from ..nbtools.utility import export
from ..nbtools.query_defns import QueryParamProvider
from .._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"


@export
def extract_query_params(
    query_source: QuerySource, *args, **kwargs
) -> Tuple[Dict[str, Any], List[str]]:
    """
    Get the parameters needed for the query.

    Parameters
    ----------
    query_params : List[str]
        Query parameters
    args : Tuple[QueryParamProvider]
        objects that implement QueryParamProvider
        (from which query parameters can be extracted).
    kwargs : Dict[str, Any]
        custom parameter list to populate queries
        (override default values and values extracted
        from QueryParamProviders).

    Returns
    -------
    Tuple[Dict[str, Any], List[str]]
        Dictionary of parameter names and values to be used
        in the query.
        List of any missing parameters

    """
    # get the required parameters for this query and build a dictionary
    req_param_names = query_source.required_params.keys()
    req_params: Dict[str, Any] = {param: None for param in req_param_names}

    # Iterate through required parameters. If any are set in the supplied
    # provider objects, assign them to our output dictionary
    query_providers = [prov for prov in args if hasattr(prov, "query_params")]
    for provider in query_providers:
        for param in req_param_names:
            if param in provider.query_params:
                req_params[param] = provider.query_params[param]

    # If any custom parameters have been supplied add these
    # overriding any parameters from the QueryParamProviders
    if kwargs:
        req_params.update(kwargs)

    # If we have missing parameters try to retrieve them
    # as attributes of the object
    missing_params = [p_name for p_name, p_value in req_params.items() if not p_value]
    if missing_params:
        _get_missing_params(args, missing_params, req_params)

    return req_params, missing_params


def _get_missing_params(
    args: Tuple[Any, ...], missing_params: List[str], req_params: Dict[str, Any]
):
    """
    Get missing params from arguments.

    Parameters
    ----------
    args : Tuple[Any]
        Args list from calling funtion
    missing_params : List[str]
        The list of missing parameters to get
    req_params : Dict[str, str]
        Dictionary of required parameters

    """
    for arg_object in [obj for obj in args if not isinstance(obj, QueryParamProvider)]:
        for m_param in missing_params:
            if isinstance(arg_object, dict) and m_param in arg_object:
                req_params[m_param] = arg_object.get(m_param, None)
            elif hasattr(arg_object, m_param):
                req_params[m_param] = getattr(arg_object, m_param)
        missing_params = [
            p_name for p_name, p_value in req_params.items() if p_value is not None
        ]
