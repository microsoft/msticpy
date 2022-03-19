# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Parameter extractor helper functions for use with IPython/Juptyer queries."""
from typing import Any, Dict, List, Mapping, Tuple

from ..._version import VERSION
from ...common.utility import export
from .query_defns import QueryParamProvider
from .query_store import QuerySource

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
    query_source : QuerySource
        Query source
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
    # get the parameters for this query
    all_params = query_source.params

    # required_params are those that don't have defaults set in the query
    # template. Build a dictionary to hold the values. This will contain
    # at least the required params plus any that are extracted from args and
    # kwargs and have been added dynamically.
    req_param_names = query_source.required_params.keys()
    req_params: Dict[str, Any] = {param: None for param in req_param_names}

    # try to retrieve any parameters as attributes of the args objects
    _get_object_params(args, all_params, req_params)

    # If any kwargs parameters have been supplied, add these.
    # These any parameters obtained from _get_object_params
    if kwargs:
        resolved_params = query_source.resolve_param_aliases(kwargs)
        req_params.update(
            {key: arg for key, arg in resolved_params.items() if key in all_params}
        )

    # Get the names of any params that were required but we didn't
    # find a value for
    missing_params = [
        p_name for p_name, p_value in req_params.items() if p_value is None
    ]
    return req_params, missing_params


def _get_object_params(
    args: Tuple[Any, ...], params: Mapping[str, Any], req_params: Dict[str, Any]
):
    """
    Get params from attributes of arg objects.

    Parameters
    ----------
    args : Tuple[Any]
        Args list from calling function
    params : Mapping[str, Dict[str, Any]]
        The list of parameter names to look for
    req_params : Dict[str, str]
        Dictionary of required parameters

    """
    remaining_params = list(params.keys())
    for arg_object in args:
        if isinstance(arg_object, (str, int, float, bool)):
            # ignore some common primitive types
            continue
        for param in remaining_params:
            if isinstance(arg_object, QueryParamProvider):
                if param in arg_object.query_params:
                    req_params[param] = arg_object.query_params[param]
            elif isinstance(arg_object, dict) and param in arg_object:
                req_params[param] = arg_object.get(param, None)
            elif hasattr(arg_object, param):
                req_params[param] = getattr(arg_object, param)
        remaining_params = [
            p_name for p_name, p_value in req_params.items() if p_value is not None
        ]
