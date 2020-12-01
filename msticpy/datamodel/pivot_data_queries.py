# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Pivot query functions class."""
import itertools
from collections import defaultdict, namedtuple
from functools import wraps
from typing import Any, Callable, Dict, Iterable, List, Optional, Tuple, Type

import pandas as pd

from ..common.timespan import TimeSpan
from .._version import VERSION
from ..data.data_providers import QueryProvider
from ..data.query_container import QueryContainer
from . import entities

__version__ = VERSION
__author__ = "Ian Hellen"


ParamAttrs = namedtuple("ParamAttrs", "type, query, family, required")
QueryParams = namedtuple("QueryParams", "all, required, full_required, param_attrs")

_DEF_IGNORE_PARAM = {"start", "end"}


class PivotQueryFunctions:
    """Class to retrieve the queries and params from a provider."""

    current = None

    def __init__(self, query_provider: QueryProvider, ignore_reqd: List[str] = None):
        """
        Instantiate PivotQueryFunctions class.

        Parameters
        ----------
        query_provider : [type]
            The query provider to load
        ignore_reqd : List[str], optional
            List of parameters to ignore when building the required
            parameters list (e.g. ['start', 'end']), by default None

        """
        self.__class__.current = self
        self._provider = query_provider
        self.param_usage: Dict[str, List[ParamAttrs]] = defaultdict(list)
        self.query_params: Dict[str, QueryParams] = {}

        ignore_params = set(ignore_reqd) if ignore_reqd else _DEF_IGNORE_PARAM

        for family, fam_dict in self._provider.query_store.data_families.items():
            for src_name, q_source in fam_dict.items():
                reqd_params = set(q_source.required_params.keys()) - ignore_params

                for param, p_attrs in q_source.params.items():
                    self.param_usage[param].append(
                        ParamAttrs(
                            p_attrs["type"],
                            src_name,
                            family,
                            bool(param in reqd_params),
                        )
                    )
                self.query_params[f"{family}.{src_name}"] = QueryParams(
                    all=list(q_source.params),
                    required=list((set(q_source.required_params) - ignore_params)),
                    full_required=list(q_source.required_params),
                    param_attrs={
                        param: ParamAttrs(
                            p_attrs["type"],
                            src_name,
                            family,
                            bool(param in reqd_params),
                        )
                        for param, p_attrs in q_source.params.items()
                    },
                )

    def get_queries_and_types_for_param(
        self, param: str
    ) -> Iterable[Tuple[str, str, str, Callable[[Any], Any]]]:
        """
        Get queries and parameter data types for `param`.

        Parameters
        ----------
        param : str
            The parameter name.

        Returns
        -------
        Iterable[Tuple[str, str, Callable[[Any], Any]]]
            Iterable of tuples listing:
            query_name, param_type, query_func

        Yields
        -------
        Iterator[Iterable[Tuple[str, str, str, Callable[[Any], Any]]]]
            Iterable of tuples listing:
            query_name, query_path, param_type, query_func

        """
        param_usage = self.param_usage.get(param)
        if param_usage is None:
            return []
        get_param_props = (
            (param.query, param.family, param.type, f"{param.family}.{param.query}")
            for param in param_usage
        )
        return [
            (q_name, q_family, p_type, getattr(self._provider, q_func), q_func)  # type: ignore
            for q_name, q_family, p_type, q_func in get_param_props
        ]

    def get_queries_for_param(
        self, param: str
    ) -> Iterable[Tuple[str, str, Callable[[Any], Any]]]:
        """
        Get the list of queries for a parameter.

        Parameters
        ----------
        param : str
            Parameter name

        Returns
        -------
        Iterable[Tuple[str, str, Callable[[Any], Any]]]
            Iterable of tuples listing:
            query_name, query_func

        Yields
        -------
        Iterator[Iterable[Tuple[str, Callable[[Any], Any]]]]
            Iterable of tuples listing:
            query_name, query_path, query_func
        """
        param_usage = self.param_usage.get(param)
        if not param_usage:
            return []
        return [
            (query_name, query_family, getattr(self._provider, query_func_name))
            for query_name, query_family, query_func_name in (
                (param.query, param.family, f"{param.family}.{param.query}")
                for param in self.param_usage.get(param)  # type:ignore
            )
        ]

    def get_params(self, query_func_name: str) -> Optional[QueryParams]:
        """
        Get the parameters for a query function.

        Parameters
        ----------
        query_func_name : str
            Query name - the name must be fully-qualified
            (e.g. 'WindowsSecurity.list_processes')

        Returns
        -------
        QueryParams
            QueryParams named tuple
            (all, required, full_required)

        """
        return self.query_params.get(query_func_name)

    def get_param_attrs(self, param_name: str) -> List[ParamAttrs]:
        """
        Get the attributes for a parameter name.

        Parameters
        ----------
        param_name : str
            Parameter name

        Returns
        -------
        List[ParamAttrs]
            List of ParamAttrs named tuples:
            (type, query, family, required)

        Notes
        -----
        Since parameters may be defined for multiple queries, the
        set of parameter attributes will be returned for each query.

        """
        return self.param_usage.get(param_name, [])


PARAM_ENTITY_MAP: Dict[str, List[Tuple[Type[entities.Entity], str]]] = {
    "account_name": [(entities.Account, "Name")],
    "host_name": [(entities.Host, "fqdn")],
    "process_name": [(entities.Process, "ProcessFilePath")],
    "source_ip_list": [(entities.IpAddress, "Address")],
    "ip_address_list": [(entities.IpAddress, "Address")],
    "ip_address": [(entities.IpAddress, "Address")],
    "user": [(entities.Account, "Name")],
    "observables": [
        (entities.IpAddress, "Address"),
        (entities.Dns, "DomainName"),
        (entities.File, "file_hash"),
        (entities.Url, "Url"),
    ],
    "logon_session_id": [
        (entities.Process, "LogonSession"),
        (entities.HostLogonSession, "SessionId"),
        (entities.Account, "LogonId"),
    ],
    "proc_op": [],
    "process_id": [(entities.Process, "ProcessId")],
    "commandline": [(entities.Process, "CommandLine")],
    "url": [(entities.Url, "Url")],
    "file_hash": [(entities.File, "file_hash")],
}


def add_data_queries_to_entities(
    provider: QueryProvider, get_timespan: Callable[[], TimeSpan]
):
    """
    Add data queries from `provider` to entities.

    Parameters
    ----------
    provider : QueryProvider
        Query provider
    get_timespan : Callable[[], TimeSpan]
        Callback to get time span

    """
    q_funcs = PivotQueryFunctions(provider)

    add_queries_to_entities(
        prov_qry_funcs=q_funcs,
        container=provider.environment,
        get_timespan=get_timespan,
    )


# pylint: disable=too-many-locals


def add_queries_to_entities(
    prov_qry_funcs: PivotQueryFunctions,
    container: str,
    get_timespan: Callable[[], TimeSpan],
):
    """
    Add data queries to entities.

    Parameters
    ----------
    prov_qry_funcs : PivotQueryFunctions
        Collection of wrapped query functions
    container : str
        The name of the container to add query functions to
    get_timespan : Callable[[], TimeSpan]
        Function to get the current timespan.

    """
    # For each parameter in the parameter map
    for param_name, entity_list in PARAM_ENTITY_MAP.items():

        param_funcs = list(prov_qry_funcs.get_queries_for_param(param_name))
        if not (entity_list and param_funcs):
            continue
        entity_funcs = itertools.product(entity_list, param_funcs)
        # For each entity/query combo that uses this parameter
        for (entity_cls, _), (name, family, func) in entity_funcs:
            func_params = prov_qry_funcs.get_params(f"{family}.{name}")
            if not func_params or len(func_params.all) == 0:
                # ignore any functions without parameters
                continue

            # If multiple params - get the ones that are available in the same entity
            # We could in the future get parameters for connected (graph) entities.
            param_entities = {
                param: (ent, attr)
                for param, ent_list in PARAM_ENTITY_MAP.items()
                for ent, attr in ent_list
                if param in func_params.all and ent == entity_cls
            }
            # Build the map of param names to entity attributes
            attr_map = {
                param: ent_attr for param, (_, ent_attr) in param_entities.items()
            }
            # Wrap the function
            cls_func = _param_and_call_wrapper(
                func, func_params.param_attrs, attr_map, get_timespan
            )

            # Add the wrapped function to the entity
            query_container = getattr(entity_cls, container, None)
            if not query_container:
                query_container = QueryContainer()
                setattr(entity_cls, container, query_container)
            setattr(query_container, name, cls_func)


# pylint: enable=too-many-locals


def _param_and_call_wrapper(
    func: Callable[[Any], pd.DataFrame],
    func_params: Dict[str, ParamAttrs],
    param_attrib_map: Dict[str, str],
    get_timespan: Callable[[], TimeSpan],
):
    """
    Wrap query function in to handle input parameters.

    Parameters
    ----------
    func : Callable[[Any], pd.DataFrame]
        The function to be wrapped
    func_params : Dict[str, ParamAttrs]
        Dict of parameters used by `func`
    param_attrib_map : Dict[str, str]
        Map of parameter name to entity attribute name.
    get_timespan : Callable[[], TimeSpan]
        The function to get the default timespan to use for queries.

    Returns
    -------
    Callable[Any, pd.DataFrame]
        The wrapped query function.

    Notes
    -----
    This function wraps the input `func` in two decorators.
    The first of these (create_data_func_exec) will execute
    the function once or many times (concat'ing the results) if
    the inputs are multi-valued.

    The second (in `wrapped_query_func`) handles the function being called
    wth arg[0] as an entity instance. In this case, it extracts the entity
    attributes (mapped to query function parameters) and uses those values
    as the input parameters to the function.

    """
    exec_query_func = _create_data_func_exec(func, func_params)

    @wraps(func)
    def wrapped_query_func(*args, **kwargs):
        """Wrap function to extract and map parameters."""
        # If time params not specified, add the global ones.
        time_params = {
            "start": kwargs.pop("start", get_timespan().start),
            "end": kwargs.pop("end", get_timespan().end),
        }
        # If this is an entity assume it's called as an instance method
        if args and isinstance(args[0], entities.Entity):
            value = args[0]
            param_dict = {
                param: getattr(value, attrib, None)
                for param, attrib in param_attrib_map.items()
                if hasattr(value, attrib)
            }
            return exec_query_func(**param_dict, **time_params, **kwargs)
        return exec_query_func(**time_params, **kwargs)

    return wrapped_query_func


def _create_data_func_exec(
    func: Callable[[Any], pd.DataFrame], func_params: Dict[str, ParamAttrs]
) -> Callable[[Any], pd.DataFrame]:
    """
    Wrap func to issue single or multiple calls to query.

    Parameters
    ----------
    func : Callable[[Any], pd.DataFrame]
        Query function to wrap

    Returns
    -------
    Callable[[Any], pd.DataFrame]
        wrapped function.

    Notes
    -----
    Query functions have parameters that can accept single values
    or lists. This wrapper handles these cases depending on the input
    parameters.

    If the input is multi-values (i.e. a DataFrame or
    other iterable) and the required parameters accept list values,
    a single call to the query function is made. If any of the
    required parameters accept only single values, repeated calls
    are made to the query function and the results concatenated into
    a single DataFrame output.

    If the inputs are all single values, a single call is made, as normal.
    """

    @wraps(func)
    def call_data_query(**kwargs):
        """Call function handling input params in different formats."""
        func_kwargs = kwargs.copy()

        # The input is a DataFrame
        if "data" in kwargs:
            return _exec_query_for_df(func, func_kwargs, func_params, kwargs)

        # The inputs are some mix of simple values and/or iterables.
        return _exec_query_for_values(func, func_kwargs, func_params, kwargs)

    return call_data_query


def _exec_query_for_df(func, func_kwargs, func_params, parent_kwargs):
    """Execute `func` for DataFrame inputs."""
    src_df = func_kwargs.pop("data")
    parent_kwargs.pop("data")
    df_iter_params, list_params = _check_df_params_require_iter(
        func_params,
        src_df,
        func_kwargs,
        **parent_kwargs,
    )

    if not df_iter_params or df_iter_params.keys() == list_params.keys():
        # If there are no iter params that are not in the list_params
        # dict - we're only using list params - we're good to go
        return func(**list_params, **func_kwargs)

    # Even if we have list params, we can't use both list params and per-row
    # iteration so ignore these and run queries per row
    row_results = []
    for _, row in src_df[list(df_iter_params.values())].iterrows():
        # build a single-line dict of {param1: row_value1...}
        col_param_dict = {param: row[col] for param, col in df_iter_params.items()}
        row_results.append(func(**col_param_dict, **func_kwargs))
    return pd.concat(row_results, ignore_index=True)


def _check_df_params_require_iter(
    func_params: Dict[str, ParamAttrs],
    src_df: pd.DataFrame,
    func_kwargs: Dict[str, Any],
    **kwargs,
) -> Tuple[Dict[str, Any], Dict[str, Any]]:
    """Return params that require iteration and don't."""
    list_params: Dict[str, Any] = {}
    df_iter_params: Dict[str, Any] = {}
    for kw_name, arg in kwargs.items():
        if kw_name in _DEF_IGNORE_PARAM:
            continue
        if (
            arg not in src_df.columns
            or not isinstance(func_kwargs.get(kw_name), str)
            or kw_name not in func_params
        ):
            # Not intended/usable as a column specification
            continue
        col_name = func_kwargs.pop(kw_name)
        if func_params[kw_name].type == "list":
            # If the parameter accepts iterable types try to use the
            # values of that column directly
            list_params[kw_name] = list(src_df[col_name].values)
            # But also store it as a param that we might need to iterate through
        df_iter_params[kw_name] = col_name
    return df_iter_params, list_params


def _exec_query_for_values(func, func_kwargs, func_params, parent_kwargs):
    """Execute `func` for value (scalar or iterable) inputs."""
    var_iter_params, simple_params = _check_var_params_require_iter(
        func_params, func_kwargs, **parent_kwargs
    )

    if not var_iter_params or var_iter_params.keys() == simple_params.keys():
        # If there are no iter params that are not in the simple_params
        # dict - we're only using list params - we're good to go
        return func(**simple_params, **func_kwargs)

    for param in var_iter_params:
        simple_params.pop(param, None)

    # Even if we have list params, we can't use both list params and per-row
    # iteration so ignore these and run queries per row
    row_results = []
    # zip the value lists into tuples
    for row in zip(*(var_iter_params.values())):
        # build a single-line dict of {param1: row_value1...}
        col_param_dict = {param: row[idx] for idx, param in enumerate(var_iter_params)}
        row_results.append(func(**simple_params, **col_param_dict, **func_kwargs))
    return pd.concat(row_results, ignore_index=True)


# pylint: disable=isinstance-second-argument-not-valid-type
def _check_var_params_require_iter(
    func_params: Dict[str, ParamAttrs], func_kwargs: Dict[str, Any], **kwargs
) -> Tuple[Dict[str, Any], Dict[str, Any]]:
    """Return params that require iteration and don't."""
    simple_params: Dict[str, Any] = {}
    var_iter_params: Dict[str, Any] = {}
    for kw_name, arg in kwargs.items():
        if kw_name in _DEF_IGNORE_PARAM:
            continue
        func_kwargs.pop(kw_name)
        if isinstance(arg, str) or not isinstance(arg, Iterable):
            # treat as scalar/simple type
            simple_params[kw_name] = arg
        else:
            if func_params[kw_name].type == "list":
                # If the parameter accepts iterable types try to use the
                # values of that column directly
                simple_params[kw_name] = list(arg)
            # but also add it to the list of iterable params
            var_iter_params[kw_name] = list(arg)
    return var_iter_params, simple_params


# pylint: enable=isinstance-second-argument-not-valid-type
