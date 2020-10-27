# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Pivot query functions class."""
import itertools
from collections import defaultdict, namedtuple
from typing import Any, Callable, Dict, Iterable, List, Tuple

from msticpy.data.query_container import QueryContainer
from msticpy.nbtools import entities

from .._version import VERSION
from ..data.data_providers import QueryProvider

__version__ = VERSION
__author__ = "Ian Hellen"


ParamAttrs = namedtuple("ParamAttrs", "type, query, family, required")
QueryParams = namedtuple("QueryParams", "all, required, full_required")


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

        ignore_params = set(ignore_reqd) if ignore_reqd else {"start", "end"}

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

        self.query_reqd_params: Dict[str, QueryParams] = {}
        for family, fam_dict in self._provider.query_store.data_families.items():
            for src_name, q_source in fam_dict.items():
                self.query_reqd_params[f"{family}.{src_name}"] = QueryParams(
                    list(q_source.params),
                    list((set(q_source.required_params) - ignore_params)),
                    list(q_source.required_params),
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
        get_param_props = (
            (param.query, param.family, param.type, f"{param.family}.{param.query}")
            for param in self.param_usage.get(param)
        )
        yield from (
            (q_name, q_family, p_type, getattr(self._provider, q_func), q_func)
            for q_name, q_family, p_type, q_func in get_param_props
        )

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
        for query_name, query_family, query_func_name in (
            (param.query, param.family, f"{param.family}.{param.query}")
            for param in self.param_usage.get(param)
        ):
            yield query_name, query_family, getattr(self._provider, query_func_name)

    def get_params(self, query_func_name: str) -> QueryParams:
        """
        Get the parameters for a query function

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
        return self.query_reqd_params.get(query_func_name)

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
        return self.param_usage.get(param_name)


PARAM_ENTITY_MAP = {
    "account_name": [(entities.Account, "Name")],
    "host_name": [(entities.Host, "fqdn")],
    "process_name": [(entities.Process, "ImageFile")],
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
    "commandline": [(entities.Process, "ProcessId")],
    "url": [(entities.Url, "Url")],
    "file_hash": [(entities.File, "file_hash")],
}


def add_queries_to_entities(az_qry_funcs: PivotQueryFunctions):
    # For each parameter
    for param_name, entity_list in PARAM_ENTITY_MAP.items():

        param_funcs = az_qry_funcs.get_queries_for_param(param_name)
        if not (entity_list and param_funcs):
            continue

        entity_funcs = itertools.product(entity_list, param_funcs)
        # For each entity/query combo that uses this parameter
        for (entity, attrib), (name, func) in entity_funcs:
            func_params = az_qry_funcs.get_params(name)
            if len(func_params.required) > 1:
                # If multiple params - get the ones that are available
                # in the same entity
                param_entities = {
                    param: ent
                    for param, ent_list in PARAM_ENTITY_MAP.items()
                    for ent, attr in ent_list
                    if param in func_params.required and ent == entity
                }

                # missing_params = set(func_params.required) - param_entities.keys()
                attr_map = {
                    param: ent_attr[1] for param, ent_attr in param_entities.items()
                }
                # Create the function
                cls_func = create_self_or_class_func(func, attrib, param_name)
            else:
                # TODO - something different here
                cls_func = create_self_or_class_func(func, attrib, param_name)

            query_container = getattr(entity, "data", None)
            if not query_container:
                query_container = QueryContainer()
                setattr(entity, "data", query_container)
            setattr(query_container, name, cls_func)
