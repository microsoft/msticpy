# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Pivot TI Provider helper functions."""
from collections import defaultdict
from typing import Callable, Dict, List, Set, Tuple, Type

import pandas as pd

from ..._version import VERSION
from ...context.contextlookup import ContextLookup
from ...datamodel import entities
from ..pivot_core.pivot_container import PivotContainer
from ..pivot_core.pivot_register import PivotRegistration, create_pivot_func

__version__ = VERSION
__author__ = "Ian Hellen"

INDICATOR_TYPES = {
    "ipv4",
    "ipv6",
    "ip",
    "host",
    "file_hash",
    "url",
    "email",
    "host_name",
    "account_name",
}

CONTEXT_ENTITY_ATTRIBS: Dict[str, List[Tuple[Type, str]]] = {
    "ipv4": [(entities.IpAddress, "Address")],
    "ipv6": [(entities.IpAddress, "Address")],
    "ip": [(entities.IpAddress, "Address")],
    "dns": [(entities.Dns, "DomainName")],
    "file_hash": [(entities.File, "file_hash")],
    "file_path": [(entities.File, "FullPath")],
    "url": [(entities.Url, "Url")],
    "host_name": [(entities.Host, "DnsName"), (entities.Dns, "DomainName")],
    "email": [
        (entities.Account, "qualified_name"),
        (entities.Mailbox, "MailboxPrimaryAddress"),
    ],
    "account_name": [(entities.Account, "Name")],
}


def init():
    """Add context providers."""
    add_context_queries_to_entities(ContextLookup())


def add_context_queries_to_entities(
    context_lookup: ContextLookup, container: str = "context", **kwargs
):
    """
    Add Context provider functions to entities.

    Parameters
    ----------
    context_lookup : ContextLookup
        ContextLookup instance.
    container : str
        The name of the container to add query functions to

    """
    context_queries = create_pivot_funcs(context_lookup)
    for obs_type, context_funcs in context_queries.items():
        if "debug" in kwargs:
            print(obs_type, context_funcs)
        context_entities = CONTEXT_ENTITY_ATTRIBS[obs_type]
        for entity, _ in context_entities:
            if not entity:
                continue
            for f_name, func in context_funcs.items():
                if "debug" in kwargs:
                    print(obs_type, f_name, func)
                query_container = getattr(entity, container, None)
                if not query_container:
                    query_container = PivotContainer()
                    setattr(entity, container, query_container)
                setattr(query_container, f_name, func)

                # Create shortcuts for non-provider-specific funcs
                if f_name.endswith(obs_type):
                    short_func_name = f"ctxt_{f_name}"
                    setattr(entity, short_func_name, func)


def create_pivot_funcs(context_lookup: ContextLookup):
    """Create the context Pivot functions."""
    obs_types_supported = _get_supported_indicator_types(context_lookup)
    context_queries: Dict[str, Dict[str, Callable[..., pd.DataFrame]]] = defaultdict(
        dict
    )

    # Add functions for ioc types that will call all providers
    context_queries.update(_get_lookup_functions(obs_types_supported, context_lookup))
    return context_queries


def _get_supported_indicator_types(
    context_lookup: ContextLookup,
) -> Dict[str, Set[str]]:
    return {
        prov_name: set(provider.supported_types) & INDICATOR_TYPES
        for prov_name, provider in context_lookup.loaded_providers.items()
    }


def _create_lookup_func(
    context_lookup: ContextLookup, ioc, ioc_name, providers
) -> Tuple[str, str, Callable[..., pd.DataFrame]]:

    suffix = f"_{ioc_name}"
    short_func_name = f"lookup{suffix}"
    func_name = f"{short_func_name}_{ioc_name}"
    params_dict = {"default_providers": providers}

    # use IoC name if ioc_type is None
    for entity_cls, entity_attr in CONTEXT_ENTITY_ATTRIBS[ioc or ioc_name]:

        pivot_reg = PivotRegistration(
            src_func_name=context_lookup.lookup_observables.__name__,
            input_type="dataframe",
            entity_map={entity_cls.__name__: entity_attr},
            func_df_param_name="data",
            func_df_col_param_name="obs_col",
            func_out_column_name="Observable",
            func_static_params=params_dict,
        )
        yield (
            func_name,
            short_func_name,
            create_pivot_func(
                target_func=context_lookup.lookup_observables, pivot_reg=pivot_reg
            ),
        )


def _get_lookup_functions(obs_types_supported, context_lookup):
    """Get functions for context types."""
    context_queries = defaultdict(dict)
    for obs_type in INDICATOR_TYPES:
        supporting_provs = [
            prov
            for prov, supp_types in obs_types_supported.items()
            if obs_type in supp_types
        ]
        for _, func_name, func in _create_lookup_func(
            context_lookup, obs_type, obs_type, supporting_provs
        ):
            context_queries[obs_type][func_name] = func
    return context_queries
