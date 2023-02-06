# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Pivot TI Provider helper functions."""
from collections import defaultdict
from typing import Callable, Dict, Set, Tuple, Type

import pandas as pd

from ..._version import VERSION
from ...context.tilookup import TILookup
from ...context.tiproviders.ti_provider_base import TIPivotProvider
from ...datamodel import entities
from ..pivot_core.pivot_container import PivotContainer
from ..pivot_core.pivot_register import PivotRegistration, create_pivot_func

__version__ = VERSION
__author__ = "Ian Hellen"

IOC_TYPES = {"ipv4", "ipv6", "dns", "file_hash", "url"}

TI_ENTITY_ATTRIBS: Dict[str, Tuple[Type, str]] = {
    "ipv4": (entities.IpAddress, "Address"),
    "ipv6": (entities.IpAddress, "Address"),
    "ip": (entities.IpAddress, "Address"),
    "dns": (entities.Dns, "DomainName"),
    "file_hash": (entities.File, "file_hash"),
    "file_path": (entities.File, "FullPath"),
    "url": (entities.Url, "Url"),
}


def add_ioc_queries_to_entities(ti_lookup: TILookup, container: str = "ti", **kwargs):
    """
    Add TI functions to entities.

    Parameters
    ----------
    ti_lookup : TILookup
        TILookup instance.
    container : str
        The name of the container to add query functions to

    """
    ioc_queries = create_ti_pivot_funcs(ti_lookup)
    for ioc, ioc_funcs in ioc_queries.items():
        if "debug" in kwargs:
            print(ioc, ioc_funcs)
        entity, _ = TI_ENTITY_ATTRIBS[ioc]
        if entity:
            for f_name, func in ioc_funcs.items():
                if "debug" in kwargs:
                    print(ioc, f_name, func)
                query_container = getattr(entity, container, None)
                if not query_container:
                    query_container = PivotContainer()
                    setattr(entity, container, query_container)
                setattr(query_container, f_name, func)

                # Create shortcuts for non-provider-specific funcs
                if f_name.endswith(ioc):
                    short_func_name = f"ti{f_name}"
                    setattr(entity, short_func_name, func)


def create_ti_pivot_funcs(ti_lookup: TILookup):
    """Create the TI Pivot functions."""
    ioc_type_supp = _get_supported_ioc_types(ti_lookup)
    ioc_queries: Dict[str, Dict[str, Callable[..., pd.DataFrame]]] = defaultdict(dict)

    # Add functions for ioc types that will call all providers
    # Non-IP types
    ioc_queries.update(_get_non_ip_functions(ioc_type_supp, ti_lookup))
    # Special case for ipv4 and ipv6 - we want to merge these into "ip" if these are equivalent
    ioc_queries.update(_get_ip_functions(ioc_type_supp, ti_lookup))

    return ioc_queries


def register_ti_pivot_providers(ti_lookup: TILookup, pivot: "Pivot"):  # type: ignore # noqa: F821
    """Register pivot functions from TI providers."""
    for _, ti_prov in ti_lookup.loaded_providers.items():
        if isinstance(ti_prov, TIPivotProvider):
            ti_prov.register_pivots(PivotRegistration, pivot)


def _get_supported_ioc_types(ti_lookup: TILookup) -> Dict[str, Set[str]]:
    return {
        ti_prov_name: set(ti_prov.supported_types) & IOC_TYPES
        for ti_prov_name, ti_prov in ti_lookup.loaded_providers.items()
    }


def _create_lookup_func(
    ti_lookup: TILookup, ioc, ioc_name, providers
) -> Tuple[str, str, Callable[..., pd.DataFrame]]:
    suffix = f"_{ioc_name}"
    short_func_name = f"lookup{suffix}"
    func_name = f"{short_func_name}_{ioc_name}"
    params_dict = {"default_providers": providers}

    # use IoC name if ioc_type is None
    entity_cls, entity_attr = TI_ENTITY_ATTRIBS[ioc or ioc_name]

    pivot_reg = PivotRegistration(
        src_func_name=ti_lookup.lookup_iocs.__name__,
        input_type="dataframe",
        entity_map={entity_cls.__name__: entity_attr},
        func_df_param_name="data",
        func_df_col_param_name="ioc_col",
        func_out_column_name="Ioc",
        func_static_params=params_dict,
    )
    return (
        func_name,
        short_func_name,
        create_pivot_func(target_func=ti_lookup.lookup_iocs, pivot_reg=pivot_reg),
    )


def _get_non_ip_functions(ioc_type_supp, ti_lookup):
    """Get functions for non-IP IoC types."""
    ioc_queries = defaultdict(dict)
    for ioc in IOC_TYPES - {"ipv4", "ipv6"}:
        supporting_provs = [
            prov for prov, supp_types in ioc_type_supp.items() if ioc in supp_types
        ]
        _, func_name, func = _create_lookup_func(ti_lookup, ioc, ioc, supporting_provs)
        ioc_queries[ioc][func_name] = func
    return ioc_queries


def _get_ip_functions(ioc_type_supp, ti_lookup):
    """Get functions for IP IoC Types."""
    # Special case for ipv4 and ipv6
    # we want to merge these into single "ip" function
    ioc_queries = defaultdict(dict)
    ip_types = {"ipv4", "ipv6"}
    ip_all_provs = [
        prov for prov, supp_types in ioc_type_supp.items() if ip_types & supp_types
    ]

    # Use None as the ioc_type to let TILookup resolve the type individually
    # - this let's us send either IpV4 and IpV6 addresses
    _, func_name, func = _create_lookup_func(ti_lookup, None, "ip", ip_all_provs)
    ioc_queries["ip"][func_name] = func

    return ioc_queries
