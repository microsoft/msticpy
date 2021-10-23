# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""VirusTotal Pivot functions."""
from enum import Flag, auto
from functools import partial
from typing import Dict, Tuple

from .._version import VERSION
from .vtlookupv3 import VTLookupV3
from ..common.utility import enum_parse
from ..datamodel.pivot import Pivot, PivotRegistration

__version__ = VERSION
__author__ = "Ian Hellen"


class VTAPIScope(Flag):
    """VT API Type enumeration."""

    PUBLIC = auto()
    PRIVATE = auto()
    ALL = PUBLIC | PRIVATE


FILE_RELATIONSHIPS = {
    "bundled_files": VTAPIScope.PUBLIC,
    "contacted_domains": VTAPIScope.PUBLIC,
    "contacted_ips": VTAPIScope.PUBLIC,
    "contacted_urls": VTAPIScope.PUBLIC,
    "dropped_files": VTAPIScope.PUBLIC,
    "execution_parents": VTAPIScope.PUBLIC,
    "email_attachments": VTAPIScope.PRIVATE,
    "email_parents": VTAPIScope.PRIVATE,
    "embedded_domains": VTAPIScope.PRIVATE,
    "embedded_ips": VTAPIScope.PRIVATE,
    "embedded_urls": VTAPIScope.PRIVATE,
    "sigma_analysis": VTAPIScope.PRIVATE,
    "similar_files": VTAPIScope.PRIVATE,
}

DOMAIN_RELATIONSHIPS = {
    "communicating_files": VTAPIScope.PUBLIC,
    "historical_whois": VTAPIScope.PUBLIC,
    "historical_ssl_certificates": VTAPIScope.PUBLIC,
    "parent": VTAPIScope.PUBLIC,
    "resolutions": VTAPIScope.PUBLIC,
    "subdomains": VTAPIScope.PUBLIC,
    "downloaded_files": VTAPIScope.PRIVATE,
    "ns_records": VTAPIScope.PRIVATE,
    "urls": VTAPIScope.PRIVATE,
}

IP_RELATIONSHIPS = {
    "communicating_files": VTAPIScope.PUBLIC,
    "historical_whois": VTAPIScope.PUBLIC,
    "historical_ssl_certificates": VTAPIScope.PUBLIC,
    "referrer_files": VTAPIScope.PUBLIC,
    "resolutions": VTAPIScope.PUBLIC,
    "subdomains": VTAPIScope.PUBLIC,
    "downloaded_files": VTAPIScope.PRIVATE,
    "urls": VTAPIScope.PRIVATE,
}

URL_RELATIONSHIPS = {
    "last_serving_ip_address": VTAPIScope.PUBLIC,
    "network_location": VTAPIScope.PUBLIC,
    "communicating_files": VTAPIScope.PUBLIC,
    "contacted_domains": VTAPIScope.PUBLIC,
    "contacted_ips": VTAPIScope.PUBLIC,
    "downloaded_files": VTAPIScope.PRIVATE,
    "redirecting_urls": VTAPIScope.PRIVATE,
    "redirects_to": VTAPIScope.PRIVATE,
    "referrer_files": VTAPIScope.PRIVATE,
    "referrer_urls": VTAPIScope.PRIVATE,
}

PIVOT_ENTITY_CATS: Dict[str, Tuple[str, Dict[str, VTAPIScope]]] = {
    "File": ("file", FILE_RELATIONSHIPS),
    "IpAddress": ("ip_address", IP_RELATIONSHIPS),
    "Dns": ("domain", DOMAIN_RELATIONSHIPS),
    "Url": ("url", URL_RELATIONSHIPS),
}

_ENTITY_PROPS = {
    "File": "Sha256",
    "IpAddress": "Address",
    "Dns": "DomainName",
    "Url": "Url",
}


# pylint: disable=no-member
def add_pivot_functions(api_scope: str = VTAPIScope.PUBLIC.name.lower()):
    """Add VT functions as pivot functions."""
    ent_funcs = _create_pivots(api_scope)
    for entity, funcs in ent_funcs.items():
        for func_name, func in funcs.items():
            pivot_reg = PivotRegistration(
                func_new_name=func_name,
                input_type="value",
                entity_map={entity: _ENTITY_PROPS[entity]},
                func_input_value_arg="id",
                can_iterate=True,
                create_shortcut=True,
            )
            Pivot.add_pivot_function(func, pivot_reg=pivot_reg, container="VT")


# pylint: disable=no-member
def _create_pivots(api_scope: str = VTAPIScope.PUBLIC.name.lower()):
    vt_client = VTLookupV3()
    scope = enum_parse(VTAPIScope, api_scope)
    if not isinstance(scope, VTAPIScope):
        # pylint: disable=not-an-iterable
        scope_names = [f"{name.lower()}" for name in VTAPIScope.__members__]
        raise ValueError(
            f"Unknown API scope {api_scope}",
            f"Valid values are {', '.join(scope_names)}",
        )

    if scope == VTAPIScope.PRIVATE:
        scope = VTAPIScope.ALL
    ent_funcs = {}
    for entity, (vt_type, category) in PIVOT_ENTITY_CATS.items():
        ent_relations = (
            rel for rel, rel_scope in category.items() if rel_scope & scope
        )
        func_dict = {}
        for relationship in ent_relations:
            f_part = partial(
                _get_relationships,
                vt_client=vt_client,
                vt_type=vt_type,
                relationship=relationship,
            )
            func_dict[_create_func_name(relationship)] = f_part
        ent_funcs[entity] = func_dict
    return ent_funcs


# pylint: enable=no-member


def _create_func_name(relationship):
    return f"vt_{relationship}"


def _get_relationships(vt_client, entity_id, vt_type, relationship):
    result_df = vt_client.lookup_ioc_relationships(
        observable=entity_id, vt_type=vt_type, relationship=relationship
    )
    return result_df.reset_index()
