# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""MSTICPy query template definition."""
from dataclasses import field
from typing import Any, Dict, List, Optional, Union

from pydantic.dataclasses import dataclass

from ..._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"


# pylint: disable=too-many-instance-attributes
@dataclass
class QueryMetadata:
    """Metadata for query definitions."""

    version: int
    description: str
    data_environments: List[str]
    data_families: List[str]
    database: Optional[str] = None
    cluster: Optional[str] = None
    clusters: Optional[List[str]] = None
    cluster_groups: Optional[List[str]] = None
    tags: List[str] = field(default_factory=list)
    data_source: Optional[str] = None
    aliases: Optional[Union[str, List[str]]] = None
    query_macros: Optional[Dict[str, Any]] = None


@dataclass
class QueryParameter:
    """Query parameter."""

    description: str
    datatype: str
    default: Any = None
    aliases: Optional[List[str]] = None


@dataclass
class QueryDefaults:
    """Default values for query definitions."""

    metadata: Optional[Dict[str, Any]] = None
    parameters: Dict[str, QueryParameter] = field(default_factory=dict)


@dataclass
class QueryArgs:
    """Query arguments."""

    query: str = ""
    uri: Optional[str] = None


@dataclass
class Query:
    """A Query definition."""

    description: str
    args: QueryArgs = field(default_factory=QueryArgs)
    metadata: Optional[Dict[str, Any]] = field(default_factory=dict)  # type: ignore
    parameters: Optional[Dict[str, QueryParameter]] = field(default_factory=dict)  # type: ignore


@dataclass
class QueryCollection:
    """Query Collection class - a query template."""

    metadata: QueryMetadata
    defaults: Optional[QueryDefaults] = None
    sources: Dict[str, Query] = field(default_factory=dict)
    file_name: Optional[str] = None
