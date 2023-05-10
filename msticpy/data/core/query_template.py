# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""MSTICPy query template definition."""
from dataclasses import dataclass, field
from typing import Any, Dict, List, Optional

from ..._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"


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


@dataclass
class QueryParameter:
    """Query parameter."""

    description: str
    datatype: str
    default: Any = None
    required: Optional[bool] = None


@dataclass
class QueryDefaults:
    """Default values for query definitions."""

    metadata: Optional[Dict[str, Any]] = None
    parameters: Dict[str, QueryParameter] = field(default_factory=dict)


@dataclass
class QueryArgs:
    """Query arguments."""

    query: str = ""


@dataclass
class Query:
    """A Query definition."""

    description: str
    metadata: Dict[str, Any] = field(default_factory=dict)
    args: QueryArgs = field(default_factory=QueryArgs)
    parameters: Dict[str, QueryParameter] = field(default_factory=dict)


@dataclass
class QueryCollection:
    """Query Collection class - a query template."""

    file_name: str
    metadata: QueryMetadata
    defaults: Optional[QueryDefaults] = None
    sources: Dict[str, Query] = field(default_factory=dict)
