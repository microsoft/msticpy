# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""MSTICPy query template definition."""

from __future__ import annotations

from dataclasses import field
from typing import Any

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
    data_environments: list[str]
    data_families: list[str]
    database: str | None = None
    cluster: str | None = None
    clusters: list[str] | None = None
    cluster_groups: list[str] | None = None
    tags: list[str] = field(default_factory=list)
    data_source: str | None = None
    aliases: str | list[str] | None = None
    query_macros: dict[str, Any] | None = None


@dataclass
class QueryParameter:
    """Query parameter."""

    description: str
    datatype: str
    default: Any = None
    aliases: list[str] | None = None


@dataclass
class QueryDefaults:
    """Default values for query definitions."""

    metadata: dict[str, Any] | None = None
    parameters: dict[str, QueryParameter] = field(default_factory=dict)


@dataclass
class QueryArgs:
    """Query arguments."""

    query: str = ""
    uri: str | None = None


@dataclass
class Query:
    """A Query definition."""

    description: str
    args: QueryArgs = field(default_factory=QueryArgs)
    metadata: dict[str, Any] | None = field(default_factory=dict)
    parameters: dict[str, QueryParameter] | None = field(default_factory=dict)


@dataclass
class QueryCollection:
    """Query Collection class - a query template."""

    metadata: QueryMetadata
    defaults: QueryDefaults | None = None
    sources: dict[str, Query] = field(default_factory=dict)
    file_name: str | None = None
