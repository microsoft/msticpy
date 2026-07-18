# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Server configuration models for the MSTICPy MCP server."""

from __future__ import annotations

from typing import Any

from pydantic import BaseModel, Field

from .._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"

#: Name of the MSTICPy config section that holds MCP server settings.
MCP_CONFIG_SECTION = "MpMcpServer"


class QueryProviderConfig(BaseModel):
    """Configuration for a single query provider to load at startup."""

    data_environment: str = Field(
        ...,
        alias="DataEnvironment",
        description="MSTICPy DataEnvironment name (e.g. MSSentinel, Kusto, Splunk).",
    )
    init_args: dict[str, Any] = Field(
        default_factory=dict,
        alias="InitArgs",
        description="Keyword arguments passed to the QueryProvider constructor.",
    )
    connect: bool = Field(
        default=True,
        alias="Connect",
        description="Whether to connect the provider at startup.",
    )
    connect_args: dict[str, Any] = Field(
        default_factory=dict,
        alias="ConnectArgs",
        description="Keyword arguments passed to provider.connect().",
    )

    model_config = {"populate_by_name": True, "extra": "ignore"}


class ComponentConfig(BaseModel):
    """Configuration for a non-query MSTICPy component to load at startup."""

    module: str = Field(..., alias="Module", description="Python module to import.")
    class_name: str = Field(..., alias="Class", description="Class name within the module.")
    init_args: dict[str, Any] = Field(default_factory=dict, alias="InitArgs")
    connect: bool = Field(default=False, alias="Connect")
    connect_args: dict[str, Any] = Field(default_factory=dict, alias="ConnectArgs")

    model_config = {"populate_by_name": True, "extra": "ignore"}


class ResultStoreConfig(BaseModel):
    """Configuration for the in-memory result store."""

    max_results: int = Field(
        default=50,
        alias="max_results",
        description="Maximum number of results retained (LRU eviction).",
        ge=1,
    )
    max_rows_per_result: int = Field(
        default=100_000,
        alias="max_rows_per_result",
        description="Maximum number of rows retained per stored result.",
        ge=1,
    )

    model_config = {"populate_by_name": True, "extra": "ignore"}


class DefaultsConfig(BaseModel):
    """Default runtime behaviour."""

    timespan_days: float = Field(
        default=7.0,
        description="Default look-back window (days) for query pivots.",
        gt=0,
    )
    sample_rows: int = Field(
        default=25,
        description="Default number of sample rows returned inline by run_pivot.",
        ge=0,
    )

    model_config = {"populate_by_name": True, "extra": "ignore"}


class McpServerConfig(BaseModel):
    """Top-level MSTICPy MCP server configuration."""

    query_providers: dict[str, QueryProviderConfig] = Field(
        default_factory=dict, alias="QueryProviders"
    )
    components: dict[str, ComponentConfig] = Field(default_factory=dict, alias="Components")
    result_store: ResultStoreConfig = Field(
        default_factory=ResultStoreConfig, alias="ResultStore"
    )
    defaults: DefaultsConfig = Field(default_factory=DefaultsConfig, alias="Defaults")

    model_config = {"populate_by_name": True, "extra": "ignore"}

    @classmethod
    def from_msticpy_settings(cls, settings: dict[str, Any] | None) -> McpServerConfig:
        """
        Create a config from the MSTICPy settings dictionary.

        Parameters
        ----------
        settings : dict[str, Any] | None
            The full MSTICPy settings dictionary (from ``get_config()``) or the
            ``MpMcpServer`` sub-section directly. ``None`` yields a default config.

        Returns
        -------
        McpServerConfig
            The parsed server configuration (defaults if the section is absent).

        """
        if not settings:
            return cls()
        section = settings.get(MCP_CONFIG_SECTION, settings)
        if not isinstance(section, dict):
            return cls()
        return cls(**section)
