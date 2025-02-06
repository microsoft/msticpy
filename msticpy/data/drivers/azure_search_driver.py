# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
Driver for querying Azure Log Analytics using the /search endpoint.

This is based on the AzureMonitorDriver but uses the /search endpoint
to allow limited querying of Basic and Auxilary tables.

"""

from __future__ import annotations

import logging
from typing import Any

import httpx
import pandas as pd

from ..._version import VERSION
from ...auth.azure_auth import az_connect
from ...common.exceptions import MsticpyDataQueryError, MsticpyKqlConnectionError
from .azure_monitor_driver import AzureMonitorDriver

__version__ = VERSION
__author__ = "Ian Hellen"

logger = logging.getLogger(__name__)

_AZURE_TOKEN_SCOPE = "https://api.loganalytics.io/.default"  # nosec
_SEARCH_ENDPOINT = "https://api.loganalytics.io/v1/workspaces/{workspace_id}/search"


class AzureSearchDriver(AzureMonitorDriver):
    """
    Allows simple querying of Azure Log Analytics Basic tables data using KQL.

    Derives from AzureMonitorDriver and uses direct REST (/search) queries via httpx
    instead of the azure-monitor-query LogsQueryClient.
    """

    def __init__(self, connection_str: str | None = None, **kwargs):
        """
        Initialize the driver.

        Parameters
        ----------
        connection_str : str, optional
            Connection string for Azure Monitor, by default None
        """
        super().__init__(connection_str=connection_str, **kwargs)
        self._auth_header: dict[str, Any] | None = None
        self._try_get_schema = False

    def _create_query_client(self, connection_str: str | None = None, **kwargs):
        """Create a query client using the /search endpoint."""
        az_auth_types = kwargs.pop("auth_types", kwargs.get("mp_az_auth"))
        if isinstance(az_auth_types, bool):
            az_auth_types = None
        if isinstance(az_auth_types, str):
            az_auth_types = [az_auth_types]
        self._connect_auth_types = az_auth_types

        self._def_timeout = kwargs.pop("timeout", self._DEFAULT_TIMEOUT)
        self._def_proxies = kwargs.pop("proxies", self._def_proxies)
        self._get_workspaces(connection_str, **kwargs)

        # check for additional Args in settings but allow kwargs to override
        connect_args = self._get_workspace_settings_args()
        connect_args.update(kwargs)
        connect_args.update(
            {"auth_methods": az_auth_types, "tenant_id": self._az_tenant_id}
        )
        credentials = az_connect(**connect_args)

        # This will still set up workspaces and tenant ID
        self._get_workspaces(connection_str, **kwargs)

        # Acquire token from Azure
        token = credentials.modern.get_token(_AZURE_TOKEN_SCOPE).token
        self._auth_header = {"Authorization": f"Bearer {token}"}

        # Mark as connected
        self._connected = True
        logger.info("Created HTTP-based query client using /search endpoint.")

    def query_with_results(
        self, query: str, **kwargs
    ) -> tuple[pd.DataFrame, dict[str, Any]]:
        """
        Execute the query via the /search endpoint and return a DataFrame + result status.

        Parameters
        ----------
        query : str
            KQL or basic log query to execute.

        Returns
        -------
        Tuple[pd.DataFrame, dict[str, Any]]
            The resulting DataFrame and a status dictionary.

        """
        if not self._connected or not hasattr(self, "_auth_header"):
            raise MsticpyKqlConnectionError(
                "Not connected. Call connect() before querying."
            )
        time_span_value = self._get_time_span_value(**kwargs)
        if not time_span_value:
            raise MsticpyDataQueryError(
                "No start/end parameters found. Please supply these values."
            )

        # We’ll mimic the original driver’s approach to picking a workspace
        workspace_id = next(iter(self._workspace_ids), None) or self._workspace_id
        if not workspace_id:
            raise MsticpyKqlConnectionError(
                "No workspace_id found. Please configure a workspace before querying."
            )

        # Build the REST URL
        search_url = _SEARCH_ENDPOINT.format(workspace_id=workspace_id)

        # Define query request body
        query_body = {
            "query": query,
            "start": time_span_value[0].isoformat(),
            "end": time_span_value[1].isoformat(),
        }

        # Time-out can be specified if needed
        timeout = kwargs.pop("timeout", 300)

        # Make request
        results = self._query_search_endpoint(search_url, query_body, timeout)
        tables = results.get("tables", [])
        if not tables:
            logger.warning("No tables found in the response.")
            return pd.DataFrame(), {"status": "no_data"}

        data_frame = self._table_to_dataframe(tables[0])

        # Create a status dictionary
        status: dict[str, Any] = {
            "status": "success",
            "rows_returned": len(data_frame),
            "columns": data_frame.columns.tolist(),
        }
        logger.info("Dataframe returned with %d rows", len(data_frame))
        return data_frame, status

    def _query_search_endpoint(self, search_url, query_body, timeout):
        try:
            with httpx.Client(timeout=timeout) as client:
                response = client.post(
                    search_url, headers=self._auth_header, json=query_body
                )
        except httpx.RequestError as req_err:
            logger.error("HTTP request error: %s", req_err)
            raise MsticpyKqlConnectionError(
                f"HTTP request to {search_url} failed.",
                title="HTTP request error",
            ) from req_err

        # Check status code
        if response.status_code != 200:
            logger.error("Request failed: %d, %s", response.status_code, response.text)
            raise MsticpyKqlConnectionError(
                f"Error {response.status_code} from /search endpoint: {response.text}"
            )

        # Parse result
        results = response.json()
        return results

    def _table_to_dataframe(self, table: dict[str, Any]) -> pd.DataFrame:
        """Convert Azure types to pandas dtypes."""
        rows = table.get("rows", [])
        col_names = [col["name"] for col in table.get("columns", [])]
        data_frame = pd.DataFrame(rows, columns=col_names)
        type_mapping = {col["name"]: col["type"] for col in table["columns"]}

        def map_azure_type(azure_type: str) -> str:
            # Basic mapping of types
            azure_to_pd = {
                "string": "str",
                "datetime": "datetime64[ns]",
                "long": "int64",
                "real": "float",
                "boolean": "bool",
                "guid": "str",
            }
            return azure_to_pd.get(azure_type, "object")

        for column in data_frame.columns:
            azure_type = type_mapping.get(column)
            if not azure_type:
                continue
            pandas_type = map_azure_type(azure_type)
            try:
                if pandas_type.startswith("datetime"):
                    data_frame[column] = pd.to_datetime(data_frame[column])
                else:
                    data_frame[column] = data_frame[column].astype(pandas_type)
            except Exception as conv_err:  # pylint: disable=broad-except
                logger.warning(
                    "Could not convert column %s to %d: %s",
                    column,
                    pandas_type,
                    conv_err,
                )
        return data_frame
