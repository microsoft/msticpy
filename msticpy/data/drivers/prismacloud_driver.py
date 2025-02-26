# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""PrismaCloud Driver class."""

import logging
from typing import Any, Optional, TypedDict, cast

import httpx
import pandas as pd

from msticpy.common.exceptions import MsticpyConnectionError, MsticpyUserError

BASE_URL_API = "https://api.prismacloud.io"
logger = logging.getLogger(__name__)


class PrismaCloudAuthError(MsticpyConnectionError):
    """Raised for authentication failures."""


class PrismaCloudQueryError(MsticpyConnectionError):
    """Raised for query execution errors."""


class QueryArgs(TypedDict, total=False):
    """
    Defines optional arguments for Prisma Cloud queries.

    Attributes
    ----------
    start_time : str, optional
        Start of the time range for the query, in ISO 8601 format.
    end_time : str, optional
        End of the time range for the query, in ISO 8601 format.
    unit : str, optional
        The unit of time for the range (e.g., "minute", "hour", "day").
    amount : int, optional
        The quantity of the time unit for the range.
    limit : int, optional
        Maximum number of results to return.
    """

    start_time: str
    end_time: str
    unit: str
    amount: int
    limit: int
    cloudtype: str


class PrismaCloudDriver:
    """Driver to connect and query Prisma Cloud."""

    ENDPOINT_MAP = {
        "assets": "/search/api/v1/asset",
        "configurations": "/search/api/v2/config",
        "events": "/search/event",
        "network": "/search",
    }

    def __init__(self, **kwargs: Any):
        """Initialize Prisma Cloud Driver."""
        valid_keys = {"timeout", "base_url", "debug", "max_retries", "headers"}
        unknown_keys = set(kwargs) - valid_keys
        if unknown_keys:
            logger.warning("Unknown configuration keys provided: %s", unknown_keys)

        self.timeout = kwargs.get("timeout", 300)
        self.base_url = kwargs.get("base_url", BASE_URL_API)
        self.debug = kwargs.get("debug", False)
        self.connected = False
        self.max_retries = kwargs.get("max_retries", 3)
        self.headers = kwargs.get(
            "headers",
            {"User-Agent": "PrismaCloudDriver/1.0", "Accept": "application/json"},
        )

        transport = httpx.HTTPTransport(retries=self.max_retries)
        self.client = httpx.Client(
            base_url=self.base_url,
            timeout=self.timeout,
            headers=self.headers,
            transport=transport,
        )

        if self.debug:
            logger.setLevel(logging.DEBUG)

        logger.debug(
            "PrismaCloudDriver initialized with base_url=%s, timeout=%d, max_retries=%d",
            self.base_url,
            self.timeout,
            self.max_retries,
        )

    def connect(self, username: str, password: str) -> "PrismaCloudDriver":
        """Authenticate with Prisma Cloud."""
        if not username or not password:
            raise PrismaCloudAuthError(
                "Both 'username' and 'password' must be provided."
            )

        logger.debug("Attempting to authenticate with username=[REDACTED]")
        try:
            response = self.client.post(
                "/login", json={"username": username, "password": password}
            )
            response.raise_for_status()
            result = self._parse_json(response)

            if result.get("message") == "login_successful":
                self.client.headers["X-Redlock-Auth"] = result["token"]
                self.connected = True
                logger.info("Prisma Cloud connection successful")
                return self

            logger.error("Login failed: %s", result.get("message", "Unknown error"))
            raise PrismaCloudAuthError(
                f"Login failed: {result.get('message', 'Unknown error')}"
            )
        except httpx.HTTPStatusError as http_err:
            self._handle_http_error(http_err)
        except httpx.RequestError as request_err:
            self._handle_connection_error(f"Request error: {request_err}")
        return self

    def refresh_token(self) -> None:
        """Refresh the authentication token."""
        logger.info("Refreshing authentication token...")
        try:
            response = self.client.post("/auth_token/extend")
            response.raise_for_status()
            result = self._parse_json(response)

            if result.get("message") == "login_successful":
                self.client.headers["X-Redlock-Auth"] = result["token"]
                logger.info("Token refreshed successfully.")
            else:
                self._handle_connection_error("Token refresh failed.")
        except httpx.RequestError as request_err:
            self._handle_connection_error(f"Error refreshing token: {request_err}")

    def _parse_json(self, response: httpx.Response) -> dict:
        """Safely parse JSON response with error handling."""
        try:
            return response.json()
        except ValueError as json_err:
            self._handle_connection_error(f"Invalid JSON response received: {json_err}")
        return {}

    def _handle_http_error(self, err: httpx.HTTPStatusError) -> None:
        """Handle HTTP Status errors."""
        error_message = (
            f"HTTP error {err.response.status_code} while connecting "
            f"to {err.request.url}: {err.response.text}"
        )
        logger.error(error_message)
        raise PrismaCloudQueryError(error_message) from err

    def _handle_connection_error(self, message: str) -> None:
        """Handle general connection errors."""
        logger.error("Connection error: %s", message)
        raise PrismaCloudQueryError(message)

    def _build_payload(self, query: str, **kwargs: QueryArgs) -> dict:
        """Construct the payload for the Prisma Cloud query."""
        payload: dict[str, Any] = {"query": query, "limit": kwargs.get("limit", 1000)}

        start_time = cast(str, kwargs.get("start_time"))
        end_time = cast(str, kwargs.get("end_time"))
        unit = cast(str, kwargs.get("unit", "minute"))
        amount = cast(int, kwargs.get("amount", 3))

        if start_time or end_time:
            time_range = self.construct_time(
                start_time=start_time, end_time=end_time, unit=unit, amount=amount
            )
            payload["timeRange"] = time_range

        return payload
   
    def _fetch_prisma_data(
        self, endpoint: str, payload: dict[str, Any], timeout: int, max_retries: int
    ) -> dict[str, Any]:
        """Handle API request and retries for errors."""
        retries = 0
        while retries < max_retries:
            try:
                response = self.client.post(endpoint, json=payload, timeout=timeout)
                response.raise_for_status()
                return self._parse_json(response)
            except httpx.HTTPStatusError as err:
                retries += 1
                if retries >= max_retries:
                    logger.error("Max retries exceeded for API request.")
                    self._handle_http_error(err)
                logger.warning("Retrying API request... Attempt %d/%d", retries, max_retries)

        self._handle_connection_error("Max retries exceeded.")
        return {}

    def _process_prisma_response(
        self, data: dict[str, Any], data_key: str
    ) -> list[dict[str, Any]]:
        """Extract the relevant records from the Prisma API response."""
        items = data
        for key in data_key.split("."):
            items = items.get(key, {})
        return items if isinstance(items, list) else []
    
    def _paginate_prisma_search(
        self,
        endpoint: str,
        base_payload: dict[str, Any],
        limitresult: int,
        limitpage: int,
        timeout: int,
        data_key: str,
    ) -> list[dict[str, Any]]:
        """Handle pagination for Prisma API query."""
        results: list[dict[str, Any]] = []
        next_token = None

        while True:
            payload = base_payload.copy()
            payload["limit"] = min(limitpage, limitresult - len(results))
            payload["nextPageToken"] = next_token

            logger.info("📤 Sending request with payload: %s", payload)
            data = self._fetch_prisma_data(endpoint, payload, timeout, self.max_retries)

            if not isinstance(data, dict):
                logger.error("❌ Unexpected API response format.")
                raise MsticpyConnectionError("Unexpected API response format.")

            items = self._process_prisma_response(data, data_key)
            if not isinstance(items, list):
                logger.error("❌ Processed data is not a list.")
                raise MsticpyConnectionError("Unexpected processed data format.")

            results.extend(items)
            logger.info("📊 Retrieved %d records so far.", len(results))

            next_token = data.get("nextPageToken")
            if not next_token or len(results) >= limitresult or not items:
                logger.info("✅ No more pages left to fetch.")
                break

        return results

    def prisma_search_network(
        self,
        query: str,
        endpoint: str,
        unit: str | None = "hour",
        amount: int = 3,
        limitresult: int = 10000,
        limitpage: int = 100,
        cloudtype: str | None = "aws",
        timeout: int = 360,
    ) -> pd.DataFrame:
        """Search network data from Prisma Cloud with pagination."""
        base_payload = {
            "query": query,
            "limit": limitpage,
            "cloudType": cloudtype,
            "saved": False,
            "default": False,
            "timeRange": {"type": "relative", "value": {"unit": unit, "amount": amount}},
        }
        results = self._paginate_prisma_search(
            endpoint,
            base_payload,
            limitresult,
            limitpage,
            timeout,
            "data.nodes",
        )

        return pd.DataFrame(results) if results else pd.DataFrame()

    def prisma_search_assets(
        self,
        query: str,
        endpoint: str,
        **kwargs: QueryArgs,
    ) -> pd.DataFrame:
        """Search assets from Prisma Cloud with pagination."""
        timeout = kwargs.get("timeout", self.timeout)
        limitresult = kwargs.get("limit", 10000)
        limitpage: int = 1000
        unit = kwargs.get("unit", "hour")
        amount = kwargs.get("amount", 4)

        logger.info("🔍 Executing asset search with query: %s", query)

        base_payload = {
            "query": query,
            "limit": limitpage,
            "timeRange": {"type": "relative", "value": {"unit": unit, "amount": amount}},
        }

        results = self._paginate_prisma_search(
            endpoint, base_payload, limitresult, limitpage, timeout, "value"
        )

        return pd.DataFrame(results) if results else pd.DataFrame()

    def prisma_search_events(
        self,
        query: str,
        endpoint: str,
        **kwargs: QueryArgs,
    ) -> pd.DataFrame:
        """
        Perform a simple event search from Prisma Cloud.
        """
        timeout = kwargs.get("timeout", self.timeout)
        max_retries = kwargs.get("max_retries", self.max_retries)
        unit = kwargs.get("unit", "day")
        amount = kwargs.get("amount", 1)
        limit = kwargs.get("limit", 5000)

        logger.info("🔍 Executing event search with query: %s", query)

        payload = {
            "query": query,
            "limit": limit,
            "timeRange": {"type": "relative", "value": {"unit": unit, "amount": amount}},
            **kwargs,
        }

        logger.info("📤 Sending request with payload: %s", payload)
        data = self._fetch_prisma_data(endpoint, payload, timeout, max_retries)

        items = data.get("data", {}).get("items", [])
        logger.info("📊 Retrieved %d event records.", len(items))
        return pd.DataFrame(items) if items else pd.DataFrame()

    def prisma_search_configurations(
        self,
        query: str,
        endpoint: str,
        **kwargs: QueryArgs,
    ) -> pd.DataFrame:
        """
        Search configurations from Prisma Cloud.
        """
        timeout = kwargs.get("timeout", self.timeout)
        max_retries = kwargs.get("max_retries", self.max_retries)

        logger.info("Executing configuration search with query: %s", query)
        payload = {"query": query, "limit": 1000, "withResourceJson": True, **kwargs}
        data = self._fetch_prisma_data(endpoint, payload, timeout, max_retries)
        items = data.get("items", [])

        logger.info("Retrieved %d configuration results.", len(items))
        return pd.DataFrame(items) if items else pd.DataFrame()
    
    # pylint: disable=inconsistent-return-statements
    def execute_query(
        self, query: str, query_source: Optional[str] = None, **kwargs: QueryArgs
    ) -> pd.DataFrame:
        """Execute a query on Prisma Cloud."""
        if "X-Redlock-Auth" not in self.client.headers:
            raise PrismaCloudQueryError("Driver not connected to Prisma Cloud.")

        if query_source not in self.ENDPOINT_MAP:
            raise MsticpyUserError(f"Invalid query source: {query_source}")

        endpoint = self.ENDPOINT_MAP[query_source]
        if endpoint == "/search/api/v2/config":
            return self.prisma_search_configurations(query, endpoint, **kwargs)
        elif endpoint == "/search/api/v1/asset":
            return self.prisma_search_assets(query, endpoint, **kwargs)
        elif endpoint == "/search":
            return self.prisma_search_network(query, endpoint, **kwargs)
        elif endpoint == "/search/event":
            return self.prisma_search_events(query, endpoint, **kwargs)

        raise MsticpyUserError(f"Query source {query_source} is not supported.")
        #payload = self._build_payload(query, **kwargs)

        #logger.info("Executing query on endpoint: %s", endpoint)
        #logger.debug("Query payload: %s", payload)

        #try:
         #   response = self.client.post(endpoint, json=payload)
          #  response.raise_for_status()
           # result = self._parse_json(response)
            #data = result.get("items", [])
            #if not data:
             #   logger.warning("No results found for query: %s", query)

            #return pd.DataFrame(data)
        #except httpx.HTTPStatusError as http_err:
         #   self._handle_http_error(http_err)
        #except httpx.RequestError as request_err:
         #   self._handle_connection_error(
          #      f"Unexpected error while executing query: {request_err}"
           # )
        #return pd.DataFrame()



    @staticmethod
    def construct_time(
        start_time: Optional[str] = None,
        end_time: Optional[str] = None,
        unit: str = "minute",
        amount: int = 3,
    ) -> dict:
        """Construct time range payload for queries."""
        if start_time and end_time:
            return {"type": "absolute", "value": {"start": start_time, "end": end_time}}
        return {"type": "relative", "value": {"unit": unit, "amount": amount}}
