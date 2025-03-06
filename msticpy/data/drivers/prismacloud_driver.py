# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""PrismaCloud Driver class."""

__author__ = "Rajamani R"

import logging
from typing import TYPE_CHECKING, Any, ClassVar, TypedDict

import httpx
import pandas as pd
from msticpy.common.exceptions import MsticpyConnectionError, MsticpyUserError

if TYPE_CHECKING:
    from collections.abc import Callable

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


class DriverConfig(TypedDict, total=False):
    """
    Represents configuration options for the Prisma Cloud Driver.

    Attributes
    ----------
        timeout (int, optional): Request timeout in seconds. Default is 300.
        base_url (str, optional): Base API URL for Prisma Cloud.
        debug (bool, optional): Enable or disable debug mode. Default is False.
        max_retries (int, optional): Maximum number of retries for API requests.
        headers (dict[str, str], optional): Custom headers for HTTP requests.
    """

    timeout: int
    base_url: str
    debug: bool
    max_retries: int
    headers: dict[str, str]


class PrismaCloudDriver:
    """
    Provide interface to connect and execute queries on Prisma Cloud.

    This driver handles authentication, query execution, and data retrieval
    from Prisma Cloud APIs. It supports multiple query types,
    including asset searches,
    configuration checks, event monitoring, and network analysis.
    The driver also
    manages authentication tokens and handles API request retries.

    Features:
        - Authenticate with Prisma Cloud using username and password.
        - Execute predefined queries on different Prisma Cloud data sources.
        - Retrieve assets, configurations, events, and network-related data.
        - Automatically refresh authentication tokens.
        - Handle API request errors and retries gracefully.
        - Use pagination for large datasets.

    Attributes
    ----------
        ENDPOINT_MAP (ClassVar[dict[str, str]]): Maps query sources to their
        respective API endpoints.
        timeout (int): Timeout for API requests in seconds (default: 300).
        base_url (str): Base URL for Prisma Cloud API.
        debug (bool): Enables or disables debug logging (default: False).
        connected (bool): Tracks authentication status.
        max_retries (int): Number of retry attempts for failed API requests.
        headers (dict[str, str]): HTTP headers used in API requests.

    Example Usage:
        ```python
        driver = PrismaCloudDriver(base_url="https://api.prismacloud.io",
        timeout=200)
        driver.connect(username="user@example.com", password="xxxxx")

        # Execute a network search query
        query_result = driver.execute_query("config where cloud.type = 'aws'",
        "network")
        print(query_result)
        ```

    Raises
    ------
        PrismaCloudAuthError: If authentication fails.
        PrismaCloudQueryError: If there is an error in executing a query.
        MsticpyUserError: If an invalid query source is provided.

    """

    # Disable the warning since this class is expected to have few methods
    # pylint: disable=too-few-public-methods
    ENDPOINT_MAP: ClassVar[dict[str, str]] = {
        "assets": "/search/api/v1/asset",
        "configurations": "/search/api/v2/config",
        "events": "/search/event",
        "network": "/search",
    }

    def __init__(self, **kwargs: DriverConfig) -> None:
        """
        Initialize the Prisma Cloud Driver and set up the HTTP client.

        This method configures the HTTP client (`self.client`)
        for communicating with the Prisma Cloud API.
        It establishes connection settings, including
        timeout, retry logic, and custom headers.
        The HTTP client is used to execute authenticated
        API requests and handle error responses.

        The transport layer (`self.transport`) is
        initialized with retry support.
        to ensure resilience against temporary failures.

        Attributes Initialized:
            self.client (httpx.Client):
                - Handles API requests and maintains authentication headers.
                - Uses a persistent session for improved efficiency.
                - Configured with a timeout, base URL, and custom headers.

            self.transport (httpx.HTTPTransport):
                - Provides automatic retry logic for API requests.
                - Ensures stability during transient network issues.
                - Controls connection reuse for performance optimization.
        """
        valid_keys = {"timeout", "base_url", "debug", "max_retries", "headers"}
        unknown_keys = set(kwargs) - valid_keys
        if unknown_keys:
            logger.warning("Unknown configuration keys provided: %s", unknown_keys)

        self.timeout: int = int(kwargs.get("timeout", 300))  # type: ignore[arg-type]
        self.base_url: str = str(kwargs.get("base_url", BASE_URL_API))
        self.debug: bool = bool(kwargs.get("debug", False))
        self.max_retries: int = int(kwargs.get("max_retries", 3))  # type: ignore[arg-type]
        self.connected: bool = False
        self.headers: dict[str, str] = dict(
            kwargs.get(
                "headers",
                {"User-Agent": "PrismaCloudDriver/1.0", "Accept": "application/json"},
            ),
        )  # type: ignore[arg-type]

        transport = httpx.HTTPTransport(retries=int(self.max_retries))
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
        """
        Authenticate with Prisma Cloud and establish a session.

        This method sends authentication credentials to the
        Prisma Cloud API and retrieves an access token if the login is successful.
        The token is then stored in `self.client.headers` to be used for subsequent
        authenticated requests.

        If authentication succeeds, the driver sets `self.connected = True`
        and logs a success message. If authentication fails,
        an exception is raised with the appropriate error message.

        Parameters
        ----------
        username : str
            The Prisma Cloud account username used for authentication.
        password : str
            The corresponding password for the Prisma Cloud account.

        Behavior
        --------
        - Sends a POST request to `/login` with the provided credentials.
        - Extracts and stores the authentication token from the response.
        - Logs a debug message indicating an authentication attempt.
        - If successful, logs an authentication success message.
        - If authentication fails, logs an error and raises `PrismaCloudAuthError`.

        Raises
        ------
        PrismaCloudAuthError
            If authentication fails due to incorrect credentials or other login issues.
        MsticpyConnectionError
            If there is a network or API request failure.

        Returns
        -------
        PrismaCloudDriver
            The instance of the driver with an authenticated session.

        Example
        -------
        driver = PrismaCloudDriver()
        driver.connect(username="user@example.com", password="xxx")

        """
        if not username or not password:
            msg = "Both 'username' and 'password' must be provided."
            raise PrismaCloudAuthError(msg)

        logger.debug("Attempting to authenticate with username=[REDACTED]")
        try:
            response = self.client.post(
                "/login",
                json={"username": username, "password": password},
            )
            response.raise_for_status()
            result = self._parse_json(response)

            if result.get("message") == "login_successful":
                self.client.headers["X-Redlock-Auth"] = result["token"]
                self.connected = True
                logger.info("Prisma Cloud connection successful")
                return self

            logger.error("Login failed: %s", result.get("message", "Unknown error"))
            msg = f"Login failed: {result.get('message', 'Unknown error')}"
            raise PrismaCloudAuthError(msg)
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

    def _parse_json(self, response: httpx.Response) -> dict[str, Any]:
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

    def _fetch_prisma_data(
        self,
        endpoint: str,
        payload: dict[str, Any],
        timeout: int,
        max_retries: int,
    ) -> dict[str, Any]:
        """
        Send an API request to Prisma Cloud with automatic retry handling.

        This method sends a POST request to the specified Prisma Cloud API endpoint
        with the given payload. It includes built-in retry logic to handle temporary
        failures such as rate limits, network issues, or API timeouts.

        If a request fails, the method retries the request up to `max_retries` times
        before logging an error and raising an exception.

        Parameters
        ----------
        endpoint : str
            The Prisma Cloud API endpoint for the request.
        payload : dict[str, Any]
            The request body, typically containing query parameters.
        timeout : int
            The maximum time (in seconds) to wait for a response.
        max_retries : int
            The number of times to retry the request in case of failure.

        Behavior
        --------
        - Sends a POST request to the specified endpoint with the payload.
        - Waits for a response within the provided timeout period.
        - If the response is successful (status code 200), it parses and returns the JSON data.
        - If an HTTP error occurs:
            - Retries the request up to `max_retries` times.
            - Logs a warning after each failed attempt.
            - Raises an error if all retries are exhausted.
        - If all retries fail, logs an error and raises `PrismaCloudQueryError`.
        """
        retries = 0
        while retries < max_retries:
            try:
                response = self.client.post(endpoint, json=payload, timeout=timeout)
                response.raise_for_status()
                return self._parse_json(response)
            except httpx.HTTPStatusError as err:
                retries += 1
                if retries >= max_retries:
                    logger.exception("Max retries exceeded for API request.")
                    self._handle_http_error(err)
                logger.warning(
                    "Retrying API request... Attempt %d/%d",
                    retries,
                    max_retries,
                )

        self._handle_connection_error("Max retries exceeded.")
        return {}

    def _process_prisma_response(
        self,
        data: dict[str, Any],
        data_key: str,
    ) -> list[dict[str, Any]]:
        """
        Extract relevant records from the Prisma API response.

        This method navigates through a nested JSON structure using the specified `data_key`,
        which represents the path to the target data. It splits `data_key` on dots (`.`)
        to handle multi-level JSON keys and attempts to retrieve the relevant records.

        If the extracted value is not a list, the method returns an empty list.

        Parameters
        ----------
        data : dict[str, Any]
            The raw JSON response from Prisma Cloud API.
        data_key : str
            The key path (dot-separated) indicating where the required
            data is stored within the JSON structure.

        Behavior
        --------
        - Starts with the full `data` dictionary.
        - Iteratively accesses each level of the JSON structure based on `data_key`.
        - If the key path leads to a list, returns it.
        - If the key path leads to a non-list value or is missing, returns an empty list.
        """
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
        """
        Retrieve paginated results from Prisma Cloud API.

        This method handles API pagination by repeatedly sending requests until the
        requested number of results (`limitresult`) is retrieved or no more pages exist.
        It uses a `nextPageToken` to navigate through the pages and aggregates the
        results into a single list.

        The method extracts the relevant data using `_process_prisma_response()`
        and logs progress at each iteration.

        Parameters
        ----------
        endpoint : str
            The Prisma Cloud API endpoint to send requests to.
        base_payload : dict[str, Any]
            The base payload for the API request, which is modified for pagination.
        limitresult : int
            The total number of results to retrieve.
        limitpage : int
            The maximum number of results per request.
        timeout : int
            The timeout duration for each API request in seconds.
        data_key : str
            The key path in the API response that contains the relevant data
            (e.g., `"data.nodes"`).

        Behavior
        --------
        - Starts with an empty list (`results`).
        - Copies `base_payload` and sets the `limit` field based on `limitpage`
        and remaining results.
        - Retrieves data from `_fetch_prisma_data()` and extracts relevant
        items using `_process_prisma_response()`.
        - Appends extracted items to `results`.
        - Checks for `nextPageToken` and continues pagination if available.
        - Stops when:
            - No `nextPageToken` is found.
            - The required `limitresult` is reached.
            - The API response contains no more data.
        """
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
                msg = "Unexpected API response format."
                raise MsticpyConnectionError(msg)

            items = self._process_prisma_response(data, data_key)
            if not isinstance(items, list):
                logger.error("❌ Processed data is not a list.")
                msg = "Unexpected processed data format."
                raise MsticpyConnectionError(msg)

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
        limitresult: int = 10000,
        limitpage: int = 100,
        **kwargs: QueryArgs,
    ) -> pd.DataFrame:
        """
        Execute a network data search on Prisma Cloud with pagination.

        This method retrieves network-related data from Prisma Cloud by executing the
        given query against the specified API endpoint. It supports pagination and
        processes large datasets efficiently.

        The function allows customization of search parameters, such as time range,
        cloud provider type, and request limits, by accepting additional keyword arguments.

        Parameters
        ----------
        query : str
            The Prisma Cloud query string to execute.
        endpoint : str
            The API endpoint to send the query to.
        limitresult : int, optional
            The maximum number of results to retrieve. Defaults to 10,000.
        limitpage : int, optional
            The number of results per page. Defaults to 100.
        **kwargs : QueryArgs
            Additional optional query parameters:
            - timeout (int): API request timeout in seconds.
            - unit (str): Time unit for the query (e.g., `"minute"`, `"hour"`).
            - amount (int): Time range quantity in the specified unit.
            - cloudtype (str): Cloud provider filter (`"aws"`, `"gcp"`, `"azure"`).

        Behavior
        --------
        - Constructs a search payload including query, time range, and cloud provider filter.
        - Calls `_paginate_prisma_search()` to retrieve results in multiple pages.
        - Converts the retrieved data into a Pandas DataFrame.

        Logging
        -------
        - Logs the request payload before sending the API request.
        - Logs the total number of records retrieved.

        Raises
        ------
        MsticpyConnectionError
            If the API request fails or returns an unexpected response.

        Example
        -------
        network_data = driver.prisma_search_network(
            query="network where cloud.type = 'aws'",
            endpoint="/search",
            limitresult=5000,
            unit="day",
            amount=7
        )

        Returns
        -------
        pd.DataFrame
            A DataFrame containing the retrieved network data.
            Returns an empty DataFrame if no results are found.
        """
        timeout = int(kwargs.get("timeout", self.timeout))  # type: ignore[arg-type]
        unit = str(kwargs.get("unit", "hour"))
        amount = int(kwargs.get("amount", 4))  # type: ignore[arg-type]
        cloudtype = str(kwargs.get("cloudtype", "aws"))
        base_payload = {
            "query": query,
            "limit": limitpage,
            "cloudType": cloudtype,
            "saved": False,
            "default": False,
            "timeRange": {
                "type": "relative",
                "value": {"unit": unit, "amount": amount},
            },
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
        """
        Execute an asset search on Prisma Cloud with pagination.

        This method retrieves asset-related data from Prisma Cloud by executing the
        given query against the specified API endpoint. It supports pagination to
        efficiently handle large datasets.

        The function allows customization of search parameters, such as time range
        and request limits, by accepting additional keyword arguments.

        Parameters
        ----------
        query : str
            The Prisma Cloud query string to execute.
        endpoint : str
            The API endpoint to send the query to.
        **kwargs : QueryArgs
            Additional optional query parameters:
            - timeout (int): API request timeout in seconds.
            - limit (int): Maximum number of results to retrieve.
            - unit (str): Time unit for the query (e.g., `"minute"`, `"hour"`).
            - amount (int): Time range quantity in the specified unit.

        Behavior
        --------
        - Constructs a search payload including query and time range.
        - Calls `_paginate_prisma_search()` to retrieve results in multiple pages.
        - Converts the retrieved data into a Pandas DataFrame.

        Logging
        -------
        - Logs the query being executed.
        - Logs the total number of records retrieved.

        Raises
        ------
        MsticpyConnectionError
            If the API request fails or returns an unexpected response.

        Example
        -------
        asset_data = driver.prisma_search_assets(
            query="config where cloud.type = 'aws'",
            endpoint="/search/api/v1/asset",
            limit=5000,
            unit="day",
            amount=7
        )

        Returns
        -------
        pd.DataFrame
            A DataFrame containing the retrieved asset data.
            Returns an empty DataFrame if no results are found.
        """
        timeout = int(kwargs.get("timeout", self.timeout))  # type: ignore[arg-type]
        limitresult = int(kwargs.get("limit", 10000))  # type: ignore[arg-type]
        limitpage: int = 1000
        unit = str(kwargs.get("unit", "hour"))
        amount = int(kwargs.get("amount", 4))  # type: ignore[arg-type]

        logger.info("Executing asset search with query: %s", query)

        base_payload = {
            "query": query,
            "limit": limitpage,
            "timeRange": {
                "type": "relative",
                "value": {"unit": unit, "amount": amount},
            },
        }

        results = self._paginate_prisma_search(
            endpoint,
            base_payload,
            limitresult,
            limitpage,
            timeout,
            "value",
        )

        return pd.DataFrame(results) if results else pd.DataFrame()

    def prisma_search_events(
        self,
        query: str,
        endpoint: str,
        **kwargs: QueryArgs,
    ) -> pd.DataFrame:
        """
        Execute an event search on Prisma Cloud.

        This method retrieves event-related data from Prisma Cloud by executing the
        given query against the specified API endpoint. It allows customization of
        time range, result limits, and other search parameters.

        The function constructs a request payload, sends the API request, and
        processes the response into a Pandas DataFrame.

        Parameters
        ----------
        query : str
            The Prisma Cloud query string to execute.
        endpoint : str
            The API endpoint to send the query to.
        **kwargs : QueryArgs
            Additional optional query parameters:
            - timeout (int): API request timeout in seconds.
            - max_retries (int): Maximum retry attempts for failed requests.
            - unit (str): Time unit for the query (e.g., `"minute"`, `"hour"`, `"day"`).
            - amount (int): Time range quantity in the specified unit.
            - limit (int): Maximum number of results to retrieve.

        Behavior
        --------
        - Constructs a search payload with the query, time range, and limit.
        - Calls `_fetch_prisma_data()` to send the request and retrieve the data.
        - Extracts event records from the response and converts them into a Pandas DataFrame.

        Logging
        -------
        - Logs the query being executed.
        - Logs the request payload before sending the API request.
        - Logs the number of event records retrieved.

        Raises
        ------
        MsticpyConnectionError
            If the API request fails or returns an unexpected response.

        Example
        -------
        event_data = driver.prisma_search_events(
            query="event where cloud.type = 'aws'",
            endpoint="/search/event",
            limit=5000,
            unit="hour",
            amount=24
        )

        Returns
        -------
        pd.DataFrame
            A DataFrame containing the retrieved event data.
            Returns an empty DataFrame if no results are found.
        """
        timeout = int(kwargs.get("timeout", self.timeout))  # type: ignore[arg-type]
        max_retries = int(kwargs.get("max_retries", self.max_retries))  # type: ignore[arg-type]
        unit = str(kwargs.get("unit", "day"))
        amount = int(kwargs.get("amount", 1))  # type: ignore[arg-type]
        limit = int(kwargs.get("limit", 5000))  # type: ignore[arg-type]

        logger.info("Executing event search with query: %s", query)

        payload = {
            "query": query,
            "limit": limit,
            "timeRange": {
                "type": "relative",
                "value": {"unit": unit, "amount": amount},
            },
            **kwargs,
        }

        logger.info("Sending request with payload: %s", payload)
        data = self._fetch_prisma_data(endpoint, payload, timeout, max_retries)

        items = data.get("data", {}).get("items", [])
        logger.info("Retrieved %d event records.", len(items))
        return pd.DataFrame(items) if items else pd.DataFrame()

    def prisma_search_configurations(
        self,
        query: str,
        endpoint: str,
        **kwargs: QueryArgs,
    ) -> pd.DataFrame:
        """
        Execute a configuration search on Prisma Cloud.

        This method retrieves configuration-related data from Prisma Cloud by executing
        the given query against the specified API endpoint. It allows customization of
        result limits and request parameters.

        The function constructs a request payload, sends the API request, and processes
        the response into a Pandas DataFrame.

        Parameters
        ----------
        query : str
            The Prisma Cloud query string to execute.
        endpoint : str
            The API endpoint to send the query to.
        **kwargs : QueryArgs
            Additional optional query parameters:
            - timeout (int): API request timeout in seconds.
            - max_retries (int): Maximum retry attempts for failed requests.
            - limit (int): Maximum number of results to retrieve (default: 1000).
            - withResourceJson (bool): Whether to include resource JSON details.

        Behavior
        --------
        - Constructs a search payload with the query and optional parameters.
        - Calls `_fetch_prisma_data()` to send the request and retrieve the data.
        - Extracts configuration from the response,converts them into a df.

        Logging
        -------
        - Logs the query being executed.
        - Logs the number of configuration records retrieved.

        Raises
        ------
        MsticpyConnectionError
            If the API request fails or returns an unexpected response.

        Example
        -------
        config_data = driver.prisma_search_configurations(
            query="config where cloud.type = 'aws'",
            endpoint="/search/api/v2/config",
            limit=5000,
            withResourceJson=True
        )

        Returns
        -------
        pd.DataFrame
            A DataFrame containing the retrieved configuration data.
            Returns an empty DataFrame if no results are found.
        """
        timeout = int(kwargs.get("timeout", self.timeout))  # type: ignore[arg-type]
        max_retries = int(kwargs.get("max_retries", self.max_retries))  # type: ignore[arg-type]

        logger.info("Executing configuration search with query: %s", query)
        payload = {"query": query, "limit": 1000, "withResourceJson": True, **kwargs}
        data = self._fetch_prisma_data(endpoint, payload, timeout, max_retries)
        items = data.get("items", [])

        logger.info("Retrieved %d configuration results.", len(items))
        return pd.DataFrame(items) if items else pd.DataFrame()

    # pylint: disable=inconsistent-return-statements
    def execute_query(
        self,
        query: str,
        query_source: str | None = None,
        **kwargs: QueryArgs,
    ) -> pd.DataFrame:
        """
        Execute a query on Prisma Cloud and return results as a DataFrame.

        This method serves as a unified interface for executing different types of
        queries on Prisma Cloud, routing the request to the appropriate search
        function based on the `query_source`.

        The function validates authentication, maps the `query_source` to the correct
        API endpoint, and dynamically calls the relevant search method.

        Parameters
        ----------
        query : str
            The Prisma Cloud query string to execute.
        query_source : str | None, optional
            The source of the query, determining which search function to use.
            Options: `"configurations"`, `"assets"`, `"events"`, `"network"`.
        **kwargs : QueryArgs
            Additional optional query parameters, such as:
            - timeout (int): API request timeout in seconds.
            - max_retries (int): Maximum retry attempts for failed requests.
            - limit (int): Maximum number of results to retrieve.
            - unit (str): Time unit for the query (e.g., `"minute"`, `"hour"`, `"day"`).
            - amount (int): Time range quantity in the specified unit.

        Behavior
        --------
        - Checks if authentication is active (`"X-Redlock-Auth"` in headers).
        - Validates `query_source` and maps it to the corresponding API endpoint.
        - Calls the appropriate Prisma Cloud search function based on the query type.
        - Converts the retrieved data into a Pandas DataFrame.

        Logging
        -------
        - Logs the query execution details.
        - Logs warnings for invalid query sources.
        - Logs errors if the API request fails.

        Raises
        ------
        PrismaCloudQueryError
            If the driver is not authenticated.
        MsticpyUserError
            If the `query_source` is invalid or not supported.

        Example
        -------
        query_result = driver.execute_query(
            query="config where cloud.type = 'aws'",
            query_source="configurations",
            limit=1000,
            unit="day",
            amount=7
        )

        Returns
        -------
        pd.DataFrame
            A DataFrame containing the retrieved query results.
            Returns an empty DataFrame if no results are found.
        """
        # Check if authentication token is present
        if "X-Redlock-Auth" not in self.client.headers:
            msg = "Driver not connected to Prisma Cloud."
            raise PrismaCloudQueryError(msg)

        # Check if query_source is valid
        if not query_source or query_source not in self.ENDPOINT_MAP:
            msg = f"Invalid or missing query source: {query_source}"
            raise MsticpyUserError(msg)

        # Find the API endpoint for the given query_source
        endpoint: str = self.ENDPOINT_MAP[query_source]

        # Dictionary that maps endpoints to their respective functions
        query_methods: dict[str, Callable[..., pd.DataFrame]] = {
            "/search/api/v2/config": self.prisma_search_configurations,
            "/search/api/v1/asset": self.prisma_search_assets,
            "/search": self.prisma_search_network,
            "/search/event": self.prisma_search_events,
        }

        # Fetch function from dictionary and execute it
        query_function = query_methods.get(endpoint)
        if query_function:
            return query_function(query, endpoint, **kwargs)

        # If endpoint is not recognized, raise an error
        msg = f"Unsupported query source: {query_source}"
        raise MsticpyUserError(msg)
