# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Cybereason Driver class."""

from __future__ import annotations

import datetime as dt
import json
import logging
import re
from asyncio import AbstractEventLoop, Future, as_completed
from concurrent.futures import ThreadPoolExecutor
from functools import partial, singledispatch
from typing import Any, ClassVar

import httpx
import pandas as pd
from tqdm.auto import tqdm
from typing_extensions import Self

from ..._version import VERSION
from ...common.exceptions import MsticpyUserConfigError
from ...common.provider_settings import (
    ProviderSettings,
    get_provider_settings,
)
from ...common.utility import mp_ua_header
from ..core.query_defns import Formatters
from ..core.query_provider_connections_mixin import _get_event_loop
from .driver_base import DriverBase, DriverProps, QuerySource

__version__ = VERSION
__author__ = "Florian Bracq"

logger: logging.Logger = logging.getLogger(__name__)

_HELP_URI = "https://msticpy.readthedocs.io/en/latest/data_acquisition/DataProviders.html"


# pylint: disable=too-many-instance-attributes
class CybereasonDriver(DriverBase):
    """Class to interact with Cybereason."""

    CONFIG_NAME: ClassVar[str] = "Cybereason"

    _CONFIG_NAME_MAP: ClassVar[dict[str, tuple[str, ...]]] = {
        "tenant_id": ("tenantid", "tenant_id"),
        "client_id": ("clientid", "client_id"),
        "client_secret": ("clientsecret", "client_secret"),
    }

    def __init__(
        self: CybereasonDriver,
        *,
        timeout: int = 120,
        max_results: int = 1000,
        debug: bool = False,
        **kwargs,
    ) -> None:
        """
        Instantiate Cybereason driver.

        Additional Parameters
        ---------------------
        timeout : int
            Query timeout in seconds. Defaults to 120 seconds
        max_results : int
            Number of total results to return. Defaults to 1000
            Max is 10,000.

        Returns
        -------
        Union[pd.DataFrame, Any]
            A DataFrame (if successfull) or
            the underlying provider result if an error.

        """
        super().__init__(**kwargs)
        logger.debug("Set timeout to %d", timeout)
        max_results = min(max_results, 100000)
        logger.debug("Set maximum results to %d", max_results)
        self.base_url: str = "https://{tenant_id}.cybereason.net"
        self.auth_endpoint: str = "/login.html"
        self.req_body: dict[str, Any] = {
            "queryPath": [],
            "totalResultLimit": max_results,
            "perGroupLimit": 100,
            "perFeatureLimit": 100,
            "templateContext": "SPECIFIC",
            "queryTimeout": timeout * 1000,
            "customFields": [],
        }
        self.search_endpoint: str = "/rest/visualsearch/query/simple"
        self._loaded: bool = True
        self.client: httpx.Client = httpx.Client(
            follow_redirects=True,
            timeout=self.get_http_timeout(timeout=timeout, def_timeout=120),
            headers=mp_ua_header(),
        )
        self.set_driver_property(
            DriverProps.FORMATTERS,
            {
                Formatters.PARAM_HANDLER: self._custom_param_handler,
                Formatters.DATETIME: self._format_datetime,
                Formatters.LIST: self._format_list,
            },
        )

        self.set_driver_property(DriverProps.SUPPORTS_THREADING, value=True)
        self.set_driver_property(
            DriverProps.MAX_PARALLEL,
            value=kwargs.get("max_threads", 4),
        )
        self._debug: bool = debug

    def query(
        self: Self,
        query: str,
        query_source: QuerySource | None = None,
        *,
        page_size: int = 100,
        **__,
    ) -> pd.DataFrame | str | None:
        """
        Execute query string and return DataFrame of results.

        Parameters
        ----------
        query : str
            The query to execute
        query_source : QuerySource
            The query definition object
        page_size : int
            Number of results to return per page. Defaults to 100

        Returns
        -------
        Union[pd.DataFrame, Any]
            A DataFrame (if successfull) or
            the underlying provider result if an error.

        """
        del query_source
        if not self._connected:
            raise self._create_not_connected_err(self.__class__.__name__)

        page_size = min(page_size, 4000)
        logger.debug("Set page size to %d", page_size)
        json_query: dict[str, Any] = json.loads(query)
        body: dict[str, Any] = {**self.req_body, **json_query}

        # The query must be executed at least once to retrieve the number
        # of results and the pagination token.
        response: dict[str, Any] = self.__execute_query(body, page_size=page_size)

        total_results: int = response["data"]["totalResults"]
        pagination_token: str = response["data"]["paginationToken"]
        results: dict[str, Any] = response["data"]["resultIdToElementDataMap"]

        logger.debug("Retrieved %d/%d results", len(results), total_results)

        df_result: pd.DataFrame | None = None

        if len(results) < total_results:
            df_result = self._exec_paginated_queries(
                body=body,
                page_size=page_size,
                pagination_token=pagination_token,
                total_results=total_results,
            )
        else:
            df_result = self._format_result_to_dataframe(result=response)
        df_result["instance"] = self.instance
        return df_result

    def _exec_paginated_queries(  # noqa: PLR0913
        self: Self,
        body: dict[str, Any],
        page_size: int,
        pagination_token: str,
        total_results: int,
        *,
        progress: bool = True,
        retry_on_error: bool = False,
        **kwargs,
    ) -> pd.DataFrame:
        """
        Return results of paginated queries.

        Parameters
        ----------
        body : dict[str, Any]
            The body of the query to execute.
        page_size: int
            number of results to return per page
        pagination_token: str
            Cybereason built-in pagination tracker
        total_results: int
            Number of results for the executed query.

        Additional Parameters
        ----------
        progress: bool, optional
            Show progress bar, by default True
        retry_on_error: bool, optional
            Retry failed queries, by default False
        **kwargs : dict[str, Any]
            Additional keyword arguments to pass to the query method.

        Returns
        -------
        pd.DataFrame
            The concatenated results of all the paginated queries.

        Notes
        -----
        This method executes the specified query multiple times to retrieve
        all the data from paginated results.
        The queries are executed asynchronously.

        """
        del kwargs
        query_tasks: dict[str, partial[dict[str, Any]]] = (
            self._create_paginated_query_tasks(
                body=body,
                page_size=page_size,
                pagination_token=pagination_token,
                total_results=total_results,
            )
        )

        logger.info("Running %s paginated queries.", len(query_tasks))
        event_loop: AbstractEventLoop = _get_event_loop()
        return event_loop.run_until_complete(
            self.__run_threaded_queries(
                query_tasks,
                progress=progress,
                retry=retry_on_error,
            ),
        )

    def connect(
        self: Self,
        connection_str: str | None = None,
        *,
        instance: str | None = None,
        **kwargs,
    ) -> None:
        """
        Connect to data source.

        Parameters
        ----------
        connection_str: Optional[str], optional
            Connect to a data source
        instance : Optional[str], optional
            Optional name of configuration instance - this
            is added as a prefix to the driver configuration key name
            when searching for configuration in the msticpyconfig.yaml
        kwargs:
            Extra parameters to connect.

        Notes
        -----
        Connection string fields:
            instance
            client_id
            client_secret

        """
        del connection_str
        cs_dict: dict[str, Any] = {}

        self._instance = instance
        cs_dict = CybereasonDriver._get_driver_settings(
            self.CONFIG_NAME,
            self._instance,
        )
        # let user override config settings with function kwargs
        cs_dict.update(kwargs)

        missing_settings = [
            setting
            for setting in ("tenant_id", "client_id", "client_secret")
            if setting not in cs_dict
        ]
        if missing_settings:
            err_msg: str = (
                "You must supply the following required connection parameter(s) "
                "to the connect function or add them to your msticpyconfig.yaml. "
                ", ".join(f"'{param}'" for param in missing_settings)
            )
            raise MsticpyUserConfigError(
                err_msg,
                title="Missing connection parameters.",
                help_uri=("Connecting to OData sources.", _HELP_URI),
            )

        # self.auth_endpoint and self.req_body are correctly set in concrete
        # instances __init__
        self.client.base_url = httpx.URL(
            self.base_url.format(tenant_id=cs_dict["tenant_id"]),
        )
        req_body: dict[str, str] = {
            "username": cs_dict["client_id"],
            "password": cs_dict["client_secret"],
        }

        # Authenticate and obtain cookie for future calls
        response = self.client.post(self.auth_endpoint, data=req_body)
        response.raise_for_status()

        logger.info("Connected.")
        self._connected: bool = True
        self.current_connection: str | None = self._instance

    @staticmethod
    def _flatten_result(entry: dict[str, Any]) -> dict[str, Any]:
        """
        Flatten Cybereason result to a format that can be handled by pandas.

        Parameters
        ----------
        entry: dict[str, Any]
            Entry to flatten

        Returns
        -------
        dict[str, Any]

        """
        result: dict[str, Any] = {}
        # Retrieve simpleValues and add them to the output
        simple_values: dict[str, Any] = entry.get("simpleValues", {})
        result = CybereasonDriver._flatten_simple_values(simple_values)

        elt_value: list[dict[str, Any]] | dict[str, Any] = entry.get(
            "elementValues",
            {},
        )
        result.update(**CybereasonDriver._flatten_element_values(elt_value))
        return result

    @staticmethod
    def _flatten_simple_values(simple_values: dict[str, Any]) -> dict[str, Any]:
        """
        Flatten "simpleValues from Cybereason result to a format that can be handled by pandas.

        Parameters
        ----------
        simple_values: dict[str, Any]
            Entry to flatten

        Returns
        -------
        dict[str, Any]

        """
        result: dict[str, Any] = {}
        for name, values in simple_values.items():
            if not values["values"]:
                return result
            result[name] = list(
                {
                    (
                        CybereasonDriver._format_to_datetime(int(value))
                        if "Time" in name
                        else value.strip().rstrip("\x00")
                    )
                    for value in values["values"]
                },
            )
            if values["totalValues"] == 1:
                result[name] = result[name][0]

        return result

    @staticmethod
    def _flatten_element_values(
        element_values: dict[str, Any] | list[dict[str, Any]],
    ) -> dict[str, Any]:
        """
        Flatten "elementValues from Cybereason result to a format that can be handled by pandas.

        Parameters
        ----------
        element_values: Union[dict[str, Any], List[str]]
            Entry to flatten

        Returns
        -------
        dict[str, Any]

        """
        result: dict[str, Any] = {}
        if isinstance(element_values, list):
            for values in element_values:
                result[values["elementType"]] = values["name"]
                result[f"{values['elementType']}.guid"] = values["guid"]
        elif isinstance(element_values, dict):
            for key, values in element_values.items():
                flattened = CybereasonDriver._flatten_result(values)
                if flattened:
                    for subkey, subvalues in flattened.items():
                        result[f"{key}.{subkey}"] = subvalues
        return result

    def _create_paginated_query_tasks(
        self: Self,
        body: dict[str, Any],
        page_size: int,
        pagination_token: str,
        total_results: int,
    ) -> dict[str, partial[dict[str, Any]]]:
        """Return dictionary of partials to execute queries."""
        # Compute the number of queries to execute
        total_pages: int = total_results // page_size + 1
        # The first query (page 0) as to be re-run due to a bug in
        # Cybereason API. The first query returns less results than the page size
        # when executed without a pagination token.
        return {
            f"{page}": partial(
                self.__execute_query,
                body=body,
                page_size=page_size,
                pagination_token=pagination_token,
                page=page,
            )
            for page in range(total_pages)
        }

    def __execute_query(
        self: Self,
        body: dict[str, Any],
        *,
        page: int = 0,
        page_size: int = 2000,
        pagination_token: str | None = None,
        max_retry: int = 3,
    ) -> dict[str, Any]:
        """
        Run query with pagination enabled.

        :raises httpx.HTTPStatusError: if max_retry reached

        Parameters
        ----------
        body: dict[str, Any]
            Body of the HTTP Request
        page_size: int
            Size of the page for results
        page: int
            Page number to query
        pagination_token: str
            Token of the current search
        max_retry: int
            Maximum retries in case of API no cuccess response

        Returns
        -------
        dict[str, Any]

        """
        if pagination_token:
            pagination: dict[str, Any] = {
                "pagination": {
                    "pageSize": page_size,
                    "page": page + 1,
                    "paginationToken": pagination_token,
                    "skip": page * page_size,
                },
            }
            headers: dict[str, Any] = {"Pagination-Token": pagination_token}
        else:
            pagination = {"pagination": {"pageSize": page_size}}
            headers = {}
        params: dict[str, Any] = {"page": page, "itemsPerPage": page_size}
        status: str | None = None
        cur_try: int = 0
        while status != "SUCCESS" and cur_try < max_retry:
            response: httpx.Response = self.client.post(
                self.search_endpoint,
                json={**body, **pagination},
                headers=headers,
                params=params,
            )
            response.raise_for_status()
            json_result: dict[str, Any] = response.json()
            status = json_result["status"]
            cur_try += 1

        if cur_try >= max_retry:
            err_msg: str = f"{status}: {json_result['message']}"
            raise httpx.HTTPStatusError(
                err_msg,
                request=response.request,
                response=response,
            )

        return json_result

    async def __run_threaded_queries(
        self: Self,
        query_tasks: dict[str, partial],
        *,
        progress: bool = True,
        retry: bool = False,
    ) -> pd.DataFrame:
        logger.info("Running %d threaded queries.", len(query_tasks))
        event_loop = _get_event_loop()
        with ThreadPoolExecutor(max_workers=4) as executor:
            results: list[pd.DataFrame] = []
            failed_tasks: dict[str, Future] = {}
            thread_tasks: dict[str, Future] = {
                query_id: event_loop.run_in_executor(executor, query_func)
                for query_id, query_func in query_tasks.items()
            }
            if progress:
                task_iter = tqdm(
                    as_completed(thread_tasks.values()),
                    unit="paginated-queries",
                    desc="Running",
                )
            else:
                task_iter = as_completed(thread_tasks.values())
            ids_and_tasks: dict[str, Future] = dict(zip(thread_tasks, task_iter))
            for query_id, thread_task in ids_and_tasks.items():
                try:
                    result: dict[str, Any] = await thread_task
                    df_result: pd.DataFrame = self._format_result_to_dataframe(result)
                    logger.info("Query task '%s' completed successfully.", query_id)
                    results.append(df_result)
                except Exception:  # pylint: disable=broad-except
                    logger.warning(
                        "Query task '%s' failed with exception",
                        query_id,
                        exc_info=True,
                    )
                    failed_tasks[query_id] = thread_task

            if retry and failed_tasks:
                for query_id, thread_task in failed_tasks.items():
                    try:
                        logger.info("Retrying query task '%s'", query_id)
                        result = await thread_task
                        df_result = self._format_result_to_dataframe(result)
                        results.append(df_result)
                    except Exception:  # pylint: disable=broad-except
                        logger.warning(
                            "Retried query task '%s' failed with exception",
                            query_id,
                            exc_info=True,
                        )
            # Sort the results by the order of the tasks
            results = [result for _, result in sorted(zip(thread_tasks, results))]
        return pd.concat(results, ignore_index=True)

    # pylint: disable=too-many-branches
    def query_with_results(
        self: Self,
        query: str,
        **__,
    ) -> tuple[pd.DataFrame, Any]:
        """
        Execute query string and return DataFrame of results.

        Parameters
        ----------
        query : str
            The kql query to execute

        Returns
        -------
        Tuple[pd.DataFrame, results.ResultSet]
            A DataFrame (if successfull) and
            Kql ResultSet.

        """
        del query
        err_msg: str = f"Not supported for {self.__class__.__name__}"
        raise NotImplementedError(err_msg)

    # Parameter Formatting method
    @staticmethod
    def _format_datetime(date_time: dt.datetime) -> int:
        """Return datetime formatted as timestamp in milliseconds."""
        return int(date_time.timestamp() * 1000)

    @staticmethod
    def _format_list(value: list[str]) -> list[str]:
        """Return list as itself."""
        return value

    # Parameter Formatting method
    @staticmethod
    def _format_to_datetime(timestamp: int) -> dt.datetime | int:
        """Return datetime from a timestamp in milliseconds."""
        try:
            return dt.datetime.fromtimestamp(
                timestamp // 1000,
                tz=dt.timezone.utc,
            )
        except TypeError:
            return timestamp

    @staticmethod
    def _format_result_to_dataframe(result: dict[str, Any]) -> pd.DataFrame:
        """Return a dataframe from a cybereason result object."""
        df_result: list[dict[str, Any]] = [
            {
                **CybereasonDriver._flatten_result(values),
                "resultId": result_id,
            }
            for result_id, values in result["data"]["resultIdToElementDataMap"].items()
        ]
        return pd.json_normalize(df_result)

    # Retrieve configuration parameters with aliases
    @staticmethod
    def _map_config_dict_name(config_dict: dict[str, str]) -> dict[str, str]:
        """Map configuration parameter names to expected values."""
        mapped_dict: dict[str, str] = config_dict.copy()
        for provided_name, value in config_dict.items():
            for req_name, alternates in CybereasonDriver._CONFIG_NAME_MAP.items():
                if provided_name.casefold() in alternates:
                    mapped_dict[req_name] = value
                    break
        return mapped_dict

    # Read values from configuration
    @staticmethod
    def _get_driver_settings(
        config_name: str,
        instance: str | None = None,
    ) -> dict[str, str]:
        """Try to retrieve config settings for Cybereason drivers."""
        config_key: str = f"{config_name}-{instance}" if instance else config_name
        drv_config: ProviderSettings | None = get_provider_settings(
            "DataProviders",
        ).get(config_key)
        app_config: dict[str, str] = {}
        if drv_config:
            app_config = dict(drv_config.args)

        if not app_config:
            return {}
        # map names to allow for different spellings
        return CybereasonDriver._map_config_dict_name(app_config)

    @staticmethod
    def _custom_param_handler(query: str, param_dict: dict[str, Any]) -> str:
        """Replace parameters in query template for Cybereason JSON queries."""
        query_dict: dict[str, Any] = json.loads(query)

        return json.dumps(_recursive_find_and_replace(query_dict, param_dict))


@singledispatch
def _recursive_find_and_replace(
    parameters: str | dict[str, Any] | list[str] | list[dict[str, Any]],
    param_dict: dict[str, Any],
) -> str | dict[str, Any] | list[str] | list[dict[str, Any]]:
    """Recursively find and replace parameters from query."""
    if isinstance(parameters, (list, str, dict)):
        return _recursive_find_and_replace(parameters, param_dict)
    return parameters


@_recursive_find_and_replace.register(dict)
def _(parameters: dict[str, Any], param_dict: dict[str, Any]) -> dict[str, Any]:
    return {
        parameter: _recursive_find_and_replace(value, param_dict)
        for parameter, value in parameters.items()
    }


@_recursive_find_and_replace.register(list)
def _(
    parameters: list[str] | list[dict[str, Any]], param_dict: dict[str, Any]
) -> list[str] | list[dict[str, Any]]:
    result: list[str] = []
    dict_result: list[dict[str, Any]] = []
    for parameter in parameters:
        updated_param: str | dict[str, Any] | list[str] | list[dict[str, Any]] = (
            _recursive_find_and_replace(
                parameter,
                param_dict,
            )
        )
        if isinstance(updated_param, list):
            result.extend([param for param in updated_param if isinstance(param, str)])
            dict_result.extend(
                [param for param in updated_param if isinstance(param, dict)]
            )
        elif isinstance(updated_param, dict):
            dict_result.append(updated_param)
        else:
            result.append(updated_param)
    return result or dict_result


@_recursive_find_and_replace.register(str)
def _(parameters: str, param_dict: dict[str, Any]) -> str | list[str]:
    """Recursively find and replace parameters from query."""
    param_regex: str = r"{([^}]+)}"
    matches: re.Match[str] | None = re.match(param_regex, parameters)
    if matches:
        result: list[str] = [
            param_dict.get(match, parameters) for match in matches.groups()
        ]
        if len(result) == 1:
            return result[0]
        return result
    return parameters
