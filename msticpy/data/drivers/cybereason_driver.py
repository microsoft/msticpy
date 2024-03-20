# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Cybereason Driver class."""
import datetime as dt
import json
import logging
import re
from asyncio import Future, as_completed
from concurrent.futures import ThreadPoolExecutor
from functools import partial, singledispatch
from typing import Any, Dict, List, Optional, Tuple, Union

import httpx
import pandas as pd
from tqdm.auto import tqdm

from ..._version import VERSION
from ...common.exceptions import MsticpyUserConfigError
from ...common.provider_settings import ProviderArgs, get_provider_settings
from ...common.utility import mp_ua_header
from ..core.query_defns import Formatters
from ..core.query_provider_connections_mixin import _get_event_loop
from .driver_base import DriverBase, DriverProps, QuerySource

__version__ = VERSION
__author__ = "Florian Bracq"

logger = logging.getLogger(__name__)

_HELP_URI = (
    "https://msticpy.readthedocs.io/en/latest/data_acquisition/DataProviders.html"
)


CybereasonSettings = Dict[str, Dict[str, Union[str, ProviderArgs]]]


# pylint: disable=too-many-instance-attributes
class CybereasonDriver(DriverBase):
    """Class to interact with Cybereason."""

    CONFIG_NAME = "Cybereason"

    _CONFIG_NAME_MAP = {
        "tenant_id": ("tenantid", "tenant_id"),
        "client_id": ("clientid", "client_id"),
        "client_secret": ("clientsecret", "client_secret"),
    }

    def __init__(self, **kwargs):
        """
        Instantiate Cybereason driver.

        Additional Parameters
        ---------------------
        timeout : int
            Query timeout in seconds. Defaults to 2min
        max_results : int
            Number of total results to return. Defaults to 1000
            Max is 10,000.
        page_size : int
            Number of results to return per page. Defaults to 100

        Returns
        -------
        Union[pd.DataFrame, Any]
            A DataFrame (if successfull) or
            the underlying provider result if an error.

        """
        super().__init__(**kwargs)
        timeout = kwargs.get("timeout", 120)  # 2 minutes in milliseconds
        logger.debug("Set timeout to %d", timeout)
        max_results = min(kwargs.get("max_results", 100000), 100000)
        logger.debug("Set maximum results to %d", max_results)
        self.base_url: str = "https://{tenant_id}.cybereason.net"
        self.auth_endpoint: str = "/login.html"
        self.req_body: Dict[str, Any] = {
            "queryPath": [],
            "totalResultLimit": max_results,
            "perGroupLimit": 100,
            "perFeatureLimit": 100,
            "templateContext": "SPECIFIC",
            "queryTimeout": timeout * 1000,
            "customFields": [],
        }
        self.search_endpoint: str = "/rest/visualsearch/query/simple"
        self._loaded = True
        self.client = httpx.Client(
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
            DriverProps.MAX_PARALLEL, value=kwargs.get("max_threads", 4)
        )
        self._debug = kwargs.get("debug", False)

    def query(
        self, query: str, query_source: QuerySource = None, **kwargs
    ) -> Union[pd.DataFrame, Any]:
        """
        Execute query string and return DataFrame of results.

        Parameters
        ----------
        query : str
            The query to execute
        query_source : QuerySource
            The query definition object

        Returns
        -------
        Union[pd.DataFrame, Any]
            A DataFrame (if successfull) or
            the underlying provider result if an error.

        """
        del query_source
        if not self._connected:
            raise self._create_not_connected_err(self.__class__.__name__)

        page_size = min(kwargs.get("page_size", 2000), 4000)
        logger.debug("Set page size to %d", page_size)
        json_query = json.loads(query)
        body = {**self.req_body, **json_query}

        # The query must be executed at least once to retrieve the number
        # of results and the pagination token.
        response = self.__execute_query(body, page_size=page_size)

        total_results = response["data"]["totalResults"]
        pagination_token = response["data"]["paginationToken"]
        results: Dict[str, Any] = response["data"]["resultIdToElementDataMap"]

        logger.debug("Retrieved %d/%d results", len(results), total_results)

        df_result: pd.DataFrame = None  # type: ignore

        if len(results) < total_results:
            df_result = self._exec_paginated_queries(
                body=body,
                page_size=page_size,
                pagination_token=pagination_token,
                total_results=total_results,
            )
        else:
            df_result = self._format_result_to_dataframe(result=response)

        return df_result

    def _exec_paginated_queries(
        self,
        body: Dict[str, Any],
        page_size: int,
        pagination_token: str,
        total_results: int,
        **kwargs,
    ) -> pd.DataFrame:
        """
        Return results of paginated queries.

        Parameters
        ----------
        body : Dict[str, Any]
            The body of the query to execute.

        Additional Parameters
        ----------
        progress: bool, optional
            Show progress bar, by default True
        retry_on_error: bool, optional
            Retry failed queries, by default False
        **kwargs : Dict[str, Any]
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
        progress = kwargs.pop("progress", True)
        retry = kwargs.pop("retry_on_error", False)

        query_tasks = self._create_paginated_query_tasks(
            body=body,
            page_size=page_size,
            pagination_token=pagination_token,
            total_results=total_results,
        )

        logger.info("Running %s paginated queries.", len(query_tasks))
        event_loop = _get_event_loop()
        return event_loop.run_until_complete(
            self.__run_threaded_queries(query_tasks, progress, retry)
        )

    def connect(
        self,
        connection_str: Optional[str] = None,
        **kwargs,
    ):
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

        Notes
        -----
        Connection string fields:
            instance
            client_id
            client_secret

        """
        cs_dict: Dict[str, Any] = {}

        self._instance = kwargs.pop("instance", None)
        cs_dict = CybereasonDriver._get_driver_settings(
            self.CONFIG_NAME, self._instance
        )
        # let user override config settings with function kwargs
        cs_dict.update(kwargs)

        missing_settings = [
            setting
            for setting in ("tenant_id", "client_id", "client_secret")
            if setting not in cs_dict
        ]
        if missing_settings:
            raise MsticpyUserConfigError(
                "You must supply the following required connection parameter(s)",
                "to the connect function or add them to your msticpyconfig.yaml.",
                ", ".join(f"'{param}'" for param in missing_settings),
                title="Missing connection parameters.",
                help_uri=("Connecting to OData sources.", _HELP_URI),
            )

        # self.auth_endpoint and self.req_body are correctly set in concrete
        # instances __init__
        self.client.base_url = httpx.URL(
            self.base_url.format(tenant_id=cs_dict["tenant_id"])
        )
        req_body: Dict[str, str] = {
            "username": cs_dict["client_id"],
            "password": cs_dict["client_secret"],
        }

        # Authenticate and obtain cookie for future calls
        response = self.client.post(self.auth_endpoint, data=req_body)
        response.raise_for_status()

        print("Connected.")
        self._connected = True
        self.current_connection = self._instance

        return self._connected

    @staticmethod
    def _flatten_result(entry: Dict[str, Any]) -> Dict[str, Any]:
        """
        Flatten Cybereason result to a format that can be handled by pandas.

        Parameters
        ----------
        entry: Dict[str, Any]
            Entry to flatten

        Returns
        -------
        Dict[str, Any]

        """
        result = {}
        # Retrieve simpleValues and add them to the output
        simple_values: Dict[str, Any] = entry.get("simpleValues", {})
        result = CybereasonDriver._flatten_simple_values(simple_values)

        elt_value = entry.get("elementValues", {})  # List or Dict
        result.update(**CybereasonDriver._flatten_element_values(elt_value))
        return result

    @staticmethod
    def _flatten_simple_values(simple_values: Dict[str, Any]) -> Dict[str, Any]:
        """
        Flatten "simpleValues from Cybereason result to a format that can be handled by pandas.

        Parameters
        ----------
        simple_values: Dict[str, Any]
            Entry to flatten

        Returns
        -------
        Dict[str, Any]

        """
        result: Dict[str, Any] = {}
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
                }
            )
            if values["totalValues"] == 1:
                result[name] = result[name][0]

        return result

    @staticmethod
    def _flatten_element_values(
        element_values: Union[Dict[str, Any], List[Dict[str, Any]]]
    ) -> Dict[str, Any]:
        """
        Flatten "elementValues from Cybereason result to a format that can be handled by pandas.

        Parameters
        ----------
        element_values: Union[Dict[str, Any], List[str]]
            Entry to flatten

        Returns
        -------
        Dict[str, Any]

        """
        result = {}
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
        self,
        body: Dict[str, Any],
        page_size: int,
        pagination_token: str,
        total_results: int,
    ) -> Dict[str, partial]:
        """Return dictionary of partials to execute queries."""
        # Compute the number of queries to execute
        total_pages = total_results // page_size + 1
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
            for page in range(0, total_pages)
        }

    def __execute_query(
        self,
        body: Dict[str, Any],
        page: int = 0,
        page_size: int = 2000,
        pagination_token: str = None,
    ) -> Dict[str, Any]:
        """
        Run query with pagination enabled.

        Parameters
        ----------
        body: Dict[str, Any]
            Body of the HTTP Request
        page_size: int
            Size of the page for results
        page: int
            Page number to query
        pagination_token: str
            Token of the current search

        Returns
        -------
        Dict[str, Any]

        """
        if pagination_token:
            pagination = {
                "pagination": {
                    "pageSize": page_size,
                    "page": page + 1,
                    "paginationToken": pagination_token,
                    "skip": page * page_size,
                }
            }
            headers = {"Pagination-Token": pagination_token}
        else:
            pagination = {"pagination": {"pageSize": page_size}}
            headers = {}
        params = {"page": page, "itemsPerPage": page_size}
        status = None
        while status != "SUCCESS":
            response = self.client.post(
                self.search_endpoint,
                json={**body, **pagination},
                headers=headers,
                params=params,
            )
            response.raise_for_status()
            json_result = response.json()
            status = json_result["status"]
        return json_result

    async def __run_threaded_queries(
        self,
        query_tasks: Dict[str, partial],
        progress: bool = True,
        retry: bool = False,
    ) -> pd.DataFrame:
        logger.info("Running %d threaded queries.", len(query_tasks))
        event_loop = _get_event_loop()
        with ThreadPoolExecutor(max_workers=4) as executor:
            results: List[pd.DataFrame] = []
            failed_tasks: Dict[str, Future] = {}
            thread_tasks = {
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
            ids_and_tasks = dict(zip(thread_tasks, task_iter))
            for query_id, thread_task in ids_and_tasks.items():
                try:
                    result = await thread_task
                    df_result = self._format_result_to_dataframe(result)
                    logger.info("Query task '%s' completed successfully.", query_id)
                    results.append(df_result)
                except Exception:  # pylint: disable=broad-except
                    logger.warning(
                        "Query task '%s' failed with exception", query_id, exc_info=True
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
    def query_with_results(self, query: str, **kwargs) -> Tuple[pd.DataFrame, Any]:
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
        raise NotImplementedError(f"Not supported for {self.__class__.__name__}")

    # Parameter Formatting method
    @staticmethod
    def _format_datetime(date_time: dt.datetime) -> int:
        """Return datetime formatted as timestamp in milliseconds."""
        return int(date_time.timestamp() * 1000)

    @staticmethod
    def _format_list(value: List) -> List:
        """Return list as itself."""
        return value

    # Parameter Formatting method
    @staticmethod
    def _format_to_datetime(timestamp: int) -> Union[dt.datetime, int]:
        """Return datetime from a timestamp in milliseconds."""
        try:
            return dt.datetime.fromtimestamp(timestamp // 1000)
        except TypeError:
            return timestamp

    @staticmethod
    def _format_result_to_dataframe(result: Dict[str, Any]) -> pd.DataFrame:
        """Return a dataframe from a cybereason result object."""
        df_result = [
            dict(
                CybereasonDriver._flatten_result(values),
                **{"resultId": result_id},
            )
            for result_id, values in result["data"]["resultIdToElementDataMap"].items()
        ]
        return pd.json_normalize(df_result)

    # Retrieve configuration parameters with aliases
    @staticmethod
    def _map_config_dict_name(config_dict: Dict[str, str]):
        """Map configuration parameter names to expected values."""
        mapped_dict = config_dict.copy()
        for provided_name in config_dict:
            for req_name, alternates in CybereasonDriver._CONFIG_NAME_MAP.items():
                if provided_name.casefold() in alternates:
                    mapped_dict[req_name] = config_dict[provided_name]
                    break
        return mapped_dict

    # Read values from configuration
    @staticmethod
    def _get_driver_settings(
        config_name: str, instance: Optional[str] = None
    ) -> Dict[str, str]:
        """Try to retrieve config settings for Cybereason drivers."""
        config_key = f"{config_name}-{instance}" if instance else config_name
        drv_config = get_provider_settings("DataProviders").get(config_key)
        app_config: Dict[str, str] = {}
        if drv_config:
            app_config = dict(drv_config.args)

        if not app_config:
            return {}
        # map names to allow for different spellings
        return CybereasonDriver._map_config_dict_name(app_config)

    @staticmethod
    def _custom_param_handler(query: str, param_dict: Dict[str, Any]) -> str:
        """Replace parameters in query template for Cybereason JSON queries."""
        query_dict = json.loads(query)

        return json.dumps(_recursive_find_and_replace(query_dict, param_dict))


@singledispatch
def _recursive_find_and_replace(
    parameters: Union[str, Dict, List], param_dict: Dict[str, Any]
):
    """Recursively find and replace parameters from query."""
    if isinstance(parameters, (list, str, dict)):
        return _recursive_find_and_replace(parameters, param_dict)
    return parameters


@_recursive_find_and_replace.register(dict)
def _(parameters: Dict[str, Any], param_dict: Dict[str, Any]):
    return {
        parameter: _recursive_find_and_replace(value, param_dict)
        for parameter, value in parameters.items()
    }


@_recursive_find_and_replace.register(list)
def _(parameters: List, param_dict: Dict[str, Any]):
    result = [
        _recursive_find_and_replace(parameter, param_dict) for parameter in parameters
    ]
    if all(isinstance(values, list) for values in result):
        try:
            return sorted({value for values in result for value in values})
        except TypeError:
            # If we have a list with different types,convert all to string.
            return sorted({str(value) for values in result for value in values})
    return result


@_recursive_find_and_replace.register(str)
def _(parameters: str, param_dict: Dict[str, Any]):
    """Recursively find and replace parameters from query."""
    param_regex = r"{([^}]+)}"
    matches = re.match(param_regex, parameters)
    if matches:
        result = [param_dict.get(match, parameters) for match in matches.groups()]
        if len(result) == 1:
            return result[0]
        return result
    return parameters
