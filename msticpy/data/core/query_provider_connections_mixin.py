# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Query Provider additional connection methods."""
import asyncio
import logging
from concurrent.futures import ThreadPoolExecutor
from datetime import datetime
from functools import partial
from itertools import tee
from typing import Any, Dict, List, Optional, Protocol, Tuple, Union

import pandas as pd
from tqdm.auto import tqdm

from ..._version import VERSION
from ...common.exceptions import MsticpyDataQueryError
from ..drivers.driver_base import DriverBase, DriverProps
from .query_source import QuerySource

__version__ = VERSION
__author__ = "Ian Hellen"

logger = logging.getLogger(__name__)


# pylint: disable=too-few-public-methods, unnecessary-ellipsis
class QueryProviderProtocol(Protocol):
    """Protocol for required properties of QueryProvider class."""

    driver_class: Any
    _driver_kwargs: Dict[str, Any]
    _additional_connections: Dict[str, Any]
    _query_provider: DriverBase

    def exec_query(self, query: str, **kwargs) -> Union[pd.DataFrame, Any]:
        """Execute a query against the provider."""
        ...

    @staticmethod
    def _get_query_options(
        params: Dict[str, Any], kwargs: Dict[str, Any]
    ) -> Dict[str, Any]:
        ...


# pylint: disable=super-init-not-called
class QueryProviderConnectionsMixin(QueryProviderProtocol):
    """Mixin additional connection handling QueryProvider class."""

    def add_connection(
        self,
        connection_str: Optional[str] = None,
        alias: Optional[str] = None,
        **kwargs,
    ):
        """
        Add an additional connection for the query provider.

        Parameters
        ----------
        connection_str : Optional[str], optional
            Connection string for the provider, by default None
        alias : Optional[str], optional
            Alias to use for the connection, by default None

        Other Parameters
        ----------------
        kwargs : Dict[str, Any]
            Other connection parameters passed to the driver.

        Notes
        -----
        Some drivers may accept types other than strings for the
        `connection_str` parameter.

        """
        # create a new instance of the driver class
        new_driver = self.driver_class(**(self._driver_kwargs))
        # connect
        new_driver.connect(connection_str=connection_str, **kwargs)
        # add to collection
        driver_key = alias or str(len(self._additional_connections))
        self._additional_connections[driver_key] = new_driver

    def list_connections(self) -> List[str]:
        """
        Return a list of current connections.

        Returns
        -------
        List[str]
            The alias and connection string for each connection.

        """
        add_connections = [
            f"{alias}: {driver.current_connection}"
            for alias, driver in self._additional_connections.items()
        ]
        return [f"Default: {self._query_provider.current_connection}", *add_connections]

    # pylint: disable=too-many-locals
    def _exec_additional_connections(self, query, **kwargs) -> pd.DataFrame:
        """
        Return results of query run query against additional connections.

        Parameters
        ----------
        query : str
            The query to execute.
        progress: bool, optional
            Show progress bar, by default True
        retry_on_error: bool, optional
            Retry failed queries, by default False
        **kwargs : Dict[str, Any]
            Additional keyword arguments to pass to the query method.

        Returns
        -------
        pd.DataFrame
            The concatenated results of the query executed against all connections.

        Notes
        -----
        This method executes the specified query against all additional connections
        added to the query provider.
        If the driver supports threading or async execution, the per-connection
        queries are executed asynchronously.
        Otherwise, the queries are executed sequentially.

        """
        progress = kwargs.pop("progress", True)
        retry = kwargs.pop("retry_on_error", False)
        # Add the initial connection
        query_tasks = {
            self._query_provider.current_connection
            or "0": partial(
                self._query_provider.query,
                query,
                **kwargs,
            )
        }
        # add the additional connections
        query_tasks.update(
            {
                name: partial(connection.query, query, **kwargs)
                for name, connection in self._additional_connections.items()
            }
        )

        logger.info("Running queries for %s connections.", len(query_tasks))
        # Run the queries threaded if supported
        if self._query_provider.get_driver_property(DriverProps.SUPPORTS_THREADING):
            logger.info("Running threaded queries.")
            event_loop = _get_event_loop()
            return event_loop.run_until_complete(
                self._exec_queries_threaded(query_tasks, progress, retry)
            )

        # standard synchronous execution
        print(f"Running query for {len(self._additional_connections)} connections.")
        return self._exec_synchronous_queries(progress, query_tasks)

    def _exec_split_query(
        self,
        split_by: str,
        query_source: QuerySource,
        query_params: Dict[str, Any],
        **kwargs,
    ) -> Union[pd.DataFrame, str, None]:
        """
        Execute a query that is split into multiple queries.

        Parameters
        ----------
        split_by : str
            The time interval to split the query by.
        query_source : QuerySource
            The query to execute.
        query_params : Dict[str, Any]
            The parameters to pass to the query.

        Other Parameters
        ----------------
        debug: bool, optional
            Return queries to be executed rather than execute them, by default False
        progress: bool, optional
            Show progress bar, by default True
        retry_on_error: bool, optional
            Retry failed queries, by default False
        **kwargs : Dict[str, Any]
            Additional keyword arguments to pass to the query method.

        Returns
        -------
        pd.DataFrame
            The concatenated results of the query executed against all connections.

        Notes
        -----
        This method executes the time-chunks of the split query.
        If the driver supports threading or async execution, the sub-queries are
        executed asynchronously. Otherwise, the queries are executed sequentially.

        """
        start = query_params.pop("start", None)
        end = query_params.pop("end", None)
        progress = kwargs.pop("progress", True)
        retry = kwargs.pop("retry_on_error", False)
        debug = kwargs.pop("debug", False)
        if not (start or end):
            print("Cannot split a query with no 'start' and 'end' parameters")
            return None

        split_queries = self._create_split_queries(
            query_source=query_source,
            query_params=query_params,
            start=start,
            end=end,
            split_by=split_by,
        )
        if debug:
            return "\n\n".join(
                f"{start}-{end}\n{query}"
                for (start, end), query in split_queries.items()
            )

        query_tasks = self._create_split_query_tasks(
            query_source, query_params, split_queries, **kwargs
        )
        # Run the queries threaded if supported
        if self._query_provider.get_driver_property(DriverProps.SUPPORTS_THREADING):
            logger.info("Running threaded queries.")
            event_loop = _get_event_loop()
            return event_loop.run_until_complete(
                self._exec_queries_threaded(query_tasks, progress, retry)
            )

        # or revert to standard synchronous execution
        return self._exec_synchronous_queries(progress, query_tasks)

    def _create_split_query_tasks(
        self,
        query_source: QuerySource,
        query_params: Dict[str, Any],
        split_queries,
        **kwargs,
    ) -> Dict[str, partial]:
        """Return dictionary of partials to execute queries."""
        # Retrieve any query options passed (other than query params)
        query_options = self._get_query_options(query_params, kwargs)
        logger.info("query_options: %s", query_options)
        logger.info("kwargs: %s", kwargs)
        if "time_span" in query_options:
            del query_options["time_span"]
        return {
            f"{start}-{end}": partial(
                self.exec_query,
                query=query_str,
                query_source=query_source,
                time_span={"start": start, "end": end},
                **query_options,
            )
            for (start, end), query_str in split_queries.items()
        }

    @staticmethod
    def _exec_synchronous_queries(
        progress: bool, query_tasks: Dict[str, Any]
    ) -> pd.DataFrame:
        logger.info("Running queries sequentially.")
        results: List[pd.DataFrame] = []
        if progress:
            query_iter = tqdm(query_tasks.items(), unit="sub-queries", desc="Running")
        else:
            query_iter = query_tasks.items()
        for con_name, query_task in query_iter:
            try:
                results.append(query_task())
            except MsticpyDataQueryError:
                print(f"Query {con_name} failed.")
        return pd.concat(results)

    def _create_split_queries(
        self,
        query_source: QuerySource,
        query_params: Dict[str, Any],
        start: datetime,
        end: datetime,
        split_by: str,
    ) -> Dict[Tuple[datetime, datetime], str]:
        """Return separate queries for split time ranges."""
        try:
            split_delta = pd.Timedelta(split_by)
        except ValueError:
            split_delta = pd.Timedelta("1D")
        logger.info("Using split delta %s", split_delta)

        ranges = _calc_split_ranges(start, end, split_delta)

        split_queries = {
            (q_start, q_end): query_source.create_query(
                formatters=self._query_provider.formatters,
                start=q_start,
                end=q_end,
                **query_params,
            )
            for q_start, q_end in ranges
        }
        logger.info("Split query into %s chunks", len(split_queries))
        return split_queries

    async def _exec_queries_threaded(
        self,
        query_tasks: Dict[str, partial],
        progress: bool = True,
        retry: bool = False,
    ) -> pd.DataFrame:
        """Return results of multiple queries run as threaded tasks."""
        logger.info("Running threaded queries for %d connections.", len(query_tasks))

        event_loop = _get_event_loop()

        with ThreadPoolExecutor(
            max_workers=self._query_provider.get_driver_property(
                DriverProps.MAX_PARALLEL
            )
        ) as executor:
            # add the additional connections
            thread_tasks = {
                query_id: event_loop.run_in_executor(executor, query_func)
                for query_id, query_func in query_tasks.items()
            }
            results: List[pd.DataFrame] = []
            failed_tasks: Dict[str, asyncio.Future] = {}
            if progress:
                task_iter = tqdm(
                    asyncio.as_completed(thread_tasks.values()),
                    unit="sub-queries",
                    desc="Running",
                )
            else:
                task_iter = asyncio.as_completed(thread_tasks.values())
            ids_and_tasks = dict(zip(thread_tasks, task_iter))
            for query_id, thread_task in ids_and_tasks.items():
                try:
                    result = await thread_task
                    logger.info("Query task '%s' completed successfully.", query_id)
                    results.append(result)
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
                        results.append(result)
                    except Exception:  # pylint: disable=broad-except
                        logger.warning(
                            "Retried query task '%s' failed with exception",
                            query_id,
                            exc_info=True,
                        )
            # Sort the results by the order of the tasks
            results = [result for _, result in sorted(zip(thread_tasks, results))]

        return pd.concat(results, ignore_index=True)


def _get_event_loop() -> asyncio.AbstractEventLoop:
    """Return the current event loop, or create a new one."""
    try:
        loop = asyncio.get_running_loop()
    except RuntimeError:
        loop = asyncio.new_event_loop()
        asyncio.set_event_loop(loop)
    return loop


def _calc_split_ranges(start: datetime, end: datetime, split_delta: pd.Timedelta):
    """Return a list of time ranges split by `split_delta`."""
    # Use pandas date_range and split the result into 2 iterables
    s_ranges, e_ranges = tee(pd.date_range(start, end, freq=split_delta))
    next(e_ranges, None)  # skip to the next item in the 2nd iterable
    # Zip them together to get a list of (start, end) tuples of ranges
    # Note: we subtract 1 nanosecond from the 'end' value of each range so
    # to avoid getting duplicated records at the boundaries of the ranges.
    # Some providers don't have nanosecond granularity so we might
    # get duplicates in these cases
    ranges = [
        (s_time, e_time - pd.Timedelta("1ns"))
        for s_time, e_time in zip(s_ranges, e_ranges)
    ]

    # Since the generated time ranges are based on deltas from 'start'
    # we need to adjust the end time on the final range.
    # If the difference between the calculated last range end and
    # the query 'end' that the user requested is small (< 10% of a delta),
    # we just replace the last "end" time with our query end time.
    if (ranges[-1][1] - end) < (split_delta / 10):
        ranges[-1] = ranges[-1][0], end
    else:
        # otherwise append a new range starting after the last range
        # in ranges and ending in 'end"
        # note - we need to add back our subtracted 1 nanosecond
        ranges.append((ranges[-1][0] + pd.Timedelta("1ns"), end))

    return ranges
