#  -------------------------------------------------------------------------
#  Copyright (c) Microsoft Corporation. All rights reserved.
#  Licensed under the MIT License. See License.txt in the project root for
#  license information.
#  --------------------------------------------------------------------------
"""Splunk Driver class."""
import logging
from datetime import datetime, timedelta
from time import sleep
from typing import Any, Dict, Iterable, Optional, Tuple, Union

import pandas as pd
from tqdm import tqdm

from ..._version import VERSION
from ...common.exceptions import (
    MsticpyConnectionError,
    MsticpyDataQueryError,
    MsticpyImportExtraError,
    MsticpyUserConfigError,
)
from ...common.utility import check_kwargs, export
from ..core.query_defns import Formatters
from .driver_base import DriverBase, DriverProps, QuerySource

try:
    import splunklib.client as sp_client
    import splunklib.results as sp_results
    from splunklib.client import AuthenticationError, HTTPError
except ImportError as imp_err:
    raise MsticpyImportExtraError(
        "Cannot use this feature without splunk-sdk installed",
        title="Error importing splunk-sdk",
        extra="splunk",
    ) from imp_err

__version__ = VERSION
__author__ = "Ashwin Patil"

logger = logging.getLogger(__name__)


SPLUNK_CONNECT_ARGS = {
    "host": "(string) The host name (the default is 'localhost').",
    "port": "(integer) The port number (the default is 8089).",
    "http_scheme": "('https' or 'http') The scheme for accessing the service "
    + "(the default is 'https').",
    "verify": "(Boolean) Enable (True) or disable (False) SSL verrification for "
    + "https connections. (optional, the default is True)",
    "owner": "(string) The owner context of the namespace (optional).",
    "app": "(string) The app context of the namespace (optional).",
    "sharing": "('global', 'system', 'app', or 'user') "
    + "The sharing mode for the namespace (the default is 'user').",
    "token": "(string) The current session token (optional). Session tokens can be"
    + " shared across multiple service instances.",
    "cookie": "(string) A session cookie. When provided, you don’t need to call"
    + " login(). This parameter is only supported for Splunk 6.2+.",
    "autologin": "(boolean) When True, automatically tries to log in again if"
    + " the session terminates.",
    "username": "(string) The Splunk account username, which is used to "
    + "authenticate the Splunk instance.",
    "password": "(string) The password for the Splunk account.",
}


@export
class SplunkDriver(DriverBase):
    """Driver to connect and query from Splunk."""

    _SPLUNK_REQD_ARGS = ["host", "username", "password"]
    _CONNECT_DEFAULTS: Dict[str, Any] = {"port": 8089}
    _TIME_FORMAT = '"%Y-%m-%d %H:%M:%S.%6N"'

    def __init__(self, **kwargs):
        """Instantiate Splunk Driver."""
        super().__init__(**kwargs)
        self.service = None
        self._loaded = True
        self._connected = False
        if kwargs.get("debug", False):
            logger.setLevel(logging.DEBUG)

        self.set_driver_property(
            DriverProps.PUBLIC_ATTRS,
            {
                "client": self.service,
                "saved_searches": self._saved_searches,
                "fired_alerts": self._fired_alerts,
            },
        )
        self.set_driver_property(
            DriverProps.FORMATTERS,
            {
                Formatters.DATETIME: self._format_datetime,
                Formatters.LIST: self._format_list,
            },
        )

    def connect(self, connection_str: Optional[str] = None, **kwargs):
        """
        Connect to Splunk via splunk-sdk.

        Parameters
        ----------
        connection_str : Optional[str], optional
            Connection string with Splunk connection parameters

        Other Parameters
        ----------------
        kwargs :
            Connection parameters can be supplied as keyword parameters.

        Notes
        -----
        Default configuration is read from the DataProviders/Splunk
        section of msticpyconfig.yaml, if available.

        """
        cs_dict = self._get_connect_args(connection_str, **kwargs)

        arg_dict = {
            key: val for key, val in cs_dict.items() if key in SPLUNK_CONNECT_ARGS
        }
        try:
            self.service = sp_client.connect(**arg_dict)
        except AuthenticationError as err:
            raise MsticpyConnectionError(
                f"Authentication error connecting to Splunk: {err}",
                title="Splunk connection",
                help_uri="https://msticpy.readthedocs.io/en/latest/DataProviders.html",
            ) from err
        except HTTPError as err:
            raise MsticpyConnectionError(
                f"Communication error connecting to Splunk: {err}",
                title="Splunk connection",
                help_uri="https://msticpy.readthedocs.io/en/latest/DataProviders.html",
            ) from err
        except Exception as err:
            raise MsticpyConnectionError(
                f"Error connecting to Splunk: {err}",
                title="Splunk connection",
                help_uri="https://msticpy.readthedocs.io/en/latest/DataProviders.html",
            ) from err
        self._connected = True
        print("connected")

    def _get_connect_args(
        self, connection_str: Optional[str], **kwargs
    ) -> Dict[str, Any]:
        """Check and consolidate connection parameters."""
        cs_dict: Dict[str, Any] = self._CONNECT_DEFAULTS
        # Fetch any config settings
        cs_dict.update(self._get_config_settings("Splunk"))
        # If a connection string - parse this and add to config
        if connection_str:
            cs_items = connection_str.split(";")
            cs_dict.update(
                {
                    cs_item.split("=")[0].strip(): cs_item.split("=")[1]
                    for cs_item in cs_items
                }
            )
        elif kwargs:
            # if connection args supplied as kwargs
            cs_dict.update(kwargs)
            check_kwargs(cs_dict, list(SPLUNK_CONNECT_ARGS.keys()))

        cs_dict["port"] = int(cs_dict["port"])
        verify_opt = cs_dict.get("verify")
        if isinstance(verify_opt, str):
            cs_dict["verify"] = "true" in verify_opt.casefold()
        elif isinstance(verify_opt, bool):
            cs_dict["verify"] = verify_opt

        missing_args = set(self._SPLUNK_REQD_ARGS) - cs_dict.keys()
        if missing_args:
            raise MsticpyUserConfigError(
                "One or more connection parameters missing for Splunk connector",
                ", ".join(missing_args),
                f"Required parameters are {', '.join(self._SPLUNK_REQD_ARGS)}",
                "All parameters:",
                *[f"{arg}: {desc}" for arg, desc in SPLUNK_CONNECT_ARGS.items()],
                title="no Splunk connection parameters",
            )
        return cs_dict

    def query(
        self, query: str, query_source: Optional[QuerySource] = None, **kwargs
    ) -> Union[pd.DataFrame, Any]:
        """
        Execute splunk query and retrieve results via OneShot or async search mode.

        Parameters
        ----------
        query : str
            Splunk query to execute via OneShot or async search mode
        query_source : QuerySource
            The query definition object

        Other Parameters
        ----------------
        count : int, optional
            Passed to Splunk job that indicates the maximum number
            of entities to return. A value of 0 indicates no maximum,
            by default, 0
        oneshot : bool, optional
            Set to True for oneshot (blocking) mode, by default False
        page_size = int, optional
            Pass to Splunk results reader in terms of fetch speed,
            which sets of result amount will be got at a time,
            by default, 100
        timeout : int, optional
            Amount of time to wait for results, by default 60

        Returns
        -------
        Union[pd.DataFrame, Any]
            Query results in a dataframe.
            or query response if an error.

        """
        del query_source
        if not self._connected:
            raise self._create_not_connected_err("Splunk")

        # default to unlimited query unless count is specified
        count = kwargs.pop("count", 0)

        # Get sets of N results at a time, N=100 by default
        page_size = kwargs.pop("page_size", 100)

        # default to 60s unless timeout is specified
        timeout = kwargs.pop("timeout", 60)

        # Normal (non-blocking) searches or oneshot (blocking) searches.
        # Defaults to Normal(non-blocking)

        # Oneshot is a blocking search that is scheduled to run immediately.
        # Instead of returning a search job, this mode returns the results
        # of the search once completed.
        # Because this is a blocking search, the results are not available
        # until the search has finished.
        # https://dev.splunk.com/enterprise/docs/python/
        # sdk-python/howtousesplunkpython/howtorunsearchespython
        is_oneshot = kwargs.get("oneshot", False)

        if is_oneshot is True:
            kwargs["output_mode"] = "json"
            query_results = self.service.jobs.oneshot(query, count=count, **kwargs)

            reader = sp_results.JSONResultsReader(  # pylint: disable=no-member
                query_results
            )  # due to DeprecationWarning of normal ResultsReader
            resp_rows = [row for row in reader if isinstance(row, dict)]
        else:
            # Set mode and initialize async job
            kwargs_normalsearch = {"exec_mode": "normal"}
            query_job = self.service.jobs.create(
                query, count=count, **kwargs_normalsearch
            )
            resp_rows, reader = self._exec_async_search(
                query_job, page_size, timeout=timeout
            )

        if len(resp_rows) == 0 or not resp_rows:
            print("Warning - query did not return any results.")
            return [row for row in reader if isinstance(row, sp_results.Message)]
        return pd.DataFrame(resp_rows)

    def query_with_results(self, query: str, **kwargs) -> Tuple[pd.DataFrame, Any]:
        """
        Execute query string and return DataFrame of results.

        Parameters
        ----------
        query : str
            Query to execute against splunk instance.

        Returns
        -------
        Union[pd.DataFrame,Any]
            A DataFrame (if successful) or
            the underlying provider result if an error occurs.

        """
        raise NotImplementedError(f"Not supported for {self.__class__.__name__}")

    @property
    def service_queries(self) -> Tuple[Dict[str, str], str]:
        """
        Return dynamic queries available on connection to service.

        Returns
        -------
        Tuple[Dict[str, str], str]
            Dictionary of query_name, query_text.
            Name of container to add queries to.

        """
        if not self.connected:
            raise self._create_not_connected_err("Splunk")
        if hasattr(self.service, "saved_searches") and self.service.saved_searches:
            queries = {
                search.name.strip().replace(" ", "_"): f"search {search['search']}"
                for search in self.service.saved_searches
            }
            return queries, "SavedSearches"
        return {}, "SavedSearches"

    @property
    def driver_queries(self) -> Iterable[Dict[str, Any]]:
        """
        Return dynamic queries available on connection to service.

        Returns
        -------
        Iterable[Dict[str, Any]]
            List of queries with properties: "name", "query", "container"
            and (optionally) "description"

        Raises
        ------
        MsticpyNotConnectedError
            If called before driver is connected.

        """
        if not self.connected:
            raise self._create_not_connected_err("Splunk")
        if hasattr(self.service, "saved_searches") and self.service.saved_searches:
            return [
                {
                    "name": search.name.strip().replace(" ", "_"),
                    "query": f"search {search['search']}",
                    "query_paths": "SavedSearches",
                    "description": "",
                }
                for search in self.service.saved_searches
            ]
        return []

    def _exec_async_search(self, query_job, page_size, timeout):
        """Execute an async search and return results."""
        # Initiate progress bar and start while loop, waiting for async query to complete
        progress_bar = tqdm(total=100, desc="Waiting Splunk job to complete")
        prev_progress = 0
        offset = 0  # Start at result 0
        start_time = datetime.now()
        end_time = start_time + timedelta(seconds=timeout)
        while True:
            while not query_job.is_ready():
                sleep(1)
            if self._retrieve_job_status(query_job, progress_bar, prev_progress):
                break
            if datetime.now() > end_time:
                raise MsticpyDataQueryError(
                    "Timeout waiting for Splunk query to complete",
                    f"Job completion reported {query_job['doneProgress']}",
                    title="Splunk query timeout",
                )
            sleep(1)
        # Update progress bar indicating job completion
        progress_bar.update(100)
        progress_bar.close()
        sleep(2)

        logger.info("Implicit parameter dump - 'page_size': %d", page_size)
        return self._retrieve_results(query_job, offset, page_size)

    @staticmethod
    def _retrieve_job_status(query_job, progress_bar, prev_progress):
        """Poll the status of a job and update the progress bar."""
        stats = {
            "is_done": query_job["isDone"],
            "done_progress": float(query_job["doneProgress"]) * 100,
            "scan_count": int(query_job["scanCount"]),
            "event_count": int(query_job["eventCount"]),
            "result_count": int(query_job["resultCount"]),
        }
        status = (
            "\r%(done_progress)03.1f%%   %(scan_count)d scanned   "
            "%(event_count)d matched   %(result_count)d results"
        ) % stats
        if prev_progress == 0:
            progress = stats["done_progress"]
        else:
            progress = stats["done_progress"] - prev_progress
        prev_progress = stats["done_progress"]
        progress_bar.update(progress)

        if stats["is_done"] == "1":
            logger.info(status)
            logger.info("Splunk job completed.")
            return True
        return False

    @staticmethod
    def _retrieve_results(query_job, offset, page_size):
        """Retrieve the results of a job, decode and return them."""
        # Retrieving all the results by paginate
        result_count = int(
            query_job["resultCount"]
        )  # Number of results this job returned

        resp_rows = []
        progress_bar_paginate = tqdm(
            total=result_count, desc="Waiting Splunk result to retrieve"
        )
        while offset < result_count:
            kwargs_paginate = {
                "count": page_size,
                "offset": offset,
                "output_mode": "json",
            }
            # Get the search results and display them
            search_results = query_job.results(**kwargs_paginate)
            # due to DeprecationWarning of normal ResultsReader
            reader = sp_results.JSONResultsReader(  # pylint: disable=no-member
                search_results
            )
            resp_rows.extend([row for row in reader if isinstance(row, dict)])
            progress_bar_paginate.update(page_size)
            offset += page_size
        # Update progress bar indicating fetch results
        progress_bar_paginate.update(result_count)
        progress_bar_paginate.close()
        logger.info("Retrieved %d results.", len(resp_rows))
        return resp_rows, reader

    @property
    def _saved_searches(self) -> Union[pd.DataFrame, Any]:
        """
        Return list of saved searches in dataframe.

        Returns
        -------
        pd.DataFrame
            Dataframe with list of saved searches with name and query columns.

        """
        return self._get_saved_searches() if self.connected else None

    def _get_saved_searches(self) -> Union[pd.DataFrame, Any]:
        # sourcery skip: class-extract-method
        """
        Return list of saved searches in dataframe.

        Returns
        -------
        pd.DataFrame
            Dataframe with list of saved searches with name and query columns.

        """
        if not self.connected:
            raise self._create_not_connected_err("Splunk")
        savedsearches = self.service.saved_searches

        out_df = pd.DataFrame(columns=["name", "query"])

        namelist = []
        querylist = []
        for savedsearch in savedsearches:
            namelist.append(savedsearch.name.replace(" ", "_"))
            querylist.append(savedsearch["search"])
        out_df["name"] = namelist
        out_df["query"] = querylist

        return out_df

    @property
    def _fired_alerts(self) -> Union[pd.DataFrame, Any]:
        """
        Return list of fired alerts in dataframe.

        Returns
        -------
        pd.DataFrame
            Dataframe with list of fired alerts with alert name and count columns.

        """
        return self._get_fired_alerts() if self.connected else None

    def _get_fired_alerts(self) -> Union[pd.DataFrame, Any]:
        """
        Return list of fired alerts in dataframe.

        Returns
        -------
        pd.DataFrame
            Dataframe with list of fired alerts with alert name and count columns.

        """
        if not self.connected:
            raise self._create_not_connected_err("Splunk")
        firedalerts = self.service.fired_alerts

        out_df = pd.DataFrame(columns=["name", "count"])

        alert_names = []
        alert_counts = []
        for alert in firedalerts:
            alert_names.append(alert.name)
            alert_counts.append(alert.count)
        out_df["name"] = alert_names
        out_df["count"] = alert_counts

        return out_df

    # Parameter Formatting methods
    @staticmethod
    def _format_datetime(date_time: datetime) -> str:
        """Return datetime-formatted string."""
        return f'"{date_time.isoformat(sep=" ")}"'

    @staticmethod
    def _format_list(param_list: Iterable[Any]) -> str:
        """Return formatted list parameter."""
        fmt_list = [f'"{item}"' for item in param_list]
        return ",".join(fmt_list)
