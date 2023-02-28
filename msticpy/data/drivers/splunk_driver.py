#  -------------------------------------------------------------------------
#  Copyright (c) Microsoft Corporation. All rights reserved.
#  Licensed under the MIT License. See License.txt in the project root for
#  license information.
#  --------------------------------------------------------------------------
"""Splunk Driver class."""
import io
import json
import ssl
import urllib
from datetime import datetime
from time import sleep
from typing import Any, Dict, Iterable, Optional, Tuple, Union

import pandas as pd
from alive_progress import alive_bar

from ..._version import VERSION
from ...common.exceptions import (
    MsticpyConnectionError,
    MsticpyImportExtraError,
    MsticpyUserConfigError,
)
from ...common.utility import check_kwargs, export
from ..core.query_defns import Formatters
from .driver_base import DriverBase, QuerySource

try:
    import splunklib.client as sp_client
    from splunklib.client import AuthenticationError, HTTPError
except ImportError as imp_err:
    raise MsticpyImportExtraError(
        "Cannot use this feature without splunk-sdk installed",
        title="Error importing splunk-sdk",
        extra="splunk",
    ) from imp_err

__version__ = VERSION
__author__ = "Ashwin Patil, Alex Muratov"

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

PROXY_CONNECT_ARGS = {
    "proxy_host": "(string) The Web Proxy host name (optional)",
    "proxy_port": "(string) The Web Proxy host name (optional)",
    "proxy_username": "(string) The Web Proxy account username, which is used to "
    + "authenticate the user instance. (optional)",
    "proxy_password": "(string) The password for the Web Proxy account. (optional)",
}


def request_handler(url, message, **kwargs):
    # pylint: disable=unused-argument
    # pylint: disable=bad-except-order
    # pylint: disable=bad-except-order
    # pylint: disable=protected-access
    """
    Request handler for urllib module.

    Parameters
    ----------
    url : url string
    message : HTTP message

    Other Parameters
    ----------------
    kwargs :
        Connection parameters can be supplied as keyword parameters.

    """
    method = message["method"].lower()
    data = message.get("body", "") if method == "post" else None
    headers = dict(message.get("headers", []))
    req = urllib.request.Request(url, data, headers)

    try:
        with urllib.request.urlopen(req) as response:
            return {
                "status": response.code,
                "reason": response.msg,
                "headers": dict(response.info()),
                "body": io.BytesIO(response.read()),
            }

    except urllib.error.URLError:
        with urllib.request.urlopen(
            req, context=ssl._create_unverified_context()
        ) as response:
            return {
                "status": response.code,
                "reason": response.msg,
                "headers": dict(response.info()),
                "body": io.BytesIO(response.read()),
            }

    except urllib.error.HTTPError as error:
        raise error  # Propagate HTTP errors via the returned response message


# the Proxy Handler proc to intercept urrlib requests
def handler(proxy):
    """
    HTTP handler for a proxy wrapper.

    Parameters
    ----------
    proxy : Optional[str]
        Connection string with proxy connection parameters
    """
    urllib.request.HTTPHandler(debuglevel=1)
    proxy_handler = urllib.request.ProxyHandler({"http": proxy, "https": proxy})

    opener = urllib.request.build_opener(proxy_handler)
    urllib.request.install_opener(opener)
    return request_handler


@export
class SplunkDriver(DriverBase):
    """Driver to connect and query from Splunk."""

    _SPLUNK_REQD_ARGS = ["host"]
    _CONNECT_DEFAULTS: Dict[str, Any] = {"port": 8089}
    _TIME_FORMAT = '"%Y-%m-%d %H:%M:%S.%6N"'

    def __init__(self, **kwargs):
        """Instantiate Splunk Driver."""
        super().__init__(**kwargs)
        self.service = None
        self._loaded = True
        self._connected = False
        self._debug = kwargs.get("debug", False)
        self.public_attribs = {
            "client": self.service,
            "saved_searches": self._saved_searches,
            "fired_alerts": self._fired_alerts,
        }
        self.formatters = {
            Formatters.DATETIME: self._format_datetime,
            Formatters.LIST: self._format_list,
        }

    def connect(self, connection_str: str = None, **kwargs):
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

        # add proxy handler if required
        proxy_dict = {
            key: val for key, val in cs_dict.items() if key in PROXY_CONNECT_ARGS
        }

        # check is there are proxy args and hook the proxy handler proc
        if len(proxy_dict) > 0:
            proxy_username = proxy_dict["proxy_username"]
            proxy_password = proxy_dict["proxy_password"]
            proxy_host = proxy_dict["proxy_host"]
            proxy_port = proxy_dict["proxy_port"]

            proxy_target = (
                f"http://{proxy_username}:{proxy_password}@{proxy_host}:{proxy_port}"
            )

            # add a new proxy handler object
            arg_dict["handler"] = handler(proxy_target)

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
        self, query: str, query_source: QuerySource = None, **kwargs
    ) -> Union[pd.DataFrame, Any]:
        # pylint: disable=not-callable
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
            Passed to Splunk oneshot method if `oneshot` is True, by default, 0

        Returns
        -------
        Union[pd.DataFrame, Any]
            Query results in a dataframe.
            or query response if an error.

        """
        del query_source
        if not self._connected:
            raise self._create_not_connected_err("Splunk")

        # create progress bar
        with alive_bar(
            total=100, title="Searching...", force_tty=True, manual=True
        ) as pbar:
            # prepare to run a search query
            kwargs_normalsearch = {"exec_mode": "normal", **kwargs}
            splunk_search_job = self.service.jobs.create(query, **kwargs_normalsearch)

            # A normal search returns the job's SID right away, so we need to poll for completion
            while True:
                while not splunk_search_job.is_ready():
                    sleep(1)
                    continue

                bar_progress = int(float(splunk_search_job["doneProgress"]) * 100)
                if bar_progress != 100:
                    pbar(bar_progress / 100)

                if splunk_search_job["isDone"] == "1":
                    pbar(1)
                    break
                sleep(1)

            # ----------------------
            search_results_json = []
            get_offset = 0
            max_get = 49000

            result_count = int(splunk_search_job["resultCount"])

            nrounds = result_count // max_get
            if result_count % max_get > 0:
                nrounds += 1

            pbar.total = 0
            pbar.manual = False
            pbar.title = "Retrieving data..."

            for i in range(nrounds):  # (get_offset < result_count):
                job_results = splunk_search_job.results(
                    **{"count": max_get, "offset": get_offset, "output_mode": "json"}
                )
                search_results_json.extend(json.loads(job_results.read())["results"])
                get_offset += max_get
                pbar((i + 1) / nrounds)

        return pd.DataFrame(search_results_json)

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
