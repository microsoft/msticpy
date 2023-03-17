#  -------------------------------------------------------------------------
#  Copyright (c) juju4. All rights reserved.
#  Licensed under the MIT License. See License.txt in the project root for
#  license information.
#  --------------------------------------------------------------------------
"""Sumologic Driver class."""
import re
import time
from datetime import datetime, timedelta
from timeit import default_timer as timer
from typing import Any, Dict, Optional, Tuple, Union

import httpx
import pandas as pd
from sumologic.sumologic import SumoLogic

from ..._version import VERSION
from ...common.exceptions import (
    MsticpyConnectionError,
    MsticpyUserConfigError,
    MsticpyUserError,
)
from ...common.provider_settings import ProviderSettings, get_provider_settings
from ...common.utility import check_kwargs, export
from .driver_base import DriverBase, QuerySource

__version__ = VERSION
__author__ = "juju4"


SUMOLOGIC_CONNECT_ARGS = {
    "connection_str": "(string) The url endpoint (the default is"
    + " 'https://api.us2.sumologic.com/api').",
    "accessid": "(string) The Sumologic accessid, which is used to "
    + "authenticate on Sumologic instance.",
    "accesskey": "(string) The matching Sumologic accesskey.",
}

_HELP_URI = "https://msticpy.readthedocs.io/en/latest/DataProviders.html"
_SL_NB_URI = (
    "https://github.com/microsoft/msticpy/blob/pr/116-sumologic"
    "-driver-2021-04-21/docs/notebooks/Sumologic-DataConnector.ipynb"
)


@export
class SumologicDriver(DriverBase):
    """Driver to connect and query from Sumologic."""

    _SUMOLOGIC_REQD_ARGS = ["connection_str", "accessid", "accesskey"]
    _CONNECT_DEFAULTS: Dict[str, Any] = {
        "connection_str": "https://api.us2.sumologic.com/api"
    }
    _TIME_FORMAT = '"%Y-%m-%d %H:%M:%S.%6N"'
    _DEF_CHECKINTERVAL = 3
    _DEF_TIMEOUT = 300

    def __init__(self, **kwargs):
        """Instantiate Sumologic Driver."""
        super().__init__(**kwargs)
        self.service = None
        self._loaded = True
        self._connected = False
        self._debug = kwargs.get("debug", False)
        self.public_attribs = {
            "client": self.service,
        }
        self.formatters = {"datetime": self._format_datetime}
        self.checkinterval = self._DEF_CHECKINTERVAL
        self.timeout = self._DEF_TIMEOUT

    def connect(self, connection_str: str = None, **kwargs):
        """
        Connect to Sumologic via sumologic-sdk.

        Parameters
        ----------
        connection_str :
            Sumologic API url endpoint. default: https://api.us2.sumologic.com/api

        Other Parameters
        ----------------
        kwargs :
            Connection parameters can be supplied as keyword parameters.

        Notes
        -----
        Default configuration is read from the DataProviders/Sumologic
        section of msticpyconfig.yaml, if available.

        """
        cs_dict = self._get_connect_args(connection_str, **kwargs)

        arg_dict = {
            key: val for key, val in cs_dict.items() if key in SUMOLOGIC_CONNECT_ARGS
        }
        try:
            # https://github.com/SumoLogic/sumologic-python-sdk/blob/master/scripts/search-job.py
            self.service = SumoLogic(
                accessId=arg_dict["accessid"],
                accessKey=arg_dict["accesskey"],
                endpoint=arg_dict["connection_str"],
            )
        except httpx.ConnectError as err:
            raise MsticpyConnectionError(
                f"Authentication error connecting to Sumologic: {err}",
                title="Sumologic connection",
                help_uri=_HELP_URI,
                nb_uri=_SL_NB_URI,
            ) from err
        except httpx.HTTPError as err:
            raise MsticpyConnectionError(
                f"Communication error connecting to Sumologic: {err}",
                title="Sumologic connection",
                help_uri=_HELP_URI,
                nb_uri=_SL_NB_URI,
            ) from err
        except Exception as err:
            raise MsticpyConnectionError(
                f"Error connecting to Sumologic: {err}",
                title="Sumologic connection",
                help_uri=_HELP_URI,
                nb_uri=_SL_NB_URI,
            ) from err
        self._connected = True
        print(f"connected with accessid {arg_dict['accessid']}")

    def _get_connect_args(
        self, connection_str: Optional[str], **kwargs
    ) -> Dict[str, Any]:
        """Check and consolidate connection parameters."""
        cs_dict: Dict[str, Any] = self._CONNECT_DEFAULTS
        # Fetch any config settings
        settings, cs_is_instance_name = self._get_sumologic_settings(connection_str)
        cs_dict.update(settings)
        # If a connection string - parse this and add to config
        if connection_str and not cs_is_instance_name:
            cs_dict["connection_str"] = connection_str
        if kwargs:
            # if connection args supplied as kwargs
            cs_dict.update(kwargs)
            check_kwargs(cs_dict, list(SUMOLOGIC_CONNECT_ARGS.keys()))

        missing_args = set(self._SUMOLOGIC_REQD_ARGS) - cs_dict.keys()
        if missing_args:
            raise MsticpyUserConfigError(
                "One or more connection parameters missing for Sumologic connector",
                ", ".join(missing_args),
                f"Required parameters are {', '.join(self._SUMOLOGIC_REQD_ARGS)}",
                "All parameters:",
                *[f"{arg}: {desc}" for arg, desc in SUMOLOGIC_CONNECT_ARGS.items()],
                title="no Sumologic connection parameters",
                help_uri=_HELP_URI,
                notebook_uri=_SL_NB_URI,
            )
        return cs_dict

    # pylint: disable=broad-except
    def _query(
        self, query: str, query_source: QuerySource = None, **kwargs
    ) -> Union[pd.DataFrame, Any]:
        """
        Execute Sumologic query and retrieve results.

        Parameters
        ----------
        query : str
            Sumologic query to execute
        query_source : QuerySource
            Not used.

        Other Parameters
        ----------------
        days : int
            Search the past X days.
        start : datetime
            A datetime() object representing the start of the search
            window. If used without end_time, the end of the search
            window is the current time.
        start_time : datetime
            alias for `start`
        end : datetime
            A datetime() object representing the end of the search window.
            If used without start_time, the search start will be the earliest
            time in the index.
        end_time:
            alias for `end`
        timezone : str
            timezone used for time range search
        byreceipttime : datetime
            if time reference should used _receiptTime (time when Sumologic
            got message) instead of _messageTime (time present in log message).
        limit : int
            An integer describing the max number of search results to return.
        forcemessagesresults : bool
            Force results to be raw messages even if aggregated query.
        verbosity : int
            Provide more verbose state. from 0 least verbose to 4 most one.
        checkinterval : int
            interval in seconds to check if results are gathered
        timeout : int
            timeout in seconds when gathering results

        Returns
        -------
        Union[pd.DataFrame, Any]
            Query results in a dataframe.
            or query response if an error.

        """
        del query_source
        if not self._connected:
            raise self._create_not_connected_err("SumoLogic")

        verbosity = kwargs.pop("verbosity", 0)
        timezone = kwargs.pop("timezone", "UTC")
        by_receipt_time = kwargs.pop("byreceipttime", False)
        self.checkinterval = kwargs.pop("checkinterval", self._DEF_CHECKINTERVAL)
        self.timeout = kwargs.pop("timeout", self._DEF_TIMEOUT)

        start_time, end_time = self._get_time_params(**kwargs)

        # default to unlimited query unless count is specified
        # https://help.sumologic.com/05Search/Search-Query-Language/Search-Operators/limit
        if "limit" in kwargs and kwargs["limit"] <= 10000:
            limit = kwargs["limit"]
            query = f"{query} | limit {limit}"
        else:
            limit = None

        if verbosity >= 1:
            print(f"INFO: from {start_time} to {end_time}, TZ {timezone}")

        if verbosity >= 2:
            print(f"DEBUG: query {query}")
            print(f"DEBUG: byreceipttime {by_receipt_time}")
            timer_start = timer()
        # submit the search job
        try:
            searchjob = self.service.search_job(
                query, start_time, end_time, timezone, by_receipt_time
            )
        except Exception as err:
            self._raise_qry_except(err, "submit search_job", "to submit search job")

        qry_count = re.search(r"\|\s*count", query, re.IGNORECASE)
        if verbosity >= 2:
            print(f"DEBUG: search job {searchjob}")
            print(f"DEBUG: search performance: {timer() - timer_start}")
            print(f"DEBUG: messages or records? {qry_count}")

        # poll for job completion
        status = self._poll_job_status(searchjob, verbosity)
        print(status["state"])

        # return the results
        return self._get_job_results(
            searchjob=searchjob,
            status=status,
            qry_count=qry_count,
            force_mssg_rstls=kwargs.pop("forcemessagesresults", False),
            limit=limit,
            verbosity=verbosity,
        )

    def _poll_job_status(self, searchjob, verbosity):
        status = self.service.search_job_status(searchjob)
        if verbosity >= 2:
            print(f"DEBUG: status {status}")
        time_counter = 0
        while status["state"] != "DONE GATHERING RESULTS":
            if status["state"] == "CANCELLED":
                break
            status = self.service.search_job_status(searchjob)
            if verbosity >= 4:
                print(
                    f"DEBUG: pending results, state {status['state']}.",
                    f"slept {time_counter}s. sleeping extra {self.checkinterval}s",
                    "until {self.timeout}s",
                )
            if time_counter < self.timeout:
                time.sleep(self.checkinterval)
                time_counter += self.checkinterval
            else:
                print(
                    f"WARN: wait more than timeout {self.timeout}. stopping. "
                    + "Use timeout argument to wait longer."
                )
                break
        return status

    # pylint: disable=inconsistent-return-statements
    def _get_job_results_messages(self, searchjob, status, limit):
        # Non-aggregated results, Messages only
        count = status["messageCount"]
        limit2 = None
        if limit is not None:
            limit2 = (
                count if count < limit and count != 0 else limit
            )  # compensate bad limit check
        try:
            result = self.service.search_job_messages(searchjob, limit=limit2)
            return result["messages"]
        except Exception as err:
            self._raise_qry_except(err, "search_job_messages", "to get job messages")

    def _get_job_results_records(  # noqa: MC0001
        self, searchjob, status, limit, verbosity
    ):
        # Aggregated results, limit
        count = status["recordCount"]
        limit2 = None
        if limit is not None:
            limit2 = (
                count if count < limit and count != 0 else limit
            )  # compensate bad limit check

            if count < limit:
                if verbosity >= 2:
                    print(f"DEBUG: No Paging, total count {count}, limit {limit}")
                try:
                    result = self.service.search_job_records(searchjob, limit=limit2)
                    return result["records"]
                except Exception as err:
                    self._raise_qry_except(
                        err, "search_job_records", "to get search records"
                    )
        else:
            # paging results
            # https://help.sumologic.com/APIs/Search-Job-API/About-the-Search-Job-API#query-parameters-2
            if verbosity >= 2:
                print(f"DEBUG: Paging, total count {count}, limit {limit}")
            try:
                job_limit = 10000
                iterations = int(count / job_limit) + (count % job_limit > 0)
                total_results = []
                for i in range(0, iterations):
                    if i == iterations:
                        job_limit2 = count - (iterations - 1) * job_limit
                    else:
                        job_limit2 = job_limit
                    if verbosity >= 2:
                        print(
                            f"DEBUG: Paging {i * job_limit} / {count}, limit {job_limit2}"
                        )
                    result = self.service.search_job_records(
                        searchjob, offset=(i * job_limit), limit=job_limit2
                    )
                    total_results.extend(result["records"])
                return total_results
            except Exception as err:
                self._raise_qry_except(
                    err,
                    "search_job_records",
                    f"to get search records (paging i {i*job_limit} / {count})",
                )

    # pylint: disable=inconsistent-return-statements
    # I don't think there are any - everything returns a list
    def _get_job_results(
        self, searchjob, status, qry_count, force_mssg_rstls, limit, verbosity
    ):
        if status["state"] != "DONE GATHERING RESULTS":
            return []
        if not qry_count or force_mssg_rstls:
            # Non-aggregated results, Messages only
            return self._get_job_results_messages(searchjob, status, limit)

        # Aggregated results, limit
        return self._get_job_results_records(searchjob, status, limit, verbosity)

    # pylint: enable=inconsistent-return-statements

    @staticmethod
    def _raise_qry_except(err: Exception, mssg: str, action: Optional[str] = None):
        if isinstance(err, httpx.HTTPError):
            raise MsticpyConnectionError(
                f"Communication error connecting to Sumologic: {err}",
                title=f"Sumologic {mssg}",
                help_uri=_HELP_URI,
                notebook_uri=_SL_NB_URI,
            ) from err
        action = action or mssg
        raise MsticpyConnectionError(
            f"Failed {action}: {err}",
            title=f"Sumologic - {mssg}",
            help_uri=_HELP_URI,
            notebook_uri=_SL_NB_URI,
        ) from err

    def _get_time_params(self, **kwargs):
        if "days" in kwargs:
            end = datetime.now()
            start = end - timedelta(days=kwargs["days"])
            return self._format_datetime(start), self._format_datetime(end)

        start = kwargs.pop("start", kwargs.pop("start_time", None))
        end = kwargs.pop("end", kwargs.pop("end_time", None))
        if start and not end:
            end = datetime.now()
        elif not start:
            raise MsticpyUserError(
                "Error! requires either 'days' or 'start' parameters",
                title="Missing parameter.",
                help_uri=_HELP_URI,
                notebook_uri=_SL_NB_URI,
            )
        return self._format_datetime(start), self._format_datetime(end)

    # pylint: disable=too-many-branches
    def query(  # noqa: MC0001
        self, query: str, query_source: QuerySource = None, **kwargs
    ) -> Union[pd.DataFrame, Any]:
        """
        Execute Sumologic query and retrieve results.

        Parameters
        ----------
        query : str
            Sumologic query to execute
        query_source : QuerySource
            Not used.

        Other Parameters
        ----------------
        days: int
            Search the past X days.
        start : datetime
            A datetime() object representing the start of the search
            window. If used without end_time, the end of the search
            window is the current time.
        start_time : datetime
            alias for `start`
        end : datetime
            A datetime() object representing the end of the search window.
            If used without start_time, the search start will be the earliest
            time in the index.
        end_time : datetime
            alias for `end`
        timeZone : str
            timezone used for time range search
        byReceiptTime : datetime
            if time reference should used _receiptTime (time when Sumologic
            got message) instead of _messageTime (time present in log message).
        limit : int
            An integer describing the max number of search results to return.
        forceMessagesResults : bool
            Force results to be raw messages even if aggregated query.
        verbosity : int
            Provide more verbose state. from 0 least verbose to 4 most one.
        normalize : bool
            If set to True, fields containing structures (i.e. subfields)
            will be flattened such that each field has it's own column in
            the dataframe. If False, there will be a single column for the
            structure, with a JSON string encoding all the contents.
        exporting : bool
            Export result to file.
        export_path : str
            file path for exporte results.
        time_columns: array[string]
            returning columns which format should be dataframe timestamp
        numeric_columns: array[string]
            returning columns which format should be dataframe numeric

        Returns
        -------
        Union[pd.DataFrame, Any]
            Query results in a dataframe.
            or query response if an error.

        """
        limit = kwargs.get("limit", None)
        verbosity = kwargs.get("verbosity", 0)
        normalize = kwargs.pop("normalize", True)
        exporting = kwargs.pop("exporting", False)
        export_path = kwargs.pop("export_path", "")
        time_columns = kwargs.pop("time_columns", [])
        numeric_columns = kwargs.pop("numeric_columns", [])

        results = self._query(query, **kwargs)
        if verbosity >= 3:
            print("DEBUG: {results}")
        if normalize:
            dataframe_res = pd.json_normalize(results)
        else:
            dataframe_res = pd.DataFrame(results)

        if limit is not None and dataframe_res.shape[0] > limit:
            dataframe_res = dataframe_res.head(limit)

        for col in dataframe_res.columns:
            try:
                if (
                    col
                    in ["map._count", "map._collectorid", "map._messageid", "map._size"]
                    + numeric_columns
                ):
                    dataframe_res[col] = pd.to_numeric(dataframe_res[col])
                # ensure timestamp format
                # https://help.sumologic.com/05Search/Get-Started-with-Search/Search-Basics/Built-in-Metadata
                # https://help.sumologic.com/05Search/Search-Query-Language/Search-Operators/timeslice
                if col in ("map._receipttime", "map._messagetime", "map._timeslice"):
                    dataframe_res[col] = pd.to_datetime(dataframe_res[col], unit="ms")
                if col in time_columns:
                    dataframe_res[col] = pd.to_datetime(dataframe_res[col])

            except Exception as err:
                self._raise_qry_except(
                    err,
                    "query",
                    f"query column type conversion: {col} -> {dataframe_res[col]}",
                )

        if exporting:
            if export_path.endswith(".xlsx"):
                if verbosity >= 2:
                    print(f"DEBUG: Exporting results to excel file {export_path}")
                dataframe_res.to_excel(export_path, index=False)
            elif export_path.endswith(".csv"):
                if verbosity >= 2:
                    print("DEBUG: Exporting results to csv file {export_path}")
                dataframe_res.to_csv(export_path, index=False)

        return dataframe_res.copy()

    def query_with_results(self, query: str, **kwargs) -> Tuple[pd.DataFrame, Any]:
        """
        Execute query string and return DataFrame of results.

        Parameters
        ----------
        query : str
            Query to execute against Sumologic instance.

        Returns
        -------
        Union[pd.DataFrame,Any]
            A DataFrame (if successful) or
            the underlying provider result if an error occurs.

        """
        raise NotImplementedError(f"Not supported for {self.__class__.__name__}")

    # Parameter Formatting methods
    @staticmethod
    def _format_datetime(date_time: datetime) -> str:
        """Return datetime-formatted string."""
        return date_time.strftime("%Y-%m-%dT%H:%M:%S")

    # Read values from configuration
    @staticmethod
    def _get_sumologic_settings(
        instance_name: str = None,
    ) -> Tuple[Dict[str, Any], bool]:
        """Get config from msticpyconfig."""
        data_provs = get_provider_settings(config_section="DataProviders")
        sl_settings = {
            name: settings
            for name, settings in data_provs.items()
            if name.startswith("Sumologic")
        }
        sumologic_settings: Optional[ProviderSettings]
        # Check if the connection string is an instance name
        sumologic_settings = sl_settings.get(f"Sumologic-{instance_name}")
        if sumologic_settings:
            is_instance_name = True
        else:
            # otherwise get the default Sumologic entry
            sumologic_settings = sl_settings.get("Sumologic")
            is_instance_name = False
        return getattr(sumologic_settings, "args", {}), is_instance_name
