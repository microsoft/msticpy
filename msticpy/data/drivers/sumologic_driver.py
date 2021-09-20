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

import pandas as pd
from requests.exceptions import ConnectionError as ConnError
from requests.exceptions import HTTPError
from sumologic.sumologic import SumoLogic

from ..._version import VERSION
from ...common.exceptions import (
    MsticpyConnectionError,
    MsticpyNotConnectedError,
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
        super().__init__()
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
        except ConnError as err:
            raise MsticpyConnectionError(
                f"Authentication error connecting to Sumologic: {err}",
                title="Sumologic connection",
                help_uri=_HELP_URI,
                nb_uri=_SL_NB_URI,
            ) from err
        except HTTPError as err:
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
        cs_dict.update(self._get_config_settings())
        # If a connection string - parse this and add to config
        if connection_str:
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
        kwargs :
            Are passed to Sumologic
            days: Search the past X days.
            start_time: A datetime() object representing the start of the search
                    window. If used without end_time, the end of the search
                    window is the current time.
            end_time: A datetime() object representing the end of the search window.
                  If used without start_time, the search start will be the earliest
                  time in the index.
            timezone: timezone used for time range search
            byreceipttime:  if time reference should used _receiptTime (time when Sumologic
                        got message) instead of _messageTime (time present in log message).
            limit: An integer describing the max number of search results to return.
            forcemessagesresults: Force results to be raw messages even if aggregated query.
            verbosity: Provide more verbose state. from 0 least verbose to 4 most one.
            checkinterval: interval in seconds to check if results are gathered
            timeout: timeout in seconds when gathering results

        Returns
        -------
        Union[pd.DataFrame, Any]
            Query results in a dataframe.
            or query response if an error.

        """
        del query_source
        if not self._connected:
            raise self._create_not_connected_err()

        verbosity = kwargs.pop("verbosity", 0)
        timezone = kwargs.pop("timezone", "UTC")
        by_receipt_time = kwargs.pop("byreceipttime", False)
        self.checkinterval = kwargs.pop("checkinterval", self._DEF_CHECKINTERVAL)
        self.timeout = kwargs.pop("timeout", self._DEF_TIMEOUT)

        start_time, end_time = self._get_time_params(**kwargs)

        # default to unlimited query unless count is specified
        if "limit" in kwargs:
            query = f"{query} | limit {kwargs['limit']}"
        limit = kwargs.pop("limit", 10000)

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
    # I don't think there are any - everything returns a list
    def _get_job_results(self, searchjob, status, qry_count, force_mssg_rstls, limit):
        if status["state"] != "DONE GATHERING RESULTS":
            return []
        if not qry_count or force_mssg_rstls:
            # Non-aggregated results, Messages only
            count = status["messageCount"]
            limit2 = (
                count if count < limit and count != 0 else limit
            )  # compensate bad limit check
            try:
                result = self.service.search_job_messages(searchjob, limit=limit2)
                return result["messages"]
            except Exception as err:
                self._raise_qry_except(
                    err, "search_job_messages", "to get job messages"
                )
        else:
            # Aggregated results
            count = status["recordCount"]
            limit2 = (
                count if count < limit and count != 0 else limit
            )  # compensate bad limit check
            try:
                result = self.service.search_job_records(searchjob, limit=limit2)
                return result["records"]
            except Exception as err:
                self._raise_qry_except(
                    err, "search_job_records", "to get search records"
                )

    # pylint: enable=inconsistent-return-statements

    @staticmethod
    def _raise_qry_except(err: Exception, mssg: str, action: Optional[str] = None):
        if isinstance(err, HTTPError):
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
            start = end - timedelta(days=int(kwargs["days"]))
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

    def query(
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
        kwargs :
            Are passed to Sumologic
            days: Search the past X days.
            start_time (or start): A datetime() object representing the start of the search
                    window. If used without end_time, the end of the search
                    window is the current time.
            end_time (or end): A datetime() object representing the end of the search window.
                  If used without start_time, the search start will be the earliest
                  time in the index.
            timeZone: timezone used for time range search
            byReceiptTime:  if time reference should used _receiptTime (time when Sumologic
                        got message) instead of _messageTime (time present in log message).
            limit: An integer describing the max number of search results to return.
            forceMessagesResults: Force results to be raw messages even if aggregated query.
            verbosity: Provide more verbose state. from 0 least verbose to 4 most one.
            normalize: If set to True, fields containing structures (i.e. subfields)
                   will be flattened such that each field has it's own column in
                   the dataframe. If False, there will be a single column for the
                   structure, with a JSON string encoding all the contents.
            exporting: Export result to file.
            export_path: file path for exporte results.

        Returns
        -------
        Union[pd.DataFrame, Any]
            Query results in a dataframe.
            or query response if an error.

        """
        verbosity = kwargs.get("verbosity", 0)
        normalize = kwargs.pop("normalize", True)
        exporting = kwargs.pop("exporting", False)
        export_path = kwargs.pop("export_path", "")

        results = self._query(query, **kwargs)
        if verbosity >= 3:
            print("DEBUG: {results}")
        if normalize:
            dataframe_res = pd.json_normalize(results)
        else:
            dataframe_res = pd.DataFrame(results)

        for col in dataframe_res.columns:
            if col in ("map._count", "map._timeslice"):
                dataframe_res[col] = pd.to_numeric(dataframe_res[col])

        if exporting:
            if export_path.endswith(".xlsx"):
                if verbosity >= 2:
                    print(f"DEBUG: Exporting results to excel file {export_path}")
                dataframe_res.to_excel(export_path, index=False)
            elif export_path.endswith(".csv"):
                if verbosity >= 2:
                    print("DEBUG: Exporting results to csv file {export_path}")
                dataframe_res.to_csv(export_path, index=False)

        return dataframe_res

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

    # Parameter Formatting methods
    @staticmethod
    def _format_datetime(date_time: datetime) -> str:
        """Return datetime-formatted string."""
        return date_time.strftime("%Y-%m-%dT%H:%M:%S")

    # Read values from configuration
    @staticmethod
    def _get_config_settings() -> Dict[Any, Any]:
        """Get config from msticpyconfig."""
        data_provs = get_provider_settings(config_section="DataProviders")
        sumologic_settings: Optional[ProviderSettings] = data_provs.get("Sumologic")
        return getattr(sumologic_settings, "args", {})

    @staticmethod
    def _create_not_connected_err():
        return MsticpyNotConnectedError(
            "Please run the connect() method before running this method.",
            title="not connected to Sumologic.",
            help_uri=_HELP_URI,
            notebook_uri=_SL_NB_URI,
        )
