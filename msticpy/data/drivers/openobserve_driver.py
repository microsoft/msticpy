#  -------------------------------------------------------------------------
#  Copyright (c) juju4. All rights reserved.
#  Licensed under the MIT License. See License.txt in the project root for
#  license information.
#  --------------------------------------------------------------------------
"""OpenObserve Driver class."""
from datetime import datetime, timedelta
from typing import Any, Dict, Optional, Tuple, Union

import httpx
import pandas as pd
from python_openobserve.openobserve import OpenObserve

from ..._version import VERSION
from ...common.exceptions import (
    MsticpyConnectionError,
    MsticpyUserConfigError,
    MsticpyUserError,
)
from ...common.provider_settings import ProviderSettings, get_provider_settings
from ...common.utility import check_kwargs, export
from .driver_base import DriverBase, DriverProps, QuerySource

__version__ = VERSION
__author__ = "juju4"


OPENOBSERVE_CONNECT_ARGS = {
    "connection_str": (
        "(string) The url endpoint (the default is" + " 'https://localhost:5080')."
    ),
    "user": "(string) OpenObserve user, which is used to authenticate.",
    "password": "(string) The matching OpenObserve password.",
    "verify": "(bool) Validate TLS certificate (default True).",
}

_HELP_URI = "https://msticpy.readthedocs.io/en/latest/DataProviders.html"
_SL_NB_URI = (
    "https://github.com/microsoft/msticpy/blob/pr/TODO/docs/"
    "notebooks/OpenObserve-DataConnector.ipynb"
)


@export
class OpenObserveDriver(DriverBase):
    """Driver to connect and query from OpenObserve."""

    _OPENOBSERVE_REQD_ARGS = ["connection_str", "user", "password", "verify"]
    _CONNECT_DEFAULTS: Dict[str, Any] = {
        "connection_str": "https://localhost:5080",
        "verify": True,
    }
    _TIME_FORMAT = '"%Y-%m-%d %H:%M:%S.%6N"'
    _DEF_TIMEOUT = 300

    def __init__(self, **kwargs):
        """Instantiate OpenObserve Driver."""
        super().__init__(**kwargs)
        self.service = None
        self._loaded = True
        self._connected = False
        self._debug = kwargs.get("debug", False)
        self.set_driver_property(DriverProps.PUBLIC_ATTRS, {"client": self.service})
        self.set_driver_property(
            DriverProps.FORMATTERS, {"datetime": self._format_datetime}
        )
        self.timeout = self._DEF_TIMEOUT

    def connect(self, connection_str: str = None, **kwargs):
        """
        Connect to OpenObserve via python-openobserve.

        Parameters
        ----------
        connection_str :
            OpenObserve API url endpoint. default: https://localhost:5080

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
            key: val for key, val in cs_dict.items() if key in OPENOBSERVE_CONNECT_ARGS
        }
        try:
            # https://github.com/JustinGuese/python-openobserve/blob/main/python_openobserve/openobserve.py#L22
            self.service = OpenObserve(
                host=arg_dict["connection_str"],
                user=arg_dict["user"],
                password=arg_dict["password"],
                verify=arg_dict["verify"],
            )
        except httpx.ConnectError as err:
            raise MsticpyConnectionError(
                f"Authentication error connecting to OpenObserve: {err}",
                title="OpenObserve connection",
                help_uri=_HELP_URI,
                nb_uri=_SL_NB_URI,
            ) from err
        except httpx.HTTPError as err:
            raise MsticpyConnectionError(
                f"Communication error connecting to OpenObserve: {err}",
                title="OpenObserve connection",
                help_uri=_HELP_URI,
                nb_uri=_SL_NB_URI,
            ) from err
        except Exception as err:
            raise MsticpyConnectionError(
                f"Error connecting to OpenObserve: {err}",
                title="OpenObserve connection",
                help_uri=_HELP_URI,
                nb_uri=_SL_NB_URI,
            ) from err
        self._connected = True
        print(f"connected with user {arg_dict['user']}")

    def _get_connect_args(
        self, connection_str: Optional[str], **kwargs
    ) -> Dict[str, Any]:
        """Check and consolidate connection parameters."""
        cs_dict: Dict[str, Any] = self._CONNECT_DEFAULTS
        # Fetch any config settings
        settings, cs_is_instance_name = self._get_openobserve_settings(connection_str)
        cs_dict.update(settings)
        # If a connection string - parse this and add to config
        if connection_str and not cs_is_instance_name:
            cs_dict["connection_str"] = connection_str
        if kwargs:
            # if connection args supplied as kwargs
            cs_dict.update(kwargs)
            check_kwargs(cs_dict, list(OPENOBSERVE_CONNECT_ARGS.keys()))

        missing_args = set(self._OPENOBSERVE_REQD_ARGS) - cs_dict.keys()
        if missing_args:
            raise MsticpyUserConfigError(
                "One or more connection parameters missing for OpenObserve connector",
                ", ".join(missing_args),
                f"Required parameters are {', '.join(self._OPENOBSERVE_REQD_ARGS)}",
                "All parameters:",
                *[f"{arg}: {desc}" for arg, desc in OPENOBSERVE_CONNECT_ARGS.items()],
                title="no OpenObserve connection parameters",
                help_uri=_HELP_URI,
                notebook_uri=_SL_NB_URI,
            )
        return cs_dict

    # pylint: disable=broad-except
    def _query(
        self, query: str, query_source: QuerySource = None, **kwargs
    ) -> Union[pd.DataFrame, Any]:
        """
        Execute OpenObserve query and retrieve results.

        Parameters
        ----------
        query : str
            OpenObserve query to execute
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
        limit : int
            An integer describing the max number of search results to return.
        verbosity : int
            Provide more verbose state. from 0 least verbose to 4 most one.
        timeout : int
            timeout in seconds when gathering results
        outformat: str
            output format. json, dataframe.

        Returns
        -------
        Union[pd.DataFrame, Any]
            Query results in a dataframe.
            or query response if an error.

        """
        del query_source
        if not self._connected:
            raise self._create_not_connected_err("OpenObserve")

        verbosity = kwargs.pop("verbosity", 0)
        timezone = kwargs.pop("timezone", "UTC")
        self.timeout = kwargs.pop("timeout", self._DEF_TIMEOUT)
        outformat = kwargs.pop("outformat", "df")

        start_time, end_time = self._get_time_params(**kwargs)

        if (
            "limit" in kwargs
            and kwargs["limit"] <= 10000
            and " limit " not in query.lower()
        ):
            limit = kwargs["limit"]
            query = f"{query} limit {limit}"
        else:
            limit = None

        if verbosity >= 1:
            print(f"INFO: from {start_time} to {end_time}, TZ {timezone}")

        if verbosity >= 2:
            print(f"DEBUG: query {query}")
        # submit the search job
        try:
            searchresults = self.service.search(
                query,
                start_time=start_time,
                end_time=end_time,
                verbosity=verbosity,
                outformat=outformat,
            )
        except Exception as err:
            self._raise_qry_except(err, "search_job", "to search job")

        # return the results
        return searchresults

    @staticmethod
    def _raise_qry_except(err: Exception, mssg: str, action: Optional[str] = None):
        if isinstance(err, httpx.HTTPError):
            raise MsticpyConnectionError(
                f"Communication error connecting to OpenObserve: {err}",
                title=f"OpenObserve {mssg}",
                help_uri=_HELP_URI,
                notebook_uri=_SL_NB_URI,
            ) from err
        action = action or mssg
        raise MsticpyConnectionError(
            f"Failed {action}: {err}",
            title=f"OpenObserve - {mssg}",
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
        Execute OpenObserve query and retrieve results.

        Parameters
        ----------
        query : str
            OpenObserve query to execute
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
        limit : int
            An integer describing the max number of search results to return.
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

        results = self._query(query, outformat="json", **kwargs)
        if verbosity >= 1:
            print("DEBUG: results shape {results.shape}")
        if verbosity >= 3:
            print("DEBUG: {results}")
        if normalize:
            dataframe_res = pd.json_normalize(results)  # type: ignore
        else:
            dataframe_res = pd.DataFrame(results)

        if limit is not None and dataframe_res.shape[0] > limit:
            dataframe_res = dataframe_res.head(limit)

        for col in dataframe_res.columns:
            try:
                if col in numeric_columns:
                    dataframe_res[col] = pd.to_numeric(dataframe_res[col])
                # ensure timestamp format
                if col in ["_timestamp"] + time_columns:
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
            Query to execute against OpenObserve instance.

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
        """
        Return datetime-formatted string.

        python-openobserve takes datetime or microseconds since epoch as input
        """
        return date_time

    # Read values from configuration
    @staticmethod
    def _get_openobserve_settings(
        instance_name: str = None,
    ) -> Tuple[Dict[str, Any], bool]:
        """Get config from msticpyconfig."""
        data_provs = get_provider_settings(config_section="DataProviders")
        sl_settings = {
            name: settings
            for name, settings in data_provs.items()
            if name.startswith("OpenObserve")
        }
        openobserve_settings: Optional[ProviderSettings]
        # Check if the connection string is an instance name
        openobserve_settings = sl_settings.get(f"OpenObserve-{instance_name}")
        if openobserve_settings:
            is_instance_name = True
        else:
            # otherwise get the default OpenObserve entry
            openobserve_settings = sl_settings.get("OpenObserve")
            is_instance_name = False
        return getattr(openobserve_settings, "args", {}), is_instance_name
