# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""KQL Driver class."""
from datetime import datetime
import re
from typing import Tuple, Union, Any, Dict, Optional, Iterable

import json
import pandas as pd
from IPython import get_ipython
from adal.adal_error import AdalError
from Kqlmagic.kql_response import KqlError
from Kqlmagic.kql_engine import KqlEngineError
from Kqlmagic.my_aad_helper import AuthenticationError

from .driver_base import DriverBase, QuerySource
from ...common.exceptions import (
    MsticpyNoDataSourceError,
    MsticpyNotConnectedError,
    MsticpyKqlConnectionError,
)
from ...common.utility import export
from ..._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"


@export
class KqlDriver(DriverBase):
    """KqlDriver class to execute kql queries."""

    def __init__(self, connection_str: str = None, **kwargs):
        """
        Instantiaite KqlDriver and optionally connect.

        Parameters
        ----------
        connection_str : str, optional
            Connection string

        """
        self._ip = get_ipython()
        self._debug = kwargs.get("debug", False)
        super().__init__()

        self.formatters = {"datetime": self._format_datetime, "list": self._format_list}
        self._loaded = self._is_kqlmagic_loaded()

        if not self._loaded:
            self._load_kql_magic()

        self._schema: Dict[str, Any] = {}

        if connection_str:
            self.current_connection = connection_str
            self.connect(connection_str)

    # pylint: disable=too-many-branches
    def connect(self, connection_str: Optional[str] = None, **kwargs):
        """
        Connect to data source.

        Parameters
        ----------
        connection_str : str
            Connect to a data source

        """
        if not connection_str:
            raise MsticpyKqlConnectionError(
                "A connection string is needed to connect to Azure Sentinel.",
                title="no connection string",
            )
        self.current_connection = connection_str
        kql_err_setting = self._get_kql_option("Kqlmagic.short_errors")
        self._connected = False
        try:
            self._set_kql_option("Kqlmagic.short_errors", False)
            if self._ip is not None:
                try:
                    self._ip.run_cell_magic("kql", line="", cell=connection_str)
                except KqlError as ex:
                    self._raise_kql_error(ex)
                except KqlEngineError as ex:
                    self._raise_kql_engine_error(ex)
                except AdalError as ex:
                    self._raise_adal_error(ex)
                except AuthenticationError as ex:
                    self._raise_authn_error(ex)
                self._connected = True
                self._schema = self._get_schema()
            else:
                print(f"Could not connect to kql query provider for {connection_str}")
            return self._connected
        finally:
            self._set_kql_option("Kqlmagic.short_errors", kql_err_setting)

    # pylint: disable=too-many-branches

    @property
    def schema(self) -> Dict[str, Dict]:
        """
        Return current data schema of connection.

        Returns
        -------
        Dict[str, Dict]
            Data schema of current connection.

        """
        return self._schema

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
        Union[pd.DataFrame, results.ResultSet]
            A DataFrame (if successfull) or
            the underlying provider result if an error.

        """
        del kwargs
        if query_source:
            try:
                table = query_source["args.table"]
            except KeyError:
                table = None
            if table:
                if " " in table.strip():
                    table = table.strip().split(" ")[0]
                if table not in self.schema:
                    raise MsticpyNoDataSourceError(
                        f"The table {table} for this query is not in your workspace",
                        " schema. Please check your workspace",
                        title=f"{table} not found.",
                    )
        data, result = self.query_with_results(query)
        return data if data is not None else result

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
        # connect or switch the connection if our connection string
        # is not the current KqlMagic connection.
        if not self.connected and self.current_connection:
            self.connect(self.current_connection)
        if not self.connected:
            raise MsticpyNotConnectedError(
                "Please run the connect() method before running a query.",
                title="not connected to a workspace.",
                help_uri=MsticpyKqlConnectionError.DEF_HELP_URI,
            )

        if self._debug:
            print(query)

        # save current auto_dataframe setting so that we can set to false
        # and restore current setting
        auto_dataframe = self._get_kql_option(option="Kqlmagic.auto_dataframe")
        self._set_kql_option(option="Kqlmagic.auto_dataframe", value=False)
        # run the query (append semicolon to prevent default output)
        if not query.strip().endswith(";"):
            query = f"{query}\n;"

        result = self._ip.run_cell_magic("kql", line="", cell=query)
        self._set_kql_option(option="Kqlmagic.auto_dataframe", value=auto_dataframe)
        if result is not None:
            if isinstance(result, pd.DataFrame):
                return result, None
            if (
                hasattr(result, "completion_query_info")
                and result.completion_query_info["StatusCode"] == 0
            ):
                data_frame = result.to_dataframe()
                if result.is_partial_table:
                    print("Warning - query returned partial results.")
                return data_frame, result

        print("Warning - query did not complete successfully.")
        if hasattr(result, "completion_query_info"):
            print(
                result.completion_query_info.get("StatusDescription"),
                f"(code: {result.completion_query_info['StatusCode']})",
            )
        return None, result

    def _load_kql_magic(self):
        """Load KqlMagic if not loaded."""
        # KqlMagic
        print("Please wait. Loading Kqlmagic extension...")
        if self._ip is not None:
            self._ip.run_line_magic("reload_ext", "Kqlmagic")
            self._loaded = True

    def _is_kqlmagic_loaded(self) -> bool:
        """Return true if kql magic is loaded."""
        if self._ip is not None:
            return self._ip.find_magic("kql") is not None
        return False

    def _get_schema(self) -> Dict[str, Dict]:
        return self._ip.run_line_magic("kql", line="--schema")

    def _get_kql_option(self, option):
        """Retrieve a current Kqlmagic notebook option."""
        return self._ip.run_line_magic("config", line=option)

    def _set_kql_option(self, option, value):
        """Set a Kqlmagic notebook option."""
        set_txt = f"{option}={value}"
        return self._ip.run_line_magic("config", line=set_txt)

    @staticmethod
    def _format_datetime(date_time: datetime) -> str:
        """Return datetime-formatted string."""
        return date_time.isoformat(sep="T") + "Z"

    @staticmethod
    def _format_list(param_list: Iterable[Any]):
        """Return formatted list parameter."""
        fmt_list = []
        for item in param_list:
            if isinstance(item, str):
                fmt_list.append(f"'{item}'")
            else:
                fmt_list.append(f"{item}")
        return ",".join(fmt_list)

    _WS_RGX = r"workspace\(['\"](?P<ws>[^'\"]+)"
    _TEN_RGX = r"tenant\(['\"](?P<tenant>[^'\"]+)"

    def _raise_kql_error(self, ex):
        kql_err = json.loads(ex.args[0]).get("error")
        if kql_err.get("code") == "WorkspaceNotFoundError":
            ex_mssgs = [
                "The workspace ID used to connect to Azure Sentinel could not be found.",
                "Please check that this is a valid workspace for your subscription",
            ]
            ws_match = re.search(self._WS_RGX, self.current_connection, re.IGNORECASE)
            if ws_match:
                ws_name = ws_match.groupdict().get("ws")
                ex_mssgs.append(f"The workspace id used was {ws_name}.")
            ex_mssgs.append(f"The full connection string was {self.current_connection}")
            raise MsticpyKqlConnectionError(*ex_mssgs, title="unknown workspace")
        raise MsticpyKqlConnectionError(
            "The service returned the following error when connecting",
            str(ex),
            title="Kql response error",
        )

    @staticmethod
    def _raise_kql_engine_error(ex):
        ex_mssgs = [
            "An error was returned from Kqlmagic KqlEngine.",
            "This can occur if you tried to connect to a second workspace using a"
            + " different tenant ID - only a single tenant ID is supported in"
            + " one notebook.",
            "Other causes of this error could be an invalid format of your"
            + " connection string",
            *(ex.args),
        ]
        raise MsticpyKqlConnectionError(*ex_mssgs, title="kql connection error")

    @staticmethod
    def _raise_adal_error(ex):
        """Adal error - usually wrong tenant ID."""
        if ex.args[0] == "Unexpected polling state code_expired":
            raise MsticpyKqlConnectionError(
                "Authentication request was not completed.",
                title="authentication timed out",
            )

        err_response = getattr(ex, "error_response")
        if err_response and "error_description" in ex.error_response:
            ex_mssgs = ex.error_response["error_description"].split("\r\n")
        else:
            ex_mssgs = [f"Full error: {ex}"]
        raise MsticpyKqlConnectionError(
            *ex_mssgs, title="could not authenticate to tenant"
        )

    @staticmethod
    def _raise_authn_error(ex):
        """Raise an authentication error."""
        ex_mssgs = [
            "The authentication failed.",
            "Please check the credentials you are using and permissions on the workspace",
            *(ex.args),
        ]
        raise MsticpyKqlConnectionError(*ex_mssgs, title="authentication failed")

    @staticmethod
    def _raise_unknown_error(ex):
        """Raise an unknown exception."""
        raise MsticpyKqlConnectionError(
            "Another exception was returned by the service",
            *ex.args,
            f"Full exception:\n{str(ex)}",
            title="connection failed",
        )
