# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""KQL Driver class."""
import json
import os
import re
import warnings
from datetime import datetime
from typing import Any, Dict, Iterable, Optional, Tuple, Union

import pandas as pd
from IPython import get_ipython

from ...common.exceptions import (
    MsticpyDataQueryError,
    MsticpyImportExtraError,
    MsticpyKqlConnectionError,
    MsticpyNoDataSourceError,
    MsticpyNotConnectedError,
)
from ...common.wsconfig import WorkspaceConfig
from ...common import pkg_config as config
from .driver_base import DriverBase, QuerySource

try:
    from Kqlmagic.kql_engine import KqlEngineError
    from Kqlmagic.kql_response import KqlError
    from Kqlmagic.my_aad_helper import AuthenticationError
    from Kqlmagic import kql as kql_exec
except ImportError as imp_err:
    raise MsticpyImportExtraError(
        "Cannot use this feature without Kqlmagic installed",
        title="Error importing Kqlmagic",
        extra="kql",
    ) from imp_err

from ..._version import VERSION
from ...common.azure_auth_core import az_connect_core
from ...common.utility import export

__version__ = VERSION
__author__ = "Ian Hellen"


_KQL_CLOUD_MAP = {
    "global": "public",
    "cn": "china",
    "usgov": "government",
    "de": "germany",
}


@export
class KqlDriver(DriverBase):
    """KqlDriver class to execute kql queries."""

    def __init__(self, connection_str: str = None, **kwargs):
        """
        Instantiate KqlDriver and optionally connect.

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

        self._set_kql_cloud()

        if connection_str:
            self.current_connection = connection_str
            self.connect(connection_str)

    # pylint: disable=too-many-branches
    def connect(self, connection_str: Optional[str] = None, **kwargs):  # noqa: MC0001
        """
        Connect to data source.

        Parameters
        ----------
        connection_str : str
            Connect to a data source

        """
        if isinstance(connection_str, WorkspaceConfig):
            connection_str = connection_str.code_connect_str
        if not connection_str:
            raise MsticpyKqlConnectionError(
                "A connection string is needed to connect to Azure Sentinel.",
                title="no connection string",
            )
        if "kqlmagic_args" in kwargs:
            connection_str = connection_str + " " + kwargs["kqlmagic_args"]
        elif "cli" in kwargs:
            namespace = kwargs["cli"]
            connection_str = _build_auth_cnt_str(namespace, connection_str, ["cli"])
        elif "msi" in kwargs:
            namespace = kwargs["msi"]
            connection_str = _build_auth_cnt_str(namespace, connection_str, ["msi"])
        self.current_connection = connection_str
        kql_err_setting = self._get_kql_option("short_errors")
        self._connected = False
        try:
            self._set_kql_option("short_errors", False)
            if self._ip is not None:
                try:
                    kql_exec(connection_str)
                except KqlError as ex:
                    self._raise_kql_error(ex)
                except KqlEngineError as ex:
                    self._raise_kql_engine_error(ex)
                except AuthenticationError as ex:
                    self._raise_authn_error(ex)
                except Exception as ex:  # pylint: disable=broad-except
                    self._raise_adal_error(ex)
                self._connected = True
                self._schema = self._get_schema()
            else:
                print(f"Could not connect to kql query provider for {connection_str}")
            return self._connected
        finally:
            self._set_kql_option("short_errors", kql_err_setting)

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
        auto_dataframe = self._get_kql_option(option="auto_dataframe")
        self._set_kql_option(option="auto_dataframe", value=False)

        # run the query (append semicolon to prevent default output)
        if not query.strip().endswith(";"):
            query = f"{query}\n;"

        result = kql_exec(query)
        self._set_kql_option(option="auto_dataframe", value=auto_dataframe)
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

        # Query failed
        err_args = []
        if hasattr(result, "completion_query_info"):
            err_desc = result.completion_query_info.get("StatusDescription")
            err_desc = f"StatusDescription {err_desc}"
            err_code = f"(err_code: {result.completion_query_info.get('StatusCode')})"
            err_args = [err_desc, err_code]
        err_args.append(f"Query:\n{query}")
        raise MsticpyDataQueryError(*err_args)

    def _load_kql_magic(self):
        """Load KqlMagic if not loaded."""
        # KqlMagic
        print("Please wait. Loading Kqlmagic extension...", end="")
        if self._ip is not None:
            with warnings.catch_warnings():
                warnings.simplefilter(action="ignore")
                self._ip.run_line_magic("reload_ext", "Kqlmagic")
        self._loaded = True
        print("done")

    def _is_kqlmagic_loaded(self) -> bool:
        """Return true if kql magic is loaded."""
        if self._ip is not None:
            return self._ip.find_magic("kql") is not None
        return bool(kql_exec("--version"))

    @staticmethod
    def _get_schema() -> Dict[str, Dict]:
        return kql_exec("--schema")

    @staticmethod
    def _get_kql_option(option):
        """Retrieve a current Kqlmagic notebook option."""
        return kql_exec(f"--config {option}").get(option)

    @staticmethod
    def _set_kql_option(option, value):
        """Set a Kqlmagic notebook option."""
        opt_val = f"'{value}'" if isinstance(value, str) else value
        return kql_exec(f"--config {option}={opt_val}")

    def _set_kql_cloud(self):
        """If cloud is set in Azure Settings override default."""
        # Check that there isn't a cloud setting in the KQLMAGIC env var
        kql_config = os.environ.get("KQLMAGIC_CONFIGURATION", "")
        if "cloud" in kql_config:
            # Set by user - we don't want to override this
            return
        kql_cloud = "public"
        try:
            az_settings = config.get_config("Azure")
            if az_settings and "cloud" in az_settings:
                kql_cloud = _KQL_CLOUD_MAP.get(az_settings["cloud"], "public")
        except KeyError:
            pass  # no Azure section in config
        if kql_cloud != self._get_kql_option("cloud"):
            self._set_kql_option("cloud", kql_cloud)

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
        if ex.args and ex.args[0] == "Unexpected polling state code_expired":
            raise MsticpyKqlConnectionError(
                "Authentication request was not completed.",
                title="authentication timed out",
            )

        err_response = getattr(ex, "error_response", None)
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


def _build_auth_cnt_str(
    namespace: dict, connection_str: str, auth_types: list = None
) -> str:
    """
    Build connection string with auth elements.

    Parameters
    ----------
    namespace : dict
        locals namespace from notebook environment
    connection_str : str
        current connection string to append auth elements to
    auth_types : list, optional
        preferred authentication types, by default ['cli', 'msi']

    Returns
    -------
    str
        completed connection string

    """
    if not auth_types:
        auth_types = ["cli", "msi"]
    creds = az_connect_core(auth_methods=auth_types)
    token = creds.modern.get_token("https://api.loganalytics.io/.default")
    namespace["token_dict"] = {
        "access_token": token.token,
        "token_type": "Bearer",
        "resource": "https://api.loganalytics.io/",
    }
    return connection_str + " " + "-try_token=locals()['token_dict']"
