# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""KQL Driver class."""

import contextlib
import json
import logging
import os
import re
import warnings
from datetime import datetime
from typing import Any, Dict, Iterable, List, Optional, Tuple, Union

import pandas as pd
from azure.core.exceptions import ClientAuthenticationError
from IPython import get_ipython

from ..._version import VERSION
from ...auth.azure_auth import AzureCloudConfig, az_connect, only_interactive_cred
from ...common.exceptions import (
    MsticpyDataQueryError,
    MsticpyImportExtraError,
    MsticpyKqlConnectionError,
    MsticpyNoDataSourceError,
    MsticpyNotConnectedError,
)
from ...common.utility import MSTICPY_USER_AGENT, export
from ...common.wsconfig import WorkspaceConfig
from ..core.query_defns import DataEnvironment
from .driver_base import DriverBase, QuerySource

_KQL_ENV_OPTS = "KQLMAGIC_CONFIGURATION"


# Need to set KQL option before importing
def _set_kql_env_option(option, value):
    """Set an item in the KqlMagic main config environment variable."""
    kql_config = os.environ.get(_KQL_ENV_OPTS, "")
    current_opts = {
        opt.split("=")[0].strip(): opt.split("=")[1]
        for opt in kql_config.split(";")
        if opt.strip() and "=" in opt
    }

    current_opts[option] = value
    kql_config = ";".join(f"{opt}={val}" for opt, val in current_opts.items())
    os.environ[_KQL_ENV_OPTS] = kql_config


_set_kql_env_option("enable_add_items_to_help", False)

try:
    from Kqlmagic import kql as kql_exec
    from Kqlmagic.kql_engine import KqlEngineError
    from Kqlmagic.kql_proxy import KqlResponse
    from Kqlmagic.kql_response import KqlError
    from Kqlmagic.my_aad_helper import AuthenticationError
except ImportError as imp_err:
    raise MsticpyImportExtraError(
        "Cannot use this feature without Kqlmagic installed",
        "Install msticpy with the [kql] extra or one of the following:",
        "%pip install Kqlmagic   # notebook",
        "python -m pip install Kqlmagic   # python",
        title="Error importing Kqlmagic",
        extra="kql",
    ) from imp_err

__version__ = VERSION
__author__ = "Ian Hellen"


_KQL_CLOUD_MAP = {
    "global": "public",
    "cn": "china",
    "usgov": "government",
    "de": "germany",
}

_KQL_OPTIONS = ["timeout"]
_KQL_ENV_OPTS = "KQLMAGIC_CONFIGURATION"

_AZ_CLOUD_MAP = {kql_cloud: az_cloud for az_cloud, kql_cloud in _KQL_CLOUD_MAP.items()}

_LOGANALYTICS_URL_BY_CLOUD = {
    "global": "https://api.loganalytics.io/",
    "cn": "https://api.loganalytics.azure.cn/",
    "usgov": "https://api.loganalytics.us/",
    "de": "https://api.loganalytics.de/",
}


# pylint: disable=too-many-instance-attributes


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

        Other Parameters
        ----------------
        debug : bool
            print out additional diagnostic information.

        """
        self._ip = get_ipython()
        self._debug = kwargs.get("debug", False)
        super().__init__(**kwargs)

        self.formatters = {"datetime": self._format_datetime, "list": self._format_list}
        self._loaded = self._is_kqlmagic_loaded()

        os.environ["KQLMAGIC_LOAD_MODE"] = "silent"
        if not self._loaded:
            self._load_kql_magic()

        self._set_kql_option("request_user_agent_tag", MSTICPY_USER_AGENT)
        self._set_kql_env_option("enable_add_items_to_help", False)
        self._schema: Dict[str, Any] = {}
        self.environment = kwargs.pop("data_environment", DataEnvironment.MSSentinel)
        self.kql_cloud, self.az_cloud = self._set_kql_cloud()
        for option, value in kwargs.items():
            self._set_kql_option(option, value)

        self.current_connection = ""
        self.current_connection_args: Dict[str, Any] = {}
        if connection_str:
            self.current_connection = connection_str
            self.current_connection_args.update(kwargs)
            self.connect(connection_str)

    # pylint: disable=too-many-branches
    def connect(self, connection_str: Optional[str] = None, **kwargs):  # noqa: MC0001
        """
        Connect to data source.

        Parameters
        ----------
        connection_str : Union[str, WorkspaceConfig, None]
            Connection string or WorkspaceConfig for the Sentinel Workspace.

        Other Parameters
        ----------------
        kqlmagic_args : str, optional
            Additional string of parameters to be passed to KqlMagic
        mp_az_auth : Union[bool, str, list, None], optional
            Optional parameter directing KqlMagic to use MSTICPy Azure authentication.
            Values can be:
            True or "default": use the settings in msticpyconfig.yaml 'Azure' section
            str: single auth method name
            ('msi', 'cli', 'env', 'vscode', 'powershell', 'cache' or 'interactive')
            List[str]: list of acceptable auth methods from
            ('msi', 'cli', 'env', 'vscode', 'powershell', 'cache' or 'interactive')
        mp_az_tenant_id: str, optional
            Optional parameter specifying a Tenant ID for use by MSTICPy Azure
            authentication.
        workspace : str, optional
            Alternative to supplying a WorkspaceConfig object as the connection_str
            parameter. Giving a workspace name will fetch the workspace
            settings from msticpyconfig.yaml.


        """
        if not self._previous_connection:
            print("Connecting...", end=" ")

        mp_az_auth = kwargs.get("mp_az_auth", "default")
        mp_az_tenant_id = kwargs.get("mp_az_tenant_id")
        workspace = kwargs.get("workspace")
        if workspace or connection_str is None:
            connection_str = WorkspaceConfig(workspace=workspace)  # type: ignore

        if isinstance(connection_str, WorkspaceConfig):
            if not mp_az_tenant_id and "tenant_id" in connection_str:
                mp_az_tenant_id = connection_str["tenant_id"]
            self._instance = connection_str.workspace_key
            connection_str = connection_str.code_connect_str

        if not connection_str:
            raise MsticpyKqlConnectionError(
                f"A connection string is needed to connect to {self._connect_target}",
                title="no connection string",
            )
        if "kqlmagic_args" in kwargs:
            connection_str = f"{connection_str} {kwargs['kqlmagic_args']}"

        # Default to using Azure Auth if possible.
        if mp_az_auth and "try_token" not in kwargs:
            self._set_az_auth_option(mp_az_auth, mp_az_tenant_id)

        self.current_connection = connection_str
        ws_in_connection = re.search(
            r"workspace\(['\"]([^'\"]+).*",
            self.current_connection,
            re.IGNORECASE,
        )
        self.workspace_id = ws_in_connection[1] if ws_in_connection else None
        self.current_connection_args.update(kwargs)
        kql_err_setting = self._get_kql_option("short_errors")
        self._connected = False
        try:
            self._set_kql_option("short_errors", False)
            if self._ip is not None:
                try:
                    kql_exec(connection_str)
                    if not self._previous_connection:
                        print("connected")
                except KqlError as ex:
                    self._raise_kql_error(ex)
                except KqlEngineError as ex:
                    self._raise_kql_engine_error(ex)
                except AuthenticationError as ex:
                    self._raise_authn_error(ex)
                except Exception as ex:  # pylint: disable=broad-except
                    self._raise_adal_error(ex)
                self._connected = True
                self._previous_connection = True
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
            A DataFrame (if successful) or
            the underlying provider result if an error.

        """
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
                        " or database schema. Please check your this",
                        title=f"{table} not found.",
                    )
        data, result = self.query_with_results(query, **kwargs)
        return data if data is not None else result

    # pylint: disable=too-many-branches
    def query_with_results(
        self, query: str, **kwargs
    ) -> Tuple[pd.DataFrame, KqlResponse]:
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
        debug = kwargs.pop("debug", self._debug)
        if debug:
            print(query)

        if (
            not self.connected
            or self.workspace_id != self._get_kql_current_connection()
        ):
            self._make_current_connection()

        # save current auto_dataframe setting so that we can set to false
        # and restore current setting
        auto_dataframe = self._get_kql_option(option="auto_dataframe")
        self._set_kql_option(option="auto_dataframe", value=False)

        # run the query (append semicolon to prevent default output)
        if not query.strip().endswith(";"):
            query = f"{query}\n;"

        # Add any Kqlmagic options from kwargs
        kql_opts = {
            option: option_val
            for option, option_val in kwargs.items()
            if option in _KQL_OPTIONS
        }
        result = kql_exec(query, options=kql_opts)
        self._set_kql_option(option="auto_dataframe", value=auto_dataframe)
        if result is not None:
            if isinstance(result, pd.DataFrame):
                return result, None
            if hasattr(result, "completion_query_info") and (
                int(result.completion_query_info.get("StatusCode", 1)) == 0
                or result.completion_query_info.get("Text")
                == "Query completed successfully"
            ):
                data_frame = result.to_dataframe()
                if result.is_partial_table:
                    print("Warning - query returned partial results.")
                if debug:
                    print("Query status:\n", "\n".join(self._get_query_status(result)))
                return data_frame, result

        return self._raise_query_failure(query, result)

    def _make_current_connection(self):
        """Switch to the current connection (self.current_connection)."""
        try:
            self.connect(self.current_connection, **(self.current_connection_args))
        except MsticpyKqlConnectionError:
            self._connected = False
        if not self.connected:
            raise MsticpyNotConnectedError(
                "Please run the connect() method before running a query.",
                title=f"not connected to a {self._connect_target}",
                help_uri=MsticpyKqlConnectionError.DEF_HELP_URI,
            )

    def _load_kql_magic(self):
        """Load KqlMagic if not loaded."""
        # KqlMagic
        print("Please wait. Loading Kqlmagic extension...", end="")
        if self._ip is not None:
            with warnings.catch_warnings():
                # Suppress logging exception about PyGObject from msal_extensions
                msal_ext_logger = logging.getLogger("msal_extensions.libsecret")
                current_level = msal_ext_logger.getEffectiveLevel()
                msal_ext_logger.setLevel(logging.CRITICAL)
                warnings.simplefilter(action="ignore")
                self._ip.run_line_magic("reload_ext", "Kqlmagic")
                msal_ext_logger.setLevel(current_level)
        self._loaded = True
        print("done")

    def _is_kqlmagic_loaded(self) -> bool:
        """Return true if kql magic is loaded."""
        if self._ip is not None:
            return self._ip.find_magic("kql") is not None
        return bool(kql_exec("--version"))

    @property
    def _connect_target(self) -> str:
        if self.environment == DataEnvironment.MSSentinel:
            return "Workspace"
        return "Kusto cluster"

    @staticmethod
    def _get_query_status(result) -> List[str]:
        return [f"{key}: '{value}'" for key, value in result.completion_query_info]

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
        kql_exec("--config short_errors=False")
        result: Any
        try:
            opt_val = f"'{value}'" if isinstance(value, str) else value
            result = kql_exec(f"--config {option}={opt_val}")
        except ValueError:
            result = None
        finally:
            kql_exec("--config short_errors=True")
        return result

    @staticmethod
    def _set_kql_env_option(option, value):
        """Set an item in the KqlMagic main config environment variable."""
        kql_config = os.environ.get(_KQL_ENV_OPTS, "")
        print(kql_config)
        current_opts = {
            opt.split("=")[0].strip(): opt.split("=")[1]
            for opt in kql_config.split(";")
        }
        print(current_opts)
        current_opts[option] = value
        kql_config = ";".join(f"{opt}={val}" for opt, val in current_opts.items())
        print(kql_config)
        os.environ[_KQL_ENV_OPTS] = kql_config

    @staticmethod
    def _get_kql_current_connection():
        """Get the current connection Workspace ID from KQLMagic."""
        connections = kql_exec("--conn")
        current_connection = [conn for conn in connections if conn.startswith(" * ")]
        return current_connection[0].strip(" * ").split("@")[0]

    def _set_kql_cloud(self):
        """If cloud is set in Azure Settings override default."""
        # Check that there isn't a cloud setting in the KQLMAGIC env var
        kql_config = os.environ.get(_KQL_ENV_OPTS, "")
        if "cloud" in kql_config:
            # Set by user - we don't want to override this
            kql_cloud = self._get_kql_option("cloud")
            az_cloud = _AZ_CLOUD_MAP.get(kql_cloud, "public")
            return kql_cloud, az_cloud
        az_cloud = AzureCloudConfig().cloud
        kql_cloud = _KQL_CLOUD_MAP.get(az_cloud, "public")
        if kql_cloud != self._get_kql_option("cloud"):
            self._set_kql_option("cloud", kql_cloud)
        return kql_cloud, az_cloud

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
        return ", ".join(fmt_list)

    @staticmethod
    def _raise_query_failure(query, result):
        """Raise query failure exception."""
        err_contents = []
        if hasattr(result, "completion_query_info"):
            q_info = result.completion_query_info
            if "StatusDescription" in q_info:
                err_contents = [
                    f"StatusDescription {q_info.get('StatusDescription')}",
                    f"(err_code: {result.completion_query_info.get('StatusCode')})",
                ]
            elif "Text" in q_info:
                err_contents = [f"StatusDescription {q_info.get('Text')}"]
            else:
                err_contents = [f"Unknown error type: {q_info}"]
        if not err_contents:
            err_contents = ["Unknown query error"]

        err_contents.append(f"Query:\n{query}")
        raise MsticpyDataQueryError(*err_contents)

    _WS_RGX = r"workspace\(['\"](?P<ws>[^'\"]+)"
    _TEN_RGX = r"tenant\(['\"](?P<tenant>[^'\"]+)"

    def _raise_kql_error(self, ex):
        kql_err = json.loads(ex.args[0]).get("error")
        if kql_err.get("code") == "WorkspaceNotFoundError":
            ex_mssgs = [
                "The workspace ID used to connect to Microsoft Sentinel could not be found.",
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
            "Please check the credentials you are using and permissions on the ",
            "workspace or cluster.",
            *(ex.args),
        ]
        raise MsticpyKqlConnectionError(*ex_mssgs, title="authentication failed")

    @staticmethod
    def _raise_unknown_error(ex):
        """Raise an unknown exception."""
        raise MsticpyKqlConnectionError(
            "Another exception was returned by the service",
            *ex.args,
            f"Full exception:\n{ex}",
            title="connection failed",
        )

    def _set_az_auth_option(
        self, mp_az_auth: Union[bool, str, list, None], mp_az_tenant_id: str = None
    ):
        """
        Build connection string with auth elements.

        Parameters
        ----------
        mp_az_auth : Union[bool, str, list, None], optional
            Optional parameter directing KqlMagic to use MSTICPy Azure authentication.
            Values can be:
            - True or "default": use the settings in msticpyconfig.yaml 'Azure' section
            - auth_method: single auth method name ('msi', 'cli', 'env' or 'interactive')
            - auth_methods: list of acceptable auth methods from ('msi', 'cli',
                'env' or 'interactive')
        mp_az_tenant_id: str, optional
            Optional parameter specifying a Tenant ID for use by MSTICPy Azure
            authentication.

        """
        # default to default auth methods
        az_config = AzureCloudConfig()
        auth_types = az_config.auth_methods
        # override if user-supplied methods on command line
        if isinstance(mp_az_auth, str) and mp_az_auth != "default":
            auth_types = [mp_az_auth]
        elif isinstance(mp_az_auth, list):
            auth_types = mp_az_auth
        # get current credentials
        creds = az_connect(auth_methods=auth_types, tenant_id=mp_az_tenant_id)
        if only_interactive_cred(creds.modern):
            print("Check your default browser for interactive sign-in prompt.")

        endpoint_uri = self._get_endpoint_uri()
        endpoint_token_uri = f"{endpoint_uri}.default"
        # obtain token for the endpoint
        with contextlib.suppress(ClientAuthenticationError):
            token = creds.modern.get_token(
                endpoint_token_uri, tenant_id=mp_az_tenant_id
            )
            # set the token values in the namespace
            endpoint_token = {
                "access_token": token.token,
                "token_type": "Bearer",
                "resource": endpoint_uri,
            }
            self._set_kql_option("try_token", endpoint_token)

    def _get_endpoint_uri(self):
        return _LOGANALYTICS_URL_BY_CLOUD[self.az_cloud]
