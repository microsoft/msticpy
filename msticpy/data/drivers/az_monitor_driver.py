# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
Azure monitor/Log Analytics KQL Driver class.

See Also
--------
Azure SDK code: https://github.com/Azure/azure-sdk-for-python/tree/main/sdk/monitor

Azure SDK docs: https://learn.microsoft.com/python/api/overview/
azure/monitor-query-readme?view=azure-python

"""

import logging
from datetime import datetime
from typing import Any, Dict, Iterable, List, Optional, Tuple, Union

import pandas as pd
from azure.core.exceptions import HttpResponseError
from azure.core.pipeline.policies import UserAgentPolicy

from ..._version import VERSION
from ...auth.azure_auth import AzureCloudConfig, az_connect
from ...common.exceptions import (
    MsticpyDataQueryError,
    MsticpyKqlConnectionError,
    MsticpyMissingDependencyError,
    MsticpyNoDataSourceError,
    MsticpyNotConnectedError,
)
from ...common.settings import get_http_proxies
from ...common.timespan import TimeSpan
from ...common.utility import export, mp_ua_header
from ...common.wsconfig import WorkspaceConfig
from ..core.query_defns import DataEnvironment
from .driver_base import DriverBase, QuerySource

logger = logging.getLogger(__name__)

try:
    from azure.monitor.query import LogsQueryClient, LogsQueryPartialResult
except ImportError as imp_err:
    raise MsticpyMissingDependencyError(
        "Cannot use this feature without Azure monitor client installed",
        title="Error importing azure.monitor.query",
        packages="azure-monitor-query",
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
class KqlDriverAZMon(DriverBase):
    """KqlDriver class to execute kql queries."""

    _DEFAULT_TIMEOUT = 300

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
        self._debug = kwargs.get("debug", False)
        super().__init__(**kwargs)

        self._schema: Dict[str, Any] = {}
        self.formatters = {"datetime": self._format_datetime, "list": self._format_list}
        self._loaded = True
        self._ua_policy = UserAgentPolicy(user_agent=mp_ua_header()["UserAgent"])

        self.environment = kwargs.pop("data_environment", DataEnvironment.MSSentinel)

        self._query_client: Optional[LogsQueryClient]
        self._az_tenant_id: Optional[str] = None
        self._workspace_id: Optional[str] = None
        self._workspace_ids: List[str] = []
        self._def_connection_str: Optional[str] = connection_str

    @property
    def url_endpoint(self) -> str:
        """Return the current URL endpoint for Azure Monitor."""
        return _LOGANALYTICS_URL_BY_CLOUD.get(
            AzureCloudConfig().cloud, _LOGANALYTICS_URL_BY_CLOUD["global"]
        )

    def connect(self, connection_str: Optional[str] = None, **kwargs):
        """
        Connect to data source.

        Parameters
        ----------
        connection_str : Union[str, WorkspaceConfig, None]
            Connection string or WorkspaceConfig for the Sentinel Workspace.

        Other Parameters
        ----------------
        auth_types: Iterable [str]
            Authentication (credential) types to use. By default the
            values configured in msticpyconfig.yaml are used. If not set,
            it will use the msticpy defaults.
        mp_az_auth : Union[bool, str, list, None], optional
            Deprecated parameter directing driver to use MSTICPy Azure authentication.
            Values can be:
            True or "default": use the settings in msticpyconfig.yaml 'Azure' section
            str: single auth method name
            List[str]: list of acceptable auth methods from
            Use `auth_types` parameter instead.
        tenant_id: str, optional
            Optional parameter specifying a Tenant ID for use by MSTICPy Azure
            authentication. By default, the tenant_id for the workspace.
        workspace : str, optional
            Alternative to supplying a WorkspaceConfig object as the connection_str
            parameter. Giving a workspace name will fetch the workspace
            settings from msticpyconfig.yaml.
        workspaces : Iterable[str], optional
            List of workspaces to run the queries against, each workspace name
            must have an entry in msticpyconfig.yaml
        workspace_ids: Iterable[str], optional
            List of workspace IDs to run the queries against. Must be supplied
            along with a `tenant_id`.

        """
        az_auth_types = kwargs.get("auth_types", kwargs.get("mp_az_auth", None))
        if isinstance(az_auth_types, bool):
            az_auth_types = None
        if isinstance(az_auth_types, str):
            az_auth_types = [az_auth_types]

        self.get_workspaces(connection_str, **kwargs)

        self._connected = False
        credentials = az_connect(
            auth_methods=az_auth_types, tenant_id=self._az_tenant_id
        )
        self._query_client = LogsQueryClient(
            credential=credentials.modern,
            endpoint=self.url_endpoint,
            proxies=kwargs.get("proxies", get_http_proxies()),
        )
        self._connected = True
        print("connected")
        return self._connected

    def get_workspaces(self, connection_str: Optional[str] = None, **kwargs):
        """Get workspace or workspaces to connect to."""
        self._az_tenant_id = kwargs.get("tenant_id", kwargs.get("mp_az_tenant_id"))
        # multiple workspace IDs
        if workspaces := kwargs.pop("workspaces, None"):
            workspace_configs = {
                WorkspaceConfig(workspace)[
                    WorkspaceConfig.CONF_WS_ID_KEY
                ]: WorkspaceConfig(workspace)[WorkspaceConfig.CONF_TENANT_ID_KEY]
                for workspace in workspaces
            }
            if len(set(workspace_configs.values()) > 1):
                raise ValueError("All workspaces must have the same tenant ID.")
            self._az_tenant_id = next(iter(workspace_configs.values()))
            self._workspace_ids = list(set(workspace_configs))
            logger.info(
                "%d configured workspaces: %s",
                len(self._workspace_ids),
                ", ".join(self._workspace_ids),
            )
            return
        if workspace_ids := kwargs.pop("workspace_ids, None"):
            if not self._az_tenant_id:
                raise ValueError(
                    "You must supply a tenant_id with a list of `workspace_ids`"
                )
            self._workspace_ids = workspace_ids
            logger.info(
                "%d configured workspaces: %s",
                len(self._workspace_ids),
                ", ".join(self._workspace_ids),
            )
            return

        # standard - single-workspace configuration
        workspace_name = kwargs.get("workspace")
        ws_config: Optional[WorkspaceConfig] = None
        connection_str = connection_str or self._def_connection_str
        if workspace_name or connection_str is None:
            ws_config = WorkspaceConfig(workspace=workspace_name)  # type: ignore
        elif isinstance(connection_str, str):
            ws_config = WorkspaceConfig.from_connection_string(connection_str)
        elif isinstance(connection_str, WorkspaceConfig):
            ws_config = connection_str

        if not ws_config:
            raise MsticpyKqlConnectionError(
                "A workspace name, config or connection string is needed"
                "to connect to a workspace.",
                title="No connection details",
            )
        if not self._az_tenant_id and WorkspaceConfig.CONF_TENANT_ID_KEY in ws_config:
            self._az_tenant_id = ws_config[WorkspaceConfig.CONF_TENANT_ID_KEY]
        self._workspace_id = ws_config[WorkspaceConfig.CONF_WS_ID_KEY]

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
        if not self._connected or self._query_client is None:
            raise MsticpyNotConnectedError(
                "Please run connect() to connect to the workspace",
                "before running a query.",
                title="Workspace not connected.",
            )
        if query_source:
            self._check_table_exists(query_source)
        data, result = self.query_with_results(query, **kwargs)
        return data if data is not None else result

    # pylint: disable=too-many-branches
    def query_with_results(
        self, query: str, **kwargs
    ) -> Tuple[pd.DataFrame, Dict[str, Any]]:
        """
        Execute query string and return DataFrame of results.

        Parameters
        ----------
        query : str
            The kql query to execute

        Returns
        -------
        Tuple[pd.DataFrame, Dict[str, Any]]
            A DataFrame (if successful) and
            Query status dictionary.

        """
        if not self._connected or self._query_client is None:
            raise MsticpyNotConnectedError(
                "Please run connect() to connect to the workspace",
                "before running a query.",
                title="Workspace not connected.",
            )
        logger.info("Query to run %s", query)
        query_options = kwargs.pop("query_options", {})
        time_span_value = self._get_time_span_value(query_options)

        server_timeout = kwargs.pop(
            "server_timeout", kwargs.pop("timeout", self._DEFAULT_TIMEOUT)
        )
        workspace_id = next(iter(self._workspace_ids), None) or self._workspace_id
        additional_workspaces = self._workspace_ids[1:] if self._workspace_ids else None
        try:
            result = self._query_client.query_workspace(
                workspace_id=workspace_id,  # type: ignore[arg-type]
                query=query,
                timespan=time_span_value,  # type: ignore[arg-type]
                server_timeout=server_timeout,
                additional_workspaces=additional_workspaces,
            )
        except HttpResponseError as http_err:
            self._raise_query_failure(query, http_err)
        # We might get an unknown exception type from azure.monitor.query
        except Exception as unknown_err:  # pylint: disable=broad-exception-caught
            self._raise_unknown_error(unknown_err)

        status = self._get_query_status(result)
        logger.info("query status %s", repr(status))

        if isinstance(result, LogsQueryPartialResult):
            table = result.partial_data[0]
        else:
            table = result.tables[0]
        data_frame = pd.DataFrame(table.rows, columns=table.columns)

        return data_frame, status

    def _get_time_span_value(self, query_options):
        default_time_params = query_options.get("default_time_params", False)
        time_params = query_options.get("time_span", {})
        if (
            default_time_params
            or "start" not in time_params
            or "end" not in time_params
        ):
            time_span_value = None
            logger.info("No time parameters supplied.")
        else:
            time_span = TimeSpan(
                start=self.formatters["datetime"](time_params["start"]),
                end=self.formatters["datetime"](time_params["end"]),
            )
            time_span_value = time_span.start, time_span.end
            logger.info("Time parameters set %s", str(time_span))
        return time_span_value

    def _check_table_exists(self, query_source):
        """Check that query table is in the workspace schema."""
        if not self.schema:
            return
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

    @staticmethod
    def _get_query_status(result) -> Dict[str, Any]:
        status = {
            "status": result.status.name,
            "tables": len(result.tables) if result.tables else 0,
        }

        if isinstance(result, LogsQueryPartialResult):
            status["partial"] = "partial results returned"
            status["tables"] = (len(result.partial_data) if result.partial_data else 0,)
        return status

    @staticmethod
    def _get_schema() -> Dict[str, Dict]:
        raise NotImplementedError("Schema not implemented.")

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
    def _raise_query_failure(query, http_err):
        """Raise query failure exception."""
        err_contents = []
        if hasattr(http_err, "message"):
            err_contents = http_err.message.split("\n")
        if not err_contents:
            err_contents = ["Unknown query error"]

        err_contents.append(f"Query:\n{query}")
        raise MsticpyDataQueryError(*err_contents, title="Query Failure") from http_err

    @staticmethod
    def _raise_unknown_error(exception):
        """Raise an unknown exception."""
        raise MsticpyKqlConnectionError(
            "An unknown exception was returned by the service",
            *exception.args,
            f"Full exception:\n{exception}",
            title="connection failed",
        )
