# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Kusto Driver subclass."""
import dataclasses
import json
import logging
from datetime import timedelta
from functools import partial
from typing import Any, Dict, List, NamedTuple, Optional, Tuple, Union

import pandas as pd
from azure.kusto.data import (
    ClientRequestProperties,
    KustoClient,
    KustoConnectionStringBuilder,
)
from azure.kusto.data.exceptions import KustoApiError, KustoServiceError
from azure.kusto.data.helpers import dataframe_from_result_table
from azure.kusto.data.response import KustoResponseDataSet

from ..._version import VERSION
from ...auth.azure_auth import az_connect, get_default_resource_name
from ...common.exceptions import (
    MsticpyDataQueryError,
    MsticpyNotConnectedError,
    MsticpyParameterError,
    MsticpyUserError,
)
from ...common.provider_settings import ProviderArgs, get_protected_setting
from ...common.settings import get_config, get_http_proxies
from ...common.utility import export
from ..core.query_defns import DataEnvironment
from ..core.query_source import QuerySource
from .driver_base import DriverBase, DriverProps

__version__ = VERSION
__author__ = "Ian Hellen"

_HELP_URL = "https://msticpy.readthedocs.io/en/latest/DataProviders/DataProv-Kusto.html"

# _KCS_CODE = "code;"
# _KCS_APP = "tenant='{tenant_id}';clientid='{client_id}';clientsecret='{clientsecret}';"
# _KCS_TEMPLATE = "azure_data-Explorer://{auth}cluster='{cluster}';database='{database}'"

# KustoClusterSettings = Dict[str, Dict[str, Union[str, ProviderArgs]]]

logger = logging.getLogger(__name__)


@dataclasses.dataclass
class KustoConfig:
    """Kusto configuration class."""

    name: str
    cluster: str
    alias: str
    path: str
    args: ProviderArgs = dataclasses.field(default_factory=ProviderArgs)
    tenant_id: Optional[str] = None
    integrated_auth: bool = False

    @dataclasses.dataclass
    class ConfigFields:
        """Kusto configuration fields."""

        CLUSTER = "Cluster"
        TENANT_ID = "TenantId"
        INTEG_AUTH = "IntegratedAuth"
        DEFAULTS = "ClusterDefaults"
        CLIENT_ID = "ClientId"
        CLIENT_SEC = "ClientSecret"
        ARGS = "Args"

    # pylint: disable=no-member
    @property
    def default_db(self):
        """Return default database for this cluster."""
        return self.args.get("database", self.args.get("default_database"))

    # pylint: disable=unsubscriptable-object, unsupported-membership-test
    def __getattr__(self, attrib):
        """Return attribute from args if not in self."""
        if attrib in self.args:
            return self.args[attrib]
        raise AttributeError(f"Invalid attribute '{attrib}'")

    def __contains__(self, attrib):
        """Return True if attribute in self or args."""
        return attrib in self.__dict__ or attrib in self.args


class AuthParams(NamedTuple):
    """NamedTuple for auth parameters."""

    method: str
    params: Dict[str, Any]
    uri: str


KFields = KustoConfig.ConfigFields

_DEFAULT_TIMEOUT = 60 * 4
_MAX_TIMEOUT = 60 * 60


@export
class AzureKustoDriver(DriverBase):
    """Kusto Driver class to execute kql queries for Azure Data Explorer."""

    def __init__(self, connection_str: Optional[str] = None, **kwargs):
        """
        Instantiate KustoDriver.

        Parameters
        ----------
        connection_str : str, optional
            Connection string

        Other Parameters
        ----------------
        debug : bool
            print out additional diagnostic information.
        timeout : int
            Query timeout in seconds, default is 240 seconds (4 minutes)
            Maximum is 3600 seconds (1 hour).

        """
        super().__init__(**kwargs)
        self.environment = kwargs.get("data_environment", DataEnvironment.Kusto)
        self._kusto_settings: Dict[str, Dict[str, KustoConfig]] = _get_kusto_settings()
        self._default_database: Optional[str] = None
        self.current_connection: Optional[str] = connection_str
        self._current_config: Union[KustoConfig, str, None] = None
        self.client: Optional[KustoClient] = None
        self._az_auth_types: Optional[List[str]] = None
        self._az_tenant_id: Optional[str] = None
        self._connection_props = ClientRequestProperties()
        timeout = min(kwargs.pop("timeout", _DEFAULT_TIMEOUT), _MAX_TIMEOUT)
        self._connection_props.set_option(
            ClientRequestProperties.request_timeout_option_name,
            timedelta(seconds=timeout),
        )
        self.add_query_filter("data_environments", "Kusto")
        self.set_driver_property(DriverProps.PUBLIC_ATTRS, self._set_public_attribs())

    @property
    def current_cluster(self) -> str:
        """Return current cluster name."""
        if not self._current_config:
            return ""
        if isinstance(self._current_config, str):
            return self._current_config
        return self._current_config.cluster

    @property
    def schema(self) -> Dict[str, Dict]:
        """Return schema for current database."""
        try:
            return self.get_database_schema()
        except ValueError:
            print("Default database not set - unable to retrieve schema")
        except MsticpyNotConnectedError:
            print("Not connected to a cluster - unable to retrieve schema")
        return {}

    @property
    def configured_clusters(self) -> Dict[str, KustoConfig]:
        """Return current Kusto config settings."""
        return self._kusto_settings["id"]

    def set_cluster(self, cluster: str):
        """Set the current cluster to `cluster` and connect."""
        self.connect(cluster=cluster)

    def set_database(self, database: str):
        """Set the default database to `database`."""
        self._default_database = database

    def connect(self, connection_str: Optional[str] = None, **kwargs):
        """
        Connect to data source.

        Either a connection string or a cluster name must be specified.
        The cluster name can be a short name or a full URI. If a short name,
        the cluster must be defined in the msticpyconfig.yaml file.
        In this case, the short name can be either the key of the cluster
        definition the host name part of the cluster URI.

        Parameters
        ----------
        connection_str : str
            Connect to a data source

        Other Parameters
        ----------------
        cluster : str, optional
            Short name or URI of cluster to connect to.
        database : str, optional
            Name to set the default database to.
        tenant_id : str, optional
            Azure tenant ID for the cluster.
        connection_str : str, optional
            Kusto connection string, including authentication credentials.
        auth_types: Union[str, list], optional
            Credential type or types to use for authentication.
            Use `msticpy.auth.azure_auth.list_auth_methods()` to get a list
            of available methods.
        mp_az_auth : Union[bool, str, list, None], optional
            Deprecated parameter to use MSTICPy Azure authentication.
            Values can be:
            True or "default": use the settings in msticpyconfig.yaml 'Azure' section
            str: single auth method name
            List[str]: list of acceptable auth methods
        mp_az_tenant_id: str, optional
            alias for `tenant_id`.

        See Also
        --------
        msticpy.auth.azure_auth.list_auth_methods

        """
        self._default_database = kwargs.pop("database", None)
        az_auth_types = kwargs.pop("auth_types", kwargs.pop("mp_az_auth", None))
        if isinstance(az_auth_types, bool):
            self._az_auth_types = None
        elif isinstance(az_auth_types, str):
            self._az_auth_types = [az_auth_types]
        else:
            self._az_auth_types = az_auth_types
        self._az_tenant_id = kwargs.pop(
            "tenant_id", kwargs.pop("mp_az_tenant_id", None)
        )

        cluster = kwargs.pop("cluster", None)
        if not connection_str and not cluster:
            raise MsticpyParameterError(
                "Must specify either a connection string or a cluster name",
                parameters=["connection_str", "cluster"],
            )

        if cluster:
            self._current_config = self._lookup_cluster_settings(cluster)
            kusto_cs = self._get_connection_string_for_cluster(self._current_config)
        else:
            kusto_cs = connection_str

        self.client = KustoClient(kusto_cs)
        proxies = get_http_proxies()
        if proxies and proxies.get("https"):
            self.client.set_proxy(proxies["https"])
        self._connected = True

    def query(
        self, query: str, query_source: Optional[QuerySource] = None, **kwargs
    ) -> Union[pd.DataFrame, Any]:
        """
        Execute query string and return DataFrame of results.

        Parameters
        ----------
        query : str
            The query to execute
        query_source : QuerySource
            The query definition object

        Other Parameters
        ----------------
        cluster : str, Optional
            Supply or override the Kusto cluster name
        database : str, Optional
            Supply or override the Kusto database name
        data_source : str, Optional
            alias for `db`
        connection_str : str, Optional


        Returns
        -------
        Union[pd.DataFrame, results.ResultSet]
            A DataFrame (if successful) or
            the underlying provider result if an error.

        """
        data, result = self.query_with_results(
            query, query_source=query_source, **kwargs
        )
        return data if data is not None else result

    def query_with_results(
        self, query: str, **kwargs
    ) -> Tuple[Optional[pd.DataFrame], Any]:
        """
        Return query results as a DataFrame and the result status.

        Parameters
        ----------
        query : str
            The query string

        Returns
        -------
        Tuple[Optional[pd.DataFrame], Any]
            DataFrame of results and the result status.

        Raises
        ------
        MsticpyNotConnectedError
            If there is no connection to the data source.
        MsticpyUserConfigError
            If no database is specified in the query or parameters
            and there is no default database.

        """
        if not self._connected:
            _raise_not_connected_error()
        query_source = kwargs.pop("query_source", None)
        cluster = kwargs.pop("cluster", None)
        if not cluster:
            # if no cluster in params, try to get it from query_source
            cluster = self._get_cluster_from_query_source(query_source=query_source)
        if connection_str := kwargs.pop("connection_str", None):
            # if user has explicitly specified a connection string
            # try to connect with that
            logger.info("Query - connecting with connection string")
            self.connect(connection_str=connection_str)
        elif cluster != self.current_cluster:
            # if user or query_source specifies a different cluster name
            logger.info(
                "Query - switching cluster from %s to %s", self.current_cluster, cluster
            )
            self.connect(cluster=cluster)

        database = self._get_query_database_name(query_source=query_source, **kwargs)
        data: Optional[pd.DataFrame] = None
        status = {"success": False}
        if not database:
            _raise_no_db_error(query_source)
        try:
            response = self.client.execute(  # type: ignore[union-attr]
                database=database, query=query
            )
            data = dataframe_from_result_table(response.primary_results[0])
            status = _parse_query_status(response)
            logger.info("Query completed: %s", str(status))
        except KustoApiError as err:
            logger.exception("Query failed: %s", err)
            _raise_kusto_error(err)
        except KustoServiceError as err:
            logger.exception("Query failed: %s", err)
            _raise_unknown_query_error(err)
        return data, status

    def get_database_names(self) -> List[str]:
        """Get a list of database names from the connected cluster."""
        if self.client is None:
            _raise_not_connected_error()
        try:
            response = self.client.execute_mgmt(  # type: ignore[union-attr]
                "NetDefaultDB", ".show databases"
            )

            # Convert the result to a DataFrame
            databases_df = dataframe_from_result_table(response.primary_results[0])
            return databases_df["DatabaseName"].tolist()
        except KustoServiceError as err:
            raise MsticpyDataQueryError(
                "Error getting database names",
                err,
                title="Kusto error",
                help_uri=_HELP_URL,
            ) from err

    def get_database_schema(
        self, database: Optional[str] = None
    ) -> Dict[str, Dict[str, str]]:
        """
        Get table names and schema from the connected cluster/database.

        Parameters
        ----------
        database : Optional[str]
            Name of the database to get schema for.
            The default is the last connected database.

        Returns
        -------
        Dict[str, Dict[str, str]]
            Dictionary of table names, each with a dictionary of
            column names and types.

        Raises
        ------
        ValueError :
            No database name specified or set as the default.
        MsticpyNotConnectedError :
            Not connected to a cluster.
        MsticpyDataQueryError :
            Error querying the cluster.

        """
        db_name = database or self._default_database
        if self.client is None:
            _raise_not_connected_error()
        if not db_name:
            raise ValueError("No database name specified")

        query = f".show database {db_name} schema"
        try:
            # Execute the query
            response = self.client.execute_mgmt(db_name, query)  # type: ignore[union-attr]
            # Convert the result to a DataFrame
            schema_dataframe = dataframe_from_result_table(response.primary_results[0])
        except KustoServiceError as err:
            raise MsticpyDataQueryError(
                "Error getting database schema",
                err,
                title="Kusto error",
                help_uri=_HELP_URL,
            ) from err

        return {
            str(table): {
                col_name: col_type.replace("System.", "")
                for col_name, col_type in cols[["ColumnName", "ColumnType"]].values
                if col_name is not None
            }
            for table, cols in schema_dataframe.groupby("TableName")
        }

    def _set_public_attribs(self):
        """Expose subset of attributes via query_provider."""
        return {
            "get_database_names": self.get_database_names,
            "get_database_schema": self.get_database_schema,
            "configured_clusters": self.configured_clusters,
            "current_cluster": self.current_cluster,
            "set_cluster": self.set_cluster,
            "set_database": self.set_database,
        }

    def _get_cluster_from_query_source(
        self, query_source: Optional[QuerySource]
    ) -> str:
        """Return cluster name from query source."""
        if not query_source:
            return self.current_cluster
        if "cluster" in query_source.metadata:
            return query_source.metadata["cluster"]
        if "clusters" in query_source.metadata:
            return query_source.metadata["clusters"][0]
        return self.current_cluster

    def _cluster_in_query_source(
        self, cluster: str, query_source: Optional[QuerySource]
    ) -> bool:
        """Return True if cluster is specified in query source."""
        if not query_source:
            return False
        cluster_config = self._lookup_cluster_settings(cluster)
        if isinstance(cluster_config, KustoConfig):
            cluster = cluster_config.cluster
        if "clusters" in query_source.metadata:
            return cluster.casefold() in [
                clust.casefold() for clust in query_source.metadata["clusters"]
            ]
        if "cluster" in query_source.metadata:
            return query_source.metadata["cluster"].casefold() == cluster.casefold()
        return False

    def _get_connection_string_for_cluster(
        self, cluster_config: Union[KustoConfig, str]
    ) -> KustoConnectionStringBuilder:
        """Return full cluster URI and credential for cluster name or URI."""
        auth_params = self._get_auth_params(cluster_config)
        if auth_params.method == "client_secret":
            credential = az_connect(auth_types=["clientsecret"], **(auth_params.params))
        else:
            credential = az_connect(
                auth_types=self._az_auth_types, **(auth_params.params)
            )
        logger.info("Credentials obtained %s", type(credential).__name__)
        token = credential.modern.get_token(get_default_resource_name(auth_params.uri))
        logger.info("Token obtained for %s", auth_params.uri)
        return KustoConnectionStringBuilder.with_aad_user_token_authentication(
            connection_string=auth_params.uri,
            user_token=token.token,
        )

    def _get_auth_params(self, cluster_config) -> AuthParams:
        """Return auth parameters for cluster name or URI."""
        if isinstance(cluster_config, str):
            # cluster is already a full URI but we don't have config for it
            # try integrated auth and use the cluster URI as the cluster name
            logger.info("Cluster config is string %s", cluster_config)
            return AuthParams(
                method="integrated",
                params={"tenant_id": self._az_tenant_id},
                uri=cluster_config,
            )

        # if we do have config, use the parameters from the config
        logger.info("Cluster config is KustoConfig")
        return self._get_auth_params_from_config(cluster_config)

    def _get_auth_params_from_config(self, cluster_config: KustoConfig) -> AuthParams:
        """Get authentication parameters for cluster from KustoConfig values."""
        method = "integrated"
        auth_params_dict = {}
        if KFields.CLIENT_SEC in cluster_config and KFields.CLIENT_ID in cluster_config:
            method = "clientsecret"
            auth_params_dict["client_id"] = cluster_config.ClientId
            auth_params_dict["client_secret"] = cluster_config.ClientSecret
            logger.info(
                "Using client secret authentication because client_secret in config"
            )
        elif KFields.INTEG_AUTH in cluster_config:
            logger.info("Using integrated auth.")
        auth_params_dict["tenant_id"] = cluster_config.tenant_id
        return AuthParams(method, auth_params_dict, cluster_config.cluster)

    def _lookup_cluster_settings(self, cluster: str) -> Union[KustoConfig, str]:
        """Return cluster URI from config if cluster name is passed."""
        cluster_key = cluster.casefold().strip()
        if cluster_key in self._kusto_settings["url"]:
            return self._kusto_settings["url"][cluster_key]
        if cluster_key in self._kusto_settings["name"]:
            return self._kusto_settings["name"][cluster_key]
        if cluster_key in self._kusto_settings["id"]:
            return self._kusto_settings["id"][cluster_key]
        if cluster_key.startswith("https://"):
            return cluster

        raise MsticpyDataQueryError(
            f"Cluster '{cluster}' not found in msticpyconfig.yaml",
            "or is not in the correct format for a a cluster URI",
            "The cluster must be a key, cluster short name of an entry defined",
            "in the 'KustoClusters' section of the config file,",
            "or it must be a valid cluster URI.",
            title="Unusable cluster identifier",
            help_uri=_HELP_URL,
        )

    def _get_query_database_name(
        self, query_source: Optional[QuerySource] = None, **kwargs
    ) -> str:
        """Get the database name from query source or kwargs."""
        if database := kwargs.get("database"):
            logger.info("Using database %s from parameter.", database)
            return database
        if query_source:
            return self._get_db_from_query_source(query_source)
        # check if database is specified in the current config
        if isinstance(self._current_config, KustoConfig):
            database = self._current_config.default_db
            if database:
                logger.info("Using database %s from current config.", database)
                return database
        if self._default_database:
            logger.info("Using database %s from _default_database.", database)
            return self._default_database
        raise ValueError(
            "No database name found in query definition or query parameters"
        )

    @staticmethod
    def _get_db_from_query_source(query_source: QuerySource) -> str:
        """Get the database name from query source metadata."""
        if database := query_source.metadata.get("database"):
            return database
        data_families = query_source.metadata.get("data_families")
        if not data_families:
            logger.info("Could not find database name in query source metadata.")
            raise ValueError(
                "Query yaml has no data families element specifying the database name."
            )

        if "." in data_families[0]:  # type: ignore
            _, database = data_families[0].split(".", maxsplit=1)  # type: ignore
        else:
            # Not expected but we can still use a DB value with no dot
            database = data_families[0]  # type: ignore
        logger.info("Using database %s from query source metadata.", database)
        return database


def _get_kusto_settings() -> Dict[str, Dict[str, KustoConfig]]:
    """Return a dictionary of Kusto cluster settings from msticpyconfig.yaml."""
    kusto_config = {
        kusto_entry: kusto_config
        for kusto_entry, kusto_config in get_config("DataProviders", {}).items()
        if kusto_entry.startswith("Kusto")
    }
    kusto_clusters = {}
    # handle legacy configuration
    for config_id, cluster_conf in kusto_config.items():
        name = config_id.replace("Kusto-", "")
        kusto_clusters[name] = cluster_conf
        kusto_clusters[name]["path"] = f"DataProviders.{config_id}"

    kusto_new_conf = {
        config_id: {**cluster_conf, "path": f"KustoClusters.{config_id}"}
        for config_id, cluster_conf in get_config("KustoClusters", {}).items()
    }
    defaults: Dict[str, Any] = kusto_new_conf.pop(KFields.DEFAULTS, {}).get(
        KFields.ARGS, {}  # type: ignore[assignment]
    )
    kusto_clusters.update(kusto_new_conf)

    cluster_by_url = _create_cluster_config(
        kusto_clusters=kusto_clusters, defaults=defaults
    )
    return {
        "url": cluster_by_url,
        "id": {conf.alias.casefold(): conf for conf in cluster_by_url.values()},
        "name": {conf.name.casefold(): conf for conf in cluster_by_url.values()},
    }


def _create_cluster_config(
    kusto_clusters: Dict[str, Any], defaults: Dict[str, Any]
) -> Dict[str, KustoConfig]:
    """Return a dictionary of Kusto cluster settings from msticpyconfig.yaml."""
    return {
        config[KFields.ARGS]
        .get(KFields.CLUSTER)
        .casefold(): KustoConfig(
            tenant_id=_setting_or_default(
                config[KFields.ARGS], KFields.TENANT_ID, defaults
            ),
            integrated_auth=_setting_or_default(
                config[KFields.ARGS], KFields.INTEG_AUTH, defaults
            )
            or False,
            args=_create_protected_args(
                _section_or_default(config[KFields.ARGS], defaults), config["path"]
            ),
            cluster=config[KFields.ARGS].get(KFields.CLUSTER),
            alias=name,
            name=get_cluster_name(config[KFields.ARGS].get(KFields.CLUSTER)),
            path=config["path"],
        )
        for name, config in kusto_clusters.items()
    }


def _setting_or_default(settings: Dict[str, Any], name: str, default: Dict[str, Any]):
    """Return a setting from the settings dictionary or the default."""
    return settings.get(name, default.get(name))


def _section_or_default(settings: Dict[str, Any], default: Dict[str, Any]):
    """Return a combined dictionary from the settings dictionary or the default."""
    return {
        key: settings.get(key, default.get(key))
        for key in (settings.keys() | default.keys())
    }


def _create_protected_args(args: Dict[str, Any], path: str) -> ProviderArgs:
    """Return a dictionary of protected settings for Kusto args config."""
    args_dict = {
        key_name: partial(
            get_protected_setting, config_path=f"{path}.Args", setting_name=key_name
        )
        if isinstance(value, dict)
        and (value.get("EnvironmentVar") or value.get("KeyVault"))
        else value
        for key_name, value in args.items()
    }
    return ProviderArgs(**args_dict)


def get_cluster_name(cluster_uri):
    """Return the cluster name from the cluster uri."""
    return cluster_uri.replace("https://", "").split(".")[0]


def _parse_query_status(response: KustoResponseDataSet) -> Dict[str, Any]:
    """Parse the query status from the Kusto response."""
    try:
        query_info_idx = response.tables_names.index("QueryCompletionInformation")
    except ValueError:
        return {
            "status": "Failed",
            "message": "QueryCompletionInformation not found in response",
        }

    df_status = dataframe_from_result_table(response.tables[query_info_idx])
    results = df_status[["EventTypeName", "Payload"]].to_dict(orient="records")
    return {
        row.get("EventTypeName", "Unknown_field"): json.loads(
            row.get("Payload", "No Payload")
        )
        for row in results
    }


def _raise_kusto_error(error):
    """Raise a Kusto error."""
    if isinstance(error, KustoApiError):
        raise MsticpyDataQueryError(
            error.error.description,
            f"error code: {error.error.code}",
            title=error.error.message,
            help_uri=_HELP_URL,
        ) from error


def _raise_no_db_error(query_source):
    """Raise an error if no database is specified."""
    if query_source:
        messages = (
            "No database found in the query definition",
            (
                "Correct the query definition or use the 'database' parameter"
                "or set the default database when connecting to the cluster."
            ),
        )
    else:
        messages = (
            (
                "No database specified. Use the 'database' parameter or set the "
                "default database when connecting to the cluster."
            ),
        )
    raise MsticpyUserError(
        *messages,
        title="No database specified",
        help_uri=_HELP_URL,
    )


def _raise_not_connected_error():
    """Raise an error if not connected to a cluster."""
    raise MsticpyNotConnectedError(
        "Please connect to the cluster before executing a query.",
        title="Not connected to cluster",
        help_uri=_HELP_URL,
    )


def _raise_unknown_query_error(err):
    """Raise an error if unknown exception raised."""
    raise MsticpyDataQueryError(
        "Unknown exception when executing query.",
        f"Exception type: {type(err)}",
        *err.args,
        title="Unknown exception during query execution",
        help_uri=_HELP_URL,
    )
