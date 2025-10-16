# pylint:disable=too-many-lines
# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Kusto Driver subclass."""
from __future__ import annotations

import base64
import dataclasses
import json
import logging
from collections.abc import Hashable
from datetime import timedelta
from functools import partial
from typing import TYPE_CHECKING, Any, NamedTuple, NoReturn

from cryptography.hazmat.primitives import hashes, serialization
from cryptography.hazmat.primitives.serialization import pkcs12
from typing_extensions import Self

from ..._version import VERSION
from ...auth.azure_auth import az_connect, get_default_resource_name
from ...auth.cloud_mappings import AzureCloudConfig
from ...common.exceptions import (
    MsticpyDataQueryError,
    MsticpyMissingDependencyError,
    MsticpyNotConnectedError,
    MsticpyParameterError,
)
from ...common.provider_settings import ProviderArgs, get_protected_setting
from ...common.settings import get_config, get_http_proxies
from ...common.utility import export
from ..core.query_defns import DataEnvironment
from ..core.query_source import QuerySource
from .driver_base import DriverBase, DriverProps

try:
    from azure.kusto.data import (
        ClientRequestProperties,
        KustoClient,
        KustoConnectionStringBuilder,
    )
    from azure.kusto.data.exceptions import KustoApiError, KustoServiceError
    from azure.kusto.data.helpers import dataframe_from_result_table

    if TYPE_CHECKING:
        from azure.kusto.data.response import KustoResponseDataSet
except ImportError as imp_err:
    IMPORT_ERR: str = "Cannot use this feature without Azure Kusto client installed"
    raise MsticpyMissingDependencyError(
        IMPORT_ERR,
        title="Error importing azure.kusto.data",
        packages="azure-kusto-data",
    ) from imp_err

if TYPE_CHECKING:
    import pandas as pd
    from azure.core.credentials import AccessToken

    from msticpy.auth.azure_auth_core import AzCredentials

__version__: str = VERSION
__author__: str = "Ian Hellen"

_HELP_URL: str = (
    "https://msticpy.readthedocs.io/en/latest/DataProviders/DataProv-Kusto.html"
)

logger: logging.Logger = logging.getLogger(__name__)


@dataclasses.dataclass
class KustoConfig:
    """Kusto configuration class."""

    name: str
    cluster: str
    alias: str
    path: str
    args: ProviderArgs = dataclasses.field(default_factory=ProviderArgs)
    tenant_id: str | None = None
    integrated_auth: bool = False
    cluster_groups: list[str] = dataclasses.field(default_factory=list)

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
        CLUSTER_GROUPS = "ClusterGroups"
        CERTIFICATE = "Certificate"

    @property
    def default_db(self: Self) -> str | None:
        """Return default database for this cluster."""
        return self.args.get("Database", self.args.get("DefaultDatabase"))

    def __getattr__(self: Self, attrib: str) -> Any:
        """Return attribute from args if not in self."""
        if attrib in self.args:
            return self.args[attrib]
        err_msg: str = f"Invalid attribute '{attrib}'"
        raise AttributeError(err_msg)

    def __contains__(self: Self, attrib: str) -> bool:
        """Return True if attribute in self or args."""
        return attrib in self.__dict__ or attrib in self.args


@dataclasses.dataclass
class QuerySourceFields:
    """Kusto query source/yaml query fields."""

    CLUSTER = "cluster"
    CLUSTERS = "clusters"
    CLUSTER_GROUPS = "cluster_groups"
    DATA_ENVS = "data_environments"
    DATA_FAMILIES = "data_families"


class AuthParams(NamedTuple):
    """NamedTuple for auth parameters."""

    method: str
    params: dict[str, Any]
    uri: str


KFields = KustoConfig.ConfigFields

_DEFAULT_TIMEOUT = 60 * 4
_MAX_TIMEOUT = 60 * 60


# pylint: disable=too-many-instance-attributes
@export
class AzureKustoDriver(DriverBase):
    """Kusto Driver class to execute kql queries for Azure Data Explorer."""

    def __init__(
        self: AzureKustoDriver,
        connection_str: str | None = None,
        *,
        debug: bool = False,
        data_environment: DataEnvironment = DataEnvironment.Kusto,
        strict_query_match: bool = False,
        timeout: int = _DEFAULT_TIMEOUT,
        proxies: dict[str, str] | None = None,
        max_threads: int = 4,
        **kwargs,
    ) -> None:
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
            (can be set here or in connect and overridden in query methods)
        proxies : dict[str, str]
            Proxy settings for Kusto queries.
            Dictionary format is {protocol: proxy_url}
            Where protocol is https, http, etc. and proxy_url can contain
            optional authentication information in the format
            "https://username:password@proxy_host:port"
            If you have a proxy configuration in msticpyconfig.yaml and
            you do not want to use it, set this to an empty dictionary.
            (can be overridden in connect method)

        """
        del kwargs
        super().__init__(data_environment=data_environment, max_threads=max_threads)
        if debug:
            logger.setLevel(logging.DEBUG)
        self._strict_query_match: bool = strict_query_match
        self._kusto_settings: dict[str, dict[str, KustoConfig]] = _get_kusto_settings()
        self._default_database: str | None = None
        self._current_connection: str | None = connection_str
        self._current_config: KustoConfig | None = None
        self.client: KustoClient | None = None
        self._az_auth_types: list[str] | None = None
        self._az_tenant_id: str | None = None
        self._def_timeout: int = min(timeout, _MAX_TIMEOUT)
        self._def_proxies: dict[str, str] | None = proxies or get_http_proxies()

        self.add_query_filter("data_environments", "Kusto")
        self.set_driver_property(DriverProps.PUBLIC_ATTRS, self._set_public_attribs())
        self.set_driver_property(DriverProps.FILTER_ON_CONNECT, value=True)
        self.set_driver_property(DriverProps.EFFECTIVE_ENV, DataEnvironment.Kusto.name)
        self.set_driver_property(DriverProps.SUPPORTS_THREADING, value=True)
        self._loaded = True

    def _set_public_attribs(self: Self) -> dict[str, Any]:
        """Expose subset of attributes via query_provider."""
        return {
            "get_database_names": self.get_database_names,
            "get_database_schema": self.get_database_schema,
            "configured_clusters": self.configured_clusters,
            "cluster_uri": self.cluster_uri,
            "cluster_name": self.cluster_name,
            "cluster_config_name": self.cluster_config_name,
            "set_cluster": self.set_cluster,
            "set_database": self.set_database,
        }

    @property
    def current_connection(self: Self) -> str | None:
        """Return current connection string or URI."""
        if self._current_connection:
            return self._current_connection
        return self.cluster_uri

    @current_connection.setter
    def current_connection(self: Self, value: str) -> None:
        """Set current connection string or URI."""
        self._current_connection = value

    @property
    def cluster_uri(self: Self) -> str:
        """Return current cluster URI."""
        return "" if not self._current_config else self._current_config.cluster

    @property
    def cluster_name(self: Self) -> str:
        """Return current cluster URI."""
        return self._current_config.name if self._current_config else ""

    @property
    def cluster_config_name(self: Self) -> str:
        """Return current cluster URI."""
        if isinstance(self._current_config, KustoConfig):
            return self._current_config.alias
        return "not defined"

    @property
    def schema(self: Self) -> dict[str, dict[str, Any]]:
        """Return schema for current database."""
        try:
            return self.get_database_schema()
        except ValueError:
            err_msg: str = "Default database not set - unable to retrieve schema."
        except MsticpyNotConnectedError:
            err_msg = "Not connected to a cluster - unable to retrieve schema."
        except MsticpyDataQueryError:
            err_msg = "Kusto Error retrieving the schema."
        logger.info(err_msg)
        return {}

    @property
    def configured_clusters(self: Self) -> dict[str, KustoConfig]:
        """Return current Kusto config settings."""
        return self._kusto_settings["id"]

    def set_cluster(self: Self, cluster: str) -> None:
        """Set the current cluster to `cluster` and connect."""
        self.connect(cluster=cluster)

    def set_database(self: Self, database: str) -> None:
        """Set the default database to `database`."""
        self._default_database = database

    def connect(
        self: Self,
        connection_str: str | None = None,
        *,
        database: str | None = None,
        timeout: int | None = None,
        auth_types: str | list[str] | None = None,
        mp_az_auth: bool | str | list[str] | None = None,
        tenant_id: str | None = None,
        mp_az_tenant_id: str | None = None,
        cluster: str | None = None,
        proxies: dict[str, str] | None = None,
        **kwargs,
    ) -> None:
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
        cluster : str, optional
            Short name or URI of cluster to connect to.

        Other Parameters
        ----------------
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
            list[str]: list of acceptable auth methods
        mp_az_tenant_id: str, optional
            alias for `tenant_id`.
        timeout : int
            Query timeout in seconds, default is 240 seconds (4 minutes)
            Maximum is 3600 seconds (1 hour).
            (can be overridden in query methods)

        See Also
        --------
        msticpy.auth.azure_auth.list_auth_methods

        """
        logger.info(
            "Connecting to Kusto cluster: connection_str=%s, "
            "database=%s, "
            "timeout=%s, "
            "auth_types=%s, "
            "mp_az_auth=%s, "
            "tenant_id=%s, "
            "mp_az_tenant_id=%s, "
            "cluster=%s, "
            "proxies=%s, ",
            connection_str,
            database,
            timeout,
            auth_types,
            mp_az_auth,
            tenant_id,
            mp_az_tenant_id,
            cluster,
            proxies,
        )
        self._default_database = database
        self._def_timeout = min(timeout or self._def_timeout, _MAX_TIMEOUT)
        az_auth_types: bool | str | list[str] | None = auth_types or mp_az_auth
        if isinstance(az_auth_types, bool):
            self._az_auth_types = None
        elif isinstance(az_auth_types, str):
            self._az_auth_types = [az_auth_types]
        else:
            self._az_auth_types = az_auth_types
        self._az_tenant_id = tenant_id or mp_az_tenant_id

        kusto_cs: KustoConnectionStringBuilder | str | None = None
        if cluster:
            self._current_config = self._lookup_cluster_settings(cluster)
            if not self._az_tenant_id:
                self._az_tenant_id = self._current_config.tenant_id
            logger.info(
                "Using cluster id: %s, retrieved url %s to build connection string",
                cluster,
                self.cluster_uri,
            )
            kusto_cs = self._get_connection_string_for_cluster(self._current_config)
            self.current_connection = cluster
        elif connection_str:
            logger.info("Using connection string %s", connection_str)
            self.current_connection = connection_str
            kusto_cs = connection_str
        else:
            err_msg: str = "Must specify either a connection string or a cluster name"
            raise MsticpyParameterError(
                err_msg,
                parameter=["connection_str", "cluster"],
            )
        if not kusto_cs:
            err_msg = "Kusto connection string required"
            raise MsticpyParameterError(err_msg)
        self.client = KustoClient(kusto_cs)
        proxies = proxies or self._def_proxies
        proxy_url: str | None = proxies.get("https") if proxies else None
        if proxy_url:
            logger.info(
                "Using proxy: %s",
                (
                    proxy_url
                    if "@" not in proxy_url  # don't log proxy credentials
                    else "****" + proxy_url.split("@")[-1]
                ),
            )
            self.client.set_proxy(proxy_url)
        self._connected = True

    def query(
        self: Self,
        query: str,
        query_source: QuerySource | None = None,
        *,
        timeout: int | None = None,
        database: str | None = None,
        **kwargs,
    ) -> pd.DataFrame | dict[str, Any] | None:
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
        database : str, Optional
            Supply or override the Kusto database name
        timeout : int
            Query timeout in seconds, default is 240 seconds (4 minutes)
            Maximum is 3600 seconds (1 hour).

        Returns
        -------
        Union[pd.DataFrame, results.ResultSet]
            A DataFrame (if successful) or
            the underlying provider result if an error.

        """
        data, result = self.query_with_results(
            query,
            query_source=query_source,
            timeout=timeout,
            database=database,
            **kwargs,
        )
        return data if data is not None else result

    def query_with_results(
        self: Self,
        query: str,
        *,
        query_source: QuerySource | None = None,
        timeout: int | None = None,
        database: str | None = None,
        **kwargs,
    ) -> tuple[pd.DataFrame | None, dict[str, Any]]:
        """
        Return query results as a DataFrame and the result status.

        Parameters
        ----------
        query : str
            The query string

        Returns
        -------
        tuple[Optional[pd.DataFrame], Any]
            DataFrame of results and the result status.

        Raises
        ------
        MsticpyNotConnectedError
            If there is no connection to the data source.
        MsticpyDataQueryError
            If no database is specified in the query or parameters
            and there is no default database.

        """
        del kwargs
        if not self._connected:
            _raise_not_connected_error()

        if query_source and not self.query_usable(query_source):
            query_spec: dict[str, str] = self._get_cluster_spec_from_query_source(
                query_source,
            )
            err_msg: str = (
                "Invalid query source - for this connection."
                f" Connected cluster is: {self.cluster_uri} ({self.cluster_config_name})"
                "The cluster in the query definition is: "
                " ".join([f"{name}: {value}" for name, value in query_spec.items()])
            )
            raise MsticpyDataQueryError(
                err_msg,
                title="Mismatched cluster for query.",
                help_uri=_HELP_URL,
            )

        database = self._get_query_database_name(
            query_source=query_source,
            database=database,
        )
        data: pd.DataFrame | None = None
        status: dict[str, bool] = {"success": False}
        connection_props = ClientRequestProperties()
        connection_props.set_option(
            ClientRequestProperties.request_timeout_option_name,
            timedelta(seconds=timeout or self._def_timeout),
        )
        if self.client is None:
            _raise_not_connected_error()
        try:
            logger.info("Query executed query=%s, database=%s", query, database)
            response: KustoResponseDataSet = self.client.execute(
                database=database,
                query=query,
                properties=connection_props,
            )
            data = dataframe_from_result_table(response.primary_results[0])
            status = _parse_query_status(response)
            logger.info("Query completed: %s", str(status))
        except KustoApiError as err:
            logger.exception("Query failed")
            _raise_kusto_error(err)
        except KustoServiceError as err:
            logger.exception("Query failed")
            _raise_unknown_query_error(err)
        return data, status

    def get_database_names(self: Self) -> list[str]:
        """Get a list of database names from the connected cluster."""
        if self.client is None:
            _raise_not_connected_error()
        try:
            connection_props = ClientRequestProperties()
            connection_props.set_option(
                ClientRequestProperties.request_timeout_option_name,
                timedelta(seconds=self._def_timeout),
            )
            logger.info("Get database names cluster: %s", self.cluster_uri)
            response: KustoResponseDataSet = self.client.execute_mgmt(
                database="NetDefaultDB",
                query=".show databases",
                properties=connection_props,
            )

            # Convert the result to a DataFrame
            databases_df: pd.DataFrame = dataframe_from_result_table(
                response.primary_results[0],
            )
            return databases_df["DatabaseName"].tolist()
        except KustoServiceError as err:
            err_msg: str = "Error getting database names"
            raise MsticpyDataQueryError(
                err_msg,
                err,
                title="Kusto error",
                help_uri=_HELP_URL,
            ) from err

    def get_database_schema(
        self: Self,
        database: str | None = None,
    ) -> dict[str, dict[str, str]]:
        """
        Get table names and schema from the connected cluster/database.

        Parameters
        ----------
        database : str | None
            Name of the database to get schema for.
            The default is the last connected database.

        Returns
        -------
        dict[str, dict[str, str]]
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
        db_name: str | None = database or self._default_database
        if self.client is None:
            _raise_not_connected_error()
        if not db_name:
            err_msg: str = "No database name specified"
            raise ValueError(err_msg)

        query: str = f".show database {db_name} schema"
        try:
            # Execute the query
            logger.info("Get database schema: %s", db_name)
            response: KustoResponseDataSet = self.client.execute_mgmt(db_name, query)
            # Convert the result to a DataFrame
            schema_dataframe: pd.DataFrame = dataframe_from_result_table(
                response.primary_results[0],
            )
        except KustoServiceError as err:
            err_msg = "Error getting database schema"
            raise MsticpyDataQueryError(
                err_msg,
                err,
                title="Kusto error",
                help_uri=_HELP_URL,
            ) from err

        return {
            str(table): {
                col_name: col_type.replace("System.", "")
                for col_name, col_type in cols[["ColumnName", "ColumnType"]].to_numpy()
                if col_name is not None
            }
            for table, cols in schema_dataframe.groupby("TableName")
        }

    def _get_cluster_spec_from_query_source(
        self: Self,
        query_source: QuerySource,
    ) -> dict[str, str]:
        """Return cluster details from query source."""
        return {
            QuerySourceFields.CLUSTER: query_source.metadata.get(
                QuerySourceFields.CLUSTER,
                "NA",
            ),
            QuerySourceFields.CLUSTERS: query_source.metadata.get(
                QuerySourceFields.CLUSTERS,
                "NA",
            ),
            QuerySourceFields.CLUSTER_GROUPS: query_source.metadata.get(
                QuerySourceFields.CLUSTER_GROUPS,
                "NA",
            ),
        }

    def _get_connection_string_for_cluster(
        self: Self,
        cluster_config: KustoConfig,
    ) -> KustoConnectionStringBuilder:
        """Return full cluster URI and credential for cluster name or URI."""
        auth_params: AuthParams = self._get_auth_params_from_config(cluster_config)
        connect_auth_types: list[str] = (
            self._az_auth_types or AzureCloudConfig().auth_methods
        )
        if auth_params.method == "clientsecret":
            logger.info("Client secret specified in config - using client secret authn")
            if "clientsecret" not in connect_auth_types:
                connect_auth_types.insert(0, "clientsecret")
            credential: AzCredentials = az_connect(
                auth_types=connect_auth_types,
                **(auth_params.params),
            )
        elif auth_params.method == "certificate":
            logger.info("Certificate specified in config - using certificate authn")
            connect_auth_types.insert(0, "certificate")
            credential = az_connect(
                auth_types=self._az_auth_types,
                **(auth_params.params),
            )
            return self._create_kusto_cert_connection_str(auth_params)
        else:
            logger.info("Using integrated authn")
            credential = az_connect(
                auth_types=self._az_auth_types,
                **(auth_params.params),
            )
        logger.info("Credentials obtained %s", type(credential.modern).__name__)
        token: AccessToken = credential.modern.get_token(
            get_default_resource_name(auth_params.uri),
        )
        logger.info("Token obtained for %s", auth_params.uri)
        return KustoConnectionStringBuilder.with_aad_user_token_authentication(
            connection_string=auth_params.uri,
            user_token=token.token,
        )

    def _create_kql_cert_connection_str(
        self: Self,
        auth_params: AuthParams,
    ) -> KustoConnectionStringBuilder:
        logger.info("Creating KQL connection string for certificate authentication")
        if not self._az_tenant_id:
            err_msg: str = (
                "Azure tenant ID must be set in config or connect parameter "
                "to use certificate authentication"
            )
            raise ValueError(err_msg)
        cert_bytes: bytes = base64.b64decode(auth_params.params["certificate"])
        (
            private_key,
            certificate,
            _,
        ) = pkcs12.load_key_and_certificates(data=cert_bytes, password=None)
        if private_key is None or certificate is None:
            err_msg = f"Could not load certificate for cluster {self.cluster_uri}"
            raise ValueError(err_msg)
        private_cert: bytes = private_key.private_bytes(
            encoding=serialization.Encoding.PEM,
            format=serialization.PrivateFormat.TraditionalOpenSSL,
            encryption_algorithm=serialization.NoEncryption(),
        )
        public_cert: bytes = certificate.public_bytes(
            encoding=serialization.Encoding.PEM,
        )
        thumbprint: bytes = certificate.fingerprint(hashes.SHA256())
        return KustoConnectionStringBuilder.with_aad_application_certificate_sni_authentication(
            connection_string=self.cluster_uri,
            aad_app_id=auth_params.params["client_id"],
            private_certificate=private_cert.decode("utf-8"),
            public_certificate=public_cert.decode("utf-8"),
            thumbprint=thumbprint.hex().upper(),
            authority_id=self._az_tenant_id,
        )

    def _get_auth_params_from_config(
        self: Self,
        cluster_config: KustoConfig,
    ) -> AuthParams:
        """Get authentication parameters for cluster from KustoConfig values."""
        method = "integrated"
        auth_params_dict: dict[str, Any] = {}
        if KFields.CLIENT_SEC in cluster_config and KFields.CLIENT_ID in cluster_config:
            method = "clientsecret"
            auth_params_dict["client_id"] = cluster_config.ClientId
            auth_params_dict["client_secret"] = cluster_config.ClientSecret
            logger.info(
                "Using client secret authentication because client_secret in config",
            )
        elif (
            KFields.CERTIFICATE in cluster_config
            and KFields.CLIENT_ID in cluster_config
        ):
            method = "certificate"
            auth_params_dict["client_id"] = cluster_config.ClientId
            auth_params_dict["certificate"] = cluster_config.Certificate
            logger.info(
                "Using client secret authentication because client_secret in config",
            )
        elif KFields.INTEG_AUTH in cluster_config:
            logger.info("Using integrated auth.")
        auth_params_dict["tenant_id"] = cluster_config.tenant_id
        return AuthParams(method, auth_params_dict, cluster_config.cluster)

    def _lookup_cluster_settings(self: Self, cluster: str) -> KustoConfig:
        """Return cluster URI from config if cluster name is passed."""
        cluster_key: str = cluster.casefold().strip()
        if cluster_key in self._kusto_settings["url"]:
            return self._kusto_settings["url"][cluster_key]
        if cluster_key in self._kusto_settings["name"]:
            return self._kusto_settings["name"][cluster_key]
        if cluster_key in self._kusto_settings["id"]:
            return self._kusto_settings["id"][cluster_key]
        if cluster_key.startswith("https://"):
            return KustoConfig(
                cluster=cluster,
                name=cluster.replace("https://", "").split(".")[0],
                alias="no_config_found",
                path="",
                tenant_id=self._az_tenant_id,
            )

        err_msg: str = (
            f"Cluster '{cluster}' not found in msticpyconfig.yaml"
            " or is not in the correct format for a a cluster URI."
            " The cluster must be a key, cluster short name of an entry defined"
            " in the 'KustoClusters' section of the config file,"
            " or it must be a valid cluster URI."
        )
        raise MsticpyDataQueryError(
            err_msg,
            title="Unusable cluster identifier",
            help_uri=_HELP_URL,
        )

    def _get_query_database_name(
        self: Self,
        query_source: QuerySource | None = None,
        *,
        database: str | None = None,
    ) -> str:
        """Get the database name from query source."""
        if database:
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
        _raise_no_db_error()

    def query_usable(self: Self, query_source: QuerySource) -> bool:
        """Return True if query source is valid for current cluster."""
        if not query_source or not isinstance(query_source, QuerySource):
            return False
        if not self._current_config:
            # This probably won't work but we can just try the query
            # and see if it works
            return True
        # Check if query source has cluster information
        if not (
            query_source.metadata.keys()
            & {
                QuerySourceFields.CLUSTER_GROUPS,
                QuerySourceFields.CLUSTERS,
                QuerySourceFields.CLUSTER,
            }
        ):
            if self._strict_query_match:
                return False
            logger.info(
                (
                    "Query source %s has no cluster information. "
                    "The query may fail on the current cluster."
                ),
                query_source.name,
            )
            return True
        # Check for matches on cluster groups or cluster id
        result = False
        result |= self._cluster_groups_match(query_source)
        if result:
            return result
        result |= self._cluster_id_matches(query_source)
        return result

    def _cluster_groups_match(self: Self, query_source: QuerySource) -> bool:
        """Return True if query source cluster group is valid for current cluster."""
        source_cluster_groups: str | list[str] = query_source.metadata.get(
            QuerySourceFields.CLUSTER_GROUPS,
            [],
        )
        if (
            source_cluster_groups
            and self._current_config
            and self._current_config.cluster_groups
        ):
            driver_groups: set[str] = {
                group.casefold() for group in self._current_config.cluster_groups
            }
            query_groups = {group.casefold() for group in source_cluster_groups}
            return bool(driver_groups.intersection(query_groups))
        return False

    def _cluster_id_matches(self: Self, query_source: QuerySource) -> bool:
        """Return True if query source cluster is valid for current cluster."""
        # Get different representations of the cluster name
        result = False
        cluster_ids: set[str] = {
            self.cluster_uri.casefold(),
            self.cluster_name.casefold(),
            self.cluster_config_name.casefold(),
        }
        source_clusters: str | list[str] = query_source.metadata.get(
            QuerySourceFields.CLUSTERS,
            [],
        )
        if source_clusters:
            query_source_clusters: set[str] = {
                cluster.casefold() for cluster in source_clusters
            }
            result |= bool(cluster_ids.intersection(query_source_clusters))
            if result:
                return result
        source_cluster: str = query_source.metadata.get(QuerySourceFields.CLUSTER)
        if source_cluster:
            result |= source_cluster.casefold() in cluster_ids
        return result

    @staticmethod
    def _get_db_from_query_source(query_source: QuerySource) -> str:
        """Get the database name from query source metadata."""
        if database := query_source.metadata.get("database"):
            return database
        data_families: list[str] = query_source.metadata.get("data_families")
        if not data_families:
            logger.info("Could not find database name in query source metadata.")
            _raise_no_db_error(query_source)

        if "." in data_families[0]:
            _, database = data_families[0].split(".", maxsplit=1)
        else:
            # Not expected but we can still use a DB value with no dot
            database = data_families[0]
        logger.info("Using database %s from query source metadata.", database)
        return database


def _get_kusto_settings() -> dict[str, dict[str, KustoConfig]]:
    """Return a dictionary of Kusto cluster settings from msticpyconfig.yaml."""
    kusto_config: dict[str, Any] = {
        kusto_entry: kusto_config
        for kusto_entry, kusto_config in get_config("DataProviders", {}).items()
        if kusto_entry.startswith("Kusto")
    }
    kusto_clusters: dict[str, Any] = {}
    # handle legacy configuration
    for config_id, cluster_conf in kusto_config.items():
        name = config_id.replace("Kusto-", "")
        kusto_clusters[name] = cluster_conf
        kusto_clusters[name]["path"] = f"DataProviders.{config_id}"

    kusto_new_conf: dict[str, Any] = {
        config_id: {**cluster_conf, "path": f"KustoClusters.{config_id}"}
        for config_id, cluster_conf in get_config("KustoClusters", {}).items()
    }
    defaults: dict[str, Any] = kusto_new_conf.pop(KFields.DEFAULTS, {}).get(
        KFields.ARGS,
        {},
    )
    kusto_clusters.update(kusto_new_conf)

    cluster_by_url: dict[str, KustoConfig] = _create_cluster_config(
        kusto_clusters=kusto_clusters,
        defaults=defaults,
    )
    return {
        "url": cluster_by_url,
        "id": {conf.alias.casefold(): conf for conf in cluster_by_url.values()},
        "name": {conf.name.casefold(): conf for conf in cluster_by_url.values()},
    }


def _create_cluster_config(
    kusto_clusters: dict[str, Any],
    defaults: dict[str, Any],
) -> dict[str, KustoConfig]:
    """Return a dictionary of Kusto cluster settings from msticpyconfig.yaml."""
    return {
        config[KFields.ARGS]
        .get(KFields.CLUSTER)
        .casefold(): KustoConfig(
            tenant_id=_setting_or_default(
                config[KFields.ARGS],
                KFields.TENANT_ID,
                defaults,
            ),
            integrated_auth=_setting_or_default(
                config[KFields.ARGS],
                KFields.INTEG_AUTH,
                defaults,
            )
            or False,
            args=_create_protected_args(
                _section_or_default(config[KFields.ARGS], defaults),
                config["path"],
            ),
            cluster=config[KFields.ARGS].get(KFields.CLUSTER),
            alias=name,
            name=get_cluster_name(config[KFields.ARGS].get(KFields.CLUSTER)),
            path=config["path"],
            cluster_groups=config.get(KFields.CLUSTER_GROUPS),
        )
        for name, config in kusto_clusters.items()
    }


def _setting_or_default(
    settings: dict[str, Any],
    name: str,
    default: dict[str, Any],
) -> Any:
    """Return a setting from the settings dictionary or the default."""
    return settings.get(name, default.get(name))


def _section_or_default(
    settings: dict[str, Any],
    default: dict[str, Any],
) -> dict[str, Any]:
    """Return a combined dictionary from the settings dictionary or the default."""
    return {
        key: settings.get(key, default.get(key))
        for key in (settings.keys() | default.keys())
    }


def _create_protected_args(args: dict[str, Any], path: str) -> ProviderArgs:
    """Return a dictionary of protected settings for Kusto args config."""
    args_dict: dict[str, Any] = {
        key_name: (
            partial(
                get_protected_setting,
                config_path=f"{path}.Args",
                setting_name=key_name,
            )
            if isinstance(value, dict)
            and (value.get("EnvironmentVar") or value.get("KeyVault"))
            else value
        )
        for key_name, value in args.items()
    }
    return ProviderArgs(**args_dict)


def get_cluster_name(cluster_uri: str) -> str:
    """Return the cluster name from the cluster uri."""
    return cluster_uri.replace("https://", "").split(".")[0]


def _parse_query_status(response: KustoResponseDataSet) -> dict[str, Any]:
    """Parse the query status from the Kusto response."""
    try:
        query_info_idx: int = response.tables_names.index("QueryCompletionInformation")
    except ValueError:
        return {
            "status": "Failed",
            "message": "QueryCompletionInformation not found in response",
        }

    df_status: pd.DataFrame = dataframe_from_result_table(
        response.tables[query_info_idx],
    )
    results: list[dict[Hashable, Any]] = df_status[
        ["EventTypeName", "Payload"]
    ].to_dict(orient="records")
    return {
        row.get("EventTypeName", "Unknown_field"): json.loads(
            row.get("Payload", "No Payload"),
        )
        for row in results
    }


def _raise_kusto_error(error: KustoApiError) -> NoReturn:
    """Raise a Kusto error."""
    err_msg: str = f"error code: {error.error.code}"
    raise MsticpyDataQueryError(
        error.error.description,
        err_msg,
        title=error.error.message,
        help_uri=_HELP_URL,
    ) from error


def _raise_no_db_error(query_source: QuerySource | None = None) -> NoReturn:
    """Raise an error if no database is specified."""
    if query_source:
        messages: tuple[str, ...] = (
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
    raise MsticpyDataQueryError(
        *messages,
        title="No database specified",
        help_uri=_HELP_URL,
    )


def _raise_not_connected_error() -> NoReturn:
    """Raise an error if not connected to a cluster."""
    err_msg: str = "Please connect to the cluster before executing a query."
    raise MsticpyNotConnectedError(
        err_msg,
        title="Not connected to cluster",
        help_uri=_HELP_URL,
    )


def _raise_unknown_query_error(err: Exception) -> NoReturn:
    """Raise an error if unknown exception raised."""
    err_msg: str = (
        f"Unknown exception when executing query. Exception type: {type(err)}"
    )
    raise MsticpyDataQueryError(
        err_msg,
        *err.args,
        title="Unknown exception during query execution",
        help_uri=_HELP_URL,
    ) from err
