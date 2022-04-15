# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Kusto Driver subclass."""
from typing import Any, Dict, Optional, Union

import pandas as pd

from ..._version import VERSION
from ...common.exceptions import MsticpyParameterError, MsticpyUserConfigError
from ...common.provider_settings import ProviderArgs, get_provider_settings
from ...common.utility import export
from ..core.query_defns import DataEnvironment
from .kql_driver import KqlDriver, QuerySource

__version__ = VERSION
__author__ = "Ian Hellen"

_KCS_CODE = "code;"
_KCS_APP = "tenant='{tenant_id}';clientid='{client_id}';clientsecret='{clientsecret}';"
_KCS_TEMPLATE = "azure_data-Explorer://{auth}cluster='{cluster}';database='{database}'"

KustoClusterSettings = Dict[str, Dict[str, Union[str, ProviderArgs]]]


@export
class KustoDriver(KqlDriver):
    """Kusto Driver class to execute kql queries for Azure Data Explorer."""

    def __init__(self, connection_str: str = None, **kwargs):
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

        """
        super().__init__(connection_str=connection_str, **kwargs)
        self.environment = kwargs.get("data_environment", DataEnvironment.Kusto)
        self._connected = True
        self._kusto_settings: KustoClusterSettings = _get_kusto_settings()
        self._cluster_uri = None

    def connect(self, connection_str: Optional[str] = None, **kwargs):
        """
        Connect to data source.

        Parameters
        ----------
        connection_str : str
            Connect to a data source

        Other Parameters
        ----------------
        cluster : str, optional
            Short name or URI of cluster to connect to.
        database : str, optional
            Name of database to connect to.
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

        """
        self.current_connection = self._get_connection_string(
            connection_str=connection_str, **kwargs
        )

        mp_az_auth = kwargs.pop("mp_az_auth", None)
        mp_az_tenant_id = kwargs.pop("mp_az_tenant_id", None)

        if (
            self._cluster_uri
        ):  # This should be set by _get_connection_string called above
            cluster_settings = self._kusto_settings.get(self._cluster_uri.casefold())
            if cluster_settings:
                if mp_az_auth is None and cluster_settings["integrated_auth"]:
                    mp_az_auth = "default"
                if mp_az_tenant_id is None and cluster_settings["tenant_id"]:
                    mp_az_tenant_id = cluster_settings["tenant_id"]

        kwargs.pop("cluster", None)
        kwargs.pop("database", None)

        super().connect(
            connection_str=self.current_connection,
            mp_az_auth=mp_az_auth,
            mp_az_tenant_id=mp_az_tenant_id,
            **kwargs,
        )

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
            A DataFrame (if successfull) or
            the underlying provider result if an error.

        """
        new_connection = self._get_connection_string(
            query_source=query_source, **kwargs
        )
        if new_connection:
            self.current_connection = new_connection
        data, result = self.query_with_results(query)
        return data if data is not None else result

    def _get_connection_string(self, query_source: QuerySource = None, **kwargs):
        """Create a connection string from arguments and configuration."""
        # If the connection string is supplied as a parameter, use that
        cluster = None
        new_connection = kwargs.get("connection_str")
        database = kwargs.get("database")
        if not new_connection:
            # try to get cluster and db from kwargs or query_source metadata
            cluster = self._lookup_cluster(kwargs.get("cluster", "Kusto"))
            if cluster and database:
                new_connection = self._create_connection(
                    cluster=cluster, database=database
                )
                self._cluster_uri = cluster
        if not new_connection and query_source:
            # try to get cluster and db from query_source metadata
            cluster = cluster or query_source.metadata.get("cluster")
            database = (
                database
                or query_source.metadata.get("database")
                or self._get_db_from_datafamily(query_source, cluster, database)
            )
            new_connection = self._create_connection(cluster=cluster, database=database)
            self._cluster_uri = cluster
        return new_connection

    def _get_db_from_datafamily(self, query_source, cluster, database):
        data_families = query_source.metadata.get("data_families")
        if (
            not isinstance(data_families, list) or len(data_families) == 0
        ) and not self.current_connection:
            # call create connection so that we throw an informative error
            self._create_connection(cluster=cluster, database=database)
        if "." in data_families[0]:  # type: ignore
            _, qry_db = data_families[0].split(".", maxsplit=1)  # type: ignore
        else:
            # Not expected but we can still use a DB value with no dot
            qry_db = data_families[0]  # type: ignore
        return qry_db

    def _create_connection(self, cluster, database):
        """Create the connection string, checking parameters."""
        if not cluster or not database:
            if cluster:
                err_mssg = "database name"
            elif database:
                err_mssg = "cluster uri"
            else:
                err_mssg = "cluster uri and database name"
            raise MsticpyParameterError(
                f"Could not determine the {err_mssg} for the query.",
                "Please update the query with the correct values or specify",
                "explicitly with the 'cluster' and 'database' parameters to",
                "this function.",
                "In the query template these values are specified in the metadata:",
                "cluster: cluster_uri",
                "data_families: [ClusterAlias.database]",
                title="Missing cluster or database names.",
                parameter=err_mssg,
            )
        cluster_key = cluster.casefold()
        if cluster_key not in self._kusto_settings:
            raise MsticpyUserConfigError(
                f"The cluster {cluster} was not found in the configuration.",
                "You must have an entry for the cluster in the 'DataProviders section",
                "of your msticyconfig.yaml",
                "Expected format:",
                "Kusto[-instance_name]:",
                "  args:",
                "    Cluster: cluster_uri",
                "    Integrated: True",
                "or",
                "Kusto[-instance_name]:",
                "  args:",
                "    Cluster: cluster_uri",
                "    TenantId: tenant_uuid",
                "    ClientId: tenant_uuid",
                "    ClientSecret: (string|KeyVault|EnvironmentVar:)",
                title="Unknown cluster.",
            )
        return self._format_connection_str(cluster, database)

    def _format_connection_str(self, cluster: str, database: str) -> Optional[str]:
        """Return connection string with client secret added."""
        fmt_items = self._kusto_settings.get(cluster.casefold())
        if not fmt_items:
            return None
        fmt_items["database"] = database
        if fmt_items.get("integrated_auth"):
            auth_string = _KCS_CODE
        else:
            # Note, we don't add the secret until required at runtime to prevent
            # it hanging around in memory as much as possible.
            fmt_items["clientsecret"] = fmt_items["args"].get("ClientSecret")  # type: ignore
            auth_string = _KCS_APP.format(**fmt_items)
        return _KCS_TEMPLATE.format(auth=auth_string, **fmt_items)

    def _lookup_cluster(self, cluster: str):
        """Return cluster URI from config if cluster name is passed."""
        if cluster.strip().casefold().startswith("https://"):
            return cluster
        return next(
            (
                kusto_config["cluster"]
                for cluster_key, kusto_config in self._kusto_settings.items()
                if (
                    cluster_key.startswith(f"https://{cluster.casefold()}.")
                    or (
                        kusto_config.get("alias", "").casefold()  # type: ignore
                        == cluster.casefold()
                    )
                )
            ),
            None,
        )

    def _get_endpoint_uri(self):
        if not self._cluster_uri.endswith("/"):
            self._cluster_uri += "/"
        return self._cluster_uri


def _get_kusto_settings() -> KustoClusterSettings:
    kusto_settings: KustoClusterSettings = {}
    for prov_name, settings in get_provider_settings("DataProviders").items():
        if not prov_name.startswith("Kusto"):
            continue
        instance = "Kusto"
        if "-" in prov_name:
            _, instance = prov_name.split("-", maxsplit=1)

        cluster = settings.args.get("Cluster")
        if not cluster:
            raise MsticpyUserConfigError(
                "Mandatory 'Cluster' setting is missing in msticpyconfig.",
                f"the Kusto entry with the missing setting is '{prov_name}'",
                title=f"No Cluster value for {prov_name}",
            )
        kusto_settings[cluster.casefold()] = {
            "tenant_id": settings.args.get("TenantId"),  # type: ignore
            "integrated_auth": settings.args.get("IntegratedAuth"),  # type: ignore
            "client_id": settings.args.get("ClientId"),  # type: ignore
            "args": settings.args,
            "cluster": cluster,
            "alias": instance,
        }
    return kusto_settings
