# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Kusto Driver subclass."""
from typing import Any, Dict, Optional, Union

import pandas as pd

from ...common.exceptions import MsticpyParameterError, MsticpyUserConfigError
from ...common.provider_settings import get_provider_settings, ProviderArgs
from ...common.utility import export
from ..query_defns import DataEnvironment
from .kql_driver import KqlDriver, QuerySource

from ..._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"

_KCS_TEMPLATE = (
    "azure_data-Explorer://"
    "tenant='{tenant}';clientid='{clientid}';clientsecret='{clientsecret}';"
    "cluster='{cluster}';"
    "database='{database}'"
)

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
        db : str, Optional
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
        # If the connection string is supplied as a parameter, use that
        cluster = database = None
        self.current_connection = kwargs.get("connection_str")
        if not self.current_connection:
            # try to get cluster and db from kwargs or query_source metadata
            cluster = kwargs.get("cluster")
            database = kwargs.get("database")
            if cluster and database:
                self.current_connection = self._create_connection(
                    cluster=cluster, database=database
                )
        if not self.current_connection and query_source:
            # try to get cluster and db from query_source metadata
            cluster = cluster or query_source.metadata.get("cluster")
            data_families = query_source.metadata.get("data_families")
            if not isinstance(data_families, list) or len(data_families) == 0:
                # call create connection so that we throw an informative error
                self._create_connection(cluster=cluster, database=database)
            if "." in data_families[0]:  # type: ignore
                _, qry_db = data_families[0].split(".", maxsplit=1)  # type: ignore
            else:
                # Not expected but we can still use a DB value with no dot
                qry_db = data_families[0]  # type: ignore
            database = database or qry_db
            self.current_connection = self._create_connection(
                cluster=cluster, database=database
            )
        data, result = self.query_with_results(query)
        return data if data is not None else result

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
                "    cluster: cluster_uri",
                "    TenantId: tenant_uuid",
                "    ClientId: tenant_uuid",
                "    ClientSecret: (string|KeyVault|EnvironmentVar:)",
                title="Unknown cluster.",
            )
        return self._create_connection_str(cluster, database)

    def _create_connection_str(self, cluster: str, database: str) -> Optional[str]:
        """Return connection string with client secret added."""
        fmt_items = self._kusto_settings.get(cluster.casefold())
        if not fmt_items:
            return None
        fmt_items["database"] = database
        # Note, we don't add the secret until required at runtime to prevent
        # it hanging around in memory as much as possible.
        fmt_items["clientsecret"] = fmt_items["args"].get("ClientSecret")  # type: ignore
        return _KCS_TEMPLATE.format(**fmt_items)


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
            "tenant": settings.args.get("TenantId"),  # type: ignore
            "clientid": settings.args.get("ClientId"),  # type: ignore
            "args": settings.args,
            "cluster": cluster,
            "alias": instance,
        }
    return kusto_settings
