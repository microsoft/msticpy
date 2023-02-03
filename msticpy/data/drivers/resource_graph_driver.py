# -------------------------------------------------------------------------
# Copyright (c) SecureWorks. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Azure Resource Graph Driver class."""
import warnings
from typing import Any, Tuple, Union

import pandas as pd

# pylint: disable=wrong-import-order, ungrouped-imports
from azure.mgmt.subscription import SubscriptionClient
from pandas.core.frame import DataFrame

from ..._version import VERSION
from ...auth.azure_auth import AzureCloudConfig, az_connect, only_interactive_cred
from ...common.exceptions import MsticpyImportExtraError, MsticpyNotConnectedError
from ...common.utility import export
from .driver_base import DriverBase, QuerySource

try:
    from azure.mgmt.resourcegraph import ResourceGraphClient
    from azure.mgmt.resourcegraph.models import (
        QueryRequest,
        QueryRequestOptions,
        QueryResponse,
        ResultFormat,
        ResultTruncated,
    )
except ImportError as imp_err:
    raise MsticpyImportExtraError(
        "Cannot use this feature without azure-mgmt-resourcegraph installed",
        title="Error importing azure-mgmt-resourcegraph",
        extra="azure",
    ) from imp_err
# pylint: enable=wrong-import-order

__version__ = VERSION
__author__ = "Ryan Cobb"


@export
class ResourceGraphDriver(DriverBase):
    """Driver to connect and query from Azure Resource Graph."""

    def __init__(self, **kwargs):
        """Instantiate Azure Resource Graph Driver."""
        super().__init__()
        self.client = None
        self.sub_client = None
        self.subscription_ids = None
        self._connected = False
        self._debug = kwargs.get("debug", False)
        self.az_cloud_config = AzureCloudConfig(cloud=kwargs.get("cloud"))

    def connect(self, connection_str: str = None, **kwargs):
        """
        Connect to Azure Resource Graph via Azure SDK.

        Parameters
        ----------
        connection_str : Optional[str], optional
            Not used.

        Other Parameters
        ----------------
        kwargs :
            Connection parameters can be supplied as keyword parameters.

        Notes
        -----
        Default configuration is read from the DataProviders/AzureCLI
        section of msticpyconfig.yaml, if available.

        """
        auth_methods = kwargs.get("auth_methods")
        auth_methods = auth_methods or self.az_cloud_config.auth_methods
        silent = kwargs.get("silent", True)

        credentials = az_connect(auth_methods=auth_methods, silent=silent)
        if only_interactive_cred(credentials.modern):
            print("Check your default browser for interactive sign-in prompt.")
        self.client = ResourceGraphClient(
            credential=credentials.modern,
            base_url=self.az_cloud_config.endpoints.resource_manager,
            credential_scopes=[self.az_cloud_config.token_uri],
        )
        self.sub_client = SubscriptionClient(
            credential=credentials.modern,
            base_url=self.az_cloud_config.endpoints.resource_manager,
            credential_scopes=[self.az_cloud_config.token_uri],
        )
        self.subscription_ids = [
            sub.subscription_id for sub in self.sub_client.subscriptions.list()
        ]

        self._connected = True
        self._loaded = True

        print("Connected")

    def query(
        self, query: str, query_source: QuerySource = None, **kwargs
    ) -> Union[pd.DataFrame, Any]:
        """
        Execute Resource Graph query and retrieve results.

        Parameters
        ----------
        query : str
            KQL query to execute
        query_source : QuerySource
            The query definition object

        Other Parameters
        ----------------
        kwargs :
            count

        Returns
        -------
        Union[pd.DataFrame, Any]
            Query results in a dataframe.
            or query response if an error.

        """
        del query_source
        result_df, result = self.query_with_results(query, **kwargs)
        if isinstance(result_df, DataFrame) and not result_df.empty:
            return result_df

        return result

    def query_with_results(self, query: str, **kwargs) -> Tuple[pd.DataFrame, Any]:
        """
        Execute query string and return DataFrame of results.

        Parameters
        ----------
        query : str
            Query to execute against Resource Graph

        Returns
        -------
        Union[pd.DataFrame,Any]
            A DataFrame (if successful) or
            the underlying provider result if an error occurs.

        """
        if not self.connected:
            self.connect()
        if not self.connected:
            raise MsticpyNotConnectedError(
                "Source is not connected. ", "Please call connect() and retry."
            )

        result_truncated = False

        top = kwargs.get("top", 1000)

        request_options = QueryRequestOptions(
            top=top,
            result_format=ResultFormat.object_array,
        )

        request = QueryRequest(
            query=query,
            subscriptions=self.subscription_ids,
            options=request_options,
        )

        response: QueryResponse = self.client.resources(request)

        # Pagination logic adapted from azure-cli-extensions
        # https://github.com/Azure/azure-cli-extensions/blob/8dade2f6fe28803d0fbdb1700c3ab4e4d71e5318/src/resource-graph/azext_resourcegraph/custom.py#L75

        if response.result_truncated == ResultTruncated.true:
            result_truncated = True

        if result_truncated and top is not None and len(response.data) < top:
            warnings.warn(
                "Unable to paginate the results of the query. "
                "Some resources may be missing from the results. "
                "To rewrite the query and enable paging, "
                "see the docs for an example: https://aka.ms/arg-results-truncated",
            )

        return pd.json_normalize(response.data), response
