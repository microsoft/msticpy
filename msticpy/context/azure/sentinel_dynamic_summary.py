# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Sentinel Dynamic Summary Mixin class."""

from datetime import datetime
from functools import singledispatchmethod
from typing import Optional

import httpx
import pandas as pd

from ..._version import VERSION
from ...common.exceptions import MsticpyAzureConnectionError, MsticpyParameterError
from ...common.pkg_config import get_config, get_http_timeout
from ...data import QueryProvider
from .azure_data import get_api_headers

# pylint: disable=unused-import
from .sentinel_dynamic_summary_types import (  # noqa: F401
    DynamicSummary,
    DynamicSummaryItem,
    df_to_dynamic_summary,
)

__version__ = VERSION
__author__ = "Ian Hellen"

_DYN_SUM_API_VERSION = "2023-03-01-preview"


class SentinelDynamicSummaryMixin:
    """Mixin class with Sentinel Dynamic Summary integrations."""

    # expose these methods as members of the Sentinel class.
    df_to_dynamic_summary = DynamicSummary.df_to_dynamic_summary
    df_to_dynamic_summaries = DynamicSummary.df_to_dynamic_summaries

    @classmethod
    def new_dynamic_summary(cls, **kwargs):
        """
        Return a new DynamicSummary object.

        Notes
        -----
        See the DynamicSummary class documentation for details
        of expected parameters.

        See Also
        --------
        DynamicSummary

        """
        return DynamicSummary.new_dynamic_summary(**kwargs)

    def list_dynamic_summaries(self) -> pd.DataFrame:
        """
        Return current list of Dynamic Summaries from a Sentinel workspace.

        Returns
        -------
        pd.DataFrame
            The current Dynamic Summary objects.

        """
        return self._list_items(  # type: ignore
            item_type="dynamic_summary", api_version=_DYN_SUM_API_VERSION
        )

    def get_dynamic_summary(
        self, summary_id: str, summary_items=False
    ) -> DynamicSummary:
        """
        Return DynamicSummary for ID.

        Parameters
        ----------
        summary_id : str
            The ID of the Dynamic summary object.
        summary_items : bool, optional
            Use a data query to retrieve the dynamic summary along
            with summary items (data records), by default, false.

        Returns
        -------
        DynamicSummary:
            DynamicSummary object.

        Raises
        ------
        MsticpyAzureConnectionError
            If API returns an error.

        """
        if summary_items:
            if not self.sent_data_query:  # type: ignore
                try:
                    self.sent_data_query = SentinelQueryProvider(
                        self.default_workspace_name  # type: ignore
                    )
                except LookupError:
                    print(
                        "Unable to find default workspace.",
                        "Use 'sentinel.set_default_workspace(workspace='my_ws_name'",
                        "and retry.",
                    )
            if self.sent_data_query:
                return df_to_dynamic_summary(
                    self.sent_data_query.get_dynamic_summary(summary_id)
                )

        dyn_sum_url = self.sent_urls["dynamic_summary"] + f"/{summary_id}"  # type: ignore

        params = {"api-version": _DYN_SUM_API_VERSION}
        response = httpx.get(
            dyn_sum_url,
            headers=get_api_headers(self.token),  # type: ignore
            params=params,
            timeout=get_http_timeout(),
        )
        if response.status_code == 200:
            return DynamicSummary.from_json(response.json())
        raise MsticpyAzureConnectionError(response.json())

    def create_dynamic_summary(
        self,
        summary: Optional[DynamicSummary] = None,
        name: Optional[str] = None,
        description: Optional[str] = None,
        data: Optional[pd.DataFrame] = None,
        **kwargs,
    ) -> Optional[str]:
        """
        Create a Dynamic Summary in the Sentinel Workspace.

        Parameters
        ----------
        summary : DynamicSummary
            DynamicSummary instance.
        name : str
            The name of the dynamic summary to create
        description : str
            Dynamic Summary description
        data : pd.DataFrame
            The summary data

        Returns
        -------
        Optional[str]
            The name/ID of the dynamic summary.

        Raises
        ------
        MsticpyAzureConnectionError
            If API returns an error.

        """
        if summary:
            if not summary.summary_name:
                raise MsticpyParameterError(
                    "DynamicSummary must have unique `summary_name`.",
                    parameters="summary_name",
                )
            return self._create_dynamic_summary(summary)
        # pylint: disable=unexpected-keyword-arg
        if not name:
            raise MsticpyParameterError(
                "DynamicSummary must have unique name", parameters="name"
            )
        return self._create_dynamic_summary(
            name, description=description, data=data, **kwargs
        )

    @singledispatchmethod
    def _create_dynamic_summary(
        self,
        summary: DynamicSummary,
    ) -> Optional[str]:
        """
        Create a Dynamic Summary in the Sentinel Workspace.

        Parameters
        ----------
        summary : DynamicSummary
            DynamicSummary instance.

        Returns
        -------
        Optional[str]
            The name/ID of the dynamic summary.

        Raises
        ------
        MsticpyAzureConnectionError
            If API returns an error.

        """
        self.check_connected()  # type: ignore
        dyn_sum_url = "/".join(
            [self.sent_urls["dynamic_summary"], summary.summary_id]  # type: ignore
        )

        params = {"api-version": _DYN_SUM_API_VERSION}
        response = httpx.put(
            dyn_sum_url,
            headers=get_api_headers(self.token),  # type: ignore
            params=params,
            content=summary.to_json_api(),
            timeout=get_http_timeout(),
        )
        if response.status_code in (200, 201):
            print("Dynamic summary created/updated.")
            return response.json().get("name")
        raise MsticpyAzureConnectionError(
            (
                "Dynamic summary create/update failed with status",
                str(response.status_code),
            ),
            "Text response:",
            response.text,
        )

    @_create_dynamic_summary.register
    def _(
        self, name: str, description: str, data: pd.DataFrame, **kwargs
    ) -> Optional[str]:
        """
        Create a Dynamic Summary in the Sentinel Workspace.

        Parameters
        ----------
        name : str
            The name of the dynamic summary to create
        description : str
            Dynamic Summary description
        data : pd.DataFrame
            The summary data

        Other Parameters
        ----------------
        relation_name : str, optional
            The relation name, by default None
        relation_id : str, optional
            The relation ID, by default None
        search_key : str, optional
            Search key for the entire summary, by default None
        tactics : Union[str, List[str], None], optional
            Relevant MITRE tactics, by default None
        techniques : Union[str, List[str], None], optional
            Relevant MITRE techniques, by default None
        source_info : str, optional
            Summary source info, by default None
        summary_items : Union[pd, DataFrame, Iterable[DynamicSummaryItem],
        List[Dict[str, Any]]], optional
            Collection of summary items, by default None

        Returns
        -------
        Optional[str]
            The name/ID of the dynamic summary.

        Raises
        ------
        MsticpyAzureConnectionError
            If API returns an error.

        """
        self.check_connected()  # type: ignore
        summary = DynamicSummary(
            summary_name=name,
            summary_description=description,
            summary_items=data,
            **kwargs,
        )
        return self.create_dynamic_summary(summary)

    def delete_dynamic_summary(
        self,
        summary_id: str,
    ):
        """
        Delete the Dynamic Summary for `summary_id`.

        Parameters
        ----------
        summary_id: str, optional
            The UUID of the summary to delete.

        Raises
        ------
        MsticpyAzureConnectionError
            If the API returns an error.

        """
        self.check_connected()  # type: ignore

        dyn_sum_url = f"{self.sent_urls['dynamic_summary']}/{summary_id}"  # type: ignore
        params = {"api-version": _DYN_SUM_API_VERSION}
        response = httpx.delete(
            dyn_sum_url,
            headers=get_api_headers(self.token),  # type: ignore
            params=params,
            timeout=get_http_timeout(),
        )
        if response.status_code == 200:
            print("Dynamic summary deleted.")
            return response.json().get("name")
        raise MsticpyAzureConnectionError(
            f"Dynamic summary deletion failed with status {response.status_code}",
            "Text response:",
            response.text,
        )

    def update_dynamic_summary(
        self,
        summary: Optional[DynamicSummary] = None,
        summary_id: Optional[str] = None,
        data: Optional[pd.DataFrame] = None,
        **kwargs,
    ):
        """
        Update a dynamic summary in the Sentinel Workspace.

        Parameters
        ----------
        summary : DynamicSummary
            DynamicSummary instance.
        summary_id : str
            The ID of the summary to update.
        data : pd.DataFrame
            The summary data

        Other Parameters
        ----------------
        name : str
            The name of the dynamic summary to create
        description : str
            Dynamic Summary description
        relation_name : str, optional
            The relation name, by default None
        relation_id : str, optional
            The relation ID, by default None
        search_key : str, optional
            Search key for the entire summary, by default None
        tactics : Union[str, List[str], None], optional
            Relevant MITRE tactics, by default None
        techniques : Union[str, List[str], None], optional
            Relevant MITRE techniques, by default None
        source_info : str, optional
            Summary source info, by default None
        summary_items : Union[pd, DataFrame, Iterable[DynamicSummaryItem],
        List[Dict[str, Any]]], optional
            Collection of summary items, by default None

        Returns
        -------
        Optional[str]
            The name/ID of the dynamic summary.

        Raises
        ------
        MsticpyParameterError
            If existing summary_id not supplied.
        MsticpyAzureConnectionError
            If API returns an error.

        """
        if (summary and not summary.summary_id) or (
            data is not None and not summary_id
        ):
            raise MsticpyParameterError(
                "You must supply a summary ID to update", parameters="summary_id"
            )
        return self.create_dynamic_summary(
            summary=summary, data=data, summary_id=summary_id, **kwargs
        )


class SentinelQueryProvider:
    """Class to encapsulate MS Sentinel data queries."""

    _SINGLE_SUMMARY = """
    DynamicSummary
    | where SummaryId == "{summary_id}"
    | where SummaryStatus == "Active" or SummaryDataType == "SummaryItem"
    """

    def __init__(self, workspace: str):
        """Initialize Sentinel Provider."""
        workspaces = get_config("AzureSentinel.Workspaces")
        self.workspace_config = ""
        self.workspace_alias = ""
        for ws_name, workspace_conf in workspaces.items():
            if workspace.casefold() in [
                ws_name.casefold(),
                workspace_conf.get("WorkspaceName", "").casefold(),
            ]:
                self.workspace_config = workspace_conf
                self.workspace_alias = ws_name
                break
        else:
            raise LookupError(f"Cannot find workspace configuration for {workspace}")

        self.qry_prov = QueryProvider("MSSentinel")
        self.qry_prov.connect(workspace=self.workspace_alias)

    def get_dynamic_summary(self, summary_id) -> pd.DataFrame:
        """Retrieve dynamic summary from MS Sentinel table."""
        return self.qry_prov.MSSentinel.get_dynamic_summary_by_id(summary_id=summary_id)

    def get_dynamic_summaries(self, start: datetime, end: datetime) -> pd.DataFrame:
        """Return dynamic summaries for date range."""
        return self.qry_prov.MSSentinel.list_dynamic_summaries(start=start, end=end)
