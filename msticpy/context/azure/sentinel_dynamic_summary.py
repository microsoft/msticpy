# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Sentinel Dynamic Summary Mixin class."""
from __future__ import annotations

import logging
from functools import singledispatchmethod
from typing import TYPE_CHECKING, Any, Callable, Iterable

import httpx
from typing_extensions import Self

from msticpy.context.azure.sentinel_utils import SentinelUtilsMixin

from ..._version import VERSION
from ...common.exceptions import MsticpyAzureConnectionError, MsticpyParameterError
from ...common.pkg_config import get_config, get_http_timeout
from ...data.core.data_providers import QueryProvider
from .azure_data import get_api_headers
from .sentinel_dynamic_summary_types import (
    DynamicSummary,
    DynamicSummaryItem,
    df_to_dynamic_summary,
)

if TYPE_CHECKING:
    from datetime import datetime

    import pandas as pd


__version__ = VERSION
__author__ = "Ian Hellen"

_DYN_SUM_API_VERSION = "2023-03-01-preview"

logger: logging.Logger = logging.getLogger(__name__)


class SentinelDynamicSummaryMixin(SentinelUtilsMixin):
    """Mixin class with Sentinel Dynamic Summary integrations."""

    # expose these methods as members of the Sentinel class.
    df_to_dynamic_summary: Callable[
        ...,
        DynamicSummary,
    ] = DynamicSummary.df_to_dynamic_summary
    df_to_dynamic_summaries: Callable[
        ...,
        list[DynamicSummary],
    ] = DynamicSummary.df_to_dynamic_summaries

    @classmethod
    def new_dynamic_summary(  # pylint:disable=too-many-arguments # noqa: PLR0913
        cls: type[Self],
        summary_id: str | None = None,
        name: str | None = None,
        description: str | None = None,
        tenant_id: str | None = None,
        azure_tenant_id: str | None = None,
        search_key: str | None = None,
        tactics: str | list[str] | None = None,
        techniques: str | list[str] | None = None,
        source_info: dict[str, Any] | None = None,
        summary_items: (
            pd.DataFrame | Iterable[DynamicSummaryItem] | list[dict[str, Any]] | None
        ) = None,
    ) -> DynamicSummary:
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
        return DynamicSummary.new_dynamic_summary(
            summary_id=summary_id,
            summary_name=name,
            summary_description=description,
            tenant_id=tenant_id,
            azure_tenant_id=azure_tenant_id,
            search_key=search_key,
            tactics=tactics,
            techniques=techniques,
            source_info=source_info,
            summary_items=summary_items,
        )

    def list_dynamic_summaries(self: Self) -> pd.DataFrame:
        """
        Return current list of Dynamic Summaries from a Sentinel workspace.

        Returns
        -------
        pd.DataFrame
            The current Dynamic Summary objects.

        """
        return self._list_items(
            item_type="dynamic_summary",
            api_version=_DYN_SUM_API_VERSION,
        )

    def get_dynamic_summary(
        self: Self,
        summary_id: str,
        *,
        summary_items: bool = False,
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
            if not self.sent_data_query:
                try:
                    self.sent_data_query: (
                        SentinelQueryProvider | None
                    ) = SentinelQueryProvider(
                        self.default_workspace_name,  # type: ignore[attr-defined]
                    )
                    logger.info(
                        "Created sentinel query provider for %s",
                        self.default_workspace_name,  # type: ignore[attr-defined]
                    )
                except LookupError:
                    logging.info(
                        "Unable to find default workspace."
                        "Use 'sentinel.set_default_workspace(workspace='my_ws_name' "
                        "and retry.",
                    )
            if self.sent_data_query:
                logger.info("Query dynamic summary for %s", summary_id)
                return df_to_dynamic_summary(
                    self.sent_data_query.get_dynamic_summary(summary_id),
                )

        dyn_sum_url = self.sent_urls["dynamic_summary"] + f"/{summary_id}"
        params = {"api-version": _DYN_SUM_API_VERSION}
        if not self._token:
            err_msg = "Token not found, can't get dynamic summary."
            raise ValueError(err_msg)
        response = httpx.get(
            dyn_sum_url,
            headers=get_api_headers(self._token),
            params=params,
            timeout=get_http_timeout(),
        )
        if response.is_success:
            logger.info("Query API for summary id %s", summary_id)
            return DynamicSummary.from_json(response.json())
        logger.info(
            "API query unsuccessful - status %d, response: %s",
            response.status_code,
            response.content.decode("utf-8"),
        )
        raise MsticpyAzureConnectionError(response.json())

    def create_dynamic_summary(  # pylint:disable=too-many-arguments #noqa: PLR0913
        self: Self,
        summary: DynamicSummary | None = None,
        name: str | None = None,
        description: str | None = None,
        data: pd.DataFrame | None = None,
        *,
        summary_id: str | None = None,
        tenant_id: str | None = None,
        azure_tenant_id: str | None = None,
        search_key: str | None = None,
        tactics: str | list[str] | None = None,
        techniques: str | list[str] | None = None,
        source_info: dict[str, Any] | None = None,
    ) -> str | None:
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
        summary_id: str | None
            Id of the summary object
        tenant_id: str | None
            Tenant Id of the Sentinel workspace
        azure_tenant_id: str | None
            Tenant Id of the Sentinel workspace
        search_key : str, optional
            Search key for the entire summary, by default None
        tactics : Union[str, List[str], None], optional
            Relevant MITRE tactics, by default None
        techniques : Union[str, List[str], None], optional
            Relevant MITRE techniques, by default None
        source_info : str, optional
            Summary source info, by default None

        Returns
        -------
        Optional[str]
            The name/ID of the dynamic summary.

        Raises
        ------
        MsticpyAzureConnectionError
            If API returns an error.

        """
        if summary is not None:
            if not summary.summary_name:
                err_msg: str = "DynamicSummary must have unique `summary_name`."
                raise MsticpyParameterError(
                    err_msg,
                    parameters="summary_name",
                )
            return self._create_dynamic_summary(summary)
        # pylint: disable=unexpected-keyword-arg
        if not name:
            err_msg = "DynamicSummary must have unique name"
            raise MsticpyParameterError(
                err_msg,
                parameters="name",
            )
        logger.info("create_dynamic_summary %s (%s)", name, description)
        return self._create_dynamic_summary(
            name,
            description=description,
            data=data,
            summary_id=summary_id,
            tenant_id=tenant_id,
            azure_tenant_id=azure_tenant_id,
            search_key=search_key,
            tactics=tactics,
            techniques=techniques,
            source_info=source_info,
        )

    @singledispatchmethod
    def _create_dynamic_summary(
        self: Self,
        summary: DynamicSummary,
    ) -> str | None:
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
        self.check_connected()
        dyn_sum_url = "/".join([self.sent_urls["dynamic_summary"], summary.summary_id])

        params: dict[str, str] = {"api-version": _DYN_SUM_API_VERSION}
        if not self._token:
            err_msg: str = "Token not found, can't create dynamic summary."
            raise ValueError(err_msg)
        response: httpx.Response = httpx.put(
            dyn_sum_url,
            headers=get_api_headers(self._token),
            params=params,
            content=summary.to_json_api(),
            timeout=get_http_timeout(),
        )
        logger.info(
            "_create_dynamic_summary (DynamicSummary) status %d",
            response.status_code,
        )
        if response.is_success:
            logger.info("Dynamic summary created/updated.")
            return response.json().get("name")
        logger.warning(
            "_create_dynamic_summary (DynamicSummary) failure %s",
            response.content.decode("utf-8"),
        )
        err_msg = (
            f"Dynamic summary create/update failed with status {response.status_code}"
        )
        raise MsticpyAzureConnectionError(
            err_msg,
            "Text response:",
            response.text,
        )

    @_create_dynamic_summary.register(str)
    def _(  # pylint:disable=too-many-arguments # noqa: PLR0913
        self: Self,
        name: str,
        description: str,
        data: pd.DataFrame,
        summary_id: str | None = None,
        tenant_id: str | None = None,
        azure_tenant_id: str | None = None,
        search_key: str | None = None,
        tactics: str | list[str] | None = None,
        techniques: str | list[str] | None = None,
        source_info: dict[str, Any] | None = None,
    ) -> str | None:
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
        summary_id: str | None
            Id of the summary object
        tenant_id: str | None
            Tenant Id of the Sentinel workspace
        azure_tenant_id: str | None
            Tenant Id of the Sentinel workspace
        search_key : str, optional
            Search key for the entire summary, by default None
        tactics : Union[str, List[str], None], optional
            Relevant MITRE tactics, by default None
        techniques : Union[str, List[str], None], optional
            Relevant MITRE techniques, by default None
        source_info : str, optional
            Summary source info, by default None

        Returns
        -------
        Optional[str]
            The name/ID of the dynamic summary.

        Raises
        ------
        MsticpyAzureConnectionError
            If API returns an error.

        """
        self.check_connected()
        summary = DynamicSummary(
            summary_name=name,
            summary_description=description,
            summary_items=data,
            summary_id=summary_id,
            tenant_id=tenant_id,
            azure_tenant_id=azure_tenant_id,
            search_key=search_key,
            tactics=tactics,
            techniques=techniques,
            source_info=source_info,
        )
        logger.info(
            "_create_dynamic_summary (DF) rows: %d",
            len(data) if data is not None else 0,
        )
        return self.create_dynamic_summary(summary)

    def delete_dynamic_summary(
        self: Self,
        summary_id: str,
    ) -> None:
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
        self.check_connected()

        dyn_sum_url = f"{self.sent_urls['dynamic_summary']}/{summary_id}"
        params = {"api-version": _DYN_SUM_API_VERSION}
        if not self._token:
            err_msg: str = "Token not found, can't delete dynamic summary."
            raise ValueError(err_msg)
        response = httpx.delete(
            dyn_sum_url,
            headers=get_api_headers(self._token),
            params=params,
            timeout=get_http_timeout(),
        )
        logger.info(
            "delete_dynamic_summary %s - status %d",
            summary_id,
            response.status_code,
        )
        if response.is_success:
            logger.info("Dynamic summary deleted.")
            return response.json().get("name")
        logger.warning(
            "delete_dynamic_summary failure %s",
            response.content.decode("utf-8"),
        )
        err_msg = f"Dynamic summary deletion failed with status {response.status_code}"
        raise MsticpyAzureConnectionError(
            err_msg,
            "Text response:",
            response.text,
        )

    def update_dynamic_summary(  # pylint:disable=too-many-arguments # noqa:PLR0913
        self: Self,
        summary: DynamicSummary | None = None,
        summary_id: str | None = None,
        data: pd.DataFrame | None = None,
        *,
        name: str | None = None,
        description: str | None = None,
        tenant_id: str | None = None,
        azure_tenant_id: str | None = None,
        search_key: str | None = None,
        tactics: str | list[str] | None = None,
        techniques: str | list[str] | None = None,
        source_info: dict[str, Any] | None = None,
    ) -> str | None:
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
                Collection of summary items, by default
        tenant_id: str | None
            Tenant Id of the Sentinel workspace
        azure_tenant_id: str | None
            Tenant Id of the Sentinel workspace

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
            err_msg: str = "You must supply a summary ID to update"
            raise MsticpyParameterError(
                err_msg,
                parameters="summary_id",
            )
        logger.info(
            "update_dynamic_summary summary %s, df %s",
            summary is not None,
            data is not None,
        )
        return self.create_dynamic_summary(
            summary=summary,
            data=data,
            name=name,
            description=description,
            summary_id=summary_id,
            tenant_id=tenant_id,
            azure_tenant_id=azure_tenant_id,
            search_key=search_key,
            tactics=tactics,
            techniques=techniques,
            source_info=source_info,
        )


class SentinelQueryProvider:
    """Class to encapsulate MS Sentinel data queries."""

    _SINGLE_SUMMARY = """
    DynamicSummary
    | where SummaryId == "{summary_id}"
    | where SummaryStatus == "Active" or SummaryDataType == "SummaryItem"
    """

    def __init__(self: SentinelQueryProvider, workspace: str) -> None:
        """Initialize Sentinel Provider."""
        workspaces = get_config("AzureSentinel.Workspaces", {})
        self.workspace_config = ""
        self.workspace_alias = ""
        for ws_name, workspace_conf in workspaces.items():
            if workspace.casefold() in [
                ws_name.casefold(),
                workspace_conf.get("WorkspaceName", "").casefold(),
            ]:
                self.workspace_config = workspace_conf
                self.workspace_alias = ws_name
                logger.info("Found workspace config %s", ws_name)
                break
        else:
            err_msg: str = f"Cannot find workspace configuration for {workspace}"
            raise LookupError(err_msg)

        self.qry_prov = QueryProvider("MSSentinel")
        self.qry_prov.connect(workspace=self.workspace_alias)

    def get_dynamic_summary(self: Self, summary_id: str) -> pd.DataFrame:
        """Retrieve dynamic summary from MS Sentinel table."""
        logger.info("Dynamic summary query for %s", summary_id)
        return self.qry_prov.MSSentinel.get_dynamic_summary_by_id(summary_id=summary_id)

    def get_dynamic_summaries(
        self: Self,
        start: datetime,
        end: datetime,
    ) -> pd.DataFrame:
        """Return dynamic summaries for date range."""
        logger.info(
            "Dynamic summary query for dynamic summaries from %s to %s",
            start.isoformat(),
            end.isoformat(),
        )
        return self.qry_prov.MSSentinel.list_dynamic_summaries(start=start, end=end)
