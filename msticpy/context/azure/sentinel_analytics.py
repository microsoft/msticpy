# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Mixin Classes for Sentinel Analytics Features."""
from __future__ import annotations

import logging
from typing import Any, Callable
from uuid import UUID, uuid4

import httpx
import pandas as pd
from azure.common.exceptions import CloudError
from IPython.display import display
from typing_extensions import Self

from ..._version import VERSION
from ...common.exceptions import MsticpyUserError
from .azure_data import get_api_headers
from .sentinel_utils import (
    SentinelUtilsMixin,
    extract_sentinel_response,
    get_http_timeout,
)

__version__ = VERSION
__author__ = "Pete Bryan"

logger: logging.Logger = logging.getLogger(__name__)


class SentinelHuntingMixin(SentinelUtilsMixin):
    """Mixin class for Sentinel Hunting feature integrations."""

    def list_hunting_queries(self: Self) -> pd.DataFrame:
        """
        Return all custom hunting queries in a Microsoft Sentinel workspace.

        Returns
        -------
        pd.DataFrame
            A table of the custom hunting queries.

        """
        saved_query_df: pd.DataFrame = self._list_items(
            item_type="ss_path",
            api_version="2020-08-01",
        )
        return saved_query_df[
            saved_query_df["properties.category"] == "Hunting Queries"
        ]

    get_hunting_queries: Callable[..., pd.DataFrame] = list_hunting_queries

    def list_saved_queries(self: Self) -> pd.DataFrame:
        """
        Return all saved queries in a Microsoft Sentinel workspace.

        Returns
        -------
        pd.DataFrame
            A table of the custom hunting queries.

        """
        saved_query_df: pd.DataFrame = self._list_items(
            item_type="ss_path",
            api_version="2020-08-01",
        )
        return saved_query_df


class SentinelAnalyticsMixin(SentinelUtilsMixin):
    """Mixin class for Sentinel Analytics feature integrations."""

    def list_alert_rules(self: Self) -> pd.DataFrame:
        """
        Return all Microsoft Sentinel alert rules for a workspace.

        Returns
        -------
        pd.DataFrame
            A table of the workspace's alert rules.

        """
        return self._list_items(
            item_type="alert_rules",
            api_version="2024-01-01-preview",
        )

    def _get_template_id(
        self: Self,
        template: str,
    ) -> str:
        """
        Get an analytic template ID.

        Parameters
        ----------
        template : str
            Template ID or Name
        res_id : str
            Sentinel workspace to get template from

        Returns
        -------
        str
            Template ID

        Raises
        ------
        MsticpyUserError
            If template not found or multiple templates found.\

        """
        try:
            UUID(template)
        except ValueError as template_name:
            templates: pd.DataFrame = self.list_analytic_templates()
            template_details: pd.DataFrame = templates[
                templates["properties.displayName"].str.contains(template)
            ]
            if len(template_details) > 1:
                display(template_details[["name", "properties.displayName"]])
                err_msg: str = "More than one template found, please specify by GUID"
                raise MsticpyUserError(err_msg) from template_name
            if not isinstance(template_details, pd.DataFrame) or template_details.empty:
                err_msg = f"Template {template_details} not found"
                raise MsticpyUserError(err_msg) from template_name
            return template_details["name"].iloc[0]
        return template

    def create_analytic_rule(  # pylint: disable=too-many-arguments, too-many-locals #noqa:PLR0913
        self: Self,
        template: str | None = None,
        name: str | None = None,
        *,
        enabled: bool = True,
        query: str | None = None,
        query_frequency: str = "PT5H",
        query_period: str = "PT5H",
        severity: str = "Medium",
        suppression_duration: str = "PT1H",
        suppression_enabled: bool = False,
        trigger_operator: str = "GreaterThan",
        trigger_threshold: int = 0,
        description: str | None = None,
        tactics: list[str] | None = None,
    ) -> str | None:
        """
        Create a Sentinel Analytics Rule.

        Parameters
        ----------
        template : str, optional
            The GUID or name of a templated to create the analytic from, by default None
        name : str, optional
            The name to give the analytic, by default None
        enabled : bool, optional
            Whether you want the analytic to be enabled once deployed, by default True
        query : str, optional
            The KQL query string to use in the anlaytic, by default None
        query_frequency : str, optional
            How often the query should run in ISO8601 format, by default "PT5H"
        query_period : str, optional
            How far back the query should look in ISO8601 format, by default "PT5H"
        severity : str, optional
            The severity to raise incidents as, by default "Medium"
            Options are; Informational, Low, Medium, or High
        suppression_duration : str, optional
            How long to suppress duplicate alerts in ISO8601 format, by default "PT1H"
        suppression_enabled : bool, optional
            Whether you want to suppress duplicates, by default False
        trigger_operator : str, optional
            The operator for the trigger, by default "GreaterThan"
        trigger_threshold : int, optional
            The threshold of events required to create the incident, by default 0
        description : str, optional
            A description of the analytic, by default None
        tactics : list, optional
            A list of MITRE ATT&CK tactics related to the analytic, by default None

        Returns
        -------
        str|None
            The name/ID of the analytic rule.

        Raises
        ------
        MsticpyUserError
            If template provided isn't found.
        CloudError
            If the API returns an error.

        """
        self.check_connected()
        if template:
            template_id: str = self._get_template_id(template)
            templates: pd.DataFrame = self.list_analytic_templates()
            template_details: pd.Series = templates[
                templates["name"] == template_id
            ].iloc[0]
            name = template_details["properties.displayName"]
            query = template_details["properties.query"]
            query_frequency = template_details["properties.queryFrequency"]
            query_period = template_details["properties.queryPeriod"]
            severity = template_details["properties.severity"]
            trigger_operator = template_details["properties.triggerOperator"]
            trigger_threshold = template_details["properties.triggerThreshold"]
            description = template_details["properties.description"]
            tactics = (
                template_details["properties.tactics"]
                if not pd.isna(template_details["properties.tactics"])
                else []
            )

        if not tactics:
            tactics = []

        if not name:
            err_msg: str = "Please specify either a template ID or analytic details."
            raise MsticpyUserError(err_msg)

        rule_id: UUID = uuid4()
        analytic_url: str = self.sent_urls["alert_rules"] + f"/{rule_id}"
        data_items: dict[str, Any] = {
            "displayName": name,
            "query": query,
            "queryFrequency": query_frequency,
            "queryPeriod": query_period,
            "severity": severity,
            "suppressionDuration": suppression_duration,
            "suppressionEnabled": str(suppression_enabled).lower(),
            "triggerOperator": trigger_operator,
            "triggerThreshold": trigger_threshold,
            "description": description,
            "tactics": tactics,
            "enabled": str(enabled).lower(),
        }
        data: dict[str, Any] = extract_sentinel_response(data_items, props=True)
        data["kind"] = "Scheduled"
        params: dict[str, str] = {"api-version": "2020-01-01"}
        if not self._token:
            err_msg = "Token not found, can't create analytic rule."
            raise ValueError(err_msg)
        response: httpx.Response = httpx.put(
            analytic_url,
            headers=get_api_headers(self._token),
            params=params,
            content=str(data),
            timeout=get_http_timeout(),
        )
        if not response.is_success:
            raise CloudError(response=response)
        logger.info("Analytic Created.")
        return response.json().get("name")

    def _get_analytic_id(self: Self, analytic: str) -> str:
        """
        Get the GUID of an analytic rule.

        Parameters
        ----------
        analytic : str
            The GUID or name of the analytic

        Returns
        -------
        str
            The analytic GUID

        Raises
        ------
        MsticpyUserError
            If analytic not found or multiple matching analytics found

        """
        try:
            UUID(analytic)
        except ValueError as analytic_name:
            analytics: pd.DataFrame = self.list_analytic_rules()
            analytic_details: pd.DataFrame = analytics[
                analytics["properties.displayName"].str.contains(analytic)
            ]
            if len(analytic_details) > 1:
                display(analytic_details[["name", "properties.displayName"]])
                err_msg: str = "More than one analytic found, please specify by GUID"
                raise MsticpyUserError(err_msg) from analytic_name
            if not isinstance(analytic_details, pd.DataFrame) or analytic_details.empty:
                err_msg = f"Analytic {analytic_details} not found"
                raise MsticpyUserError(err_msg) from analytic_name
            return analytic_details["name"].iloc[0]
        return analytic

    def delete_analytic_rule(
        self: Self,
        analytic_rule: str,
    ) -> None:
        """
        Delete a deployed Analytic rule from a Sentinel workspace.

        Parameters
        ----------
        analytic_rule : str
            The GUID or name of the analytic.

        Raises
        ------
        CloudError
            If the API returns an error.

        """
        self.check_connected()
        analytic_id: str = self._get_analytic_id(analytic_rule)
        analytic_url: str = self.sent_urls["alert_rules"] + f"/{analytic_id}"
        params: dict[str, str] = {"api-version": "2020-01-01"}
        if not self._token:
            err_msg: str = "Token not found, can't delete analytic rule."
            raise ValueError(err_msg)
        response: httpx.Response = httpx.delete(
            analytic_url,
            headers=get_api_headers(self._token),
            params=params,
            timeout=get_http_timeout(),
        )
        if response.is_error:
            raise CloudError(response=response)
        logger.info("Analytic Deleted.")

    def list_analytic_templates(self) -> pd.DataFrame:
        """
        List Analytic Templates.

        Returns
        -------
        pd.DataFrame
            A DataFrame containing the analytics templates

        Raises
        ------
        CloudError
            If a valid result is not returned.

        """
        return self._list_items(item_type="alert_template")

    get_alert_rules: Callable[..., pd.DataFrame] = list_alert_rules
    list_analytic_rules: Callable[..., pd.DataFrame] = list_alert_rules
    get_analytic_rules: Callable[..., pd.DataFrame] = list_alert_rules
