# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Mixin Classes for Sentinel Analytics Features."""
from typing import Optional
from uuid import UUID, uuid4

import httpx
import pandas as pd
from azure.common.exceptions import CloudError
from IPython.display import display

from ..._version import VERSION
from ...common.exceptions import MsticpyUserError
from .azure_data import get_api_headers
from .sentinel_utils import _build_sent_data, get_http_timeout

__version__ = VERSION
__author__ = "Pete Bryan"


class SentinelHuntingMixin:
    """Mixin class for Sentinel Hunting feature integrations."""

    def list_hunting_queries(self) -> pd.DataFrame:
        """
        Return all custom hunting queries in a Microsoft Sentinel workspace.

        Returns
        -------
        pd.DataFrame
            A table of the custom hunting queries.

        """
        saved_query_df = self._list_items(  # type: ignore
            item_type="ss_path", api_version="2020-08-01"
        )
        return saved_query_df[
            saved_query_df["properties.category"] == "Hunting Queries"
        ]

    get_hunting_queries = list_hunting_queries

    def list_saved_queries(self) -> pd.DataFrame:
        """
        Return all saved queries in a Microsoft Sentinel workspace.

        Returns
        -------
        pd.DataFrame
            A table of the custom hunting queries.

        """
        saved_query_df = self._list_items(  # type: ignore
            item_type="ss_path", api_version="2020-08-01"
        )
        return saved_query_df

    get_hunting_queries = list_hunting_queries


class SentinelAnalyticsMixin:
    """Mixin class for Sentinel Analytics feature integrations."""

    def list_alert_rules(self) -> pd.DataFrame:
        """
        Return all Microsoft Sentinel alert rules for a workspace.

        Returns
        -------
        pd.DataFrame
            A table of the workspace's alert rules.

        """
        return self._list_items(  # type: ignore
            item_type="alert_rules", api_version="2022-11-01"
        )

    def _get_template_id(
        self,
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
            return template
        except ValueError as template_name:
            templates = self.list_analytic_templates()
            template_details = templates[
                templates["properties.displayName"].str.contains(template)
            ]
            if len(template_details) > 1:
                display(template_details[["name", "properties.displayName"]])
                raise MsticpyUserError(
                    "More than one template found, please specify by GUID"
                ) from template_name
            if not isinstance(template_details, pd.DataFrame) or template_details.empty:
                raise MsticpyUserError(
                    f"Template {template_details} not found"
                ) from template_name
            return template_details["name"].iloc[0]

    def create_analytic_rule(  # pylint: disable=too-many-arguments, too-many-locals
        self,
        template: str = None,
        name: str = None,
        enabled: bool = True,
        query: str = None,
        query_frequency: str = "PT5H",
        query_period: str = "PT5H",
        severity: str = "Medium",
        suppression_duration: str = "PT1H",
        suppression_enabled: bool = False,
        trigger_operator: str = "GreaterThan",
        trigger_threshold: int = 0,
        description: str = None,
        tactics: list = None,
    ) -> Optional[str]:
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
        Optional[str]
            The name/ID of the analytic rule.

        Raises
        ------
        MsticpyUserError
            If template provided isn't found.
        CloudError
            If the API returns an error.

        """
        self.check_connected()  # type: ignore
        if template:
            template_id = self._get_template_id(template)
            templates = self.list_analytic_templates()
            template_details = templates[templates["name"] == template_id].iloc[0]
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
            raise MsticpyUserError(
                "Please specify either a template ID or analytic details."
            )

        rule_id = uuid4()
        analytic_url = self.sent_urls["alert_rules"] + f"/{rule_id}"  # type: ignore
        data_items = {
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
        data = _build_sent_data(data_items, props=True)
        data["kind"] = "Scheduled"
        params = {"api-version": "2020-01-01"}
        response = httpx.put(
            analytic_url,
            headers=get_api_headers(self.token),  # type: ignore
            params=params,
            content=str(data),
            timeout=get_http_timeout(),
        )
        if response.status_code != 201:
            raise CloudError(response=response)
        print("Analytic Created.")
        return response.json().get("name")

    def _get_analytic_id(self, analytic: str) -> str:
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
            return analytic
        except ValueError as analytic_name:
            analytics = self.list_analytic_rules()
            analytic_details = analytics[
                analytics["properties.displayName"].str.contains(analytic)
            ]
            if len(analytic_details) > 1:
                display(analytic_details[["name", "properties.displayName"]])
                raise MsticpyUserError(
                    "More than one analytic found, please specify by GUID"
                ) from analytic_name
            if not isinstance(analytic_details, pd.DataFrame) or analytic_details.empty:
                raise MsticpyUserError(
                    f"Analytic {analytic_details} not found"
                ) from analytic_name
            return analytic_details["name"].iloc[0]

    def delete_analytic_rule(
        self,
        analytic_rule: str,
    ):
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
        self.check_connected()  # type: ignore
        analytic_id = self._get_analytic_id(analytic_rule)
        analytic_url = self.sent_urls["alert_rules"] + f"/{analytic_id}"  # type: ignore
        params = {"api-version": "2020-01-01"}
        response = httpx.delete(
            analytic_url,
            headers=get_api_headers(self.token),  # type: ignore
            params=params,
            timeout=get_http_timeout(),
        )
        if response.status_code != 200:
            raise CloudError(response=response)
        print("Analytic Deleted.")

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
        return self._list_items(item_type="alert_template")  # type: ignore

    get_alert_rules = list_alert_rules
    list_analytic_rules = list_alert_rules
    get_analytic_rules = list_alert_rules
