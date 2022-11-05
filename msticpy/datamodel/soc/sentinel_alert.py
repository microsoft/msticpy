# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Sentinel Alert class."""
import json
from typing import Any, Dict, List

import pandas as pd

from ...datamodel.entities import Alert

_FEATURES = [
    "AdditionalData",
    "AlertLink",
    "AlertName",
    "AlertSeverity",
    "ProcessingEndTime",
    "ResourceId",
    "SourceComputerId",
    "SourceSystem",
    "Status",
    "Tactics",
    "TenantId",
    "VendorOriginalId",
]

_ID_PROPERTIES: List[str] = [
    "AzSubscriptionId",
    "AzResourceId",
    "WorkspaceId",
    "AgentId",
    "TenantId",
    "SourceComputerId",
    "ResourceId",
    "WorkspaceSubscriptionId",
    "WorkspaceResourceGroup",
    "ProviderAlertId",
    "SystemAlertId",
    "ResourceIdentifiers",
]

_QUERY_PROPERTIES: List[str] = [
    "Query Period",
    "Trigger Operator",
    "Trigger Threshold",
    "Search Query Results Overall Count",
    "Data Sources",
    "Query",
    "Query Start Time UTC",
    "Query End Time UTC",
    "Analytic Rule Ids",
    "Event Grouping",
    "Analytic Rule Name",
]


class SentinelAlert(Alert):
    """Security Alert Class."""

    def __init__(
        self,
        src_event=None,
        src_entity=None,
        **kwargs,
    ):
        """
        Create a new instance of the entity type.

        Parameters
        ----------
        src_entity : Mapping[str, Any], optional
            Create entity from existing entity or
            other mapping object that implements entity properties.
            (the default is None)
        src_event : Mapping[str, Any], optional
            Create entity from event properties such as a Pandas Series
            (the default is None)

        """
        self._custom_query_params: Dict[str, Any] = {}
        super().__init__(src_entity, src_event, **kwargs)
        if (
            isinstance(
                src_event,
                pd.Series,
            )
            and not src_event.empty
        ):
            self._add_sentinel_items(src_event)
            self._add_extended_sent_props()
        self._ids: Dict[str, str] = {}
        if self.__dict__ is not None:
            for id_property in _ID_PROPERTIES:
                if id_property in self.properties:
                    self._ids[id_property] = self.properties[id_property]

    def _add_extended_sent_props(self):
        if self.ExtendedProperties:
            try:
                ext_ents = json.loads(self.ExtendedProperties)
                for item_name, value in ext_ents.items():
                    self.__dict__[item_name] = value
            except json.JSONDecodeError:
                pass
        if self.AdditionalData:
            for item_name, value in self.AdditionalData.items():
                if item_name not in self.__dict__:
                    self.__dict__[item_name] = value

    def _add_sentinel_items(self, src_event):
        for feature in _FEATURES:
            if feature not in self.__dict__:
                self.__dict__.update({feature: src_event.get(feature, "")})

    @property
    def ids(self) -> Dict[str, str]:
        """Return a collection of Identity properties for the alert."""
        return self._ids

    @property
    def is_in_workspace(self) -> bool:
        """Return True if the alert has a Log Analytics WorkspaceID."""
        return "WorkspaceId" in self._ids and "AgentId" in self._ids

    @property
    def is_in_log_analytics(self) -> bool:
        """Return True if the alert originates from a Log Analytics Workspace host."""
        return "TenantId" in self._ids

    @property
    def links(self):
        """Return all links associated with the alert."""
        alert_links = {}
        if "ExtendedLinks" in self.__dict__:
            try:
                links = json.loads(self.__dict__["ExtendedLinks"])
                for link in links:
                    alert_links[link["Label"]] = link["Href"]
            except json.JSONDecodeError:
                pass
        return alert_links

    @property
    def incident(self):
        """Return any incident details associated with the alert."""
        if "InvestigationName" not in self.__dict__:
            self.__dict__["InvestigationName"] = None
        return self.InvestigationName

    @property
    def analytic(self):
        """Return any Sentinel Analytics associated with the alert."""
        return {
            query_property: self.__dict__[query_property]
            for query_property in _QUERY_PROPERTIES
            if query_property in self.__dict__
        }
