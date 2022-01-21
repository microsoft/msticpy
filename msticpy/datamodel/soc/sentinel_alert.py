# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Security Alert class."""
import html
import json
import re
from collections import Counter

from typing import Any, Dict, List, Optional, Union
import pandas as pd

from ...data.query_defns import DataEnvironment
from ...datamodel.entities import Entity, Alert

_FEATURES = [
    "AdditionalData",
    "AlertLink",
    "AlertName",
    "AlertSeverity",
    "ConfidenceLevel",
    "ConfidenceScore",
    "Count",
    "Description",
    "ExtendedLinks",
    "ExtendedProperties",
    "IsIncident",
    "ProcessingEndTime",
    "ProductComponentName",
    "ProductName",
    "RemediationSteps",
    "ResourceId",
    "Severity",
    "SourceComputerId",
    "SourceSystem",
    "Status",
    "SystemAlertId",
    "SystemAlertIds",
    "Tactics",
    "Techniques",
    "TenantId",
    "VendorOriginalId",
    "WorkspaceResourceGroup",
    "WorkspaceSubscriptionId",
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
    "ResourceId",
]


class SentinelProps:
    @property
    def properties(self) -> Dict[str, Any]:
        """
        Return a dictionary of the Alert or Event properties.

        Returns
        -------
        Dict[str, Any]
            dictionary of the Alert or Event properties.

        """
        return self.__dict__

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
    def query(self) -> Dict[str, Any]:
        return self._get_query()

    @property
    def data_environment(self) -> DataEnvironment:
        """Return the data environment of the alert for subsequent queries."""
        if self.is_in_log_analytics:
            return DataEnvironment.LogAnalytics
        return DataEnvironment.Kusto

    @property
    def links(self):
        alert_links = {}
        if "ExtendedLinks" in self.__dict__:
            try:
                links = json.loads(self.__dict__["ExtendedLinks"])
                for link in links:
                    alert_links.update({link["Label"]: link["Href"]})
            except:
                pass
        return alert_links

    @property
    def incident(self):
        incident = {}
        if "ExtendedProperties" in self.__dict__:
            try:
                details = json.loads(self.__dict__["ExtendedProperties"])
                if "InvestigationName" in details:
                    incident = details
            except:
                pass
        return incident


class SentinelAlert(Alert, SentinelProps):
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
            self._create_from_ent(src_event)
        self._ids: Dict[str, str] = {}
        if self.__dict__ is not None:
            for id_property in _ID_PROPERTIES:
                if id_property in self.properties:
                    self._ids[id_property] = self.properties[id_property]

    def _create_from_ent(self, src_event):
        for feature in _FEATURES:
            if feature not in self.__dict__:
                self.__dict__.update({feature: src_event.get(feature, "")})

    def _get_query(self):
        if self.__dict__["ExtendedProperties"]:
            try:
                query_data = json.loads(self.__dict__["ExtendedProperties"])
            except json.JSONDecodeError:
                query_data = None
            if query_data and "Query" in query_data:
                return query_data["Query"]

    def subscription_filter(self, operator="=="):
        """Return a KQL subscription filter clause derived from the alert properties."""
        if self.is_in_log_analytics:
            return "true"
        if self.is_in_azure_sub:
            return (
                f"AzureResourceSubscriptionId {operator} "
                f"'{self._ids['AzSubscriptionId']}'"
            )
        if self.is_in_workspace:
            return f"WorkspaceId {operator} '{self._ids['WorkspaceId']}'"

        # Otherwise we default to including everything
        return "true"

    def get_entities_of_type(self, entity_type: str) -> List[Entity]:
        """
        Return entity collection for a give entity type.

        Parameters
        ----------
        entity_type : str, optional
            The entity type.

        Returns
        -------
        List[Entity]
            The entities matching `entity_type`.

        """
        class_type = Entity.ENTITY_NAME_MAP.get(entity_type, None)
        return [
            p
            for p in self.Entities
            if p["Type"] == entity_type or class_type and isinstance(p, class_type)
        ]

    def get_all_entities(self) -> pd.DataFrame:
        """
        Return a DataFrame of the Alert or Event entities.

        Returns
        -------
        DataFrame
            Pandas DataFrame of the Alert or Event entities.

        """
        entity = []
        ent_type = []
        for item in self.Entities:
            if "Address" in item:
                entity.append(item["Address"])
                ent_type.append(item["Type"])
            elif "Url" in item:
                entity.append(item["Url"])
                ent_type.append(item["Type"])
            elif "HostName" in item:
                entity.append(item["HostName"])
                ent_type.append(item["Type"])
            elif "Entity" in item:
                entity.append(item["Entity"])
                ent_type.append(item["Type"])
            elif item["Type"] == "account":
                entity.append(item["Name"])
                ent_type.append(item["Type"])

        return pd.DataFrame({"Entity": entity, "Type": ent_type})

    def to_html(self, show_entities: bool = False) -> str:
        """Return the item as HTML string."""
        html_doc = pd.DataFrame(self.__dict__).to_html()

        if self.__dict__ is not None and "ExtendedProperties" in self.__dict__:
            ext_prop_title = "<br/><h3>ExtendedProperties:</h3>"
            ext_prop_html = pd.DataFrame(
                pd.Series(self.__dict__["ExtendedProperties"])
            ).to_html()
            html_doc = html_doc + ext_prop_title + ext_prop_html

        if show_entities and self.Entities:
            entity_title = "<br/><h3>Entities:</h3><br/>"
            entity_html = "<br/>".join([ent for ent in self.Entities])
            html_doc = html_doc + entity_title + entity_html
        else:
            e_counts = Counter([ent["Type"] for ent in self.Entities])
            e_counts_str = ", ".join([f"{e}: {c}" for e, c in e_counts.items()])
            html_doc = html_doc + f"<h3>Entity counts: </h3>{e_counts_str}"
        return html_doc

    @staticmethod
    def _format_entity(entity):
        str_entity = str(entity)
        if str_entity:
            str_entity = str_entity.replace("\n", ", ")
        return html.escape(str_entity)

    @staticmethod
    def _get_subscription_from_resource(resource_id) -> Optional[str]:
        """Extract subscription Id from resource string."""
        sub_regex = r"^subscriptions([/]+)/"
        sub_ids = re.findall(sub_regex, resource_id, re.RegexFlag.I)
        if sub_ids:
            return sub_ids[0]

        return None
