# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Alert Entity class."""
import json
from datetime import datetime
from typing import Any, Dict, List, Mapping, Optional, Tuple

import pandas as pd

from ..._version import VERSION
from ...common.utility import export
from .entity import Entity, camelcase_property_names

__version__ = VERSION
__author__ = "Ian Hellen"


# pylint: disable=invalid-name


@export
class Alert(Entity):
    """
    Alert Entity class.

    Attributes
    ----------
    AlertDisplayName : str
        Alert DisplayName
    CompromisedEntity : str
        Alert CompromisedEntity
    Count : int
        Alert Count
    StartTimeUtc : datetime
        Alert StartTime
    EndTimeUtc : datetime
        Alert EndTime
    Severity : str
        Alert Severity
    SystemAlertIds : List[str]
        Alert SystemAlertIds
    AlertType : str
        Alert AlertType
    VendorName : str
        Alert VendorName
    ProviderName : str
        Alert ProviderName

    """

    ID_PROPERTIES = ["SystemAlertIds"]

    def __init__(
        self,
        src_entity: Mapping[str, Any] = None,
        src_event: Mapping[str, Any] = None,
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
            Create entity from event properties
            (the default is None)


        Other Parameters
        ----------------
        kwargs : Dict[str, Any]
            Supply the entity properties as a set of
            kw arguments.

        """
        self.DisplayName: Optional[str] = None
        self.CompromisedEntity: Optional[str] = None
        self.Count: Any = None
        self.StartTimeUtc: Optional[datetime] = None
        self.EndTimeUtc: Optional[datetime] = None
        self.Severity: Any = None
        self.SystemAlertIds: List[str] = []
        self.AlertType: Optional[str] = None
        self.VendorName: Optional[str] = None
        self.ProviderName: Optional[str] = None
        self.Entities: Optional[List] = None
        super().__init__(src_entity=src_entity, **kwargs)
        if src_entity is not None:
            self._create_from_ent(src_entity)

        if isinstance(src_event, pd.Series) and not src_event.empty:
            self._create_from_event(src_event)

    def _create_from_ent(self, src_entity):  # noqa: MC0001
        if "StartTime" in src_entity:
            self.TimeGeneratedUtc = src_entity["StartTime"]
        if "TimeGenerated" in src_entity:
            self.TimeGeneratedUtc = src_entity["TimeGenerated"]
        if "EndTime" in src_entity:
            self.EndTimeUtc = src_entity["EndTime"]
        if "StartTime" in src_entity:
            self.StartTime = src_entity["StartTime"]
        if "AlertDisplayName" in src_entity:
            self.DisplayName = src_entity["AlertDisplayName"]
        if "SystemAlertId" in src_entity:
            self.SystemAlertIds.append(src_entity["SystemAlertId"])
        elif "ID" in src_entity:
            self.SystemAlertIds.append(src_entity["ID"])
        if "Name" in src_entity:
            self.DisplayName = src_entity["Name"]
        if "Entities" in src_entity and src_entity["Entities"]:
            if isinstance(src_entity["Entities"], str):
                try:
                    ents = _extract_entities(json.loads(src_entity["Entities"]))
                except json.JSONDecodeError:
                    ents = []
            else:
                ents = _extract_entities(src_entity["Entities"])
            self.Entities = self._create_entities(ents)
        self._add_additional_data(src_entity)

    @property
    def description_str(self) -> str:
        """Return Entity Description."""
        if self.StartTime and self.CompromisedEntity:
            return f"{self.DisplayName} ({self.StartTime}) {self.CompromisedEntity}"
        else:
            return f"{self.DisplayName} - {self.SystemAlertIds}"

    @property
    def name_str(self) -> str:
        """Return Entity Name."""
        alert_name = self.AlertDisplayName or self.DisplayName or None
        return f"Alert: {alert_name}" or self.__class__.__name__

    def _add_additional_data(self, src_entity: Mapping[str, Any]):
        """Populate additional alert properties."""
        if isinstance(src_entity, dict):
            prop_list = src_entity.items()
        elif type(src_entity).__name__ == "SecurityAlert":
            prop_list = src_entity.properties.items()  # type: ignore
        # pylint: disable=all
        elif isinstance(src_entity, Mapping):
            prop_list = src_entity.iteritems()  # type: ignore
        # pylint: enable=all
        else:
            return

        for prop_name, prop in prop_list:
            if prop_name not in self._entity_schema:
                self.AdditionalData[prop_name] = prop
            elif prop_name not in self.__dict__:
                self.__dict__[prop_name] = prop
            else:
                continue

    def _create_from_event(self, src_event):
        """Create Alert from an alert event."""
        self.TimeGenerated = src_event.get("StartTime", src_event.get("TimeGenerated"))
        self.AlertDisplayName = src_event.get(
            "AlertDisplayName", src_event.get("DisplayName", src_event.get("Name"))
        )
        self.StartTimeUtc = src_event.get("StartTimeUtc", src_event.get("StartTime"))
        self.EndTimeUtc = src_event.get("EndTimeUtc", src_event.get("EndTime"))
        self.Severity = src_event.get("Severity", src_event.get("AlertSeverity"))
        self.SystemAlertIds = src_event.get("SystemAlertId", src_event.get("ID"))
        if isinstance(src_event["Entities"], str):
            try:
                ents = _extract_entities(json.loads(src_event["Entities"]))
            except json.JSONDecodeError:
                ents = []
        else:
            ents = _extract_entities(src_event["Entities"])
        self.Entities = self._create_entities(ents)
        for ent in self._entity_schema:
            if ent not in self.__dict__:
                self.__dict__[ent] = src_event.get(ent)
        if "ExtendedProperties" in src_event:
            ext_props = json.loads(src_event["ExtendedProperties"])
            self._add_additional_data(ext_props)

    _entity_schema = {
        # CompromisedEntity (type String)
        "CompromisedEntity": None,
        # Count (type Int)
        "Count": None,
        # StartTimeUtc (type Datetime)
        "StartTimeUtc": None,
        # EndTimeUtc (type Datetime)
        "EndTimeUtc": None,
        # Severity (type String)
        "Severity": None,
        # SystemAlertIds (type String)
        "SystemAlertId": None,
        # AlertType (type System.String)
        "AlertType": None,
        # VendorName (type System.String)
        "VendorName": None,
        # ProviderName (type System.String)
        "ProviderName": None,
        # List of associated entities (type List)
        "Entities": None,
        # Time the alert was generated (type String)
        "TimeGenerated": None,
        # The product that generated the alert (type String)
        "ProductName": None,
        # The product component that generated the alert (type String)
        "ProductComponentName": None,
        # The version of the product generating the alert, if relevant (type String)
        "ProductVersion": None,
        # The time the alert was made available for consumption (type String)
        "ProcessingEndTime": None,
        # The life cycle status of the alert. This field is optional and all alerts would have the status (type String)
        "Status": None,
        # The alert provider or product internal life cycle status (type String)
        "ProviderAlertStatus": None,
        # The confidence level of this alert (type String)
        "ConfidenceLevel": None,
        # The confidence score of the alert (type Float)
        "ConfidenceScore": None,
        # The confidence score calculation status (type String)
        "ConfidenceScoreStatus": None,
        # A list of reasons for the confidence level of this alert (type List)
        "ConfidenceReasons": None,
        # The kill chain related intent behind the alert (type String)
        "Intent": None,
        # The kill chain related techniques behind the alert (type List)
        "Techniques": None,
        # The kill chain related sub-techniques behind the alert (type List)
        "SubTechniques": None,
        # If the alert is an incident or a regular alert (type Bool)
        "IsIncident": None,
        # If the alert is in preview (type Bool)
        "IsPreview": None,
        # Unique id for the specific alert instance set by the provider (type String)
        "ProviderAlertId": None,
        # Key to correlate multiple alerts together (type String)
        "CorrelationKey": None,
        # Identifiers of the Investigations created by the provider for the Alert (type List)
        "InvestigationIds": None,
        # The resource identifiers for this alert (type List)
        "ResourceIdentifiers": None,
        # Display name of the main entity being reported on (type String)
        "CompromisedEntity": None,
        # The display name of the alert (type String)
        "AlertDisplayName": None,
        # Alert description (type String)
        "Description": None,
        # Description arguments to build up Description field in placeholders (type Dict)
        "DescriptionArguments": None,
        # SupportingEvidence (type Dict)
        "SupportingEvidence": None,
        # Manual action items to take to remediate the alert (type List)
        "RemediationSteps": None,
        # A bag of fields which will be presented to the use (type Dict)
        "ExtendedProperties": None,
        # A bag for all links related to the alert (type Dict)
        "ExtendedLinks": None,
        # Metadata associated with the alert (type Dict)
        "Metadata": None,
        # A list of edges contained in this alert (type Dict)
        "Edges": None,
        # A direct link to view the specific alert in originating product portal (type String)
        "AlertUri": None,
        # Used to provide details about an anomaly in the data found by ML algorithms (type Dict)
        "Anomaly": None,
        # Used to provide details about a policy assocaited with the alert (type Dict)
        "AlertPolicy": None,
    }

    def _create_entities(self, entities):
        """Create alert entities from returned dicts."""
        new_ents = []
        for ent in entities:
            if isinstance(ent, Tuple):
                ent_details = ent[1]
                ent_type = ent[0]
            elif isinstance(ent, Dict):
                ent_details = ent
                ent_type = ent.get("Type", "Unknown")
            else:
                ent_details = ent
                ent_type = "Unknown"
            new_ent = camelcase_property_names(ent_details)
            ent_obj = Entity.instantiate_entity(
                new_ent, entity_type=Entity.ENTITY_NAME_MAP[ent_type.lower()]
            )
            new_ents.append(ent_obj)
        return new_ents

    def to_html(self) -> str:
        """Return the item as HTML string."""
        return (
            """
            <h3>Alert: '{name}'</h3>
            <b>Alert_time:</b> {start},
            <b>Compr_entity:</b> {entity},
            <b>Alert_id:</b> {id}
            """.format(
                start=self.properties.get(
                    "StartTimeUtc",
                    self.properties.get("StartTime", "no timestamp"),
                ),
                name=self.properties.get(
                    "AlertDisplayName",
                    self.properties.get("DisplayName", "no alert name"),
                ),
                entity=self.properties.get("CompromisedEntity", "unknown"),
                id=self.properties.get("SystemAlertId", "unknown"),
            )
            if self.properties
            else "Alert has no data."
        )


def _extract_entities(ents: list):
    """Extract all entities from a set and replace $ref elements."""
    base_ents = _generate_base_ents(ents)
    out_ents = []
    for entity in ents:
        if isinstance(entity, dict) and "$ref" in entity:
            out_ents.append(_find_original_entity(entity, base_ents))
        else:
            for k, val in entity.items():
                if isinstance(val, (list, dict)):
                    if isinstance(val, list):
                        nested_ents = []
                        for item in val:
                            if isinstance(item, dict) and "$ref" in item:
                                nested_ents.append(
                                    _find_original_entity(item, base_ents)
                                )
                                entity[k] = nested_ents
                    elif isinstance(val, dict) and "$ref" in val:
                        entity[k] = _find_original_entity(val, base_ents)
            out_ents.append(entity)
    return out_ents


def _find_original_entity(ent, base_ents):
    """Find the original entity referenced by $ref entity."""
    try:
        id = ent["$ref"]
        return next(bent for bent in base_ents if ("$id" in bent) and bent["$id"] == id)
    except StopIteration:
        return ent


def _generate_base_ents(ents: list) -> list:  # noqa: MC0001
    """Generate a list of all enties form a set of nested entities."""
    base_ents = []
    for ent in ents:
        base_ents.append(ent)
        for _, item in ent.items():
            if isinstance(item, list):
                for prop in item:
                    if isinstance(prop, dict) and "$id" in prop.keys():
                        base_ents.append(prop)
                        for val in prop:
                            if isinstance(prop[val], list):
                                for p in prop[val]:
                                    if isinstance(p, dict) and "$id" in p.keys():
                                        base_ents.append(p)
                            elif (
                                isinstance(prop[val], dict)
                                and "$id" in prop[val].keys()
                            ):
                                base_ents.append(val)
            elif isinstance(item, dict) and "$id" in item.keys():
                base_ents.append(item)
    return base_ents
