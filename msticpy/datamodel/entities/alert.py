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
    DisplayName : str
        Alert DisplayName
    CompromisedEntity : str
        Alert CompromisedEntity
    Count : int
        Alert Count
    StartTime : datetime
        Alert StartTime
    EndTime : datetime
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
        self.StartTime: Optional[datetime] = None
        self.EndTime: Optional[datetime] = None
        self.Severity: Any = None
        self.SystemAlertIds: List[str] = []
        self.AlertType: Optional[str] = None
        self.VendorName: Optional[str] = None
        self.ProviderName: Optional[str] = None
        self.Entities: Optional[List] = None
        super().__init__(src_entity=src_entity, **kwargs)
        if src_entity:
            self._create_from_ent(src_entity)

        if isinstance(src_event, pd.Series) or src_event:
            self._create_from_event(src_event)

    def _create_from_ent(self, src_entity):  # noqa: MC0001
        if "StartTime" in src_entity or "TimeGenerated" in src_entity:
            self.TimeGenerated = src_entity["StartTime"] or src_entity["TimeGenerated"]
        if "EndTime" in src_entity:
            self.EndTime = src_entity["EndTime"]
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

    def _extract_entities(self, src_row):  # noqa: MC0001
        input_entities = []
        if isinstance(src_row.Entities, str):
            try:
                ext_props = json.loads(src_row["Entities"])
                for item in ext_props:
                    for k, v in item.items():
                        if isinstance(v, dict) and "$ref" in v.keys():
                            item[k] = [x for x in ext_props if x["$id"] == v["$ref"]][0]
                    input_entities.append(item)
            except json.JSONDecodeError:
                pass
        if isinstance(src_row.ExtendedProperties, str):
            try:
                ext_props = json.loads(src_row["ExtendedProperties"])
                for ent, val in ext_props.items():
                    if ent in ["IpAddress", "Username"]:
                        input_entities.append({"Entity": val, "Type": ent})
            except json.JSONDecodeError:
                pass
        return input_entities

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
        return f"Alert: {self.DisplayName}" or self.__class__.__name__

    def _add_additional_data(self, src_entity: Mapping[str, Any]):
        """Populate additional alert properties."""
        entity_props = set(self.__dict__.keys()) | {"AlertDisplayName", "SystemAlertId"}
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
            if prop_name not in entity_props:
                self.AdditionalData[prop_name] = prop

    def _create_from_event(self, src_event):
        """Create Alert from an alert event."""
        self.TimeGenerated = src_event.get("StartTime", src_event.get("TimeGenerated"))
        self.DisplayName = src_event.get("DisplayName", src_event.get("Name"))
        self.CompromisedEntity = src_event.get("CompromisedEntity")
        self.StartTime = src_event.get("StartTime")
        self.EndTime = src_event.get("EndTime")
        self.Severity = src_event.get("AlertSeverity")
        self.SystemAlertIds = src_event.get("SystemAlertId", src_event.get("ID"))
        self.AlertType = src_event.get("AlertType")
        self.VendorName = src_event.get("VendorName")
        self.ProviderName = src_event.get("ProviderName")
        if isinstance(src_event["Entities"], str):
            try:
                ents = _extract_entities(json.loads(src_event["Entities"]))
            except json.JSONDecodeError:
                ents = []
        else:
            ents = _extract_entities(src_event["Entities"])
        self.Entities = self._create_entities(ents)

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

    _entity_schema = {
        # DisplayName (type System.String)
        "DisplayName": None,
        # CompromisedEntity (type System.String)
        "CompromisedEntity": None,
        # Count (type System.Nullable`1[System.Int32])
        "Count": None,
        # StartTimeUtc (type System.Nullable`1[System.DateTime])
        "StartTime": None,
        # EndTimeUtc (type System.Nullable`1[System.DateTime])
        "EndTime": None,
        # Severity (type System.Nullable`1
        # [Microsoft.Azure.Security.Detection.AlertContracts.V3.Severity])
        "Severity": None,
        # SystemAlertIds (type System.Collections.Generic.List`1[System.String])
        "SystemAlertIds": None,
        # AlertType (type System.String)
        "AlertType": None,
        # VendorName (type System.String)
        "VendorName": None,
        # ProviderName (type System.String)
        "ProviderName": None,
        # List of associated entities
        "Entities": None,
        # Time the alert was generated.
        "TimeGenerated": None,
    }

    def to_html(self, show_entities=False) -> str:
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
            out_ents.append(_find_og_ent(entity, base_ents))
        else:
            for k, val in entity.items():
                if isinstance(val, (list, dict)):
                    if isinstance(val, list):
                        nested_ents = []
                        for item in val:
                            if isinstance(item, dict) and "$ref" in item:
                                nested_ents.append(_find_og_ent(item, base_ents))
                                entity[k] = nested_ents
                    elif isinstance(val, dict) and "$ref" in val:
                        entity[k] = _find_og_ent(val, base_ents)
            out_ents.append(entity)
    return out_ents


def _find_og_ent(ent, base_ents):
    """Find the original entity referenced by $ref entity."""
    id = ent["$ref"]
    return next(bent for bent in base_ents if ("$id" in bent) and bent["$id"] == id)


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
