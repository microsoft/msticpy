# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Alert Entity class."""
from datetime import datetime
from typing import Any, List, Mapping, Optional, Tuple, Dict
import json

from ..._version import VERSION
from ...common.utility import export
from .entity import Entity, ent_camel

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
    StartTimeUtc : datetime
        Alert StartTimeUtc
    EndTimeUtc : datetime
        Alert EndTimeUtc
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

    def __init__(self, src_entity: Mapping[str, Any] = None, src_event: Mapping[str, Any] = None, **kwargs):
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
        self.StartTimeUtc: datetime = datetime.min
        self.EndTimeUtc: datetime = datetime.min
        self.Severity: Any = None
        self.SystemAlertIds: List[str] = []
        self.AlertType: Optional[str] = None
        self.VendorName: Optional[str] = None
        self.ProviderName: Optional[str] = None
        self.Entities: Optional[List] = None
        super().__init__(src_entity=src_entity, **kwargs)
        if src_entity is not None:
            if "AlertDisplayName" in src_entity:
                self.DisplayName = src_entity["AlertDisplayName"]
            if "SystemAlertId" in src_entity:
                self.SystemAlertIds.append(src_entity["SystemAlertId"])
            if "Name" in src_entity:
                self.DisplayName = src_entity["Name"]
            if "Entities" in src_entity and src_entity["Entities"]:
                if isinstance(src_entity["Entities"], str):
                    ents = _extract_entities(src_entity["Entities"])
                else:
                    ents = src_entity["Entities"]
                self.Entities = self._create_entities(ents)
            self._add_additional_data(src_entity)

        if src_event is not None:
            self._create_from_event(src_event)

    @property
    def description_str(self) -> str:
        """Return Entity Description."""
        return f"{self.DisplayName} ({self.StartTimeUtc}) {self.CompromisedEntity}"

    @property
    def name_str(self) -> str:
        """Return Entity Name."""
        return self.DisplayName or self.__class__.__name__

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
        self.DisplayName = src_event.get("DisplayName", src_event.get("Name"))
        self.CompromisedEntity = src_event.get("CompromisedEntity")
        self.StartTimeUtc = src_event.get("StartTime")
        self.EndTimeUtc = src_event.get("EndTime")
        self.Severity = src_event.get("AlertSeverity")
        self.SystemAlertIds = src_event.get("SystemAlertId", src_event.get("ID"))
        self.AlertType = src_event.get("AlertType")
        self.VendorName = src_event.get("VendorName")
        self.ProviderName = src_event.get("ProviderName")
        if isinstance(src_event["Entities"], str):
            ents = _extract_entities(src_event["Entities"])
        else:
            ents = src_event["Entities"]
        self.Entities = self._create_entities(ents)

    def _create_entities(self, entities):
        """Create alert entities from  returned dicts."""
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
            new_ent = ent_camel(ent_details)
            ent_obj = Entity.ENTITY_NAME_MAP[ent_type.lower()](src_event=new_ent)
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
        "StartTimeUtc": None,
        # EndTimeUtc (type System.Nullable`1[System.DateTime])
        "EndTimeUtc": None,
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
    }

    def to_html(self, show_entities=False) -> str:
        """Return the item as HTML string."""
        if self.properties:
            title = """
            <h3>Alert: '{name}'</h3>
            <b>Alert_time:</b> {start},
            <b>Compr_entity:</b> {entity},
            <b>Alert_id:</b> {id}
            """.format(
                start=self.properties.get(
                    "StartTimeUtc", self.properties.get("StartTime", "no timestamp")
                ),
                name=self.properties.get(
                    "AlertDisplayName",
                    self.properties.get("DisplayName", "no alert name"),
                ),
                entity=self.properties.get("CompromisedEntity", "unknown"),
                id=self.properties["SystemAlertId"],
            )
        else:
            title = "Alert has no data."
        return title + super().to_html(show_entities)

def _extract_entities(src_entities):
    input_entities = []
    try:
        input_entities += json.loads(src_entities)
    except json.JSONDecodeError:
        pass
    return input_entities
