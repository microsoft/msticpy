# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Incident Entity class."""
from datetime import datetime
from typing import Any, List, Mapping, Optional, Dict, Tuple

from ..._version import VERSION
from ...common.utility import export
from ..entities.entity import Entity, ent_camel
from ..entities.alert import Alert

__version__ = VERSION
__author__ = "Pete Bryan"


@export
class Incident(Entity):
    """
    Incident Entity class.

    Attributes
    ----------


    """

    ID_PROPERTIES = ["IncidentID"]

    def __init__(
        self,
        src_entity: Mapping[str, Any] = None,
        src_event: Mapping[str, Any] = None,
        src_event_type: str = "Sentinel",
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
        self.TimeGenerated: Optional[datetime] = None
        self.DisplayName: Optional[str] = None
        self.IncidentID: Optional[str] = None
        self.Severity: Optional[str] = None
        self.Status: Optional[str] = None
        self.Owner: Optional[Dict] = None
        self.Classification: Optional[str] = None
        self.Labels: Optional[List] = None
        self.Alerts: Optional[List] = None
        self.Entities: Optional[List] = None
        self.AdditionalData: Optional[Dict] = None

        super().__init__(src_entity=src_entity, **kwargs)
        if src_event is not None:
            if src_event_type == "Sentinel":
                self._create_from_sent_event(src_event)

    @property
    def description_str(self) -> str:
        """Return Entity Description."""
        return f"{self.TimeGenerated} - {self.DisplayName} - {self.IncidentID}"

    @property
    def name_str(self) -> str:
        """Return Entity Description."""
        return self.DisplayName

    def _create_from_sent_event(self, src_event):
        self.TimeGenerated = src_event["properties.createdTimeUtc"]
        self.DisplayName = src_event["properties.title"]
        self.IncidentID = src_event["id"].split("/")[-1]
        self.Severity = src_event["properties.severity"]
        self.Status = src_event["properties.status"]
        if "properties.classification" in src_event:
            self.Classification = src_event["properties.classification"]
        self.Labels = src_event["properties.labels"]
        owner = {
            "Name": src_event["properties.owner.assignedTo"],
            "Email": src_event["properties.owner.email"],
            "UPN": src_event["properties.owner.userPrincipalName"],
            "ID": src_event["properties.owner.objectId"],
        }
        self.Owner = owner
        additionaldata = {
            "Alert Count": src_event["properties.additionalData.alertsCount"],
            "Bookmarks Count": src_event["properties.additionalData.bookmarksCount"],
            "Comments Count": src_event["properties.additionalData.commentsCount"],
            "Products": src_event["properties.additionalData.alertProductNames"],
            "Tactics": src_event["properties.additionalData.tactics"],
            "Related Rule IDs": src_event["properties.relatedAnalyticRuleIds"],
            "Incident Number": src_event["properties.incidentNumber"],
        }
        self.AdditionalData = additionaldata
        if "Alerts" in src_event:
            self._add_alerts(src_event)

        if "Entities" in src_event:
            self.Entities = self._create_entities(src_event["Entities"])

    def _add_alerts(self, src_event):
        """Add alerts to incident."""
        new_alerts = []
        for alrt in src_event["Alerts"]:
            new_alerts.append(Alert(src_entity=alrt))
        self.Alerts = new_alerts

    def _create_entities(self, entities):
        """Create incident entities from API returned dicts."""
        new_ents = []
        for ent in entities:
            if isinstance(ent, Tuple):
                ent_details = ent[1]
                ent_type = ent[0]
            if isinstance(ent, Dict):
                ent_details = ent
                ent_type = ent["Type"]
            new_ent = ent_camel(ent_details)
            ent_obj = Entity.ENTITY_NAME_MAP[ent_type.lower()](src_event=new_ent)
            new_ents.append(ent_obj)
        return new_ents

    _entity_schema = {
        # Time the Incident was Generated
        "TimeGenerated": None,
        # Incident Name
        "DisplayName": None,
        # Some unique identifier
        "IncidentID": None,
        # Incident severity
        "Severity": None,
        # Current status of the incident i.e. Open, Closed, etc
        "Status": None,
        # Object containing details of incident owner, could be email, display name, etc
        "Owner": None,
        # Post triage setting i.e. FP, TP, etc
        "Classification": None,
        # List of labels applied to an icident
        "Labels": None,
        # Assocaited alert GUIDs
        "Alerts": None,
        # List of associated entities
        "Entities": None,
        # Dynamic bag of other items
        "AdditionalData ": None,
    }
