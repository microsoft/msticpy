# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Incident Entity class."""
from typing import Any, Dict, List, Mapping, Optional

import pandas as pd

from ..._version import VERSION
from ...common.utility import export
from ..entities.alert import Alert
from ..entities.entity import Entity, camelcase_property_names

__version__ = VERSION
__author__ = "Pete Bryan"


# pylint: disable=invalid-name, too-many-instance-attributes
@export
class Incident(Entity):
    """Incident Entity class."""

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
        src_event_type: str
            The type of src_event, by default "Sentinel".

        Other Parameters
        ----------------
        kwargs : Dict[str, Any]
            Supply the entity properties as a set of
            kw arguments.

        """
        self.DisplayName: Optional[str] = None
        self.IncidentID: Optional[str] = None
        self.Severity: Optional[str] = None
        self.Status: Optional[str] = None
        self.Owner: Optional[Dict] = None
        self.Classification: Optional[str] = None
        self.Labels: Optional[List] = None
        self.Alerts: Optional[List] = None
        self.Entities: Optional[List] = None

        super().__init__(src_entity=src_entity, **kwargs)

        if src_entity:
            self._create_from_sent_event(src_entity)

        if isinstance(src_event, pd.Series) and src_event_type == "Sentinel":
            self._create_from_sent_event(src_event)

    @property
    def description_str(self) -> str:
        """Return Entity Description."""
        return f"{self.TimeGenerated} - {self.DisplayName} - {self.IncidentID}"

    @property
    def name_str(self) -> str:
        """Return Entity Description."""
        return f"Incident: {self.DisplayName}"

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
        self.StartTime = src_event["properties.firstActivityTimeUtc"]
        self.EndTime = src_event["properties.lastActivityTimeUtc"]
        if "Alerts" in src_event:
            self._add_alerts(src_event)

        if "Entities" in src_event:
            self.Entities = _create_entities(src_event["Entities"])

    def _add_alerts(self, src_event):
        """Add alerts to incident."""
        if src_event["Alerts"] and isinstance(src_event["Alerts"], list):
            new_alerts = [Alert(src_entity=alrt) for alrt in src_event["Alerts"]]
            self.Alerts = new_alerts

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
        "StartTime": None,
        "EndTime": None,
    }


def _create_entities(entities):
    """Create incident entities from API returned dicts."""
    new_ents = []
    for ent in entities:
        if isinstance(ent, tuple):
            ent_details = ent[1]
            ent_type = ent[0]
        elif isinstance(ent, dict):
            ent_details = ent
            ent_type = ent["Type"]
        else:
            ent_details = ent
            ent_type = "unknown"
        new_ent = camelcase_property_names(ent_details)
        ent_obj = Entity.ENTITY_NAME_MAP[ent_type.lower()](src_event=new_ent)
        new_ents.append(ent_obj)
    return new_ents
