# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Security Alert class."""
import json
from typing import Any, Mapping, List, Dict

from ..entities import Entity, Alert

_FEATURES: List[str] = ['AdditionalData',
    'AlertLink',
    'AlertName',
    'AlertSeverity',
    'ConfidenceLevel',
    'ConfidenceScore',
    'Count',
    'Description',
    'ExtendedLinks',
    'ExtendedProperties',
    'IsIncident',
    'ProcessingEndTime',
    'ProductComponentName',
    'ProductName',
    'RemediationSteps',
    'ResourceId',
    'Severity',
    'SourceComputerId',
    'SourceSystem',
    'Status',
    'SystemAlertId',
    'SystemAlertIds',
    'Tactics',
    'Techniques',
    'TenantId',
    'VendorOriginalId',
    'WorkspaceResourceGroup',
    'WorkspaceSubscriptionId']

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

class SecurityAlert(Alert):
    """Security Alert Class."""
    def __init__(
        self,
        src_event: Mapping[str, Any] = None,
        src_entity: Mapping[str, Any] = None,
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
        super().__init__(src_entity, src_event, **kwargs)
        # Extract and cache alert ID properties
        self._ids: Dict[str, str] = {}
        if self._source_data is not None:
            for id_property in _ID_PROPERTIES:
                if id_property in self.properties:
                    self._ids[id_property] = self.properties[id_property]

    def _create_from_event(self, src_event):
        """Create an entity from an event."""
        for feature in _FEATURES:
            if feature not in self.__dict__:
                self.__dict__.update({feature: src_event.get(feature, None)})

    @property
    def entities(self) -> List[Entity]:
        """Return a list of the Security Alert entities."""
        return self.Entities

    @property
    def query_params(self) -> Dict[str, Any]:
        """
        Query parameters derived from alert.

        Returns
        -------
            Dict[str, Any]
                Dictionary of parameter names/value

        """
        params_dict = super().query_params
        if (
            "system_alert_id" not in params_dict
            or params_dict["system_alert_id"] is None
        ):
            params_dict["system_alert_id"] = self.SystemAlertId
        return params_dict

