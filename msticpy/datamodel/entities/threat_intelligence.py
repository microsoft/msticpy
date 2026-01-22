# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Threatintelligence Entity class."""

from __future__ import annotations

from collections.abc import Mapping
from typing import Any

from ..._version import VERSION
from ...common.utility import export
from .entity import Entity

__version__ = VERSION
__author__ = "Ian Hellen"


# pylint: disable=invalid-name


@export
class Threatintelligence(Entity):
    """
    Threatintelligence Entity class.

    Attributes
    ----------
    ProviderName : str
        Threatintelligence ProviderName
    ThreatType : str
        Threatintelligence ThreatType
    ThreatName : str
        Threatintelligence ThreatName
    Confidence : str
        Threatintelligence Confidence
    ReportLink : str
        Threatintelligence ReportLink
    ThreatDescription : str
        Threatintelligence ThreatDescription

    """

    ID_PROPERTIES = ["ProviderName", "ThreatName", "ReportLink"]

    def __init__(self, src_entity: Mapping[str, Any] = None, **kwargs):
        """
        Create a new instance of the entity type.

            :param src_entity: instantiate entity using properties of src entity
            :param kwargs: key-value pair representation of entity
        """
        self.ProviderName: str | None = None
        self.ThreatType: str | None = None
        self.ThreatName: str | None = None
        self.Confidence: str | None = None
        self.ReportLink: str | None = None
        self.ThreatDescription: str | None = None
        super().__init__(src_entity=src_entity, **kwargs)

    @property
    def description_str(self) -> str:
        """Return Entity Description."""
        return f"{self.ThreatName} ({self.ThreatType})"

    @property
    def name_str(self) -> str:
        """Return Entity Name."""
        return self.ThreatName or self.__class__.__name__

    _entity_schema = {
        # String Name of the provider from whom this
        # Threat Intelligence information was received
        "ProviderName": None,
        "ThreatType": None,
        "ThreatName": None,
        "Confidence": None,
        "ReportLink": None,
        "ThreatDescription": None,
        "TimeGenerated": None,
        "StartTime": None,
        "EndTime": None,
    }
