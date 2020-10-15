# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Threatintelligence Entity class."""
import pprint
from abc import ABC, abstractmethod
from enum import Enum
from ipaddress import IPv4Address, IPv6Address, ip_address
from typing import Any, Dict, Mapping, Type, Union, Optional

from ..._version import VERSION
from ...common.utility import export
from .entity import Entity

__version__ = VERSION
__author__ = "Ian Hellen"


_ENTITY_ENUMS: Dict[str, Type] = {}


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

    def __init__(self, src_entity: Mapping[str, Any] = None, **kwargs):
        """
        Create a new instance of the entity type.

            :param src_entity: instantiate entity using properties of src entity
            :param kwargs: key-value pair representation of entity
        """
        super().__init__(src_entity=src_entity, **kwargs)

    @property
    def description_str(self) -> str:
        """Return Entity Description."""
        return f"{self.DisplayName} ({self.StartTimeUtc}) {self.CompromisedEntity}"

    _entity_schema = {
        # String Name of the provider from whom this
        # Threat Intelligence information was received
        "ProviderName": None,
        "ThreatType": None,
        "ThreatName": None,
        "Confidence": None,
        "ReportLink": None,
        "ThreatDescription": None,
    }
