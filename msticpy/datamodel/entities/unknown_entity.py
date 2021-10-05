# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Threatintelligence Entity class."""
from typing import Any, Dict, Mapping

from ..._version import VERSION
from ...common.utility import export
from .entity import Entity

__version__ = VERSION
__author__ = "Ian Hellen"


# pylint: disable=invalid-name


@export
class UnknownEntity(Entity):
    """Generic Entity class."""

    def __init__(self, src_entity: Mapping[str, Any] = None, **kwargs):
        """
        Create a new instance of the entity type.

            :param src_entity: instantiate entity using properties of src entity
            :param kwargs: key-value pair representation of entity
        """
        super().__init__(src_entity=src_entity, **kwargs)
        if src_entity:
            self.__dict__.update(src_entity)

    @property
    def description_str(self) -> str:
        """Return Entity Description."""
        return "OtherEntity"

    @property
    def name_str(self) -> str:
        """Return Entity Name."""
        return self.__class__.__name__

    _entity_schema: Dict[str, Any] = {
        "TimeGenerated": None,
        "StartTime": None,
        "EndTime": None,
    }
