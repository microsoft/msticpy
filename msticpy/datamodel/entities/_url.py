# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Url Entity class."""
from typing import Any, Dict, Mapping, Optional
from urllib3.exceptions import LocationParseError
from urllib3.util import parse_url, Url as Urllib3_Url

from ..._version import VERSION
from ...common.utility import export
from .entity import Entity

__version__ = VERSION
__author__ = "Ian Hellen"


# pylint: disable=invalid-name


@export
class Url(Entity):
    """URL Entity."""

    def __init__(self, src_entity: Mapping[str, Any] = None, **kwargs):
        """
        Create a new instance of the entity type.

            :param src_entity: instantiate entity using properties of src entity
            :param kwargs: key-value pair representation of entity
        """
        self.Url: Optional[str] = None
        super().__init__(src_entity=src_entity, **kwargs)

    @property
    def description_str(self) -> str:
        """Return Entity Description."""
        return f"{self.Url}"

    @property
    def parsed(self) -> Optional[Urllib3_Url]:
        """Return Urllib3-parsed URL."""
        if self.Url:
            try:
                return parse_url(self.Url)
            except LocationParseError:
                pass
        return None

    _entity_schema: Dict[str, Any] = {}
