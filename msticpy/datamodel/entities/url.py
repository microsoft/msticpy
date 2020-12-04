# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Url Entity class."""
from typing import Any, Dict, Mapping

from ..._version import VERSION
from ...common.utility import export
from ...sectools.domain_utils import url_components
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
        super().__init__(src_entity=src_entity, **kwargs)
        if self.Url:
            self.__dict__.update(url_components(self.Url))

    @property
    def description_str(self) -> str:
        """Return Entity Description."""
        return f"{self.Url}"

    # # We need to do some trickery with the Url defined as
    # # a property since base Entity class expects to be able to set
    # # attributes directly in self.__dict__
    # @property
    # def Url(self) -> Optional[str]:
    #     """Return Url."""
    #     if self._url is None and "Url" in self.__dict__:
    #         self.Url = self.__dict__["Url"]
    #     return self._url

    # @Url.setter
    # def Url(self, url):
    #     """Return host component of Url."""
    #     self._url = url
    #     if url:
    #         self.__dict__.update(url_components(url))

    _entity_schema: Dict[str, Any] = {}
