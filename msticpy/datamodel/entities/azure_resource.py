# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""AzureResource Entity class."""
from typing import Any, Dict, Mapping, Optional

from ..._version import VERSION
from ...common.utility import export
from .entity import Entity

__version__ = VERSION
__author__ = "Ian Hellen"

# pylint: disable=invalid-name


@export
class AzureResource(Entity):
    """
    AzureResource Entity class.

    Attributes
    ----------
    ResourceId : str
        AzureResource ResourceId
    SubscriptionId : str
        AzureResource SubscriptionId
    ResourceIdParts : Dict[str, str]
        AzureResource ResourceIdParts


    """

    def __init__(self, src_entity: Mapping[str, Any] = None, **kwargs):
        """
        Create a new instance of the entity type.

        Parameters
        ----------
        src_entity : Mapping[str, Any], optional
            Create entity from existing entity or
            other mapping object that implements entity properties.
            (the default is None)

        Other Parameters
        ----------------
        kwargs : Dict[str, Any]
            Supply the entity properties as a set of
            kw arguments.

        """
        self.ResourceId: Optional[str] = None
        self.SubscriptionId: Optional[str] = None
        self.ResourceIdParts: Dict[str, str] = {}
        super().__init__(src_entity=src_entity, **kwargs)

    @property
    def description_str(self) -> str:
        """Return Entity Description."""
        return self.ResourceId or self.__name__

    _entity_schema = {
        # ResourceId (type System.String)
        "ResourceId": None,
        # SubscriptionId (type System.String)
        "SubscriptionId": None,
        # ResourceIdParts (type System.Collections.Generic.IReadOnlyDictionary`2
        # [System.String,System.String])
        "ResourceIdParts": None,
    }
