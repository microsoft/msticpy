# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Url Entity class."""
from typing import Any, Dict, Mapping, Optional

from urllib3.exceptions import LocationParseError
from urllib3.util import parse_url

from ..._version import VERSION
from ...common.utility import export
from .entity import Entity

__version__ = VERSION
__author__ = "Ian Hellen"


# pylint: disable=invalid-name


@export
class Url(Entity):
    """
    URL Entity.

    Attributes
    ----------
    Url : str
        The URL
    DetonationVerdict : str
        The verdict of the URL detection

    """

    ID_PROPERTIES = ["Url"]

    def __init__(
        self,
        src_entity: Mapping[str, Any] = None,
        src_event: Mapping[str, Any] = None,
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
        self.Url: Optional[str] = None
        self.DetonationVerdict: Optional[str] = None
        super().__init__(src_entity=src_entity, **kwargs)
        if src_event:
            self._create_from_event(src_event)

    def _create_from_event(self, src_event):
        self.Url = src_event["Url"]
        if "AdditionalData" in src_event:
            self.DetonationVerdict = src_event["AdditionalData"]["DetonationVerdict"]

    @property
    def description_str(self) -> str:
        """Return Entity Description."""
        return self.Url or super().description_str

    @property
    def name_str(self) -> str:
        """Return Entity Name."""
        return self.Url or self.__class__.__name__

    def __getitem__(self, key: str):
        """Allow property get using dictionary key syntax."""
        if self.Url:
            val = _url_components(self.Url).get(key)
            if val:
                return val
        return super().__getitem__(key)

    def __getattr__(self, name: str):
        """Return the value of the named property 'name'."""
        if self.Url:
            val = _url_components(self.Url).get(name)
            if val:
                return val
        return super().__getattr__(name)

    _entity_schema: Dict[str, Any] = {
        # Url (type System.String)
        "Url": None,
        "DetonationVerdict": None,
        "TimeGenerated": None,
        "StartTime": None,
        "EndTime": None,
    }


def _url_components(url: str) -> Dict[str, str]:
    """Return parsed Url components as dict."""
    try:
        return parse_url(url)._asdict()
    except LocationParseError:
        return {}
